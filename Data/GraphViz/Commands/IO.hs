{-# LANGUAGE MultiParamTypeClasses #-}

{- |
   Module      : Data.GraphViz.Commands.IO
   Description : IO-related functions for graphviz.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   Various utility functions to help with custom I\/O of Dot code.
-}
module Data.GraphViz.Commands.IO
       ( -- * Encoding
         -- $encoding
         toUTF8
         -- * Operations on files
       , writeDotFile
       , readDotFile
         -- * Operations on handles
       , hPutDot
       , hPutCompactDot
       , hGetDot
       , hGetStrict
         -- * Special cases for standard input and output
       , putDot
       , readDot
         -- * Running external commands
       , runCommand
       ) where

import Data.GraphViz.Exception
import Data.GraphViz.Printing       (runDotCode, toDot)
import Data.GraphViz.Types          (ParseDotRepr, PrintDotRepr, parseDotGraph,
                                     printDotGraph)
import Text.PrettyPrint.Leijen.Text (displayT, renderOneLine)

import           Control.Concurrent       (MVar, forkIO, newEmptyMVar, putMVar,
                                           takeMVar)
import           Control.Exception        (IOException, evaluate, finally)
import           Control.Monad            (liftM)
import qualified Data.ByteString          as SB
import           Data.ByteString.Lazy     (ByteString)
import qualified Data.ByteString.Lazy     as B
import           Data.Text.Encoding.Error (UnicodeException)
import           Data.Text.Lazy           (Text)
import qualified Data.Text.Lazy.Encoding  as T
import           System.Exit              (ExitCode(ExitSuccess))
import           System.FilePath          ((<.>))
import           System.IO                (Handle, IOMode(ReadMode, WriteMode),
                                           hClose, hGetContents, hPutChar,
                                           stdin, stdout, withFile)
import           System.IO.Temp           (withSystemTempFile)
import           System.Process           (runInteractiveProcess,
                                           waitForProcess)


-- -----------------------------------------------------------------------------

-- | Correctly render Graphviz output in a more machine-oriented form
--   (i.e. more compact than the output of 'renderDot').
renderCompactDot :: (PrintDotRepr dg n) => dg n -> Text
renderCompactDot = displayT . renderOneLine
                   . runDotCode
                   . toDot

-- -----------------------------------------------------------------------------
-- Encoding

{- $encoding
  By default, Dot code should be in UTF-8.  However, by usage of the
  /charset/ attribute, users are able to specify that the ISO-8859-1
  (aka Latin1) encoding should be used instead:
  <http://www.graphviz.org/doc/info/attrs.html#d:charset>

  To simplify matters, graphviz does /not/ work with ISO-8859-1.  If
  you wish to deal with existing Dot code that uses this encoding, you
  will need to manually read that file in to a 'Text' value.

  If a non-UTF-8 encoding is used, then a 'GraphvizException' will
  be thrown.
-}

-- | Explicitly convert a (lazy) 'ByteString' to a 'Text' value using
--   UTF-8 encoding, throwing a 'GraphvizException' if there is a
--   decoding error.
toUTF8 :: ByteString -> Text
toUTF8 = mapException fE . T.decodeUtf8
  where
    fE   :: UnicodeException -> GraphvizException
    fE e = NotUTF8Dot $ show e

-- -----------------------------------------------------------------------------
-- Low-level Input/Output

-- | Output the @DotRepr@ to the specified 'Handle'.
hPutDot :: (PrintDotRepr dg n) => Handle -> dg n -> IO ()
hPutDot = toHandle printDotGraph

-- | Output the @DotRepr@ to the spcified 'Handle' in a more compact,
--   machine-oriented form.
hPutCompactDot :: (PrintDotRepr dg n) => Handle -> dg n -> IO ()
hPutCompactDot = toHandle renderCompactDot

toHandle        :: (dg n -> Text) -> Handle -> dg n -> IO ()
toHandle f h dg = do B.hPutStr h . T.encodeUtf8 $ f dg
                     hPutChar h '\n'

-- | Strictly read in a 'Text' value using an appropriate encoding.
hGetStrict :: Handle -> IO Text
hGetStrict = liftM (toUTF8 . B.fromChunks . (:[]))
             . SB.hGetContents

-- | Read in and parse a @DotRepr@ value from the specified 'Handle'.
hGetDot :: (ParseDotRepr dg n) => Handle -> IO (dg n)
hGetDot = liftM parseDotGraph . hGetStrict

-- | Write the specified @DotRepr@ to file.
writeDotFile   :: (PrintDotRepr dg n) => FilePath -> dg n -> IO ()
writeDotFile f = withFile f WriteMode . flip hPutDot

-- | Read in and parse a @DotRepr@ value from a file.
readDotFile   :: (ParseDotRepr dg n) => FilePath -> IO (dg n)
readDotFile f = withFile f ReadMode hGetDot

-- | Print the specified @DotRepr@ to 'stdout'.
putDot :: (PrintDotRepr dg n) => dg n -> IO ()
putDot = hPutDot stdout

-- | Read in and parse a @DotRepr@ value from 'stdin'.
readDot :: (ParseDotRepr dg n) => IO (dg n)
readDot = hGetDot stdin

-- -----------------------------------------------------------------------------

-- | Run an external command on the specified @DotRepr@.  Remember to
--   use 'hSetBinaryMode' on the 'Handle' for the output function if
--   necessary.
--
--   If the command was unsuccessful, then a 'GraphvizException' is
--   thrown.
--
--   For performance reasons, a temporary file is used to store the
--   generated Dot code.  As such, this is only suitable for local
--   commands.
runCommand :: (PrintDotRepr dg n)
              => String           -- ^ Command to run
              -> [String]         -- ^ Command-line arguments
              -> (Handle -> IO a) -- ^ Obtaining the output; should be strict.
              -> dg n
              -> IO a
runCommand cmd args hf dg
  = handle (throwIO . notRunnable) $
    withSystemTempFile ("graphviz" <.> "gv") $ \dotFile dotHandle -> do
      finally (hPutCompactDot dotHandle dg) (hClose dotHandle)
      bracket
        (runInteractiveProcess cmd (args ++ [dotFile]) Nothing Nothing)
        (\(inh,outh,errh,_) -> hClose inh >> hClose outh >> hClose errh)
        $ \(inp,outp,errp,prc) -> do

          -- Not using it, so close it off directly.
          hClose inp

          -- Need to make sure both the output and error handles are
          -- really fully consumed.
          mvOutput <- newEmptyMVar
          mvErr    <- newEmptyMVar

          forkIO $ signalWhenDone hGetContents' errp mvErr
          forkIO $ signalWhenDone hf' outp mvOutput

          -- When these are both able to be taken, then the forks are finished
          err <- takeMVar mvErr
          output <- takeMVar mvOutput

          exitCode <- waitForProcess prc

          case exitCode of
            ExitSuccess -> return output
            _           -> throw . GVProgramExc $ othErr ++ err
  where
    notRunnable   :: IOException -> GraphvizException
    notRunnable e = GVProgramExc $ unwords
                    [ "Unable to call the command "
                    , cmd
                    , " with the arguments: \""
                    , unwords args
                    , "\" because of: "
                    , show e
                    ]

    -- Augmenting the hf function to let it work within the forkIO:
    hf' = handle (throwIO . fErr) . hf
    fErr :: IOException -> GraphvizException
    fErr e = GVProgramExc $ "Error re-directing the output from "
             ++ cmd ++ ": " ++ show e

    othErr = "Error messages from " ++ cmd ++ ":\n"

-- -----------------------------------------------------------------------------
-- Utility functions

-- | A version of 'hGetContents' that fully evaluates the contents of
--   the 'Handle' (that is, until EOF is reached).  The 'Handle' is
--   not closed.
hGetContents'   :: Handle -> IO String
hGetContents' h = do r <- hGetContents h
                     evaluate $ length r
                     return r

-- | Store the result of the 'Handle' consumption into the 'MVar'.
signalWhenDone        :: (Handle -> IO a) -> Handle -> MVar a -> IO ()
signalWhenDone f h mv = f h >>= putMVar mv >> return ()
