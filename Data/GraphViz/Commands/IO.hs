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
       ) where

import Data.GraphViz.State(initialState)
import Data.GraphViz.Types(DotRepr, printDotGraph, parseDotGraph)
import Data.GraphViz.Printing(toDot)
import Data.GraphViz.Exception
import Text.PrettyPrint.Leijen.Text(displayT, renderCompact)

import qualified Data.Text.Lazy.Encoding as T
import Data.Text.Encoding.Error(UnicodeException(DecodeError))
import Data.Text.Lazy(Text)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy(ByteString)
import Control.Monad(liftM)
import Control.Monad.Trans.State
import System.IO(Handle,IOMode(ReadMode,WriteMode),withFile,stdout,stdin,hPutChar)

-- -----------------------------------------------------------------------------

-- | Correctly render Graphviz output in a more machine-oriented form
--   (i.e. more compact than the output of 'renderDot').
renderCompactDot :: (DotRepr dg n) => dg n -> Text
renderCompactDot = displayT . renderCompact
                   . flip evalState initialState
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

  If a file uses a non-UTF-8 encoding, then a 'GraphvizException' will
  be thrown.
-}

-- | Read a UTF-8 encoded (lazy) 'ByteString', throwing a
--   'GraphvizException' if there is a decoding error.
toUTF8 :: ByteString -> Text
toUTF8 = mapException (\e@DecodeError{} -> NotUTF8Dot $ show e)
         . T.decodeUtf8

-- -----------------------------------------------------------------------------
-- Output

hPutDot :: (DotRepr dg n) => Handle -> dg n -> IO ()
hPutDot = toHandle printDotGraph

hPutCompactDot :: (DotRepr dg n) => Handle -> dg n -> IO ()
hPutCompactDot = toHandle renderCompactDot

toHandle        :: (DotRepr dg n) => (dg n -> Text) -> Handle -> dg n
                   -> IO ()
toHandle f h dg = do B.hPutStr h . T.encodeUtf8 $ f dg
                     hPutChar h '\n'

-- | Strictly read in a 'Text' value using an appropriate encoding.
hGetStrict :: Handle -> IO Text
hGetStrict = liftM (toUTF8 . B.fromChunks . (:[]))
             . SB.hGetContents

hGetDot :: (DotRepr dg n) => Handle -> IO (dg n)
hGetDot = liftM parseDotGraph . hGetStrict

writeDotFile   :: (DotRepr dg n) => FilePath -> dg n -> IO ()
writeDotFile f = withFile f WriteMode . flip hPutDot

readDotFile   :: (DotRepr dg n) => FilePath -> IO (dg n)
readDotFile f = withFile f ReadMode hGetDot

putDot :: (DotRepr dg n) => dg n -> IO ()
putDot = hPutDot stdout

readDot :: (DotRepr dg n) => IO (dg n)
readDot = hGetDot stdin
