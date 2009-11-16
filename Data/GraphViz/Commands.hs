{- |
   Module      : Data.GraphViz.Commands
   Description : Functions to run Graphviz commands.
   Copyright   : (c) Matthew Sackman, Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines functions to call the various Graphviz
   commands.  It is based upon code originally written for version 0.5
   of /Graphalyze/:
     <http://hackage.haskell.org/cgi-bin/hackage-scripts/package/Graphalyze-0.5>

   Whilst various output formats are supported (see 'GraphvizOutput'
   for a complete list), it is not yet possible to choose a desired
   renderer and formatter.  Being able to determine which renderers
   and formatters are applicable for a specific 'GraphvizOutput' is
   not easy (there is no listing of available renderers or formatters
   on the Graphviz website), and for the most part the default ones do
   the job well.
-}
module Data.GraphViz.Commands
    ( -- * The different Graphviz tools available.
      GraphvizCommand(..)
    , dirCommand
    , undirCommand
    , commandFor
      -- * The possible outputs that Graphviz supports.
    , GraphvizResult
    , GraphvizOutput(..)
    , GraphvizCanvas(..)
      -- * Running Graphviz.
    , RunResult(..)
    , runGraphviz
    , runGraphvizCommand
    , addExtension
    , runGraphvizCanvas
    , runGraphvizCanvas'
    , graphvizWithHandle
    )
    where

-- Want to use the extensible-exception version
import Prelude hiding (catch)

import Data.GraphViz.Types
import Data.GraphViz.Types.Printing
-- This is here just for Haddock linking purposes.
import Data.GraphViz.Attributes(Attribute(Z))

import System.IO( Handle, IOMode(WriteMode), hClose, hPutStrLn
                , hGetContents, openFile, hSetBinaryMode)
import System.Exit(ExitCode(ExitSuccess))
import System.Process(runInteractiveProcess, waitForProcess)
import Data.Array.IO(newArray_, hGetArray, hPutArray)
import Control.Concurrent(forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception.Extensible( SomeException(..), finally, catch
                                   , tryJust, bracket, evaluate, handle)
import Control.Monad(liftM,unless)
import System.FilePath(FilePath, (<.>))

-- -----------------------------------------------------------------------------

-- | The available Graphviz commands.  The following directions are
--   based upon those in the Graphviz man page (available online at
--   <http://graphviz.org/pdf/dot.1.pdf>, or if installed on your
--   system @man graphviz@).  Note that any command can be used on
--   both directed and undirected graphs.
data GraphvizCommand = Dot   -- ^ For hierachical graphs (ideal for
                             --   directed graphs).
                     | Neato -- ^ For symmetric layouts of graphs
                             --   (ideal for undirected graphs).
                     | TwoPi -- ^ For radial layout of graphs.
                     | Circo -- ^ For circular layout of graphs.
                     | Fdp   -- ^ For symmetric layout of graphs.
                       deriving (Eq, Ord, Show, Read)

showCmd        :: GraphvizCommand -> String
showCmd Dot    = "dot"
showCmd Neato  = "neato"
showCmd TwoPi  = "twopi"
showCmd Circo  = "circo"
showCmd Fdp    = "fdp"

-- | The default command for directed graphs.
dirCommand :: GraphvizCommand
dirCommand = Dot

-- | The default command for undirected graphs.
undirCommand :: GraphvizCommand
undirCommand = Neato

-- | The appropriate (default) Graphviz command for the given graph.
commandFor    :: DotGraph a -> GraphvizCommand
commandFor dg = if directedGraph dg
                then dirCommand
                else undirCommand

-- -----------------------------------------------------------------------------

-- | This class is for those data types that are valid options for the
--   Graphviz tools to use with the @-T@ argument.  Note that not all
--   valid output types are necessarily available on your system: to
--   determine which actual formats are supported on your system, run
--   @dot -T?@.  For more information, see:
--     <http://graphviz.org/doc/info/output.html>
class GraphvizResult o where
    outputCall :: o -> String
    isBinary :: o -> Bool

-- | The possible Graphviz output formats.  Note that which formats
--   are available on your system depend on how it was built (e.g. if
--   it wasn't built with PNG support, then using 'Png' will probably
--   result in an error.  See the documentation for 'GraphvizResult'
--   for more information.
data GraphvizOutput = Bmp       -- ^ Windows Bitmap Format.
                    | Canon     -- ^ Pretty-printed Dot output with no
                                --   layout performed.
                    | DotOutput -- ^ Reproduces the input along with
                                --   layout information.
                    | XDot      -- ^ As with 'DotOutput', but provides
                                --   even more information on how the
                                --   graph is drawn.
                    | Eps       -- ^ Encapsulated PostScript.
                    | Fig       -- ^ FIG graphics language.
                    | Gd        -- ^ Internal GD library format.
                    | Gd2       -- ^ Compressed version of 'Gd'.
                    | Gif       -- ^ Graphics Interchange Format.
                    | Ico       -- ^ Icon image file format.
                    | Imap      -- ^ Server-side imagemap.
                    | Cmapx     -- ^ Client-side imagemap.
                    | ImapNP    -- ^ As for 'Imap', except only
                                --   rectangles are used as active
                                --   areas.
                    | CmapxNP   -- ^ As for 'Cmapx', except only
                                --   rectangles are used as active
                                --   areas.
                    | Jpeg      -- ^ The JPEG image format.
                    | Pdf       -- ^ Portable Document Format.
                    | Plain     -- ^ Simple text format.
                    | PlainExt  -- ^ As for 'Plain', but provides port
                                --   names on head and tail nodes when
                                --   applicable.
                    | Png       -- ^ Portable Network Graphvics format.
                    | Ps        -- ^ PostScript.
                    | Ps2       -- ^ PostScript for PDF.
                    | Svg       -- ^ Scalable Vector Graphics format.
                    | SvgZ      -- ^ Compressed SVG format.
                    | Tiff      -- ^ Tagged Image File Format.
                    | Vml       -- ^ Vector Markup Language; 'Svg' is
                                --   usually preferred.
                    | VmlZ      -- ^ Compressed VML format; 'SvgZ' is
                                --   usually preferred.
                    | Vrml      -- ^ Virtual Reality Modeling Language
                                --   format; requires nodes to have a
                                --   'Z' attribute.
                    | WBmp      -- ^ Wireless BitMap format;
                                --   monochrome format usually used
                                --   for mobile computing devices.
                      deriving (Eq, Ord, Show, Read)

instance GraphvizResult GraphvizOutput where
    outputCall Bmp       = "bmp"
    outputCall Canon     = "canon"
    outputCall DotOutput = "dot"
    outputCall XDot      = "xdot"
    outputCall Eps       = "eps"
    outputCall Fig       = "fig"
    outputCall Gd        = "gd"
    outputCall Gd2       = "gd2"
    outputCall Gif       = "gif"
    outputCall Ico       = "ico"
    outputCall Imap      = "imap"
    outputCall Cmapx     = "cmapx"
    outputCall ImapNP    = "imap_np"
    outputCall CmapxNP   = "cmapx_np"
    outputCall Jpeg      = "jpeg"
    outputCall Pdf       = "pdf"
    outputCall Plain     = "plain"
    outputCall PlainExt  = "plain-ext"
    outputCall Png       = "png"
    outputCall Ps        = "ps"
    outputCall Ps2       = "ps2"
    outputCall Svg       = "svg"
    outputCall SvgZ      = "svgz"
    outputCall Tiff      = "tiff"
    outputCall Vml       = "vml"
    outputCall VmlZ      = "vmlz"
    outputCall Vrml      = "vrml"
    outputCall WBmp      = "wbmp"

    -- These are the known text-based outputs.
    isBinary Canon     = False
    isBinary DotOutput = False
    isBinary XDot      = False
    isBinary Eps       = False
    isBinary Fig       = False
    isBinary Imap      = False
    isBinary Cmapx     = False
    isBinary ImapNP    = False
    isBinary CmapxNP   = False
    isBinary Plain     = False
    isBinary PlainExt  = False
    isBinary Ps        = False
    isBinary Svg       = False
    isBinary Vml       = False
    isBinary Vrml      = False
    isBinary _         = True

-- | A default file extension for each 'GraphvizOutput'.  Note that
--   for cases such as 'Gtk' where there is no actual file produced,
--   the value returned isn't necessarily sensible.
defaultExtension           :: GraphvizOutput -> String
defaultExtension Bmp       = "bmp"
defaultExtension Canon     = "dot"
defaultExtension DotOutput = "dot"
defaultExtension XDot      = "dot"
defaultExtension Eps       = "eps"
defaultExtension Fig       = "fig"
defaultExtension Gd        = "gd"
defaultExtension Gd2       = "gd2"
defaultExtension Gif       = "gif"
defaultExtension Ico       = "ico"
defaultExtension Imap      = "map"
defaultExtension Cmapx     = "map"
defaultExtension ImapNP    = "map"
defaultExtension CmapxNP   = "map"
defaultExtension Jpeg      = "jpg"
defaultExtension Pdf       = "pdf"
defaultExtension Plain     = "txt"
defaultExtension PlainExt  = "txt"
defaultExtension Png       = "png"
defaultExtension Ps        = "ps"
defaultExtension Ps2       = "ps"
defaultExtension Svg       = "svg"
defaultExtension SvgZ      = "svgz"
defaultExtension Tiff      = "tif"
defaultExtension Vml       = "vml"
defaultExtension VmlZ      = "vmlz"
defaultExtension Vrml      = "vrml"
defaultExtension WBmp      = "wbmp"

-- | Unlike 'GraphvizOutput', these items do not produce an output
--   file; instead, they directly draw a canvas (i.e. a window) with
--   the resulting image.  See the documentation for 'GraphvizResult'
--   for more information.
data GraphvizCanvas = Gtk | Xlib
                      deriving (Eq, Ord, Read, Show)

instance GraphvizResult GraphvizCanvas where
    outputCall Gtk       = "gtk"
    outputCall Xlib      = "xlib"

    -- Since there's no output, we arbitrarily choose binary mode.
    isBinary _ = True

-- -----------------------------------------------------------------------------

-- | Represents the result of running a command.
data RunResult = Error String
               | Success
               deriving (Eq, Ord, Read, Show)

-- | Return the error if there is one, otherwise return Success.
maybeErr :: Either String a -> RunResult
maybeErr = either Error (const Success)

-- | Run the recommended Graphviz command on this graph, saving the result
--   to the file provided (note: file extensions are /not/ checked).
runGraphviz    :: (PrintDot n) => DotGraph n -> GraphvizOutput -> FilePath
                  -> IO RunResult
runGraphviz gr = runGraphvizCommand (commandFor gr) gr

-- | Run the chosen Graphviz command on this graph, saving the result
--   to the file provided (note: file extensions are /not/ checked).
runGraphvizCommand :: (PrintDot n) => GraphvizCommand -> DotGraph n
                      -> GraphvizOutput -> FilePath -> IO RunResult
runGraphvizCommand cmd gr t fp
    = do pipe <- tryJust (\(SomeException _) -> return ())
                 $ openFile fp WriteMode
         case pipe of
           (Left  _) -> return $ Error $ "Unable to open file " ++ fp
           (Right f) -> do hSetBinaryMode f $ isBinary t
                           liftM maybeErr
                             $ graphvizWithHandle cmd gr t (toFile f)
    where
      toFile f h = do squirt h f
                      hClose h
                      hClose f

-- | Append the default extension for the provided 'GraphvizOutput' to
--   the provided 'FilePath' for the output file.
addExtension          :: (GraphvizOutput -> FilePath -> IO a)
                         -> GraphvizOutput -> FilePath -> IO a
addExtension cmd t fp = cmd t fp'
    where
      fp' = fp <.> defaultExtension t

-- | Run the chosen Graphviz command on this graph, but send the
--   result to the given handle rather than to a file.
--
--   If the command was unsuccessful, then @Left error@ is returned.
graphvizWithHandle :: (PrintDot n)  => GraphvizCommand
                      -> DotGraph n -> GraphvizOutput
                      -> (Handle -> IO a) -> IO (Either String a)
graphvizWithHandle = graphvizWithHandle'

-- This version is not exported as we don't want to let arbitrary
-- @Handle -> IO a@ functions to be used for GraphvizCanvas outputs.
graphvizWithHandle' :: (PrintDot n, GraphvizResult o)
                       => GraphvizCommand -> DotGraph n -> o
                       -> (Handle -> IO a) -> IO (Either String a)
graphvizWithHandle' cmd gr t f
  = handle notRunnable
    $ bracket
        (runInteractiveProcess cmd' args Nothing Nothing)
        (\(inh,outh,errh,_) -> hClose inh >> hClose outh >> hClose errh)
        $ \(inp,outp,errp,prc) -> do
          -- The input and error are text, not binary
          hSetBinaryMode inp False
          forkIO $ hPutStrLn inp $ printDotGraph gr

          hSetBinaryMode outp $ isBinary t
          hSetBinaryMode errp False

          err <- hGetContents errp

          -- Need to make sure both the output and error handles are
          -- really fully consumed
          mvOutput <- newEmptyMVar -- Actually get the value out
          mvDone   <- newEmptyMVar
          -- Need to consider what to do if action throws an error,
          -- e.g. disk full
          let signalWhenDone action = (action >> return ())
                                        `finally` putMVar mvDone ()

          _ <- forkIO $ signalWhenDone $ evaluate (length err)
          _ <- forkIO $ signalWhenDone $ f' mvOutput outp
          takeMVar mvDone
          takeMVar mvDone

          output <- takeMVar mvOutput

          exitCode <- waitForProcess prc

          case exitCode of
            ExitSuccess -> return output
            _           -> return $ Left $ othErr ++ err
    where
      notRunnable SomeException{} = return . Left $
                                    "Unable to call the Graphviz command "
                                    ++ showCmd cmd ++
                                    " with the arguments: "
                                    ++ unwords args
      cmd' = showCmd cmd
      args = ["-T" ++ outputCall t]

      -- Augmenting the f function to let it work within the forkIO:
      f' mv h = (f h >>= (putMVar mv . Right))
                `catch`
                (\SomeException{} -> putMVar mv $ Left fErr)
      fErr = "Error re-directing the output from " ++ cmd'

      othErr = "Error messages from " ++ cmd' ++ ":\n"

-- | Run the chosen Graphviz command on this graph and render it using
--   the given canvas type.
runGraphvizCanvas          :: (PrintDot n) => GraphvizCommand -> DotGraph n
                              -> GraphvizCanvas -> IO RunResult
runGraphvizCanvas cmd gr c = liftM maybeErr
                             $ graphvizWithHandle' cmd gr c nullHandle
    where
      nullHandle :: Handle -> IO ()
      nullHandle = const (return ())

-- | Run the recommended Graphviz command on this graph and render it
--   using the given canvas type.
runGraphvizCanvas'   :: (PrintDot n) => DotGraph n -> GraphvizCanvas
                        -> IO RunResult
runGraphvizCanvas' d = runGraphvizCanvas (commandFor d) d

{- |
   This function is based upon code taken from the /mohws/ project,
   available under a 3-Clause BSD license.  The actual function is
   taken from: <http://code.haskell.org/mohws/src/Util.hs> It provides
   an efficient way of transferring data from one 'Handle' to another.
 -}
squirt :: Handle -> Handle -> IO ()
squirt rd wr = do
  arr <- newArray_ (0, bufsize-1)
  let loop = do
        r <- hGetArray rd arr bufsize
        unless (r == 0)
             $ if r < bufsize
                then hPutArray wr arr r
                else hPutArray wr arr bufsize >> loop
  loop
    where
      -- This was originally separate
      bufsize :: Int
      bufsize = 4 * 1024
