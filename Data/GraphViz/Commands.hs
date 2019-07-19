{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}

{- |
   Module      : Data.GraphViz.Commands
   Description : Functions to run Graphviz commands.
   Copyright   : (c) Matthew Sackman, Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines functions to call the various Graphviz
   commands.

   Whilst various output formats are supported (see 'GraphvizOutput'
   for a complete list), it is not yet possible to choose a desired
   renderer and formatter.  Being able to determine which renderers
   and formatters are applicable for a specific 'GraphvizOutput' is
   not easy (there is no listing of available renderers or formatters
   on the Graphviz website), and for the most part the default ones do
   the job well.

   Please note that for 'GraphvizOutput' and 'GraphvizCanvas', you
   will see that they are instances of a @GraphvizResult@ class; this is
   an internal class that should not be visible outside this module, but
   Haddock is being too helpful for its own good.
-}
module Data.GraphViz.Commands
    ( -- * The different Graphviz tools available.
      GraphvizCommand(..)
    , dirCommand
    , undirCommand
    , commandFor
      -- * The possible outputs that Graphviz supports.
      -- $outputs
    , GraphvizOutput(..)
    , GraphvizCanvas(..)
      -- * Running Graphviz.
    , runGraphviz
    , runGraphvizCommand
    , addExtension
    , runGraphvizCanvas
    , runGraphvizCanvas'
    , graphvizWithHandle
      -- * Testing if Graphviz is installed
    , isGraphvizInstalled
    , quitWithoutGraphviz
    ) where

import Data.GraphViz.Types
-- This is here just for Haddock linking purposes.
import Data.GraphViz.Commands.Available
import Data.GraphViz.Commands.IO        (runCommand)
import Data.GraphViz.Exception

import           Control.Monad    (liftM, unless)
import qualified Data.ByteString  as SB
import           Data.Maybe       (isJust)
import           Data.Version     (Version (..), showVersion)
import           System.Directory (findExecutable)
import           System.Exit      (ExitCode (..), exitWith)
import           System.FilePath  ((<.>))
import           System.IO        (Handle, hPutStrLn, hSetBinaryMode, stderr)

-- -----------------------------------------------------------------------------

showCmd           :: GraphvizCommand -> String
showCmd Dot       = "dot"
showCmd Neato     = "neato"
showCmd TwoPi     = "twopi"
showCmd Circo     = "circo"
showCmd Fdp       = "fdp"
showCmd Sfdp      = "sfdp"
showCmd Osage     = "osage"
showCmd Patchwork = "patchwork"

-- | The default command for directed graphs.
dirCommand :: GraphvizCommand
dirCommand = Dot

-- | The default command for undirected graphs.
undirCommand :: GraphvizCommand
undirCommand = Neato

-- | The appropriate (default) Graphviz command for the given graph.
commandFor    :: (DotRepr dg n) => dg n -> GraphvizCommand
commandFor dg = if graphIsDirected dg
                then dirCommand
                else undirCommand

-- -----------------------------------------------------------------------------

{- $outputs

   The list of output types supported by Graphviz is dependent upon
   how it is built on your system.  To determine which actual formats
   are available on your system, run @dot -T?@.  Trying to use an
   output type that is not supported by your installation of Graphviz
   will result in an error.

   The outputs defined here in 'GraphvizOutput' and 'GraphvizCanvas'
   are those from the default list of available outputs.  For more
   information, see:
     <http://graphviz.org/doc/info/output.html>

-}

-- | This class is for those data types that are valid options for the
--   Graphviz tools to use with the @-T@ argument.
class GraphvizResult o where
  outputCall :: o -> String

-- | The possible Graphviz output formats (that is, those that
--   actually produce a file).
data GraphvizOutput = Bmp       -- ^ Windows Bitmap Format.
                    | Canon     -- ^ Pretty-printed Dot output with no
                                --   layout performed.
                    | DotOutput -- ^ Reproduces the input along with
                                --   layout information.
                    | XDot (Maybe Version)
                      -- ^ As with 'DotOutput', but provides even more
                      --   information on how the graph is drawn.  The
                      --   optional 'Version' is the same as
                      --   specifying the @XDotVersion@ attribute.
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
                    | Png       -- ^ Portable Network Graphics format.
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
                                --   third dimension set via the @Pos@
                                --   attribute (and with a @Dim@ value
                                --   of at least @3@).
                    | WBmp      -- ^ Wireless BitMap format;
                                --   monochrome format usually used
                                --   for mobile computing devices.
                    | WebP      -- ^ Google's WebP format; requires
                                --   Graphviz >= 2.29.0.
                    deriving (Eq, Ord, Show, Read)

instance GraphvizResult GraphvizOutput where
  outputCall Bmp       = "bmp"
  outputCall Canon     = "canon"
  outputCall DotOutput = "dot"
  outputCall (XDot mv) = "xdot" ++ maybe "" showVersion mv
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
  outputCall WebP      = "webp"

-- | A default file extension for each 'GraphvizOutput'.
defaultExtension           :: GraphvizOutput -> String
defaultExtension Bmp       = "bmp"
defaultExtension Canon     = "gv"
defaultExtension DotOutput = "gv"
defaultExtension XDot{}    = "gv"
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
defaultExtension WebP      = "webp"

-- | Unlike 'GraphvizOutput', these items do not produce an output
--   file; instead, they directly draw a canvas (i.e. a window) with
--   the resulting image.
data GraphvizCanvas = Gtk | Xlib
                    deriving (Eq, Ord, Bounded, Enum, Read, Show)

instance GraphvizResult GraphvizCanvas where
  outputCall Gtk       = "gtk"
  outputCall Xlib      = "xlib"

-- -----------------------------------------------------------------------------

-- | Run the recommended Graphviz command on this graph, saving the result
--   to the file provided (note: file extensions are /not/ checked).
runGraphviz    :: (PrintDotRepr dg n) => dg n -> GraphvizOutput -> FilePath
                  -> IO FilePath
runGraphviz gr = runGraphvizCommand (commandFor gr) gr

-- | Run the chosen Graphviz command on this graph, saving the result
--   to the file provided (note: file extensions are /not/ checked).
runGraphvizCommand :: (PrintDotRepr dg n) => GraphvizCommand -> dg n
                      -> GraphvizOutput -> FilePath
                      -> IO FilePath
runGraphvizCommand cmd gr t fp
  = handle (throwIO . addExc) $ graphvizWithHandle cmd gr t toFile
  where
    addFl = (++) ("Unable to create " ++ fp ++ "\n")
    toFile h = SB.hGetContents h >>= SB.writeFile fp >> return fp

    addExc (GVProgramExc e) = GVProgramExc $ addFl e
    addExc e                = e

-- | Append the default extension for the provided 'GraphvizOutput' to
--   the provided 'FilePath' for the output file.
addExtension          :: (GraphvizOutput -> FilePath -> a)
                         -> GraphvizOutput -> FilePath -> a
addExtension cmd t fp = cmd t fp'
  where
    fp' = fp <.> defaultExtension t

-- | Run the chosen Graphviz command on this graph, but send the
--   result to the given handle rather than to a file.
--
--   Note that the @'Handle' -> 'IO' a@ function /must/ fully consume
--   the input from the 'Handle'; e.g. use strict @ByteStrings@ rather
--   than lazy ones.
--
--   If the command was unsuccessful, then a 'GraphvizException' is
--   thrown.
graphvizWithHandle :: (PrintDotRepr dg n)
                      => GraphvizCommand  -- ^ Which command to run
                      -> dg n             -- ^ The 'DotRepr' to use
                      -> GraphvizOutput   -- ^ The 'GraphvizOutput' type
                      -> (Handle -> IO a) -- ^ Extract the output
                      -> IO a             -- ^ The error or the result.
graphvizWithHandle = graphvizWithHandle'

-- This version is not exported as we don't want to let arbitrary
-- @Handle -> IO a@ functions to be used for GraphvizCanvas outputs.
graphvizWithHandle' :: (PrintDotRepr dg n, GraphvizResult o)
                       => GraphvizCommand -> dg n -> o
                       -> (Handle -> IO a) -> IO a
graphvizWithHandle' cmd dg t f = runCommand (showCmd cmd)
                                            ["-T" ++ outputCall t]
                                            f'
                                            dg
  where
    f' h = hSetBinaryMode h True >> f h

-- | Run the chosen Graphviz command on this graph and render it using
--   the given canvas type.
runGraphvizCanvas          :: (PrintDotRepr dg n) => GraphvizCommand -> dg n
                              -> GraphvizCanvas -> IO ()
runGraphvizCanvas cmd gr c = graphvizWithHandle' cmd gr c nullHandle
  where
    nullHandle :: Handle -> IO ()
    nullHandle = liftM (const ()) . SB.hGetContents

-- | Run the recommended Graphviz command on this graph and render it
--   using the given canvas type.
runGraphvizCanvas'   :: (PrintDotRepr dg n) => dg n -> GraphvizCanvas -> IO ()
runGraphvizCanvas' d = runGraphvizCanvas (commandFor d) d

-- -----------------------------------------------------------------------------

-- | Is the Graphviz suite of tools installed?  This is determined by
--   whether @dot@ is available in the @PATH@.
isGraphvizInstalled :: IO Bool
isGraphvizInstalled = liftM isJust . findExecutable $ showCmd Dot

-- | If Graphviz does not seem to be available, print the provided
--   error message and then exit fatally.
quitWithoutGraphviz     :: String -> IO ()
quitWithoutGraphviz err = do hasGraphviz <- isGraphvizInstalled
                             unless hasGraphviz
                               $ hPutStrLn stderr err >> exitWith (ExitFailure 1)
