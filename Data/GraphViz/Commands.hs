{- |
   Module      : Data.GraphViz.Commands
   Description : Functions to run GraphViz commands.
   Copyright   : (c) Matthew Sackman, Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines functions to call the various GraphViz
   commands.

   Most of these functions were from version 0.5 of /Graphalyze/:

   <http://hackage.haskell.org/cgi-bin/hackage-scripts/package/Graphalyze-0.5>
-}

module Data.GraphViz.Commands
    ( GraphvizCommand(..)
    , dirCommand
    , undirCommand
    , commandFor
    , GraphvizOutput(..)
    , runGraphviz
    , runGraphvizCommand
    , graphvizWithHandle
    )
    where

import System.IO
import System.Exit
import System.Process
import Data.Array.IO
import Control.Concurrent
import Control.Exception

import Data.GraphViz.Types

-- | The available Graphviz commands.
data GraphvizCommand = Dot | Neato | TwoPi | Circo | Fdp

instance Show GraphvizCommand where
    show Dot = "dot"
    show Neato  = "neato"
    show TwoPi  = "twopi"
    show Circo  = "circo"
    show Fdp    = "fdp"

-- | The default command for directed graphs.
dirCommand :: GraphvizCommand
dirCommand = Dot

-- | The default command for undirected graphs.
undirCommand :: GraphvizCommand
undirCommand = Neato

-- | The appropriate (default) GraphViz command for the given graph.
commandFor    :: DotGraph -> GraphvizCommand
commandFor dg = if (directedGraph dg)
                then dirCommand
                else undirCommand

-- | The possible Graphviz outputs, obtained by running /dot -Txxx/.
--   Note that it is not possible to choose between output variants,
--   and that not all of these may be available on your system.
--
--   This will probably be improved in future.
data GraphvizOutput = Canon
                    | Cmap
                    | Cmapx
                    | Cmapx_np
                    | Dia
                    | DotOutput
                    | Eps
                    | Fig
                    | Gd
                    | Gd2
                    | Gif
                    | Gtk
                    | Hpgl
                    | Imap
                    | Imap_np
                    | Ismap
                    | Jpe
                    | Jpeg
                    | Jpg
                    | Mif
                    | Mp
                    | Pcl
                    | Pdf
                    | Pic
                    | Plain
                    | PlainExt
                    | Png
                    | Ps
                    | Ps2
                    | Svg
                    | Svgz
                    | Tk
                    | Vml
                    | Vmlz
                    | Vrml
                    | Vtx
                    | Wbmp
                    | Xdot
                    | Xlib

instance Show GraphvizOutput where
    show Canon     = "canon"
    show Cmap      = "cmap"
    show Cmapx     = "cmapx"
    show Cmapx_np  = "cmapx_np"
    show Dia       = "dia"
    show DotOutput = "dot"
    show Eps       = "eps"
    show Fig       = "fig"
    show Gd        = "gd"
    show Gd2       = "gd2"
    show Gif       = "gif"
    show Gtk       = "gtk"
    show Hpgl      = "hpgl"
    show Imap      = "imap"
    show Imap_np   = "imap_np"
    show Ismap     = "ismap"
    show Jpe       = "jpe"
    show Jpeg      = "jpeg"
    show Jpg       = "jpg"
    show Mif       = "mif"
    show Mp        = "mp"
    show Pcl       = "pcl"
    show Pdf       = "pdf"
    show Pic       = "pic"
    show Plain     = "plain"
    show PlainExt  = "plain-ext"
    show Png       = "png"
    show Ps        = "ps"
    show Ps2       = "ps2"
    show Svg       = "svg"
    show Svgz      = "svgz"
    show Tk        = "tk"
    show Vml       = "vml"
    show Vmlz      = "vmlz"
    show Vrml      = "vrml"
    show Vtx       = "vtx"
    show Wbmp      = "wbmp"
    show Xdot      = "xdot"
    show Xlib      = "xlib"

-- | Run the recommended Graphviz command on this graph, saving the result
--   to the file provided (note: file extensions are /not/ checked).
--   Returns @True@ if successful, @False@ otherwise.
runGraphviz         :: DotGraph -> GraphvizOutput -> FilePath -> IO Bool
runGraphviz gr t fp = runGraphvizCommand (commandFor gr) gr t fp

-- | Run the chosen Graphviz command on this graph, saving the result
--   to the file provided (note: file extensions are /not/ checked).
--   Returns @True@ if successful, @False@ otherwise.
runGraphvizCommand :: GraphvizCommand -> DotGraph -> GraphvizOutput
                   -> FilePath -> IO Bool
runGraphvizCommand  cmd gr t fp
    = do pipe <- tryJust catcher $ openFile fp WriteMode
         case pipe of
           (Left _)  -> return False
           (Right f) -> do file <- graphvizWithHandle cmd gr t (flip squirt f)
                           hClose f
                           case file of
                             (Just _) -> return True
                             _        -> return False
    where
      catcher   :: IOError -> Maybe ()
      catcher _ = Just ()

-- graphvizWithHandle sometimes throws an error about handles not
-- being closed properly: investigate.

-- | Run the chosen Graphviz command on this graph, but send the result to the
--   given handle rather than to a file.
--   The result is wrapped in 'Maybe' rather than throwing an error.
graphvizWithHandle :: (Show a) => GraphvizCommand -> DotGraph -> GraphvizOutput
                   -> (Handle -> IO a) -> IO (Maybe a)
graphvizWithHandle cmd gr t f
    = do (inp, outp, errp, proc) <- runInteractiveCommand command
         forkIO $ hPrint inp gr >> hClose inp
         forkIO $ (hGetContents errp >>= hPutStr stderr >> hClose errp)
         a <- f outp
         -- Don't close outp until f finishes.
         a `seq` hClose outp
         exitCode <- waitForProcess proc
         case exitCode of
           ExitSuccess -> return (Just a)
           _           -> return Nothing
    where
      command = (show cmd) ++ " -T" ++ (show t)

{- |
   This function is taken from the /mohws/ project, available under a
   3-Clause BSD license.  The actual function is taken from:
   <http://code.haskell.org/mohws/src/Util.hs>
   It provides an efficient way of transferring data from one 'Handle'
   to another.
 -}
squirt :: Handle -> Handle -> IO ()
squirt rd wr = do
  arr <- newArray_ (0, bufsize-1)
  let loop = do
        r <- hGetArray rd arr bufsize
        if (r == 0)
          then return ()
          else if (r < bufsize)
                then hPutArray wr arr r
                else hPutArray wr arr bufsize >> loop
  loop
    where
      -- This was originally separate
      bufsize :: Int
      bufsize = 4 * 1024

