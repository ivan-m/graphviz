{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}
{- |
   Module      : Data.GraphViz.Commands.Available
   Description : Available command-line programs
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   These are the known programs that read in Dot graphs.

 -}
module Data.GraphViz.Commands.Available where

import Data.GraphViz.Parsing
import Data.GraphViz.Printing

-- -----------------------------------------------------------------------------

-- | The available Graphviz commands.  The following directions are
--   based upon those in the Graphviz man page (available online at
--   <http://graphviz.org/pdf/dot.1.pdf>, or if installed on your
--   system @man graphviz@).  Note that any command can be used on
--   both directed and undirected graphs.
--
--   When used with the 'Layout' attribute, it overrides any actual
--   command called on the dot graph.
data GraphvizCommand = Dot       -- ^ For hierachical graphs (ideal for
                                 --   directed graphs).
                     | Neato     -- ^ For symmetric layouts of graphs
                                 --   (ideal for undirected graphs).
                     | TwoPi     -- ^ For radial layout of graphs.
                     | Circo     -- ^ For circular layout of graphs.
                     | Fdp       -- ^ Spring-model approach for
                                 --   undirected graphs.
                     | Sfdp      -- ^ As with Fdp, but ideal for large
                                 --   graphs.
                     | Osage     -- ^ Filter for drawing clustered graphs,
                                 --   requires Graphviz >= 2.28.0.
                     | Patchwork -- ^ Draw clustered graphs as treemaps,
                                 --   requires Graphviz >= 2.28.0.
                     deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot GraphvizCommand where
  unqtDot Dot       = text "dot"
  unqtDot Neato     = text "neato"
  unqtDot TwoPi     = text "twopi"
  unqtDot Circo     = text "circo"
  unqtDot Fdp       = text "fdp"
  unqtDot Sfdp      = text "sfdp"
  unqtDot Osage     = text "osage"
  unqtDot Patchwork = text "patchwork"

instance ParseDot GraphvizCommand where
  parseUnqt = stringValue [ ("dot", Dot)
                          , ("neato", Neato)
                          , ("twopi", TwoPi)
                          , ("circo", Circo)
                          , ("fdp", Fdp)
                          , ("sfdp", Sfdp)
                          , ("osage", Osage)
                          , ("patchwork", Patchwork)
                          ]
