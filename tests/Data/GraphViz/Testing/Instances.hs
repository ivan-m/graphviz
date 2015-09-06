{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
   Module      : Data.GraphViz.Testing.Instances
   Description : 'Arbitrary' instances for graphviz.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module exports the 'Arbitrary' instances for the various types
   used to represent Graphviz Dot code.

   Note that they do not generally generate /sensible/ values for the
   various types; in particular, there's no guarantee that the
   'Attributes' chosen for a particular value type are indeed legal
   for that type.
 -}
module Data.GraphViz.Testing.Instances() where

import Data.Graph.Inductive.Arbitrary              ()
import Data.GraphViz.Testing.Instances.Canonical   ()
import Data.GraphViz.Testing.Instances.Generalised ()
import Data.GraphViz.Testing.Instances.Graph       ()

-- -----------------------------------------------------------------------------
