{-# LANGUAGE OverloadedStrings #-}

{- |
   Module      : Data.GraphViz.Attributes.Colors.Brewer
   Description : Specification of Brewer colors.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   You almost definitely do /not/ want to use this module.  It is only
   defined for completeness when parsing existing Dot code.

   Graphviz contains a list of colors known as the /Brewer color
   schemes/.

   These colors are available under an Apache-style license:
   <http://www.graphviz.org/doc/info/colors.html#brewer_license>.  As
   such, they are not recommended for general use, and have only been
   included in this package for completeness.

   The complete list of Brewer colors can be found at
   <http://www.graphviz.org/doc/info/colors.html#brewer>.

 -}
module Data.GraphViz.Attributes.Colors.Brewer
    ( BrewerScheme(..)
    , BrewerName(..)
    , BrewerColor(..)
    ) where

{-

 This is a virtual module designed just to re-export the Brewer colors.

 -}

import Data.GraphViz.Attributes.ColorScheme(BrewerScheme(..), BrewerName(..))

-- To get the instances
import Data.GraphViz.Parsing()
import Data.GraphViz.Printing()

import Data.Word(Word8)

-- -----------------------------------------------------------------------------

-- Note: we do /not/ have {Print,Parse}Dot instances for this; it's covered in Color.

-- | This value should be between @1@ and the level of the
--   'BrewerScheme' being used.
data BrewerColor = BC BrewerScheme Word8
                   deriving (Eq, Ord, Show, Read)
