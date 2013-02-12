{-# OPTIONS_HADDOCK hide #-}

{- |
   Module      : Data.GraphViz.Attributes.Same
   Description : Consider Attributes equal on constructors.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module is used when @a1 == a2@ should return @True@ if they
   are the same Attribute, even if they don't have the same value
   (typically for 'Set's).
-}
module Data.GraphViz.Attributes.Same
       ( SameAttr
       , SAttrs
       , toSAttr
       , unSame
       , unSameSet
       ) where

import Data.GraphViz.Attributes.Complete(Attribute, Attributes, sameAttribute)

import Data.Function(on)
import qualified Data.Set as Set
import Data.Set(Set)

-- -----------------------------------------------------------------------------

-- | Defined as a wrapper around 'Attribute' where equality is based
--   solely upon the constructor, not the contents.
newtype SameAttr = SA { getAttr :: Attribute }
                 deriving (Show, Read)

instance Eq SameAttr where
  (==) = sameAttribute `on` getAttr

instance Ord SameAttr where
  compare sa1 sa2
    | sa1 == sa2 = EQ
    | otherwise  = (compare `on` getAttr) sa1 sa2


type SAttrs = Set SameAttr

toSAttr :: Attributes -> SAttrs
toSAttr = Set.fromList . map SA

unSame :: SAttrs -> Attributes
unSame = map getAttr . Set.toList

unSameSet :: SAttrs -> Set Attribute
unSameSet = Set.mapMonotonic getAttr
