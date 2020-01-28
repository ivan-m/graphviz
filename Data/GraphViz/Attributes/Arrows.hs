{-# LANGUAGE CPP, OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}
{- |
   Module      : Data.GraphViz.Attributes.Arrows
   Description : Arrow types
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Data.GraphViz.Attributes.Arrows where

import Data.GraphViz.Internal.Util (bool)
import Data.GraphViz.Parsing
import Data.GraphViz.Printing

import Data.Maybe (isJust)

#if !MIN_VERSION_base (4,13,0)
import Data.Monoid ((<>))
#endif

-- -----------------------------------------------------------------------------

-- | /Dot/ has a basic grammar of arrow shapes which allows usage of
--   up to 1,544,761 different shapes from 9 different basic
--   'ArrowShape's.  Note that whilst an explicit list is used in the
--   definition of 'ArrowType', there must be at least one tuple and a
--   maximum of 4 (since that is what is required by Dot).  For more
--   information, see: <http://graphviz.org/doc/info/arrows.html>
--
--   The 19 basic arrows shown on the overall attributes page have
--   been defined below as a convenience.  Parsing of the 5
--   backward-compatible special cases is also supported.
newtype ArrowType = AType [(ArrowModifier, ArrowShape)]
    deriving (Eq, Ord, Show, Read)

-- Used for default
normal :: ArrowType
normal = AType [(noMods, Normal)]

-- Used for backward-compatible parsing
eDiamond, openArr, halfOpen, emptyArr, invEmpty :: ArrowType

eDiamond = AType [(openMod, Diamond)]
openArr = AType [(noMods, Vee)]
halfOpen = AType [(ArrMod FilledArrow LeftSide, Vee)]
emptyArr = AType [(openMod, Normal)]
invEmpty = AType [ (noMods, Inv)
                 , (openMod, Normal)]

instance PrintDot ArrowType where
  unqtDot (AType mas) = hcat $ mapM appMod mas
    where
      appMod (m, a) = unqtDot m <> unqtDot a

instance ParseDot ArrowType where
  parseUnqt = specialArrowParse
              `onFail`
              (AType <$> many1 (liftA2 (,) parseUnqt parseUnqt))

specialArrowParse :: Parse ArrowType
specialArrowParse = stringValue [ ("ediamond", eDiamond)
                                , ("open", openArr)
                                , ("halfopen", halfOpen)
                                , ("empty", emptyArr)
                                , ("invempty", invEmpty)
                                ]

data ArrowShape = Box
                | Crow
                | Diamond
                | DotArrow
                | Inv
                | NoArrow
                | Normal
                | Tee
                | Vee
                deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot ArrowShape where
  unqtDot Box      = text "box"
  unqtDot Crow     = text "crow"
  unqtDot Diamond  = text "diamond"
  unqtDot DotArrow = text "dot"
  unqtDot Inv      = text "inv"
  unqtDot NoArrow  = text "none"
  unqtDot Normal   = text "normal"
  unqtDot Tee      = text "tee"
  unqtDot Vee      = text "vee"

instance ParseDot ArrowShape where
  parseUnqt = stringValue [ ("box", Box)
                          , ("crow", Crow)
                          , ("diamond", Diamond)
                          , ("dot", DotArrow)
                          , ("inv", Inv)
                          , ("none", NoArrow)
                          , ("normal", Normal)
                          , ("tee", Tee)
                          , ("vee", Vee)
                          ]

-- | What modifications to apply to an 'ArrowShape'.
data ArrowModifier = ArrMod { arrowFill :: ArrowFill
                            , arrowSide :: ArrowSide
                            }
                   deriving (Eq, Ord, Show, Read)

-- | Apply no modifications to an 'ArrowShape'.
noMods :: ArrowModifier
noMods = ArrMod FilledArrow BothSides

-- | 'OpenArrow' and 'BothSides'
openMod :: ArrowModifier
openMod = ArrMod OpenArrow BothSides

instance PrintDot ArrowModifier where
  unqtDot (ArrMod f s) = unqtDot f <> unqtDot s

instance ParseDot ArrowModifier where
  parseUnqt = liftA2 ArrMod parseUnqt parseUnqt

data ArrowFill = OpenArrow
               | FilledArrow
               deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot ArrowFill where
  unqtDot OpenArrow   = char 'o'
  unqtDot FilledArrow = empty

instance ParseDot ArrowFill where
  parseUnqt = bool FilledArrow OpenArrow . isJust <$> optional (character 'o')

  -- Not used individually
  parse = parseUnqt

-- | Represents which side (when looking towards the node the arrow is
--   pointing to) is drawn.
data ArrowSide = LeftSide
               | RightSide
               | BothSides
               deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot ArrowSide where
  unqtDot LeftSide  = char 'l'
  unqtDot RightSide = char 'r'
  unqtDot BothSides = empty

instance ParseDot ArrowSide where
  parseUnqt = getSideType <$> optional (oneOf $ map character ['l', 'r'])
    where
      getSideType = maybe BothSides
                          (bool RightSide LeftSide . (==) 'l')

  -- Not used individually
  parse = parseUnqt
