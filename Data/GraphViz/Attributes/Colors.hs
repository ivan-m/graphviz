{-# LANGUAGE CPP, OverloadedStrings #-}

{- |
   Module      : Data.GraphViz.Attributes.Colors
   Description : Specification of Color-related types and functions.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines the various colors, etc. for Graphviz.  For
   information on colors in general, see:
     <http://graphviz.org/doc/info/attrs.html#k:color>
   For named colors, see:
     <http://graphviz.org/doc/info/colors.html>

   Note that the ColorBrewer Color Schemes (shortened to just
   \"Brewer\" for the rest of this module) are covered by the
   following license (also available in the LICENSE file of this
   library):
     <http://graphviz.org/doc/info/colors.html#brewer_license>
-}
module Data.GraphViz.Attributes.Colors
       ( -- * Color schemes.
         ColorScheme(..)
         -- * Colors
       , Color(..)
       , ColorList
       , WeightedColor(..)
       , toWC
       , toColorList
       , NamedColor(toColor)
       , toWColor
         -- * Conversion to\/from @Colour@.
       , toColour
       , fromColour
       , fromAColour
       ) where

import Data.GraphViz.Attributes.Colors.Brewer (BrewerColor(..))
import Data.GraphViz.Attributes.Colors.SVG    (SVGColor, svgColour)
import Data.GraphViz.Attributes.Colors.X11    (X11Color(Transparent), x11Colour)
import Data.GraphViz.Attributes.ColorScheme   (ColorScheme(..))
import Data.GraphViz.Exception
import Data.GraphViz.Internal.State
import Data.GraphViz.Internal.Util            (bool)
import Data.GraphViz.Parsing
import Data.GraphViz.Printing

import Data.Colour              (AlphaColour, alphaChannel, black, darken,
                                 opaque, over, withOpacity)
import Data.Colour.RGBSpace     (uncurryRGB)
import Data.Colour.RGBSpace.HSV (hsv)
import Data.Colour.SRGB         (Colour, sRGB, sRGB24, toSRGB24)

import           Data.Char      (isHexDigit)
import           Data.Maybe     (isJust)
import qualified Data.Text.Lazy as T
import           Data.Word      (Word8)
import           Numeric        (readHex, showHex)

#if !MIN_VERSION_base (4,13,0)
import Data.Monoid ((<>))
#endif

-- -----------------------------------------------------------------------------

-- | Defining a color for use with Graphviz.  Note that named colors
--   have been split up into 'X11Color's and those based upon the
--   Brewer color schemes.
data Color = RGB { red   :: Word8
                 , green :: Word8
                 , blue  :: Word8
                 }
           | RGBA { red   :: Word8
                  , green :: Word8
                  , blue  :: Word8
                  , alpha :: Word8
                  }
             -- | The 'hue', 'saturation' and 'value' values must all
             --   be @0 <= x <=1@.
           | HSV { hue        :: Double
                 , saturation :: Double
                 , value      :: Double
                 }
           | X11Color X11Color
           | SVGColor SVGColor
           | BrewerColor BrewerColor
           deriving (Eq, Ord, Show, Read)

instance PrintDot Color where
  unqtDot (RGB  r g b)     = hexColor [r,g,b]
  unqtDot (RGBA r g b a)   = hexColor [r,g,b,a]
  unqtDot (HSV  h s v)     = hcat . punctuate comma $ mapM unqtDot [h,s,v]
  unqtDot (SVGColor name)  = printNC False name
  unqtDot (X11Color name)  = printNC False name
  unqtDot (BrewerColor bc) = printNC False bc

  -- Some cases might not need quotes.
  toDot (X11Color name)  = printNC True name
  toDot (SVGColor name)  = printNC True name
  toDot (BrewerColor bc) = printNC True bc
  toDot c                = dquotes $ unqtDot c

  unqtListToDot = hcat . punctuate colon . mapM unqtDot

  -- These three might not need to be quoted if they're on their own.
  listToDot [X11Color name]  = printNC True name
  listToDot [SVGColor name]  = printNC True name
  listToDot [BrewerColor bc] = printNC True bc
  listToDot cs               = dquotes $ unqtListToDot cs

hexColor :: [Word8] -> DotCode
hexColor = (<>) (char '#') . hcat . mapM word8Doc

word8Doc   :: Word8 -> DotCode
word8Doc w = text $ padding `T.append` simple
  where
    simple = T.pack $ showHex w ""
    padding = T.replicate count (T.singleton '0')
    count = 2 - findCols 1 w
    findCols c n
      | n < 16 = c
      | otherwise = findCols (c+1) (n `div` 16)

instance ParseDot Color where
  parseUnqt = oneOf [ parseHexBased
                    , parseHSV
                      -- Have to parse BrewerColor first, as some of them may appear to be X11 colors
                    , parseNC (undefined :: BrewerColor) False
                    , parseNC (undefined :: SVGColor) False
                    , parseX11Color False
                    ]
              `onFail`
              fail "Could not parse Color"
    where
      parseHexBased
          = character '#' *>
            do cs <- many1 parse2Hex
               return $ case cs of
                          [r,g,b] -> RGB r g b
                          [r,g,b,a] -> RGBA r g b a
                          _ -> throw . NotDotCode
                               $ "Not a valid hex Color specification: "
                                  ++ show cs
      parseHSV = HSV <$> parseUnqt
                     <*  parseSep
                     <*> parseUnqt
                     <*  parseSep
                     <*> parseUnqt
      parseSep = character ',' *> whitespace <|> whitespace1

      parse2Hex = do c1 <- satisfy isHexDigit
                     c2 <- satisfy isHexDigit
                     let [(n, [])] = readHex [c1, c2]
                     return n

  parse = quotedParse parseUnqt
          `onFail` -- These three might not need to be quoted
          oneOf [ parseNC (undefined :: BrewerColor) True
                , parseNC (undefined :: SVGColor) True
                , parseX11Color True
                ]
          `onFail`
          fail "Could not parse Color"

  parseUnqtList = sepBy1 parseUnqt (character ':')
                  `onFail`
                  do cs <- getColorScheme
                     failBad $ "Error parsing list of Colors with color scheme of "
                               ++ show cs

  parseList = fmap (:[])
              -- Potentially unquoted single color
              (oneOf [ parseNC (undefined :: BrewerColor) True
                     , parseNC (undefined :: SVGColor) True
                     , parseX11Color True
                     ]
              )
              `onFail`
              quotedParse parseUnqtList
              `onFail`
              do cs <- getColorScheme
                 failBad $ "Error parsing list of Colors with color scheme of "
                           ++ show cs

-- | The sum of the optional weightings /must/ sum to at most @1@.
type ColorList = [WeightedColor]

-- | A 'Color' tagged with an optional weighting.
data WeightedColor = WC { wColor    :: Color
                          -- | Must be in range @0 <= W <= 1@.
                        , weighting :: Maybe Double
                        }
                   deriving (Eq, Ord, Show, Read)

-- | For colors without weightings.
toWC :: Color -> WeightedColor
toWC = (`WC` Nothing)

-- | For a list of colors without weightings.
toColorList :: [Color] -> ColorList
toColorList = map toWC

instance PrintDot WeightedColor where
  unqtDot (WC c mw) = unqtDot c
                      <> maybe empty ((semi<>) . unqtDot) mw

  toDot (WC c Nothing) = toDot c
  toDot wc             = dquotes $ unqtDot wc

  unqtListToDot = hcat . punctuate colon . mapM unqtDot

  -- Might not need quoting
  listToDot [wc] = toDot wc
  listToDot wcs  = dquotes $ unqtListToDot wcs

instance ParseDot WeightedColor where
  parseUnqt = WC <$> parseUnqt <*> optional (character ';' *> parseUnqt)

  parse = quotedParse parseUnqt
          `onFail`
          -- Using parse rather than parseUnqt as there shouldn't be
          -- any quotes, but to avoid copy-pasting the oneOf block.
          (toWC <$> parse)

  parseUnqtList = sepBy1 parseUnqt (character ':')
                  `onFail`
                  do cs <- getColorScheme
                     failBad $ "Error parsing a ColorList with color scheme of "
                               ++ show cs

  parseList = quotedParse parseUnqtList
              `onFail`
              ((:[]) . toWC <$> parse)
              -- Potentially unquoted un-weighted single color
              `onFail`
              do cs <- getColorScheme
                 failBad $ "Error parsing ColorList with color scheme of "
                           ++ show cs

-- -----------------------------------------------------------------------------

-- | More easily convert named colors to an overall 'Color' value.
class NamedColor nc where
    colorScheme :: nc -> ColorScheme

    toColor :: nc -> Color

    printNC :: Bool -> nc -> DotCode

    -- | Bool is for whether quoting is needed.
    parseNC' :: Bool -> Parse nc

toWColor :: (NamedColor nc) => nc -> WeightedColor
toWColor = toWC . toColor

-- First value just used for type
parseNC :: (NamedColor nc) => nc -> Bool -> Parse Color
parseNC nc q = fmap (toColor . (`asTypeOf` nc))
               $ parseNC' q

instance NamedColor BrewerColor where
    colorScheme (BC bs _) = Brewer bs

    toColor = BrewerColor

    printNC = printNamedColor (\ (BC _ l) -> l)

    parseNC' = parseNamedColor mBCS parseUnqt (const True) BC
        where
          mBCS (Brewer bs) = Just bs
          mBCS _           = Nothing

instance NamedColor X11Color where
    colorScheme = const X11

    toColor = X11Color

    printNC = printNamedColor id

    parseNC' = parseNamedColor mX11 (parseColorScheme False) (isJust . mX11) (const id)
        where
          mX11 X11 = Just X11
          mX11 _   = Nothing

instance NamedColor SVGColor where
    colorScheme = const SVG

    toColor = SVGColor

    printNC = printNamedColor id

    parseNC' = parseNamedColor mSVG (parseColorScheme False) (isJust . mSVG) (const id)
        where
          mSVG SVG = Just SVG
          mSVG _   = Nothing

printNamedColor :: (NamedColor nc, PrintDot lv) => (nc -> lv)
                   -> Bool -> nc -> DotCode
printNamedColor fl q c = do currentCS <- getColorScheme
                            if cs == currentCS
                               then (bool unqtDot toDot q) lv
                               else bool id dquotes q
                                    $ fslash <> printColorScheme False cs
                                      <> fslash <> unqtDot lv
    where
      cs = colorScheme c
      lv = fl c

parseNamedColor :: (ParseDot lv)
                   => (ColorScheme -> Maybe cs) -> Parse cs -> (cs -> Bool)
                   -> (cs -> lv -> nc) -> Bool -> Parse nc
parseNamedColor gcs parseCS vcs mkC q
    = do Just cs <- gcs <$> getColorScheme
         lv <- bool parseUnqt parse q
               `onFail`
               mQts (string "//" *> parseUnqt)
         return $ mkC cs lv
      `onFail`
      mQts ( do character '/'
                cs <- parseCS
                character '/'
                if vcs cs
                   then mkC cs <$>  parseUnqt
                   else fail "Explicit colorscheme not as expected."
           )
    where
      mQts = bool id quotedParse q

-- -----------------------------------------------------------------------------

-- X11 has a special case when parsing: '/yyyy'

parseX11Color   :: Bool -> Parse Color
parseX11Color q = X11Color
                  <$> parseNC' q
                      `onFail`
                      bool id quotedParse q (character '/' *> parseUnqt)
                      `onFail`
                      -- Can use X11 colors within brewer colorscheme.
                      do cs <- getColorScheme
                         case cs of
                           Brewer{} -> bool parseUnqt parse q
                           _        -> fail "Unable to parse an X11 color within Brewer"

-- -----------------------------------------------------------------------------

-- | Attempt to convert a 'Color' into a 'Colour' value with an alpha
--   channel.  The use of 'Maybe' is because the RGB values of the
--   'BrewerColor's haven't been stored here (primarily for licensing
--   reasons).
toColour                :: Color -> Maybe (AlphaColour Double)
toColour (RGB r g b)    = Just . opaque $ sRGB24 r g b
toColour (RGBA r g b a) = Just . withOpacity (sRGB24 r g b) $ toOpacity a
-- Colour expects the hue to be an angle, so multiply by 360
toColour (HSV h s v)    = Just . opaque . uncurryRGB sRGB $ hsv (h*360) s v
toColour (X11Color c)   = Just $ x11Colour c
toColour (SVGColor c)   = Just . opaque $ svgColour c
toColour BrewerColor{}  = Nothing

toOpacity   :: Word8 -> Double
toOpacity a = fromIntegral a / maxWord

-- | Convert a 'Colour' value to an 'RGB' 'Color'.
fromColour :: Colour Double -> Color
fromColour = uncurryRGB RGB . toSRGB24

-- | Convert an 'AlphaColour' to an 'RGBA' 'Color'.  The exception to
--   this is for any 'AlphaColour' which has @alphaChannel ac == 0@;
--   these are converted to @X11Color 'Transparent'@ (note that the
--   'Show' instance for such an 'AlphaColour' is @\"transparent\"@).
fromAColour :: AlphaColour Double -> Color
fromAColour ac
  | a == 0    = X11Color Transparent
  | otherwise = rgb $ round a'
  where
    a = alphaChannel ac
    a' = a * maxWord
    rgb = uncurryRGB RGBA $ toSRGB24 colour
    colour = darken (recip a) (ac `over` black)

-- | The 'maxBound' of a 'Word8' value.
maxWord :: Double
maxWord = fromIntegral (maxBound :: Word8)
