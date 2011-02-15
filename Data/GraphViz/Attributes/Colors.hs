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

   When parsing and printing 'Color' values (more specifically, the
   Brewer color values), the actual 'ColorScheme' being used isn't
   taken into account.  Also, the \"@\/foo\/bar@\" style of overriding
   the 'ColorScheme' being used isn't supported for parsing purposes
   (and will result in a parsing failure).
-}
module Data.GraphViz.Attributes.Colors
       ( -- * Color schemes.
         ColorScheme(..)
       , BrewerScheme(..)
       , BrewerName(..)
         -- * Colors
       , Color(..)
       , X11Color(..)
         -- * Conversion to\/from @Colour@.
       , toColour
       , x11Colour
       , fromColour
       , fromAColour
       ) where

import Data.GraphViz.Parsing
import Data.GraphViz.Printing
import Data.GraphViz.Attributes.ColorScheme

import Data.Colour( AlphaColour, opaque, transparent, withOpacity
                  , over, black, alphaChannel, darken)
import Data.Colour.SRGB(Colour, sRGB, sRGB24, toSRGB24)
import Data.Colour.RGBSpace(uncurryRGB)
import Data.Colour.RGBSpace.HSV(hsv)

import Data.Char(isHexDigit)
import Numeric(showHex, readHex)
import Data.Word(Word8)
import qualified Data.Text.Lazy as T
import Control.Monad(liftM, liftM2)

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
           | BrewerColor BrewerScheme Word8 -- ^ This value should be
                                            --   between @1@ and the
                                            --   level of the
                                            --   'BrewerScheme' being
                                            --   used.
             deriving (Eq, Ord, Show, Read)

instance PrintDot Color where
    unqtDot (RGB  r g b)    = hexColor [r,g,b]
    unqtDot (RGBA r g b a)  = hexColor [r,g,b,a]
    unqtDot (HSV  h s v)    = hcat . punctuate comma $ map unqtDot [h,s,v]
    unqtDot (X11Color name) = unqtDot name
    unqtDot (BrewerColor _ n) = unqtDot n

    toDot (X11Color name) = toDot name
    toDot (BrewerColor _ n) = toDot n
    toDot c               = dquotes $ unqtDot c

    unqtListToDot = hcat . punctuate colon . map unqtDot

    -- These two don't need to be quoted if they're on their own.
    listToDot [X11Color name] = toDot name
    listToDot [BrewerColor _ n] = toDot n
    listToDot cs              = dquotes $ unqtListToDot cs

hexColor :: [Word8] -> DotCode
hexColor = (<>) (char '#') . hcat . map word8Doc

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
                      , liftM X11Color parseUnqt
                      , liftM (BrewerColor undefined) parseUnqt
                      ]
        where
          parseHexBased
              = do character '#'
                   cs <- many1 parse2Hex
                   return $ case cs of
                              [r,g,b] -> RGB r g b
                              [r,g,b,a] -> RGBA r g b a
                              _ -> error $ "Not a valid hex Color specification: "
                                            ++ show cs
          parseHSV = do h <- parse
                        parseSep
                        s <- parse
                        parseSep
                        v <- parse
                        return $ HSV h s v
          parseSep = oneOf [ string ","
                           , whitespace
                           ]
          parse2Hex = do c1 <- satisfy isHexDigit
                         c2 <- satisfy isHexDigit
                         let [(n, [])] = readHex [c1, c2]
                         return n


    parse = quotedParse parseUnqt
            `onFail` -- These two can be unquoted
            oneOf [ liftM X11Color parseUnqt
                  , liftM (BrewerColor undefined) parseUnqt
                  ]

    parseUnqtList = sepBy1 parseUnqt (character ':')

    parseList = liftM return
                -- Unquoted single color
                (oneOf [ liftM X11Color parseUnqt
                       , liftM (BrewerColor undefined) parseUnqt
                       ]
                )
                `onFail`
                quotedParse parseUnqtList

-- -----------------------------------------------------------------------------

-- | The X11 colors that Graphviz uses.  Note that these are slightly
--   different from the \"normal\" X11 colors used (e.g. the inclusion
--   of @Crimson@).  Graphviz's list of colors also duplicated almost
--   all @Gray@ colors with @Grey@ ones; parsing of an 'X11Color'
--   which is specified using \"grey\" will succeed, even for those
--   that don't have the duplicate spelling (e.g. @DarkSlateGray1@).
data X11Color = AliceBlue
              | AntiqueWhite
              | AntiqueWhite1
              | AntiqueWhite2
              | AntiqueWhite3
              | AntiqueWhite4
              | Aquamarine
              | Aquamarine1
              | Aquamarine2
              | Aquamarine3
              | Aquamarine4
              | Azure
              | Azure1
              | Azure2
              | Azure3
              | Azure4
              | Beige
              | Bisque
              | Bisque1
              | Bisque2
              | Bisque3
              | Bisque4
              | Black
              | BlanchedAlmond
              | Blue
              | Blue1
              | Blue2
              | Blue3
              | Blue4
              | BlueViolet
              | Brown
              | Brown1
              | Brown2
              | Brown3
              | Brown4
              | Burlywood
              | Burlywood1
              | Burlywood2
              | Burlywood3
              | Burlywood4
              | CadetBlue
              | CadetBlue1
              | CadetBlue2
              | CadetBlue3
              | CadetBlue4
              | Chartreuse
              | Chartreuse1
              | Chartreuse2
              | Chartreuse3
              | Chartreuse4
              | Chocolate
              | Chocolate1
              | Chocolate2
              | Chocolate3
              | Chocolate4
              | Coral
              | Coral1
              | Coral2
              | Coral3
              | Coral4
              | CornFlowerBlue
              | CornSilk
              | CornSilk1
              | CornSilk2
              | CornSilk3
              | CornSilk4
              | Crimson
              | Cyan
              | Cyan1
              | Cyan2
              | Cyan3
              | Cyan4
              | DarkGoldenrod
              | DarkGoldenrod1
              | DarkGoldenrod2
              | DarkGoldenrod3
              | DarkGoldenrod4
              | DarkGreen
              | Darkkhaki
              | DarkOliveGreen
              | DarkOliveGreen1
              | DarkOliveGreen2
              | DarkOliveGreen3
              | DarkOliveGreen4
              | DarkOrange
              | DarkOrange1
              | DarkOrange2
              | DarkOrange3
              | DarkOrange4
              | DarkOrchid
              | DarkOrchid1
              | DarkOrchid2
              | DarkOrchid3
              | DarkOrchid4
              | DarkSalmon
              | DarkSeaGreen
              | DarkSeaGreen1
              | DarkSeaGreen2
              | DarkSeaGreen3
              | DarkSeaGreen4
              | DarkSlateBlue
              | DarkSlateGray
              | DarkSlateGray1
              | DarkSlateGray2
              | DarkSlateGray3
              | DarkSlateGray4
              | DarkTurquoise
              | DarkViolet
              | DeepPink
              | DeepPink1
              | DeepPink2
              | DeepPink3
              | DeepPink4
              | DeepSkyBlue
              | DeepSkyBlue1
              | DeepSkyBlue2
              | DeepSkyBlue3
              | DeepSkyBlue4
              | DimGray
              | DodgerBlue
              | DodgerBlue1
              | DodgerBlue2
              | DodgerBlue3
              | DodgerBlue4
              | Firebrick
              | Firebrick1
              | Firebrick2
              | Firebrick3
              | Firebrick4
              | FloralWhite
              | ForestGreen
              | Gainsboro
              | GhostWhite
              | Gold
              | Gold1
              | Gold2
              | Gold3
              | Gold4
              | Goldenrod
              | Goldenrod1
              | Goldenrod2
              | Goldenrod3
              | Goldenrod4
              | Gray
              | Gray0
              | Gray1
              | Gray2
              | Gray3
              | Gray4
              | Gray5
              | Gray6
              | Gray7
              | Gray8
              | Gray9
              | Gray10
              | Gray11
              | Gray12
              | Gray13
              | Gray14
              | Gray15
              | Gray16
              | Gray17
              | Gray18
              | Gray19
              | Gray20
              | Gray21
              | Gray22
              | Gray23
              | Gray24
              | Gray25
              | Gray26
              | Gray27
              | Gray28
              | Gray29
              | Gray30
              | Gray31
              | Gray32
              | Gray33
              | Gray34
              | Gray35
              | Gray36
              | Gray37
              | Gray38
              | Gray39
              | Gray40
              | Gray41
              | Gray42
              | Gray43
              | Gray44
              | Gray45
              | Gray46
              | Gray47
              | Gray48
              | Gray49
              | Gray50
              | Gray51
              | Gray52
              | Gray53
              | Gray54
              | Gray55
              | Gray56
              | Gray57
              | Gray58
              | Gray59
              | Gray60
              | Gray61
              | Gray62
              | Gray63
              | Gray64
              | Gray65
              | Gray66
              | Gray67
              | Gray68
              | Gray69
              | Gray70
              | Gray71
              | Gray72
              | Gray73
              | Gray74
              | Gray75
              | Gray76
              | Gray77
              | Gray78
              | Gray79
              | Gray80
              | Gray81
              | Gray82
              | Gray83
              | Gray84
              | Gray85
              | Gray86
              | Gray87
              | Gray88
              | Gray89
              | Gray90
              | Gray91
              | Gray92
              | Gray93
              | Gray94
              | Gray95
              | Gray96
              | Gray97
              | Gray98
              | Gray99
              | Gray100
              | Green
              | Green1
              | Green2
              | Green3
              | Green4
              | GreenYellow
              | HoneyDew
              | HoneyDew1
              | HoneyDew2
              | HoneyDew3
              | HoneyDew4
              | HotPink
              | HotPink1
              | HotPink2
              | HotPink3
              | HotPink4
              | IndianRed
              | IndianRed1
              | IndianRed2
              | IndianRed3
              | IndianRed4
              | Indigo
              | Ivory
              | Ivory1
              | Ivory2
              | Ivory3
              | Ivory4
              | Khaki
              | Khaki1
              | Khaki2
              | Khaki3
              | Khaki4
              | Lavender
              | LavenderBlush
              | LavenderBlush1
              | LavenderBlush2
              | LavenderBlush3
              | LavenderBlush4
              | LawnGreen
              | LemonChiffon
              | LemonChiffon1
              | LemonChiffon2
              | LemonChiffon3
              | LemonChiffon4
              | LightBlue
              | LightBlue1
              | LightBlue2
              | LightBlue3
              | LightBlue4
              | LightCoral
              | LightCyan
              | LightCyan1
              | LightCyan2
              | LightCyan3
              | LightCyan4
              | LightGoldenrod
              | LightGoldenrod1
              | LightGoldenrod2
              | LightGoldenrod3
              | LightGoldenrod4
              | LightGoldenrodYellow
              | LightGray
              | LightPink
              | LightPink1
              | LightPink2
              | LightPink3
              | LightPink4
              | LightSalmon
              | LightSalmon1
              | LightSalmon2
              | LightSalmon3
              | LightSalmon4
              | LightSeaGreen
              | LightSkyBlue
              | LightSkyBlue1
              | LightSkyBlue2
              | LightSkyBlue3
              | LightSkyBlue4
              | LightSlateBlue
              | LightSlateGray
              | LightSteelBlue
              | LightSteelBlue1
              | LightSteelBlue2
              | LightSteelBlue3
              | LightSteelBlue4
              | LightYellow
              | LightYellow1
              | LightYellow2
              | LightYellow3
              | LightYellow4
              | LimeGreen
              | Linen
              | Magenta
              | Magenta1
              | Magenta2
              | Magenta3
              | Magenta4
              | Maroon
              | Maroon1
              | Maroon2
              | Maroon3
              | Maroon4
              | MediumAquamarine
              | MediumBlue
              | MediumOrchid
              | MediumOrchid1
              | MediumOrchid2
              | MediumOrchid3
              | MediumOrchid4
              | MediumPurple
              | MediumPurple1
              | MediumPurple2
              | MediumPurple3
              | MediumPurple4
              | MediumSeaGreen
              | MediumSlateBlue
              | MediumSpringGreen
              | MediumTurquoise
              | MediumVioletRed
              | MidnightBlue
              | MintCream
              | MistyRose
              | MistyRose1
              | MistyRose2
              | MistyRose3
              | MistyRose4
              | Moccasin
              | NavajoWhite
              | NavajoWhite1
              | NavajoWhite2
              | NavajoWhite3
              | NavajoWhite4
              | Navy
              | NavyBlue
              | OldLace
              | OliveDrab
              | OliveDrab1
              | OliveDrab2
              | OliveDrab3
              | OliveDrab4
              | Orange
              | Orange1
              | Orange2
              | Orange3
              | Orange4
              | OrangeRed
              | OrangeRed1
              | OrangeRed2
              | OrangeRed3
              | OrangeRed4
              | Orchid
              | Orchid1
              | Orchid2
              | Orchid3
              | Orchid4
              | PaleGoldenrod
              | PaleGreen
              | PaleGreen1
              | PaleGreen2
              | PaleGreen3
              | PaleGreen4
              | PaleTurquoise
              | PaleTurquoise1
              | PaleTurquoise2
              | PaleTurquoise3
              | PaleTurquoise4
              | PaleVioletRed
              | PaleVioletRed1
              | PaleVioletRed2
              | PaleVioletRed3
              | PaleVioletRed4
              | PapayaWhip
              | PeachPuff
              | PeachPuff1
              | PeachPuff2
              | PeachPuff3
              | PeachPuff4
              | Peru
              | Pink
              | Pink1
              | Pink2
              | Pink3
              | Pink4
              | Plum
              | Plum1
              | Plum2
              | Plum3
              | Plum4
              | PowderBlue
              | Purple
              | Purple1
              | Purple2
              | Purple3
              | Purple4
              | Red
              | Red1
              | Red2
              | Red3
              | Red4
              | RosyBrown
              | RosyBrown1
              | RosyBrown2
              | RosyBrown3
              | RosyBrown4
              | RoyalBlue
              | RoyalBlue1
              | RoyalBlue2
              | RoyalBlue3
              | RoyalBlue4
              | SaddleBrown
              | Salmon
              | Salmon1
              | Salmon2
              | Salmon3
              | Salmon4
              | SandyBrown
              | SeaGreen
              | SeaGreen1
              | SeaGreen2
              | SeaGreen3
              | SeaGreen4
              | SeaShell
              | SeaShell1
              | SeaShell2
              | SeaShell3
              | SeaShell4
              | Sienna
              | Sienna1
              | Sienna2
              | Sienna3
              | Sienna4
              | SkyBlue
              | SkyBlue1
              | SkyBlue2
              | SkyBlue3
              | SkyBlue4
              | SlateBlue
              | SlateBlue1
              | SlateBlue2
              | SlateBlue3
              | SlateBlue4
              | SlateGray
              | SlateGray1
              | SlateGray2
              | SlateGray3
              | SlateGray4
              | Snow
              | Snow1
              | Snow2
              | Snow3
              | Snow4
              | SpringGreen
              | SpringGreen1
              | SpringGreen2
              | SpringGreen3
              | SpringGreen4
              | SteelBlue
              | SteelBlue1
              | SteelBlue2
              | SteelBlue3
              | SteelBlue4
              | Tan
              | Tan1
              | Tan2
              | Tan3
              | Tan4
              | Thistle
              | Thistle1
              | Thistle2
              | Thistle3
              | Thistle4
              | Tomato
              | Tomato1
              | Tomato2
              | Tomato3
              | Tomato4
              | Transparent -- ^ Equivalent to setting @Style [SItem Invisible []]@.
              | Turquoise
              | Turquoise1
              | Turquoise2
              | Turquoise3
              | Turquoise4
              | Violet
              | VioletRed
              | VioletRed1
              | VioletRed2
              | VioletRed3
              | VioletRed4
              | Wheat
              | Wheat1
              | Wheat2
              | Wheat3
              | Wheat4
              | White
              | WhiteSmoke
              | Yellow
              | Yellow1
              | Yellow2
              | Yellow3
              | Yellow4
              | YellowGreen
              deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot X11Color where
  unqtDot AliceBlue            = unqtDot "aliceblue"
  unqtDot AntiqueWhite         = unqtDot "antiquewhite"
  unqtDot AntiqueWhite1        = unqtDot "antiquewhite1"
  unqtDot AntiqueWhite2        = unqtDot "antiquewhite2"
  unqtDot AntiqueWhite3        = unqtDot "antiquewhite3"
  unqtDot AntiqueWhite4        = unqtDot "antiquewhite4"
  unqtDot Aquamarine           = unqtDot "aquamarine"
  unqtDot Aquamarine1          = unqtDot "aquamarine1"
  unqtDot Aquamarine2          = unqtDot "aquamarine2"
  unqtDot Aquamarine3          = unqtDot "aquamarine3"
  unqtDot Aquamarine4          = unqtDot "aquamarine4"
  unqtDot Azure                = unqtDot "azure"
  unqtDot Azure1               = unqtDot "azure1"
  unqtDot Azure2               = unqtDot "azure2"
  unqtDot Azure3               = unqtDot "azure3"
  unqtDot Azure4               = unqtDot "azure4"
  unqtDot Beige                = unqtDot "beige"
  unqtDot Bisque               = unqtDot "bisque"
  unqtDot Bisque1              = unqtDot "bisque1"
  unqtDot Bisque2              = unqtDot "bisque2"
  unqtDot Bisque3              = unqtDot "bisque3"
  unqtDot Bisque4              = unqtDot "bisque4"
  unqtDot Black                = unqtDot "black"
  unqtDot BlanchedAlmond       = unqtDot "blanchedalmond"
  unqtDot Blue                 = unqtDot "blue"
  unqtDot Blue1                = unqtDot "blue1"
  unqtDot Blue2                = unqtDot "blue2"
  unqtDot Blue3                = unqtDot "blue3"
  unqtDot Blue4                = unqtDot "blue4"
  unqtDot BlueViolet           = unqtDot "blueviolet"
  unqtDot Brown                = unqtDot "brown"
  unqtDot Brown1               = unqtDot "brown1"
  unqtDot Brown2               = unqtDot "brown2"
  unqtDot Brown3               = unqtDot "brown3"
  unqtDot Brown4               = unqtDot "brown4"
  unqtDot Burlywood            = unqtDot "burlywood"
  unqtDot Burlywood1           = unqtDot "burlywood1"
  unqtDot Burlywood2           = unqtDot "burlywood2"
  unqtDot Burlywood3           = unqtDot "burlywood3"
  unqtDot Burlywood4           = unqtDot "burlywood4"
  unqtDot CadetBlue            = unqtDot "cadetblue"
  unqtDot CadetBlue1           = unqtDot "cadetblue1"
  unqtDot CadetBlue2           = unqtDot "cadetblue2"
  unqtDot CadetBlue3           = unqtDot "cadetblue3"
  unqtDot CadetBlue4           = unqtDot "cadetblue4"
  unqtDot Chartreuse           = unqtDot "chartreuse"
  unqtDot Chartreuse1          = unqtDot "chartreuse1"
  unqtDot Chartreuse2          = unqtDot "chartreuse2"
  unqtDot Chartreuse3          = unqtDot "chartreuse3"
  unqtDot Chartreuse4          = unqtDot "chartreuse4"
  unqtDot Chocolate            = unqtDot "chocolate"
  unqtDot Chocolate1           = unqtDot "chocolate1"
  unqtDot Chocolate2           = unqtDot "chocolate2"
  unqtDot Chocolate3           = unqtDot "chocolate3"
  unqtDot Chocolate4           = unqtDot "chocolate4"
  unqtDot Coral                = unqtDot "coral"
  unqtDot Coral1               = unqtDot "coral1"
  unqtDot Coral2               = unqtDot "coral2"
  unqtDot Coral3               = unqtDot "coral3"
  unqtDot Coral4               = unqtDot "coral4"
  unqtDot CornFlowerBlue       = unqtDot "cornflowerblue"
  unqtDot CornSilk             = unqtDot "cornsilk"
  unqtDot CornSilk1            = unqtDot "cornsilk1"
  unqtDot CornSilk2            = unqtDot "cornsilk2"
  unqtDot CornSilk3            = unqtDot "cornsilk3"
  unqtDot CornSilk4            = unqtDot "cornsilk4"
  unqtDot Crimson              = unqtDot "crimson"
  unqtDot Cyan                 = unqtDot "cyan"
  unqtDot Cyan1                = unqtDot "cyan1"
  unqtDot Cyan2                = unqtDot "cyan2"
  unqtDot Cyan3                = unqtDot "cyan3"
  unqtDot Cyan4                = unqtDot "cyan4"
  unqtDot DarkGoldenrod        = unqtDot "darkgoldenrod"
  unqtDot DarkGoldenrod1       = unqtDot "darkgoldenrod1"
  unqtDot DarkGoldenrod2       = unqtDot "darkgoldenrod2"
  unqtDot DarkGoldenrod3       = unqtDot "darkgoldenrod3"
  unqtDot DarkGoldenrod4       = unqtDot "darkgoldenrod4"
  unqtDot DarkGreen            = unqtDot "darkgreen"
  unqtDot Darkkhaki            = unqtDot "darkkhaki"
  unqtDot DarkOliveGreen       = unqtDot "darkolivegreen"
  unqtDot DarkOliveGreen1      = unqtDot "darkolivegreen1"
  unqtDot DarkOliveGreen2      = unqtDot "darkolivegreen2"
  unqtDot DarkOliveGreen3      = unqtDot "darkolivegreen3"
  unqtDot DarkOliveGreen4      = unqtDot "darkolivegreen4"
  unqtDot DarkOrange           = unqtDot "darkorange"
  unqtDot DarkOrange1          = unqtDot "darkorange1"
  unqtDot DarkOrange2          = unqtDot "darkorange2"
  unqtDot DarkOrange3          = unqtDot "darkorange3"
  unqtDot DarkOrange4          = unqtDot "darkorange4"
  unqtDot DarkOrchid           = unqtDot "darkorchid"
  unqtDot DarkOrchid1          = unqtDot "darkorchid1"
  unqtDot DarkOrchid2          = unqtDot "darkorchid2"
  unqtDot DarkOrchid3          = unqtDot "darkorchid3"
  unqtDot DarkOrchid4          = unqtDot "darkorchid4"
  unqtDot DarkSalmon           = unqtDot "darksalmon"
  unqtDot DarkSeaGreen         = unqtDot "darkseagreen"
  unqtDot DarkSeaGreen1        = unqtDot "darkseagreen1"
  unqtDot DarkSeaGreen2        = unqtDot "darkseagreen2"
  unqtDot DarkSeaGreen3        = unqtDot "darkseagreen3"
  unqtDot DarkSeaGreen4        = unqtDot "darkseagreen4"
  unqtDot DarkSlateBlue        = unqtDot "darkslateblue"
  unqtDot DarkSlateGray        = unqtDot "darkslategray"
  unqtDot DarkSlateGray1       = unqtDot "darkslategray1"
  unqtDot DarkSlateGray2       = unqtDot "darkslategray2"
  unqtDot DarkSlateGray3       = unqtDot "darkslategray3"
  unqtDot DarkSlateGray4       = unqtDot "darkslategray4"
  unqtDot DarkTurquoise        = unqtDot "darkturquoise"
  unqtDot DarkViolet           = unqtDot "darkviolet"
  unqtDot DeepPink             = unqtDot "deeppink"
  unqtDot DeepPink1            = unqtDot "deeppink1"
  unqtDot DeepPink2            = unqtDot "deeppink2"
  unqtDot DeepPink3            = unqtDot "deeppink3"
  unqtDot DeepPink4            = unqtDot "deeppink4"
  unqtDot DeepSkyBlue          = unqtDot "deepskyblue"
  unqtDot DeepSkyBlue1         = unqtDot "deepskyblue1"
  unqtDot DeepSkyBlue2         = unqtDot "deepskyblue2"
  unqtDot DeepSkyBlue3         = unqtDot "deepskyblue3"
  unqtDot DeepSkyBlue4         = unqtDot "deepskyblue4"
  unqtDot DimGray              = unqtDot "dimgray"
  unqtDot DodgerBlue           = unqtDot "dodgerblue"
  unqtDot DodgerBlue1          = unqtDot "dodgerblue1"
  unqtDot DodgerBlue2          = unqtDot "dodgerblue2"
  unqtDot DodgerBlue3          = unqtDot "dodgerblue3"
  unqtDot DodgerBlue4          = unqtDot "dodgerblue4"
  unqtDot Firebrick            = unqtDot "firebrick"
  unqtDot Firebrick1           = unqtDot "firebrick1"
  unqtDot Firebrick2           = unqtDot "firebrick2"
  unqtDot Firebrick3           = unqtDot "firebrick3"
  unqtDot Firebrick4           = unqtDot "firebrick4"
  unqtDot FloralWhite          = unqtDot "floralwhite"
  unqtDot ForestGreen          = unqtDot "forestgreen"
  unqtDot Gainsboro            = unqtDot "gainsboro"
  unqtDot GhostWhite           = unqtDot "ghostwhite"
  unqtDot Gold                 = unqtDot "gold"
  unqtDot Gold1                = unqtDot "gold1"
  unqtDot Gold2                = unqtDot "gold2"
  unqtDot Gold3                = unqtDot "gold3"
  unqtDot Gold4                = unqtDot "gold4"
  unqtDot Goldenrod            = unqtDot "goldenrod"
  unqtDot Goldenrod1           = unqtDot "goldenrod1"
  unqtDot Goldenrod2           = unqtDot "goldenrod2"
  unqtDot Goldenrod3           = unqtDot "goldenrod3"
  unqtDot Goldenrod4           = unqtDot "goldenrod4"
  unqtDot Gray                 = unqtDot "gray"
  unqtDot Gray0                = unqtDot "gray0"
  unqtDot Gray1                = unqtDot "gray1"
  unqtDot Gray2                = unqtDot "gray2"
  unqtDot Gray3                = unqtDot "gray3"
  unqtDot Gray4                = unqtDot "gray4"
  unqtDot Gray5                = unqtDot "gray5"
  unqtDot Gray6                = unqtDot "gray6"
  unqtDot Gray7                = unqtDot "gray7"
  unqtDot Gray8                = unqtDot "gray8"
  unqtDot Gray9                = unqtDot "gray9"
  unqtDot Gray10               = unqtDot "gray10"
  unqtDot Gray11               = unqtDot "gray11"
  unqtDot Gray12               = unqtDot "gray12"
  unqtDot Gray13               = unqtDot "gray13"
  unqtDot Gray14               = unqtDot "gray14"
  unqtDot Gray15               = unqtDot "gray15"
  unqtDot Gray16               = unqtDot "gray16"
  unqtDot Gray17               = unqtDot "gray17"
  unqtDot Gray18               = unqtDot "gray18"
  unqtDot Gray19               = unqtDot "gray19"
  unqtDot Gray20               = unqtDot "gray20"
  unqtDot Gray21               = unqtDot "gray21"
  unqtDot Gray22               = unqtDot "gray22"
  unqtDot Gray23               = unqtDot "gray23"
  unqtDot Gray24               = unqtDot "gray24"
  unqtDot Gray25               = unqtDot "gray25"
  unqtDot Gray26               = unqtDot "gray26"
  unqtDot Gray27               = unqtDot "gray27"
  unqtDot Gray28               = unqtDot "gray28"
  unqtDot Gray29               = unqtDot "gray29"
  unqtDot Gray30               = unqtDot "gray30"
  unqtDot Gray31               = unqtDot "gray31"
  unqtDot Gray32               = unqtDot "gray32"
  unqtDot Gray33               = unqtDot "gray33"
  unqtDot Gray34               = unqtDot "gray34"
  unqtDot Gray35               = unqtDot "gray35"
  unqtDot Gray36               = unqtDot "gray36"
  unqtDot Gray37               = unqtDot "gray37"
  unqtDot Gray38               = unqtDot "gray38"
  unqtDot Gray39               = unqtDot "gray39"
  unqtDot Gray40               = unqtDot "gray40"
  unqtDot Gray41               = unqtDot "gray41"
  unqtDot Gray42               = unqtDot "gray42"
  unqtDot Gray43               = unqtDot "gray43"
  unqtDot Gray44               = unqtDot "gray44"
  unqtDot Gray45               = unqtDot "gray45"
  unqtDot Gray46               = unqtDot "gray46"
  unqtDot Gray47               = unqtDot "gray47"
  unqtDot Gray48               = unqtDot "gray48"
  unqtDot Gray49               = unqtDot "gray49"
  unqtDot Gray50               = unqtDot "gray50"
  unqtDot Gray51               = unqtDot "gray51"
  unqtDot Gray52               = unqtDot "gray52"
  unqtDot Gray53               = unqtDot "gray53"
  unqtDot Gray54               = unqtDot "gray54"
  unqtDot Gray55               = unqtDot "gray55"
  unqtDot Gray56               = unqtDot "gray56"
  unqtDot Gray57               = unqtDot "gray57"
  unqtDot Gray58               = unqtDot "gray58"
  unqtDot Gray59               = unqtDot "gray59"
  unqtDot Gray60               = unqtDot "gray60"
  unqtDot Gray61               = unqtDot "gray61"
  unqtDot Gray62               = unqtDot "gray62"
  unqtDot Gray63               = unqtDot "gray63"
  unqtDot Gray64               = unqtDot "gray64"
  unqtDot Gray65               = unqtDot "gray65"
  unqtDot Gray66               = unqtDot "gray66"
  unqtDot Gray67               = unqtDot "gray67"
  unqtDot Gray68               = unqtDot "gray68"
  unqtDot Gray69               = unqtDot "gray69"
  unqtDot Gray70               = unqtDot "gray70"
  unqtDot Gray71               = unqtDot "gray71"
  unqtDot Gray72               = unqtDot "gray72"
  unqtDot Gray73               = unqtDot "gray73"
  unqtDot Gray74               = unqtDot "gray74"
  unqtDot Gray75               = unqtDot "gray75"
  unqtDot Gray76               = unqtDot "gray76"
  unqtDot Gray77               = unqtDot "gray77"
  unqtDot Gray78               = unqtDot "gray78"
  unqtDot Gray79               = unqtDot "gray79"
  unqtDot Gray80               = unqtDot "gray80"
  unqtDot Gray81               = unqtDot "gray81"
  unqtDot Gray82               = unqtDot "gray82"
  unqtDot Gray83               = unqtDot "gray83"
  unqtDot Gray84               = unqtDot "gray84"
  unqtDot Gray85               = unqtDot "gray85"
  unqtDot Gray86               = unqtDot "gray86"
  unqtDot Gray87               = unqtDot "gray87"
  unqtDot Gray88               = unqtDot "gray88"
  unqtDot Gray89               = unqtDot "gray89"
  unqtDot Gray90               = unqtDot "gray90"
  unqtDot Gray91               = unqtDot "gray91"
  unqtDot Gray92               = unqtDot "gray92"
  unqtDot Gray93               = unqtDot "gray93"
  unqtDot Gray94               = unqtDot "gray94"
  unqtDot Gray95               = unqtDot "gray95"
  unqtDot Gray96               = unqtDot "gray96"
  unqtDot Gray97               = unqtDot "gray97"
  unqtDot Gray98               = unqtDot "gray98"
  unqtDot Gray99               = unqtDot "gray99"
  unqtDot Gray100              = unqtDot "gray100"
  unqtDot Green                = unqtDot "green"
  unqtDot Green1               = unqtDot "green1"
  unqtDot Green2               = unqtDot "green2"
  unqtDot Green3               = unqtDot "green3"
  unqtDot Green4               = unqtDot "green4"
  unqtDot GreenYellow          = unqtDot "greenyellow"
  unqtDot HoneyDew             = unqtDot "honeydew"
  unqtDot HoneyDew1            = unqtDot "honeydew1"
  unqtDot HoneyDew2            = unqtDot "honeydew2"
  unqtDot HoneyDew3            = unqtDot "honeydew3"
  unqtDot HoneyDew4            = unqtDot "honeydew4"
  unqtDot HotPink              = unqtDot "hotpink"
  unqtDot HotPink1             = unqtDot "hotpink1"
  unqtDot HotPink2             = unqtDot "hotpink2"
  unqtDot HotPink3             = unqtDot "hotpink3"
  unqtDot HotPink4             = unqtDot "hotpink4"
  unqtDot IndianRed            = unqtDot "indianred"
  unqtDot IndianRed1           = unqtDot "indianred1"
  unqtDot IndianRed2           = unqtDot "indianred2"
  unqtDot IndianRed3           = unqtDot "indianred3"
  unqtDot IndianRed4           = unqtDot "indianred4"
  unqtDot Indigo               = unqtDot "indigo"
  unqtDot Ivory                = unqtDot "ivory"
  unqtDot Ivory1               = unqtDot "ivory1"
  unqtDot Ivory2               = unqtDot "ivory2"
  unqtDot Ivory3               = unqtDot "ivory3"
  unqtDot Ivory4               = unqtDot "ivory4"
  unqtDot Khaki                = unqtDot "khaki"
  unqtDot Khaki1               = unqtDot "khaki1"
  unqtDot Khaki2               = unqtDot "khaki2"
  unqtDot Khaki3               = unqtDot "khaki3"
  unqtDot Khaki4               = unqtDot "khaki4"
  unqtDot Lavender             = unqtDot "lavender"
  unqtDot LavenderBlush        = unqtDot "lavenderblush"
  unqtDot LavenderBlush1       = unqtDot "lavenderblush1"
  unqtDot LavenderBlush2       = unqtDot "lavenderblush2"
  unqtDot LavenderBlush3       = unqtDot "lavenderblush3"
  unqtDot LavenderBlush4       = unqtDot "lavenderblush4"
  unqtDot LawnGreen            = unqtDot "lawngreen"
  unqtDot LemonChiffon         = unqtDot "lemonchiffon"
  unqtDot LemonChiffon1        = unqtDot "lemonchiffon1"
  unqtDot LemonChiffon2        = unqtDot "lemonchiffon2"
  unqtDot LemonChiffon3        = unqtDot "lemonchiffon3"
  unqtDot LemonChiffon4        = unqtDot "lemonchiffon4"
  unqtDot LightBlue            = unqtDot "lightblue"
  unqtDot LightBlue1           = unqtDot "lightblue1"
  unqtDot LightBlue2           = unqtDot "lightblue2"
  unqtDot LightBlue3           = unqtDot "lightblue3"
  unqtDot LightBlue4           = unqtDot "lightblue4"
  unqtDot LightCoral           = unqtDot "lightcoral"
  unqtDot LightCyan            = unqtDot "lightcyan"
  unqtDot LightCyan1           = unqtDot "lightcyan1"
  unqtDot LightCyan2           = unqtDot "lightcyan2"
  unqtDot LightCyan3           = unqtDot "lightcyan3"
  unqtDot LightCyan4           = unqtDot "lightcyan4"
  unqtDot LightGoldenrod       = unqtDot "lightgoldenrod"
  unqtDot LightGoldenrod1      = unqtDot "lightgoldenrod1"
  unqtDot LightGoldenrod2      = unqtDot "lightgoldenrod2"
  unqtDot LightGoldenrod3      = unqtDot "lightgoldenrod3"
  unqtDot LightGoldenrod4      = unqtDot "lightgoldenrod4"
  unqtDot LightGoldenrodYellow = unqtDot "lightgoldenrodyellow"
  unqtDot LightGray            = unqtDot "lightgray"
  unqtDot LightPink            = unqtDot "lightpink"
  unqtDot LightPink1           = unqtDot "lightpink1"
  unqtDot LightPink2           = unqtDot "lightpink2"
  unqtDot LightPink3           = unqtDot "lightpink3"
  unqtDot LightPink4           = unqtDot "lightpink4"
  unqtDot LightSalmon          = unqtDot "lightsalmon"
  unqtDot LightSalmon1         = unqtDot "lightsalmon1"
  unqtDot LightSalmon2         = unqtDot "lightsalmon2"
  unqtDot LightSalmon3         = unqtDot "lightsalmon3"
  unqtDot LightSalmon4         = unqtDot "lightsalmon4"
  unqtDot LightSeaGreen        = unqtDot "lightseagreen"
  unqtDot LightSkyBlue         = unqtDot "lightskyblue"
  unqtDot LightSkyBlue1        = unqtDot "lightskyblue1"
  unqtDot LightSkyBlue2        = unqtDot "lightskyblue2"
  unqtDot LightSkyBlue3        = unqtDot "lightskyblue3"
  unqtDot LightSkyBlue4        = unqtDot "lightskyblue4"
  unqtDot LightSlateBlue       = unqtDot "lightslateblue"
  unqtDot LightSlateGray       = unqtDot "lightslategray"
  unqtDot LightSteelBlue       = unqtDot "lightsteelblue"
  unqtDot LightSteelBlue1      = unqtDot "lightsteelblue1"
  unqtDot LightSteelBlue2      = unqtDot "lightsteelblue2"
  unqtDot LightSteelBlue3      = unqtDot "lightsteelblue3"
  unqtDot LightSteelBlue4      = unqtDot "lightsteelblue4"
  unqtDot LightYellow          = unqtDot "lightyellow"
  unqtDot LightYellow1         = unqtDot "lightyellow1"
  unqtDot LightYellow2         = unqtDot "lightyellow2"
  unqtDot LightYellow3         = unqtDot "lightyellow3"
  unqtDot LightYellow4         = unqtDot "lightyellow4"
  unqtDot LimeGreen            = unqtDot "limegreen"
  unqtDot Linen                = unqtDot "linen"
  unqtDot Magenta              = unqtDot "magenta"
  unqtDot Magenta1             = unqtDot "magenta1"
  unqtDot Magenta2             = unqtDot "magenta2"
  unqtDot Magenta3             = unqtDot "magenta3"
  unqtDot Magenta4             = unqtDot "magenta4"
  unqtDot Maroon               = unqtDot "maroon"
  unqtDot Maroon1              = unqtDot "maroon1"
  unqtDot Maroon2              = unqtDot "maroon2"
  unqtDot Maroon3              = unqtDot "maroon3"
  unqtDot Maroon4              = unqtDot "maroon4"
  unqtDot MediumAquamarine     = unqtDot "mediumaquamarine"
  unqtDot MediumBlue           = unqtDot "mediumblue"
  unqtDot MediumOrchid         = unqtDot "mediumorchid"
  unqtDot MediumOrchid1        = unqtDot "mediumorchid1"
  unqtDot MediumOrchid2        = unqtDot "mediumorchid2"
  unqtDot MediumOrchid3        = unqtDot "mediumorchid3"
  unqtDot MediumOrchid4        = unqtDot "mediumorchid4"
  unqtDot MediumPurple         = unqtDot "mediumpurple"
  unqtDot MediumPurple1        = unqtDot "mediumpurple1"
  unqtDot MediumPurple2        = unqtDot "mediumpurple2"
  unqtDot MediumPurple3        = unqtDot "mediumpurple3"
  unqtDot MediumPurple4        = unqtDot "mediumpurple4"
  unqtDot MediumSeaGreen       = unqtDot "mediumseagreen"
  unqtDot MediumSlateBlue      = unqtDot "mediumslateblue"
  unqtDot MediumSpringGreen    = unqtDot "mediumspringgreen"
  unqtDot MediumTurquoise      = unqtDot "mediumturquoise"
  unqtDot MediumVioletRed      = unqtDot "mediumvioletred"
  unqtDot MidnightBlue         = unqtDot "midnightblue"
  unqtDot MintCream            = unqtDot "mintcream"
  unqtDot MistyRose            = unqtDot "mistyrose"
  unqtDot MistyRose1           = unqtDot "mistyrose1"
  unqtDot MistyRose2           = unqtDot "mistyrose2"
  unqtDot MistyRose3           = unqtDot "mistyrose3"
  unqtDot MistyRose4           = unqtDot "mistyrose4"
  unqtDot Moccasin             = unqtDot "moccasin"
  unqtDot NavajoWhite          = unqtDot "navajowhite"
  unqtDot NavajoWhite1         = unqtDot "navajowhite1"
  unqtDot NavajoWhite2         = unqtDot "navajowhite2"
  unqtDot NavajoWhite3         = unqtDot "navajowhite3"
  unqtDot NavajoWhite4         = unqtDot "navajowhite4"
  unqtDot Navy                 = unqtDot "navy"
  unqtDot NavyBlue             = unqtDot "navyblue"
  unqtDot OldLace              = unqtDot "oldlace"
  unqtDot OliveDrab            = unqtDot "olivedrab"
  unqtDot OliveDrab1           = unqtDot "olivedrab1"
  unqtDot OliveDrab2           = unqtDot "olivedrab2"
  unqtDot OliveDrab3           = unqtDot "olivedrab3"
  unqtDot OliveDrab4           = unqtDot "olivedrab4"
  unqtDot Orange               = unqtDot "orange"
  unqtDot Orange1              = unqtDot "orange1"
  unqtDot Orange2              = unqtDot "orange2"
  unqtDot Orange3              = unqtDot "orange3"
  unqtDot Orange4              = unqtDot "orange4"
  unqtDot OrangeRed            = unqtDot "orangered"
  unqtDot OrangeRed1           = unqtDot "orangered1"
  unqtDot OrangeRed2           = unqtDot "orangered2"
  unqtDot OrangeRed3           = unqtDot "orangered3"
  unqtDot OrangeRed4           = unqtDot "orangered4"
  unqtDot Orchid               = unqtDot "orchid"
  unqtDot Orchid1              = unqtDot "orchid1"
  unqtDot Orchid2              = unqtDot "orchid2"
  unqtDot Orchid3              = unqtDot "orchid3"
  unqtDot Orchid4              = unqtDot "orchid4"
  unqtDot PaleGoldenrod        = unqtDot "palegoldenrod"
  unqtDot PaleGreen            = unqtDot "palegreen"
  unqtDot PaleGreen1           = unqtDot "palegreen1"
  unqtDot PaleGreen2           = unqtDot "palegreen2"
  unqtDot PaleGreen3           = unqtDot "palegreen3"
  unqtDot PaleGreen4           = unqtDot "palegreen4"
  unqtDot PaleTurquoise        = unqtDot "paleturquoise"
  unqtDot PaleTurquoise1       = unqtDot "paleturquoise1"
  unqtDot PaleTurquoise2       = unqtDot "paleturquoise2"
  unqtDot PaleTurquoise3       = unqtDot "paleturquoise3"
  unqtDot PaleTurquoise4       = unqtDot "paleturquoise4"
  unqtDot PaleVioletRed        = unqtDot "palevioletred"
  unqtDot PaleVioletRed1       = unqtDot "palevioletred1"
  unqtDot PaleVioletRed2       = unqtDot "palevioletred2"
  unqtDot PaleVioletRed3       = unqtDot "palevioletred3"
  unqtDot PaleVioletRed4       = unqtDot "palevioletred4"
  unqtDot PapayaWhip           = unqtDot "papayawhip"
  unqtDot PeachPuff            = unqtDot "peachpuff"
  unqtDot PeachPuff1           = unqtDot "peachpuff1"
  unqtDot PeachPuff2           = unqtDot "peachpuff2"
  unqtDot PeachPuff3           = unqtDot "peachpuff3"
  unqtDot PeachPuff4           = unqtDot "peachpuff4"
  unqtDot Peru                 = unqtDot "peru"
  unqtDot Pink                 = unqtDot "pink"
  unqtDot Pink1                = unqtDot "pink1"
  unqtDot Pink2                = unqtDot "pink2"
  unqtDot Pink3                = unqtDot "pink3"
  unqtDot Pink4                = unqtDot "pink4"
  unqtDot Plum                 = unqtDot "plum"
  unqtDot Plum1                = unqtDot "plum1"
  unqtDot Plum2                = unqtDot "plum2"
  unqtDot Plum3                = unqtDot "plum3"
  unqtDot Plum4                = unqtDot "plum4"
  unqtDot PowderBlue           = unqtDot "powderblue"
  unqtDot Purple               = unqtDot "purple"
  unqtDot Purple1              = unqtDot "purple1"
  unqtDot Purple2              = unqtDot "purple2"
  unqtDot Purple3              = unqtDot "purple3"
  unqtDot Purple4              = unqtDot "purple4"
  unqtDot Red                  = unqtDot "red"
  unqtDot Red1                 = unqtDot "red1"
  unqtDot Red2                 = unqtDot "red2"
  unqtDot Red3                 = unqtDot "red3"
  unqtDot Red4                 = unqtDot "red4"
  unqtDot RosyBrown            = unqtDot "rosybrown"
  unqtDot RosyBrown1           = unqtDot "rosybrown1"
  unqtDot RosyBrown2           = unqtDot "rosybrown2"
  unqtDot RosyBrown3           = unqtDot "rosybrown3"
  unqtDot RosyBrown4           = unqtDot "rosybrown4"
  unqtDot RoyalBlue            = unqtDot "royalblue"
  unqtDot RoyalBlue1           = unqtDot "royalblue1"
  unqtDot RoyalBlue2           = unqtDot "royalblue2"
  unqtDot RoyalBlue3           = unqtDot "royalblue3"
  unqtDot RoyalBlue4           = unqtDot "royalblue4"
  unqtDot SaddleBrown          = unqtDot "saddlebrown"
  unqtDot Salmon               = unqtDot "salmon"
  unqtDot Salmon1              = unqtDot "salmon1"
  unqtDot Salmon2              = unqtDot "salmon2"
  unqtDot Salmon3              = unqtDot "salmon3"
  unqtDot Salmon4              = unqtDot "salmon4"
  unqtDot SandyBrown           = unqtDot "sandybrown"
  unqtDot SeaGreen             = unqtDot "seagreen"
  unqtDot SeaGreen1            = unqtDot "seagreen1"
  unqtDot SeaGreen2            = unqtDot "seagreen2"
  unqtDot SeaGreen3            = unqtDot "seagreen3"
  unqtDot SeaGreen4            = unqtDot "seagreen4"
  unqtDot SeaShell             = unqtDot "seashell"
  unqtDot SeaShell1            = unqtDot "seashell1"
  unqtDot SeaShell2            = unqtDot "seashell2"
  unqtDot SeaShell3            = unqtDot "seashell3"
  unqtDot SeaShell4            = unqtDot "seashell4"
  unqtDot Sienna               = unqtDot "sienna"
  unqtDot Sienna1              = unqtDot "sienna1"
  unqtDot Sienna2              = unqtDot "sienna2"
  unqtDot Sienna3              = unqtDot "sienna3"
  unqtDot Sienna4              = unqtDot "sienna4"
  unqtDot SkyBlue              = unqtDot "skyblue"
  unqtDot SkyBlue1             = unqtDot "skyblue1"
  unqtDot SkyBlue2             = unqtDot "skyblue2"
  unqtDot SkyBlue3             = unqtDot "skyblue3"
  unqtDot SkyBlue4             = unqtDot "skyblue4"
  unqtDot SlateBlue            = unqtDot "slateblue"
  unqtDot SlateBlue1           = unqtDot "slateblue1"
  unqtDot SlateBlue2           = unqtDot "slateblue2"
  unqtDot SlateBlue3           = unqtDot "slateblue3"
  unqtDot SlateBlue4           = unqtDot "slateblue4"
  unqtDot SlateGray            = unqtDot "slategray"
  unqtDot SlateGray1           = unqtDot "slategray1"
  unqtDot SlateGray2           = unqtDot "slategray2"
  unqtDot SlateGray3           = unqtDot "slategray3"
  unqtDot SlateGray4           = unqtDot "slategray4"
  unqtDot Snow                 = unqtDot "snow"
  unqtDot Snow1                = unqtDot "snow1"
  unqtDot Snow2                = unqtDot "snow2"
  unqtDot Snow3                = unqtDot "snow3"
  unqtDot Snow4                = unqtDot "snow4"
  unqtDot SpringGreen          = unqtDot "springgreen"
  unqtDot SpringGreen1         = unqtDot "springgreen1"
  unqtDot SpringGreen2         = unqtDot "springgreen2"
  unqtDot SpringGreen3         = unqtDot "springgreen3"
  unqtDot SpringGreen4         = unqtDot "springgreen4"
  unqtDot SteelBlue            = unqtDot "steelblue"
  unqtDot SteelBlue1           = unqtDot "steelblue1"
  unqtDot SteelBlue2           = unqtDot "steelblue2"
  unqtDot SteelBlue3           = unqtDot "steelblue3"
  unqtDot SteelBlue4           = unqtDot "steelblue4"
  unqtDot Tan                  = unqtDot "tan"
  unqtDot Tan1                 = unqtDot "tan1"
  unqtDot Tan2                 = unqtDot "tan2"
  unqtDot Tan3                 = unqtDot "tan3"
  unqtDot Tan4                 = unqtDot "tan4"
  unqtDot Thistle              = unqtDot "thistle"
  unqtDot Thistle1             = unqtDot "thistle1"
  unqtDot Thistle2             = unqtDot "thistle2"
  unqtDot Thistle3             = unqtDot "thistle3"
  unqtDot Thistle4             = unqtDot "thistle4"
  unqtDot Tomato               = unqtDot "tomato"
  unqtDot Tomato1              = unqtDot "tomato1"
  unqtDot Tomato2              = unqtDot "tomato2"
  unqtDot Tomato3              = unqtDot "tomato3"
  unqtDot Tomato4              = unqtDot "tomato4"
  unqtDot Transparent          = unqtDot "transparent"
  unqtDot Turquoise            = unqtDot "turquoise"
  unqtDot Turquoise1           = unqtDot "turquoise1"
  unqtDot Turquoise2           = unqtDot "turquoise2"
  unqtDot Turquoise3           = unqtDot "turquoise3"
  unqtDot Turquoise4           = unqtDot "turquoise4"
  unqtDot Violet               = unqtDot "violet"
  unqtDot VioletRed            = unqtDot "violetred"
  unqtDot VioletRed1           = unqtDot "violetred1"
  unqtDot VioletRed2           = unqtDot "violetred2"
  unqtDot VioletRed3           = unqtDot "violetred3"
  unqtDot VioletRed4           = unqtDot "violetred4"
  unqtDot Wheat                = unqtDot "wheat"
  unqtDot Wheat1               = unqtDot "wheat1"
  unqtDot Wheat2               = unqtDot "wheat2"
  unqtDot Wheat3               = unqtDot "wheat3"
  unqtDot Wheat4               = unqtDot "wheat4"
  unqtDot White                = unqtDot "white"
  unqtDot WhiteSmoke           = unqtDot "whitesmoke"
  unqtDot Yellow               = unqtDot "yellow"
  unqtDot Yellow1              = unqtDot "yellow1"
  unqtDot Yellow2              = unqtDot "yellow2"
  unqtDot Yellow3              = unqtDot "yellow3"
  unqtDot Yellow4              = unqtDot "yellow4"
  unqtDot YellowGreen          = unqtDot "yellowgreen"

instance ParseDot X11Color where
  parseUnqt = oneOf
              $ reverse [ stringRep  AliceBlue "aliceblue"
                        , stringRep  AntiqueWhite "antiquewhite"
                        , stringRep  AntiqueWhite1 "antiquewhite1"
                        , stringRep  AntiqueWhite2 "antiquewhite2"
                        , stringRep  AntiqueWhite3 "antiquewhite3"
                        , stringRep  AntiqueWhite4 "antiquewhite4"
                        , stringRep  Aquamarine "aquamarine"
                        , stringRep  Aquamarine1 "aquamarine1"
                        , stringRep  Aquamarine2 "aquamarine2"
                        , stringRep  Aquamarine3 "aquamarine3"
                        , stringRep  Aquamarine4 "aquamarine4"
                        , stringRep  Azure "azure"
                        , stringRep  Azure1 "azure1"
                        , stringRep  Azure2 "azure2"
                        , stringRep  Azure3 "azure3"
                        , stringRep  Azure4 "azure4"
                        , stringRep  Beige "beige"
                        , stringRep  Bisque "bisque"
                        , stringRep  Bisque1 "bisque1"
                        , stringRep  Bisque2 "bisque2"
                        , stringRep  Bisque3 "bisque3"
                        , stringRep  Bisque4 "bisque4"
                        , stringRep  Black "black"
                        , stringRep  BlanchedAlmond "blanchedalmond"
                        , stringRep  Blue "blue"
                        , stringRep  Blue1 "blue1"
                        , stringRep  Blue2 "blue2"
                        , stringRep  Blue3 "blue3"
                        , stringRep  Blue4 "blue4"
                        , stringRep  BlueViolet "blueviolet"
                        , stringRep  Brown "brown"
                        , stringRep  Brown1 "brown1"
                        , stringRep  Brown2 "brown2"
                        , stringRep  Brown3 "brown3"
                        , stringRep  Brown4 "brown4"
                        , stringRep  Burlywood "burlywood"
                        , stringRep  Burlywood1 "burlywood1"
                        , stringRep  Burlywood2 "burlywood2"
                        , stringRep  Burlywood3 "burlywood3"
                        , stringRep  Burlywood4 "burlywood4"
                        , stringRep  CadetBlue "cadetblue"
                        , stringRep  CadetBlue1 "cadetblue1"
                        , stringRep  CadetBlue2 "cadetblue2"
                        , stringRep  CadetBlue3 "cadetblue3"
                        , stringRep  CadetBlue4 "cadetblue4"
                        , stringRep  Chartreuse "chartreuse"
                        , stringRep  Chartreuse1 "chartreuse1"
                        , stringRep  Chartreuse2 "chartreuse2"
                        , stringRep  Chartreuse3 "chartreuse3"
                        , stringRep  Chartreuse4 "chartreuse4"
                        , stringRep  Chocolate "chocolate"
                        , stringRep  Chocolate1 "chocolate1"
                        , stringRep  Chocolate2 "chocolate2"
                        , stringRep  Chocolate3 "chocolate3"
                        , stringRep  Chocolate4 "chocolate4"
                        , stringRep  Coral "coral"
                        , stringRep  Coral1 "coral1"
                        , stringRep  Coral2 "coral2"
                        , stringRep  Coral3 "coral3"
                        , stringRep  Coral4 "coral4"
                        , stringRep  CornFlowerBlue "cornflowerblue"
                        , stringRep  CornSilk "cornsilk"
                        , stringRep  CornSilk1 "cornsilk1"
                        , stringRep  CornSilk2 "cornsilk2"
                        , stringRep  CornSilk3 "cornsilk3"
                        , stringRep  CornSilk4 "cornsilk4"
                        , stringRep  Crimson "crimson"
                        , stringRep  Cyan "cyan"
                        , stringRep  Cyan1 "cyan1"
                        , stringRep  Cyan2 "cyan2"
                        , stringRep  Cyan3 "cyan3"
                        , stringRep  Cyan4 "cyan4"
                        , stringRep  DarkGoldenrod "darkgoldenrod"
                        , stringRep  DarkGoldenrod1 "darkgoldenrod1"
                        , stringRep  DarkGoldenrod2 "darkgoldenrod2"
                        , stringRep  DarkGoldenrod3 "darkgoldenrod3"
                        , stringRep  DarkGoldenrod4 "darkgoldenrod4"
                        , stringRep  DarkGreen "darkgreen"
                        , stringRep  Darkkhaki "darkkhaki"
                        , stringRep  DarkOliveGreen "darkolivegreen"
                        , stringRep  DarkOliveGreen1 "darkolivegreen1"
                        , stringRep  DarkOliveGreen2 "darkolivegreen2"
                        , stringRep  DarkOliveGreen3 "darkolivegreen3"
                        , stringRep  DarkOliveGreen4 "darkolivegreen4"
                        , stringRep  DarkOrange "darkorange"
                        , stringRep  DarkOrange1 "darkorange1"
                        , stringRep  DarkOrange2 "darkorange2"
                        , stringRep  DarkOrange3 "darkorange3"
                        , stringRep  DarkOrange4 "darkorange4"
                        , stringRep  DarkOrchid "darkorchid"
                        , stringRep  DarkOrchid1 "darkorchid1"
                        , stringRep  DarkOrchid2 "darkorchid2"
                        , stringRep  DarkOrchid3 "darkorchid3"
                        , stringRep  DarkOrchid4 "darkorchid4"
                        , stringRep  DarkSalmon "darksalmon"
                        , stringRep  DarkSeaGreen "darkseagreen"
                        , stringRep  DarkSeaGreen1 "darkseagreen1"
                        , stringRep  DarkSeaGreen2 "darkseagreen2"
                        , stringRep  DarkSeaGreen3 "darkseagreen3"
                        , stringRep  DarkSeaGreen4 "darkseagreen4"
                        , stringRep  DarkSlateBlue "darkslateblue"
                        , stringReps DarkSlateGray ["darkslategray", "darkslategrey"]
                        , stringReps DarkSlateGray1 ["darkslategray1", "darkslategrey1"]
                        , stringReps DarkSlateGray2 ["darkslategray2", "darkslategrey2"]
                        , stringReps DarkSlateGray3 ["darkslategray3", "darkslategrey3"]
                        , stringReps DarkSlateGray4 ["darkslategray4", "darkslategrey4"]
                        , stringRep  DarkTurquoise "darkturquoise"
                        , stringRep  DarkViolet "darkviolet"
                        , stringRep  DeepPink "deeppink"
                        , stringRep  DeepPink1 "deeppink1"
                        , stringRep  DeepPink2 "deeppink2"
                        , stringRep  DeepPink3 "deeppink3"
                        , stringRep  DeepPink4 "deeppink4"
                        , stringRep  DeepSkyBlue "deepskyblue"
                        , stringRep  DeepSkyBlue1 "deepskyblue1"
                        , stringRep  DeepSkyBlue2 "deepskyblue2"
                        , stringRep  DeepSkyBlue3 "deepskyblue3"
                        , stringRep  DeepSkyBlue4 "deepskyblue4"
                        , stringReps DimGray ["dimgray", "dimgrey"]
                        , stringRep  DodgerBlue "dodgerblue"
                        , stringRep  DodgerBlue1 "dodgerblue1"
                        , stringRep  DodgerBlue2 "dodgerblue2"
                        , stringRep  DodgerBlue3 "dodgerblue3"
                        , stringRep  DodgerBlue4 "dodgerblue4"
                        , stringRep  Firebrick "firebrick"
                        , stringRep  Firebrick1 "firebrick1"
                        , stringRep  Firebrick2 "firebrick2"
                        , stringRep  Firebrick3 "firebrick3"
                        , stringRep  Firebrick4 "firebrick4"
                        , stringRep  FloralWhite "floralwhite"
                        , stringRep  ForestGreen "forestgreen"
                        , stringRep  Gainsboro "gainsboro"
                        , stringRep  GhostWhite "ghostwhite"
                        , stringRep  Gold "gold"
                        , stringRep  Gold1 "gold1"
                        , stringRep  Gold2 "gold2"
                        , stringRep  Gold3 "gold3"
                        , stringRep  Gold4 "gold4"
                        , stringRep  Goldenrod "goldenrod"
                        , stringRep  Goldenrod1 "goldenrod1"
                        , stringRep  Goldenrod2 "goldenrod2"
                        , stringRep  Goldenrod3 "goldenrod3"
                        , stringRep  Goldenrod4 "goldenrod4"
                        , stringReps Gray ["gray", "grey"]
                        , stringReps Gray0 ["gray0", "grey0"]
                        , stringReps Gray1 ["gray1", "grey1"]
                        , stringReps Gray2 ["gray2", "grey2"]
                        , stringReps Gray3 ["gray3", "grey3"]
                        , stringReps Gray4 ["gray4", "grey4"]
                        , stringReps Gray5 ["gray5", "grey5"]
                        , stringReps Gray6 ["gray6", "grey6"]
                        , stringReps Gray7 ["gray7", "grey7"]
                        , stringReps Gray8 ["gray8", "grey8"]
                        , stringReps Gray9 ["gray9", "grey9"]
                        , stringReps Gray10 ["gray10", "grey10"]
                        , stringReps Gray11 ["gray11", "grey11"]
                        , stringReps Gray12 ["gray12", "grey12"]
                        , stringReps Gray13 ["gray13", "grey13"]
                        , stringReps Gray14 ["gray14", "grey14"]
                        , stringReps Gray15 ["gray15", "grey15"]
                        , stringReps Gray16 ["gray16", "grey16"]
                        , stringReps Gray17 ["gray17", "grey17"]
                        , stringReps Gray18 ["gray18", "grey18"]
                        , stringReps Gray19 ["gray19", "grey19"]
                        , stringReps Gray20 ["gray20", "grey20"]
                        , stringReps Gray21 ["gray21", "grey21"]
                        , stringReps Gray22 ["gray22", "grey22"]
                        , stringReps Gray23 ["gray23", "grey23"]
                        , stringReps Gray24 ["gray24", "grey24"]
                        , stringReps Gray25 ["gray25", "grey25"]
                        , stringReps Gray26 ["gray26", "grey26"]
                        , stringReps Gray27 ["gray27", "grey27"]
                        , stringReps Gray28 ["gray28", "grey28"]
                        , stringReps Gray29 ["gray29", "grey29"]
                        , stringReps Gray30 ["gray30", "grey30"]
                        , stringReps Gray31 ["gray31", "grey31"]
                        , stringReps Gray32 ["gray32", "grey32"]
                        , stringReps Gray33 ["gray33", "grey33"]
                        , stringReps Gray34 ["gray34", "grey34"]
                        , stringReps Gray35 ["gray35", "grey35"]
                        , stringReps Gray36 ["gray36", "grey36"]
                        , stringReps Gray37 ["gray37", "grey37"]
                        , stringReps Gray38 ["gray38", "grey38"]
                        , stringReps Gray39 ["gray39", "grey39"]
                        , stringReps Gray40 ["gray40", "grey40"]
                        , stringReps Gray41 ["gray41", "grey41"]
                        , stringReps Gray42 ["gray42", "grey42"]
                        , stringReps Gray43 ["gray43", "grey43"]
                        , stringReps Gray44 ["gray44", "grey44"]
                        , stringReps Gray45 ["gray45", "grey45"]
                        , stringReps Gray46 ["gray46", "grey46"]
                        , stringReps Gray47 ["gray47", "grey47"]
                        , stringReps Gray48 ["gray48", "grey48"]
                        , stringReps Gray49 ["gray49", "grey49"]
                        , stringReps Gray50 ["gray50", "grey50"]
                        , stringReps Gray51 ["gray51", "grey51"]
                        , stringReps Gray52 ["gray52", "grey52"]
                        , stringReps Gray53 ["gray53", "grey53"]
                        , stringReps Gray54 ["gray54", "grey54"]
                        , stringReps Gray55 ["gray55", "grey55"]
                        , stringReps Gray56 ["gray56", "grey56"]
                        , stringReps Gray57 ["gray57", "grey57"]
                        , stringReps Gray58 ["gray58", "grey58"]
                        , stringReps Gray59 ["gray59", "grey59"]
                        , stringReps Gray60 ["gray60", "grey60"]
                        , stringReps Gray61 ["gray61", "grey61"]
                        , stringReps Gray62 ["gray62", "grey62"]
                        , stringReps Gray63 ["gray63", "grey63"]
                        , stringReps Gray64 ["gray64", "grey64"]
                        , stringReps Gray65 ["gray65", "grey65"]
                        , stringReps Gray66 ["gray66", "grey66"]
                        , stringReps Gray67 ["gray67", "grey67"]
                        , stringReps Gray68 ["gray68", "grey68"]
                        , stringReps Gray69 ["gray69", "grey69"]
                        , stringReps Gray70 ["gray70", "grey70"]
                        , stringReps Gray71 ["gray71", "grey71"]
                        , stringReps Gray72 ["gray72", "grey72"]
                        , stringReps Gray73 ["gray73", "grey73"]
                        , stringReps Gray74 ["gray74", "grey74"]
                        , stringReps Gray75 ["gray75", "grey75"]
                        , stringReps Gray76 ["gray76", "grey76"]
                        , stringReps Gray77 ["gray77", "grey77"]
                        , stringReps Gray78 ["gray78", "grey78"]
                        , stringReps Gray79 ["gray79", "grey79"]
                        , stringReps Gray80 ["gray80", "grey80"]
                        , stringReps Gray81 ["gray81", "grey81"]
                        , stringReps Gray82 ["gray82", "grey82"]
                        , stringReps Gray83 ["gray83", "grey83"]
                        , stringReps Gray84 ["gray84", "grey84"]
                        , stringReps Gray85 ["gray85", "grey85"]
                        , stringReps Gray86 ["gray86", "grey86"]
                        , stringReps Gray87 ["gray87", "grey87"]
                        , stringReps Gray88 ["gray88", "grey88"]
                        , stringReps Gray89 ["gray89", "grey89"]
                        , stringReps Gray90 ["gray90", "grey90"]
                        , stringReps Gray91 ["gray91", "grey91"]
                        , stringReps Gray92 ["gray92", "grey92"]
                        , stringReps Gray93 ["gray93", "grey93"]
                        , stringReps Gray94 ["gray94", "grey94"]
                        , stringReps Gray95 ["gray95", "grey95"]
                        , stringReps Gray96 ["gray96", "grey96"]
                        , stringReps Gray97 ["gray97", "grey97"]
                        , stringReps Gray98 ["gray98", "grey98"]
                        , stringReps Gray99 ["gray99", "grey99"]
                        , stringReps Gray100 ["gray100", "grey100"]
                        , stringRep  Green "green"
                        , stringRep  Green1 "green1"
                        , stringRep  Green2 "green2"
                        , stringRep  Green3 "green3"
                        , stringRep  Green4 "green4"
                        , stringRep  GreenYellow "greenyellow"
                        , stringRep  HoneyDew "honeydew"
                        , stringRep  HoneyDew1 "honeydew1"
                        , stringRep  HoneyDew2 "honeydew2"
                        , stringRep  HoneyDew3 "honeydew3"
                        , stringRep  HoneyDew4 "honeydew4"
                        , stringRep  HotPink "hotpink"
                        , stringRep  HotPink1 "hotpink1"
                        , stringRep  HotPink2 "hotpink2"
                        , stringRep  HotPink3 "hotpink3"
                        , stringRep  HotPink4 "hotpink4"
                        , stringRep  IndianRed "indianred"
                        , stringRep  IndianRed1 "indianred1"
                        , stringRep  IndianRed2 "indianred2"
                        , stringRep  IndianRed3 "indianred3"
                        , stringRep  IndianRed4 "indianred4"
                        , stringRep  Indigo "indigo"
                        , stringRep  Ivory "ivory"
                        , stringRep  Ivory1 "ivory1"
                        , stringRep  Ivory2 "ivory2"
                        , stringRep  Ivory3 "ivory3"
                        , stringRep  Ivory4 "ivory4"
                        , stringRep  Khaki "khaki"
                        , stringRep  Khaki1 "khaki1"
                        , stringRep  Khaki2 "khaki2"
                        , stringRep  Khaki3 "khaki3"
                        , stringRep  Khaki4 "khaki4"
                        , stringRep  Lavender "lavender"
                        , stringRep  LavenderBlush "lavenderblush"
                        , stringRep  LavenderBlush1 "lavenderblush1"
                        , stringRep  LavenderBlush2 "lavenderblush2"
                        , stringRep  LavenderBlush3 "lavenderblush3"
                        , stringRep  LavenderBlush4 "lavenderblush4"
                        , stringRep  LawnGreen "lawngreen"
                        , stringRep  LemonChiffon "lemonchiffon"
                        , stringRep  LemonChiffon1 "lemonchiffon1"
                        , stringRep  LemonChiffon2 "lemonchiffon2"
                        , stringRep  LemonChiffon3 "lemonchiffon3"
                        , stringRep  LemonChiffon4 "lemonchiffon4"
                        , stringRep  LightBlue "lightblue"
                        , stringRep  LightBlue1 "lightblue1"
                        , stringRep  LightBlue2 "lightblue2"
                        , stringRep  LightBlue3 "lightblue3"
                        , stringRep  LightBlue4 "lightblue4"
                        , stringRep  LightCoral "lightcoral"
                        , stringRep  LightCyan "lightcyan"
                        , stringRep  LightCyan1 "lightcyan1"
                        , stringRep  LightCyan2 "lightcyan2"
                        , stringRep  LightCyan3 "lightcyan3"
                        , stringRep  LightCyan4 "lightcyan4"
                        , stringRep  LightGoldenrod "lightgoldenrod"
                        , stringRep  LightGoldenrod1 "lightgoldenrod1"
                        , stringRep  LightGoldenrod2 "lightgoldenrod2"
                        , stringRep  LightGoldenrod3 "lightgoldenrod3"
                        , stringRep  LightGoldenrod4 "lightgoldenrod4"
                        , stringRep  LightGoldenrodYellow "lightgoldenrodyellow"
                        , stringReps LightGray ["lightgray", "lightgrey"]
                        , stringRep  LightPink "lightpink"
                        , stringRep  LightPink1 "lightpink1"
                        , stringRep  LightPink2 "lightpink2"
                        , stringRep  LightPink3 "lightpink3"
                        , stringRep  LightPink4 "lightpink4"
                        , stringRep  LightSalmon "lightsalmon"
                        , stringRep  LightSalmon1 "lightsalmon1"
                        , stringRep  LightSalmon2 "lightsalmon2"
                        , stringRep  LightSalmon3 "lightsalmon3"
                        , stringRep  LightSalmon4 "lightsalmon4"
                        , stringRep  LightSeaGreen "lightseagreen"
                        , stringRep  LightSkyBlue "lightskyblue"
                        , stringRep  LightSkyBlue1 "lightskyblue1"
                        , stringRep  LightSkyBlue2 "lightskyblue2"
                        , stringRep  LightSkyBlue3 "lightskyblue3"
                        , stringRep  LightSkyBlue4 "lightskyblue4"
                        , stringRep  LightSlateBlue "lightslateblue"
                        , stringReps LightSlateGray ["lightslategray", "lightslategrey"]
                        , stringRep  LightSteelBlue "lightsteelblue"
                        , stringRep  LightSteelBlue1 "lightsteelblue1"
                        , stringRep  LightSteelBlue2 "lightsteelblue2"
                        , stringRep  LightSteelBlue3 "lightsteelblue3"
                        , stringRep  LightSteelBlue4 "lightsteelblue4"
                        , stringRep  LightYellow "lightyellow"
                        , stringRep  LightYellow1 "lightyellow1"
                        , stringRep  LightYellow2 "lightyellow2"
                        , stringRep  LightYellow3 "lightyellow3"
                        , stringRep  LightYellow4 "lightyellow4"
                        , stringRep  LimeGreen "limegreen"
                        , stringRep  Linen "linen"
                        , stringRep  Magenta "magenta"
                        , stringRep  Magenta1 "magenta1"
                        , stringRep  Magenta2 "magenta2"
                        , stringRep  Magenta3 "magenta3"
                        , stringRep  Magenta4 "magenta4"
                        , stringRep  Maroon "maroon"
                        , stringRep  Maroon1 "maroon1"
                        , stringRep  Maroon2 "maroon2"
                        , stringRep  Maroon3 "maroon3"
                        , stringRep  Maroon4 "maroon4"
                        , stringRep  MediumAquamarine "mediumaquamarine"
                        , stringRep  MediumBlue "mediumblue"
                        , stringRep  MediumOrchid "mediumorchid"
                        , stringRep  MediumOrchid1 "mediumorchid1"
                        , stringRep  MediumOrchid2 "mediumorchid2"
                        , stringRep  MediumOrchid3 "mediumorchid3"
                        , stringRep  MediumOrchid4 "mediumorchid4"
                        , stringRep  MediumPurple "mediumpurple"
                        , stringRep  MediumPurple1 "mediumpurple1"
                        , stringRep  MediumPurple2 "mediumpurple2"
                        , stringRep  MediumPurple3 "mediumpurple3"
                        , stringRep  MediumPurple4 "mediumpurple4"
                        , stringRep  MediumSeaGreen "mediumseagreen"
                        , stringRep  MediumSlateBlue "mediumslateblue"
                        , stringRep  MediumSpringGreen "mediumspringgreen"
                        , stringRep  MediumTurquoise "mediumturquoise"
                        , stringRep  MediumVioletRed "mediumvioletred"
                        , stringRep  MidnightBlue "midnightblue"
                        , stringRep  MintCream "mintcream"
                        , stringRep  MistyRose "mistyrose"
                        , stringRep  MistyRose1 "mistyrose1"
                        , stringRep  MistyRose2 "mistyrose2"
                        , stringRep  MistyRose3 "mistyrose3"
                        , stringRep  MistyRose4 "mistyrose4"
                        , stringRep  Moccasin "moccasin"
                        , stringRep  NavajoWhite "navajowhite"
                        , stringRep  NavajoWhite1 "navajowhite1"
                        , stringRep  NavajoWhite2 "navajowhite2"
                        , stringRep  NavajoWhite3 "navajowhite3"
                        , stringRep  NavajoWhite4 "navajowhite4"
                        , stringRep  Navy "navy"
                        , stringRep  NavyBlue "navyblue"
                        , stringRep  OldLace "oldlace"
                        , stringRep  OliveDrab "olivedrab"
                        , stringRep  OliveDrab1 "olivedrab1"
                        , stringRep  OliveDrab2 "olivedrab2"
                        , stringRep  OliveDrab3 "olivedrab3"
                        , stringRep  OliveDrab4 "olivedrab4"
                        , stringRep  Orange "orange"
                        , stringRep  Orange1 "orange1"
                        , stringRep  Orange2 "orange2"
                        , stringRep  Orange3 "orange3"
                        , stringRep  Orange4 "orange4"
                        , stringRep  OrangeRed "orangered"
                        , stringRep  OrangeRed1 "orangered1"
                        , stringRep  OrangeRed2 "orangered2"
                        , stringRep  OrangeRed3 "orangered3"
                        , stringRep  OrangeRed4 "orangered4"
                        , stringRep  Orchid "orchid"
                        , stringRep  Orchid1 "orchid1"
                        , stringRep  Orchid2 "orchid2"
                        , stringRep  Orchid3 "orchid3"
                        , stringRep  Orchid4 "orchid4"
                        , stringRep  PaleGoldenrod "palegoldenrod"
                        , stringRep  PaleGreen "palegreen"
                        , stringRep  PaleGreen1 "palegreen1"
                        , stringRep  PaleGreen2 "palegreen2"
                        , stringRep  PaleGreen3 "palegreen3"
                        , stringRep  PaleGreen4 "palegreen4"
                        , stringRep  PaleTurquoise "paleturquoise"
                        , stringRep  PaleTurquoise1 "paleturquoise1"
                        , stringRep  PaleTurquoise2 "paleturquoise2"
                        , stringRep  PaleTurquoise3 "paleturquoise3"
                        , stringRep  PaleTurquoise4 "paleturquoise4"
                        , stringRep  PaleVioletRed "palevioletred"
                        , stringRep  PaleVioletRed1 "palevioletred1"
                        , stringRep  PaleVioletRed2 "palevioletred2"
                        , stringRep  PaleVioletRed3 "palevioletred3"
                        , stringRep  PaleVioletRed4 "palevioletred4"
                        , stringRep  PapayaWhip "papayawhip"
                        , stringRep  PeachPuff "peachpuff"
                        , stringRep  PeachPuff1 "peachpuff1"
                        , stringRep  PeachPuff2 "peachpuff2"
                        , stringRep  PeachPuff3 "peachpuff3"
                        , stringRep  PeachPuff4 "peachpuff4"
                        , stringRep  Peru "peru"
                        , stringRep  Pink "pink"
                        , stringRep  Pink1 "pink1"
                        , stringRep  Pink2 "pink2"
                        , stringRep  Pink3 "pink3"
                        , stringRep  Pink4 "pink4"
                        , stringRep  Plum "plum"
                        , stringRep  Plum1 "plum1"
                        , stringRep  Plum2 "plum2"
                        , stringRep  Plum3 "plum3"
                        , stringRep  Plum4 "plum4"
                        , stringRep  PowderBlue "powderblue"
                        , stringRep  Purple "purple"
                        , stringRep  Purple1 "purple1"
                        , stringRep  Purple2 "purple2"
                        , stringRep  Purple3 "purple3"
                        , stringRep  Purple4 "purple4"
                        , stringRep  Red "red"
                        , stringRep  Red1 "red1"
                        , stringRep  Red2 "red2"
                        , stringRep  Red3 "red3"
                        , stringRep  Red4 "red4"
                        , stringRep  RosyBrown "rosybrown"
                        , stringRep  RosyBrown1 "rosybrown1"
                        , stringRep  RosyBrown2 "rosybrown2"
                        , stringRep  RosyBrown3 "rosybrown3"
                        , stringRep  RosyBrown4 "rosybrown4"
                        , stringRep  RoyalBlue "royalblue"
                        , stringRep  RoyalBlue1 "royalblue1"
                        , stringRep  RoyalBlue2 "royalblue2"
                        , stringRep  RoyalBlue3 "royalblue3"
                        , stringRep  RoyalBlue4 "royalblue4"
                        , stringRep  SaddleBrown "saddlebrown"
                        , stringRep  Salmon "salmon"
                        , stringRep  Salmon1 "salmon1"
                        , stringRep  Salmon2 "salmon2"
                        , stringRep  Salmon3 "salmon3"
                        , stringRep  Salmon4 "salmon4"
                        , stringRep  SandyBrown "sandybrown"
                        , stringRep  SeaGreen "seagreen"
                        , stringRep  SeaGreen1 "seagreen1"
                        , stringRep  SeaGreen2 "seagreen2"
                        , stringRep  SeaGreen3 "seagreen3"
                        , stringRep  SeaGreen4 "seagreen4"
                        , stringRep  SeaShell "seashell"
                        , stringRep  SeaShell1 "seashell1"
                        , stringRep  SeaShell2 "seashell2"
                        , stringRep  SeaShell3 "seashell3"
                        , stringRep  SeaShell4 "seashell4"
                        , stringRep  Sienna "sienna"
                        , stringRep  Sienna1 "sienna1"
                        , stringRep  Sienna2 "sienna2"
                        , stringRep  Sienna3 "sienna3"
                        , stringRep  Sienna4 "sienna4"
                        , stringRep  SkyBlue "skyblue"
                        , stringRep  SkyBlue1 "skyblue1"
                        , stringRep  SkyBlue2 "skyblue2"
                        , stringRep  SkyBlue3 "skyblue3"
                        , stringRep  SkyBlue4 "skyblue4"
                        , stringRep  SlateBlue "slateblue"
                        , stringRep  SlateBlue1 "slateblue1"
                        , stringRep  SlateBlue2 "slateblue2"
                        , stringRep  SlateBlue3 "slateblue3"
                        , stringRep  SlateBlue4 "slateblue4"
                        , stringReps SlateGray ["slategray", "slategrey"]
                        , stringReps SlateGray1 ["slategray1", "slategrey1"]
                        , stringReps SlateGray2 ["slategray2", "slategrey2"]
                        , stringReps SlateGray3 ["slategray3", "slategrey3"]
                        , stringReps SlateGray4 ["slategray4", "slategrey4"]
                        , stringRep  Snow "snow"
                        , stringRep  Snow1 "snow1"
                        , stringRep  Snow2 "snow2"
                        , stringRep  Snow3 "snow3"
                        , stringRep  Snow4 "snow4"
                        , stringRep  SpringGreen "springgreen"
                        , stringRep  SpringGreen1 "springgreen1"
                        , stringRep  SpringGreen2 "springgreen2"
                        , stringRep  SpringGreen3 "springgreen3"
                        , stringRep  SpringGreen4 "springgreen4"
                        , stringRep  SteelBlue "steelblue"
                        , stringRep  SteelBlue1 "steelblue1"
                        , stringRep  SteelBlue2 "steelblue2"
                        , stringRep  SteelBlue3 "steelblue3"
                        , stringRep  SteelBlue4 "steelblue4"
                        , stringRep  Tan "tan"
                        , stringRep  Tan1 "tan1"
                        , stringRep  Tan2 "tan2"
                        , stringRep  Tan3 "tan3"
                        , stringRep  Tan4 "tan4"
                        , stringRep  Thistle "thistle"
                        , stringRep  Thistle1 "thistle1"
                        , stringRep  Thistle2 "thistle2"
                        , stringRep  Thistle3 "thistle3"
                        , stringRep  Thistle4 "thistle4"
                        , stringRep  Tomato "tomato"
                        , stringRep  Tomato1 "tomato1"
                        , stringRep  Tomato2 "tomato2"
                        , stringRep  Tomato3 "tomato3"
                        , stringRep  Tomato4 "tomato4"
                        , stringReps Transparent ["transparent", "invis", "none"]
                        , stringRep  Turquoise "turquoise"
                        , stringRep  Turquoise1 "turquoise1"
                        , stringRep  Turquoise2 "turquoise2"
                        , stringRep  Turquoise3 "turquoise3"
                        , stringRep  Turquoise4 "turquoise4"
                        , stringRep  Violet "violet"
                        , stringRep  VioletRed "violetred"
                        , stringRep  VioletRed1 "violetred1"
                        , stringRep  VioletRed2 "violetred2"
                        , stringRep  VioletRed3 "violetred3"
                        , stringRep  VioletRed4 "violetred4"
                        , stringRep  Wheat "wheat"
                        , stringRep  Wheat1 "wheat1"
                        , stringRep  Wheat2 "wheat2"
                        , stringRep  Wheat3 "wheat3"
                        , stringRep  Wheat4 "wheat4"
                        , stringRep  White "white"
                        , stringRep  WhiteSmoke "whitesmoke"
                        , stringRep  Yellow "yellow"
                        , stringRep  Yellow1 "yellow1"
                        , stringRep  Yellow2 "yellow2"
                        , stringRep  Yellow3 "yellow3"
                        , stringRep  Yellow4 "yellow4"
                        , stringRep  YellowGreen "yellowgreen"
                        ]

-- | Attempt to convert a 'Color' into a 'Colour' value with an alpha
--   channel.  The use of 'Maybe' is because 'BrewerColor' values
--   cannot be converted without knowing which actual 'BrewerName' and
--   level color scheme is being used.
toColour                :: Color -> Maybe (AlphaColour Double)
toColour (RGB r g b)    = Just . opaque $ sRGB24 r g b
toColour (RGBA r g b a) = Just . withOpacity (sRGB24 r g b) $ toOpacity a
-- Colour expects the hue to be an angle, so multiply by 360
toColour (HSV h s v)    = Just . opaque . uncurryRGB sRGB $ hsv (h*360) s v
toColour (X11Color c)   = Just $ x11Colour c
toColour BrewerColor{}  = Nothing

toOpacity   :: Word8 -> Double
toOpacity a = fromIntegral a / maxWord

-- | Convert an 'X11Color' to its equivalent 'Colour' value.  Note
--   that it uses 'AlphaColour' because of 'Transparent'; all other
--   'X11Color' values are completely opaque.
x11Colour                      :: X11Color -> AlphaColour Double
x11Colour AliceBlue            = opaque $ sRGB24 240 248 255
x11Colour AntiqueWhite         = opaque $ sRGB24 250 235 215
x11Colour AntiqueWhite1        = opaque $ sRGB24 255 239 219
x11Colour AntiqueWhite2        = opaque $ sRGB24 238 223 204
x11Colour AntiqueWhite3        = opaque $ sRGB24 205 192 176
x11Colour AntiqueWhite4        = opaque $ sRGB24 139 131 120
x11Colour Aquamarine           = opaque $ sRGB24 127 255 212
x11Colour Aquamarine1          = opaque $ sRGB24 127 255 212
x11Colour Aquamarine2          = opaque $ sRGB24 118 238 198
x11Colour Aquamarine3          = opaque $ sRGB24 102 205 170
x11Colour Aquamarine4          = opaque $ sRGB24 69  139 116
x11Colour Azure                = opaque $ sRGB24 240 255 255
x11Colour Azure1               = opaque $ sRGB24 240 255 255
x11Colour Azure2               = opaque $ sRGB24 224 238 238
x11Colour Azure3               = opaque $ sRGB24 193 205 205
x11Colour Azure4               = opaque $ sRGB24 131 139 139
x11Colour Beige                = opaque $ sRGB24 245 245 220
x11Colour Bisque               = opaque $ sRGB24 255 228 196
x11Colour Bisque1              = opaque $ sRGB24 255 228 196
x11Colour Bisque2              = opaque $ sRGB24 238 213 183
x11Colour Bisque3              = opaque $ sRGB24 205 183 158
x11Colour Bisque4              = opaque $ sRGB24 139 125 107
x11Colour Black                = opaque $ sRGB24 0   0   0
x11Colour BlanchedAlmond       = opaque $ sRGB24 255 235 205
x11Colour Blue                 = opaque $ sRGB24 0   0   255
x11Colour Blue1                = opaque $ sRGB24 0   0   255
x11Colour Blue2                = opaque $ sRGB24 0   0   238
x11Colour Blue3                = opaque $ sRGB24 0   0   205
x11Colour Blue4                = opaque $ sRGB24 0   0   139
x11Colour BlueViolet           = opaque $ sRGB24 138 43  226
x11Colour Brown                = opaque $ sRGB24 165 42  42
x11Colour Brown1               = opaque $ sRGB24 255 64  64
x11Colour Brown2               = opaque $ sRGB24 238 59  59
x11Colour Brown3               = opaque $ sRGB24 205 51  51
x11Colour Brown4               = opaque $ sRGB24 139 35  35
x11Colour Burlywood            = opaque $ sRGB24 222 184 135
x11Colour Burlywood1           = opaque $ sRGB24 255 211 155
x11Colour Burlywood2           = opaque $ sRGB24 238 197 145
x11Colour Burlywood3           = opaque $ sRGB24 205 170 125
x11Colour Burlywood4           = opaque $ sRGB24 139 115 85
x11Colour CadetBlue            = opaque $ sRGB24 95  158 160
x11Colour CadetBlue1           = opaque $ sRGB24 152 245 255
x11Colour CadetBlue2           = opaque $ sRGB24 142 229 238
x11Colour CadetBlue3           = opaque $ sRGB24 122 197 205
x11Colour CadetBlue4           = opaque $ sRGB24 83  134 139
x11Colour Chartreuse           = opaque $ sRGB24 127 255 0
x11Colour Chartreuse1          = opaque $ sRGB24 127 255 0
x11Colour Chartreuse2          = opaque $ sRGB24 118 238 0
x11Colour Chartreuse3          = opaque $ sRGB24 102 205 0
x11Colour Chartreuse4          = opaque $ sRGB24 69  139 0
x11Colour Chocolate            = opaque $ sRGB24 210 105 30
x11Colour Chocolate1           = opaque $ sRGB24 255 127 36
x11Colour Chocolate2           = opaque $ sRGB24 238 118 33
x11Colour Chocolate3           = opaque $ sRGB24 205 102 29
x11Colour Chocolate4           = opaque $ sRGB24 139 69  19
x11Colour Coral                = opaque $ sRGB24 255 127 80
x11Colour Coral1               = opaque $ sRGB24 255 114 86
x11Colour Coral2               = opaque $ sRGB24 238 106 80
x11Colour Coral3               = opaque $ sRGB24 205 91  69
x11Colour Coral4               = opaque $ sRGB24 139 62  47
x11Colour CornFlowerBlue       = opaque $ sRGB24 100 149 237
x11Colour CornSilk             = opaque $ sRGB24 255 248 220
x11Colour CornSilk1            = opaque $ sRGB24 255 248 220
x11Colour CornSilk2            = opaque $ sRGB24 238 232 205
x11Colour CornSilk3            = opaque $ sRGB24 205 200 177
x11Colour CornSilk4            = opaque $ sRGB24 139 136 120
x11Colour Crimson              = opaque $ sRGB24 220 20  60
x11Colour Cyan                 = opaque $ sRGB24 0   255 255
x11Colour Cyan1                = opaque $ sRGB24 0   255 255
x11Colour Cyan2                = opaque $ sRGB24 0   238 238
x11Colour Cyan3                = opaque $ sRGB24 0   205 205
x11Colour Cyan4                = opaque $ sRGB24 0   139 139
x11Colour DarkGoldenrod        = opaque $ sRGB24 184 134 11
x11Colour DarkGoldenrod1       = opaque $ sRGB24 255 185 15
x11Colour DarkGoldenrod2       = opaque $ sRGB24 238 173 14
x11Colour DarkGoldenrod3       = opaque $ sRGB24 205 149 12
x11Colour DarkGoldenrod4       = opaque $ sRGB24 139 101 8
x11Colour DarkGreen            = opaque $ sRGB24 0   100 0
x11Colour Darkkhaki            = opaque $ sRGB24 189 183 107
x11Colour DarkOliveGreen       = opaque $ sRGB24 85  107 47
x11Colour DarkOliveGreen1      = opaque $ sRGB24 202 255 112
x11Colour DarkOliveGreen2      = opaque $ sRGB24 188 238 104
x11Colour DarkOliveGreen3      = opaque $ sRGB24 162 205 90
x11Colour DarkOliveGreen4      = opaque $ sRGB24 110 139 61
x11Colour DarkOrange           = opaque $ sRGB24 255 140 0
x11Colour DarkOrange1          = opaque $ sRGB24 255 127 0
x11Colour DarkOrange2          = opaque $ sRGB24 238 118 0
x11Colour DarkOrange3          = opaque $ sRGB24 205 102 0
x11Colour DarkOrange4          = opaque $ sRGB24 139 69  0
x11Colour DarkOrchid           = opaque $ sRGB24 153 50  204
x11Colour DarkOrchid1          = opaque $ sRGB24 191 62  255
x11Colour DarkOrchid2          = opaque $ sRGB24 178 58  238
x11Colour DarkOrchid3          = opaque $ sRGB24 154 50  205
x11Colour DarkOrchid4          = opaque $ sRGB24 104 34  139
x11Colour DarkSalmon           = opaque $ sRGB24 233 150 122
x11Colour DarkSeaGreen         = opaque $ sRGB24 143 188 143
x11Colour DarkSeaGreen1        = opaque $ sRGB24 193 255 193
x11Colour DarkSeaGreen2        = opaque $ sRGB24 180 238 180
x11Colour DarkSeaGreen3        = opaque $ sRGB24 155 205 155
x11Colour DarkSeaGreen4        = opaque $ sRGB24 105 139 105
x11Colour DarkSlateBlue        = opaque $ sRGB24 72  61  139
x11Colour DarkSlateGray        = opaque $ sRGB24 47  79  79
x11Colour DarkSlateGray1       = opaque $ sRGB24 151 255 255
x11Colour DarkSlateGray2       = opaque $ sRGB24 141 238 238
x11Colour DarkSlateGray3       = opaque $ sRGB24 121 205 205
x11Colour DarkSlateGray4       = opaque $ sRGB24 82  139 139
x11Colour DarkTurquoise        = opaque $ sRGB24 0   206 209
x11Colour DarkViolet           = opaque $ sRGB24 148 0   211
x11Colour DeepPink             = opaque $ sRGB24 255 20  147
x11Colour DeepPink1            = opaque $ sRGB24 255 20  147
x11Colour DeepPink2            = opaque $ sRGB24 238 18  137
x11Colour DeepPink3            = opaque $ sRGB24 205 16  118
x11Colour DeepPink4            = opaque $ sRGB24 139 10  80
x11Colour DeepSkyBlue          = opaque $ sRGB24 0   191 255
x11Colour DeepSkyBlue1         = opaque $ sRGB24 0   191 255
x11Colour DeepSkyBlue2         = opaque $ sRGB24 0   178 238
x11Colour DeepSkyBlue3         = opaque $ sRGB24 0   154 205
x11Colour DeepSkyBlue4         = opaque $ sRGB24 0   104 139
x11Colour DimGray              = opaque $ sRGB24 105 105 105
x11Colour DodgerBlue           = opaque $ sRGB24 30  144 255
x11Colour DodgerBlue1          = opaque $ sRGB24 30  144 255
x11Colour DodgerBlue2          = opaque $ sRGB24 28  134 238
x11Colour DodgerBlue3          = opaque $ sRGB24 24  116 205
x11Colour DodgerBlue4          = opaque $ sRGB24 16  78  139
x11Colour Firebrick            = opaque $ sRGB24 178 34  34
x11Colour Firebrick1           = opaque $ sRGB24 255 48  48
x11Colour Firebrick2           = opaque $ sRGB24 238 44  44
x11Colour Firebrick3           = opaque $ sRGB24 205 38  38
x11Colour Firebrick4           = opaque $ sRGB24 139 26  26
x11Colour FloralWhite          = opaque $ sRGB24 255 250 240
x11Colour ForestGreen          = opaque $ sRGB24 34  139 34
x11Colour Gainsboro            = opaque $ sRGB24 220 220 220
x11Colour GhostWhite           = opaque $ sRGB24 248 248 255
x11Colour Gold                 = opaque $ sRGB24 255 215 0
x11Colour Gold1                = opaque $ sRGB24 255 215 0
x11Colour Gold2                = opaque $ sRGB24 238 201 0
x11Colour Gold3                = opaque $ sRGB24 205 173 0
x11Colour Gold4                = opaque $ sRGB24 139 117 0
x11Colour Goldenrod            = opaque $ sRGB24 218 165 32
x11Colour Goldenrod1           = opaque $ sRGB24 255 193 37
x11Colour Goldenrod2           = opaque $ sRGB24 238 180 34
x11Colour Goldenrod3           = opaque $ sRGB24 205 155 29
x11Colour Goldenrod4           = opaque $ sRGB24 139 105 20
x11Colour Gray                 = opaque $ sRGB24 192 192 192
x11Colour Gray0                = opaque $ sRGB24 0   0   0
x11Colour Gray1                = opaque $ sRGB24 3   3   3
x11Colour Gray2                = opaque $ sRGB24 5   5   5
x11Colour Gray3                = opaque $ sRGB24 8   8   8
x11Colour Gray4                = opaque $ sRGB24 10  10  10
x11Colour Gray5                = opaque $ sRGB24 13  13  13
x11Colour Gray6                = opaque $ sRGB24 15  15  15
x11Colour Gray7                = opaque $ sRGB24 18  18  18
x11Colour Gray8                = opaque $ sRGB24 20  20  20
x11Colour Gray9                = opaque $ sRGB24 23  23  23
x11Colour Gray10               = opaque $ sRGB24 26  26  26
x11Colour Gray11               = opaque $ sRGB24 28  28  28
x11Colour Gray12               = opaque $ sRGB24 31  31  31
x11Colour Gray13               = opaque $ sRGB24 33  33  33
x11Colour Gray14               = opaque $ sRGB24 36  36  36
x11Colour Gray15               = opaque $ sRGB24 38  38  38
x11Colour Gray16               = opaque $ sRGB24 41  41  41
x11Colour Gray17               = opaque $ sRGB24 43  43  43
x11Colour Gray18               = opaque $ sRGB24 46  46  46
x11Colour Gray19               = opaque $ sRGB24 48  48  48
x11Colour Gray20               = opaque $ sRGB24 51  51  51
x11Colour Gray21               = opaque $ sRGB24 54  54  54
x11Colour Gray22               = opaque $ sRGB24 56  56  56
x11Colour Gray23               = opaque $ sRGB24 59  59  59
x11Colour Gray24               = opaque $ sRGB24 61  61  61
x11Colour Gray25               = opaque $ sRGB24 64  64  64
x11Colour Gray26               = opaque $ sRGB24 66  66  66
x11Colour Gray27               = opaque $ sRGB24 69  69  69
x11Colour Gray28               = opaque $ sRGB24 71  71  71
x11Colour Gray29               = opaque $ sRGB24 74  74  74
x11Colour Gray30               = opaque $ sRGB24 77  77  77
x11Colour Gray31               = opaque $ sRGB24 79  79  79
x11Colour Gray32               = opaque $ sRGB24 82  82  82
x11Colour Gray33               = opaque $ sRGB24 84  84  84
x11Colour Gray34               = opaque $ sRGB24 87  87  87
x11Colour Gray35               = opaque $ sRGB24 89  89  89
x11Colour Gray36               = opaque $ sRGB24 92  92  92
x11Colour Gray37               = opaque $ sRGB24 94  94  94
x11Colour Gray38               = opaque $ sRGB24 97  97  97
x11Colour Gray39               = opaque $ sRGB24 99  99  99
x11Colour Gray40               = opaque $ sRGB24 102 102 102
x11Colour Gray41               = opaque $ sRGB24 105 105 105
x11Colour Gray42               = opaque $ sRGB24 107 107 107
x11Colour Gray43               = opaque $ sRGB24 110 110 110
x11Colour Gray44               = opaque $ sRGB24 112 112 112
x11Colour Gray45               = opaque $ sRGB24 115 115 115
x11Colour Gray46               = opaque $ sRGB24 117 117 117
x11Colour Gray47               = opaque $ sRGB24 120 120 120
x11Colour Gray48               = opaque $ sRGB24 122 122 122
x11Colour Gray49               = opaque $ sRGB24 125 125 125
x11Colour Gray50               = opaque $ sRGB24 127 127 127
x11Colour Gray51               = opaque $ sRGB24 130 130 130
x11Colour Gray52               = opaque $ sRGB24 133 133 133
x11Colour Gray53               = opaque $ sRGB24 135 135 135
x11Colour Gray54               = opaque $ sRGB24 138 138 138
x11Colour Gray55               = opaque $ sRGB24 140 140 140
x11Colour Gray56               = opaque $ sRGB24 143 143 143
x11Colour Gray57               = opaque $ sRGB24 145 145 145
x11Colour Gray58               = opaque $ sRGB24 148 148 148
x11Colour Gray59               = opaque $ sRGB24 150 150 150
x11Colour Gray60               = opaque $ sRGB24 153 153 153
x11Colour Gray61               = opaque $ sRGB24 156 156 156
x11Colour Gray62               = opaque $ sRGB24 158 158 158
x11Colour Gray63               = opaque $ sRGB24 161 161 161
x11Colour Gray64               = opaque $ sRGB24 163 163 163
x11Colour Gray65               = opaque $ sRGB24 166 166 166
x11Colour Gray66               = opaque $ sRGB24 168 168 168
x11Colour Gray67               = opaque $ sRGB24 171 171 171
x11Colour Gray68               = opaque $ sRGB24 173 173 173
x11Colour Gray69               = opaque $ sRGB24 176 176 176
x11Colour Gray70               = opaque $ sRGB24 179 179 179
x11Colour Gray71               = opaque $ sRGB24 181 181 181
x11Colour Gray72               = opaque $ sRGB24 184 184 184
x11Colour Gray73               = opaque $ sRGB24 186 186 186
x11Colour Gray74               = opaque $ sRGB24 189 189 189
x11Colour Gray75               = opaque $ sRGB24 191 191 191
x11Colour Gray76               = opaque $ sRGB24 194 194 194
x11Colour Gray77               = opaque $ sRGB24 196 196 196
x11Colour Gray78               = opaque $ sRGB24 199 199 199
x11Colour Gray79               = opaque $ sRGB24 201 201 201
x11Colour Gray80               = opaque $ sRGB24 204 204 204
x11Colour Gray81               = opaque $ sRGB24 207 207 207
x11Colour Gray82               = opaque $ sRGB24 209 209 209
x11Colour Gray83               = opaque $ sRGB24 212 212 212
x11Colour Gray84               = opaque $ sRGB24 214 214 214
x11Colour Gray85               = opaque $ sRGB24 217 217 217
x11Colour Gray86               = opaque $ sRGB24 219 219 219
x11Colour Gray87               = opaque $ sRGB24 222 222 222
x11Colour Gray88               = opaque $ sRGB24 224 224 224
x11Colour Gray89               = opaque $ sRGB24 227 227 227
x11Colour Gray90               = opaque $ sRGB24 229 229 229
x11Colour Gray91               = opaque $ sRGB24 232 232 232
x11Colour Gray92               = opaque $ sRGB24 235 235 235
x11Colour Gray93               = opaque $ sRGB24 237 237 237
x11Colour Gray94               = opaque $ sRGB24 240 240 240
x11Colour Gray95               = opaque $ sRGB24 242 242 242
x11Colour Gray96               = opaque $ sRGB24 245 245 245
x11Colour Gray97               = opaque $ sRGB24 247 247 247
x11Colour Gray98               = opaque $ sRGB24 250 250 250
x11Colour Gray99               = opaque $ sRGB24 252 252 252
x11Colour Gray100              = opaque $ sRGB24 255 255 255
x11Colour Green                = opaque $ sRGB24 0   255 0
x11Colour Green1               = opaque $ sRGB24 0   255 0
x11Colour Green2               = opaque $ sRGB24 0   238 0
x11Colour Green3               = opaque $ sRGB24 0   205 0
x11Colour Green4               = opaque $ sRGB24 0   139 0
x11Colour GreenYellow          = opaque $ sRGB24 173 255 47
x11Colour HoneyDew             = opaque $ sRGB24 240 255 240
x11Colour HoneyDew1            = opaque $ sRGB24 240 255 240
x11Colour HoneyDew2            = opaque $ sRGB24 224 238 224
x11Colour HoneyDew3            = opaque $ sRGB24 193 205 193
x11Colour HoneyDew4            = opaque $ sRGB24 131 139 131
x11Colour HotPink              = opaque $ sRGB24 255 105 180
x11Colour HotPink1             = opaque $ sRGB24 255 110 180
x11Colour HotPink2             = opaque $ sRGB24 238 106 167
x11Colour HotPink3             = opaque $ sRGB24 205 96  144
x11Colour HotPink4             = opaque $ sRGB24 139 58  98
x11Colour IndianRed            = opaque $ sRGB24 205 92  92
x11Colour IndianRed1           = opaque $ sRGB24 255 106 106
x11Colour IndianRed2           = opaque $ sRGB24 238 99  99
x11Colour IndianRed3           = opaque $ sRGB24 205 85  85
x11Colour IndianRed4           = opaque $ sRGB24 139 58  58
x11Colour Indigo               = opaque $ sRGB24 75  0   130
x11Colour Ivory                = opaque $ sRGB24 255 255 240
x11Colour Ivory1               = opaque $ sRGB24 255 255 240
x11Colour Ivory2               = opaque $ sRGB24 238 238 224
x11Colour Ivory3               = opaque $ sRGB24 205 205 193
x11Colour Ivory4               = opaque $ sRGB24 139 139 131
x11Colour Khaki                = opaque $ sRGB24 240 230 140
x11Colour Khaki1               = opaque $ sRGB24 255 246 143
x11Colour Khaki2               = opaque $ sRGB24 238 230 133
x11Colour Khaki3               = opaque $ sRGB24 205 198 115
x11Colour Khaki4               = opaque $ sRGB24 139 134 78
x11Colour Lavender             = opaque $ sRGB24 230 230 250
x11Colour LavenderBlush        = opaque $ sRGB24 255 240 245
x11Colour LavenderBlush1       = opaque $ sRGB24 255 240 245
x11Colour LavenderBlush2       = opaque $ sRGB24 238 224 229
x11Colour LavenderBlush3       = opaque $ sRGB24 205 193 197
x11Colour LavenderBlush4       = opaque $ sRGB24 139 131 134
x11Colour LawnGreen            = opaque $ sRGB24 124 252 0
x11Colour LemonChiffon         = opaque $ sRGB24 255 250 205
x11Colour LemonChiffon1        = opaque $ sRGB24 255 250 205
x11Colour LemonChiffon2        = opaque $ sRGB24 238 233 191
x11Colour LemonChiffon3        = opaque $ sRGB24 205 201 165
x11Colour LemonChiffon4        = opaque $ sRGB24 139 137 112
x11Colour LightBlue            = opaque $ sRGB24 173 216 230
x11Colour LightBlue1           = opaque $ sRGB24 191 239 255
x11Colour LightBlue2           = opaque $ sRGB24 178 223 238
x11Colour LightBlue3           = opaque $ sRGB24 154 192 205
x11Colour LightBlue4           = opaque $ sRGB24 104 131 139
x11Colour LightCoral           = opaque $ sRGB24 240 128 128
x11Colour LightCyan            = opaque $ sRGB24 224 255 255
x11Colour LightCyan1           = opaque $ sRGB24 224 255 255
x11Colour LightCyan2           = opaque $ sRGB24 209 238 238
x11Colour LightCyan3           = opaque $ sRGB24 180 205 205
x11Colour LightCyan4           = opaque $ sRGB24 122 139 139
x11Colour LightGoldenrod       = opaque $ sRGB24 238 221 130
x11Colour LightGoldenrod1      = opaque $ sRGB24 255 236 139
x11Colour LightGoldenrod2      = opaque $ sRGB24 238 220 130
x11Colour LightGoldenrod3      = opaque $ sRGB24 205 190 112
x11Colour LightGoldenrod4      = opaque $ sRGB24 139 129 76
x11Colour LightGoldenrodYellow = opaque $ sRGB24 250 250 210
x11Colour LightGray            = opaque $ sRGB24 211 211 211
x11Colour LightPink            = opaque $ sRGB24 255 182 193
x11Colour LightPink1           = opaque $ sRGB24 255 174 185
x11Colour LightPink2           = opaque $ sRGB24 238 162 173
x11Colour LightPink3           = opaque $ sRGB24 205 140 149
x11Colour LightPink4           = opaque $ sRGB24 139 95  101
x11Colour LightSalmon          = opaque $ sRGB24 255 160 122
x11Colour LightSalmon1         = opaque $ sRGB24 255 160 122
x11Colour LightSalmon2         = opaque $ sRGB24 238 149 114
x11Colour LightSalmon3         = opaque $ sRGB24 205 129 98
x11Colour LightSalmon4         = opaque $ sRGB24 139 87  66
x11Colour LightSeaGreen        = opaque $ sRGB24 32  178 170
x11Colour LightSkyBlue         = opaque $ sRGB24 135 206 250
x11Colour LightSkyBlue1        = opaque $ sRGB24 176 226 255
x11Colour LightSkyBlue2        = opaque $ sRGB24 164 211 238
x11Colour LightSkyBlue3        = opaque $ sRGB24 141 182 205
x11Colour LightSkyBlue4        = opaque $ sRGB24 96  123 139
x11Colour LightSlateBlue       = opaque $ sRGB24 132 112 255
x11Colour LightSlateGray       = opaque $ sRGB24 119 136 153
x11Colour LightSteelBlue       = opaque $ sRGB24 176 196 222
x11Colour LightSteelBlue1      = opaque $ sRGB24 202 225 255
x11Colour LightSteelBlue2      = opaque $ sRGB24 188 210 238
x11Colour LightSteelBlue3      = opaque $ sRGB24 162 181 205
x11Colour LightSteelBlue4      = opaque $ sRGB24 110 123 139
x11Colour LightYellow          = opaque $ sRGB24 255 255 224
x11Colour LightYellow1         = opaque $ sRGB24 255 255 224
x11Colour LightYellow2         = opaque $ sRGB24 238 238 209
x11Colour LightYellow3         = opaque $ sRGB24 205 205 180
x11Colour LightYellow4         = opaque $ sRGB24 139 139 122
x11Colour LimeGreen            = opaque $ sRGB24 50  205 50
x11Colour Linen                = opaque $ sRGB24 250 240 230
x11Colour Magenta              = opaque $ sRGB24 255 0   255
x11Colour Magenta1             = opaque $ sRGB24 255 0   255
x11Colour Magenta2             = opaque $ sRGB24 238 0   238
x11Colour Magenta3             = opaque $ sRGB24 205 0   205
x11Colour Magenta4             = opaque $ sRGB24 139 0   139
x11Colour Maroon               = opaque $ sRGB24 176 48  96
x11Colour Maroon1              = opaque $ sRGB24 255 52  179
x11Colour Maroon2              = opaque $ sRGB24 238 48  167
x11Colour Maroon3              = opaque $ sRGB24 205 41  144
x11Colour Maroon4              = opaque $ sRGB24 139 28  98
x11Colour MediumAquamarine     = opaque $ sRGB24 102 205 170
x11Colour MediumBlue           = opaque $ sRGB24 0   0   205
x11Colour MediumOrchid         = opaque $ sRGB24 186 85  211
x11Colour MediumOrchid1        = opaque $ sRGB24 224 102 255
x11Colour MediumOrchid2        = opaque $ sRGB24 209 95  238
x11Colour MediumOrchid3        = opaque $ sRGB24 180 82  205
x11Colour MediumOrchid4        = opaque $ sRGB24 122 55  139
x11Colour MediumPurple         = opaque $ sRGB24 147 112 219
x11Colour MediumPurple1        = opaque $ sRGB24 171 130 255
x11Colour MediumPurple2        = opaque $ sRGB24 159 121 238
x11Colour MediumPurple3        = opaque $ sRGB24 137 104 205
x11Colour MediumPurple4        = opaque $ sRGB24 93  71  139
x11Colour MediumSeaGreen       = opaque $ sRGB24 60  179 113
x11Colour MediumSlateBlue      = opaque $ sRGB24 123 104 238
x11Colour MediumSpringGreen    = opaque $ sRGB24 0   250 154
x11Colour MediumTurquoise      = opaque $ sRGB24 72  209 204
x11Colour MediumVioletRed      = opaque $ sRGB24 199 21  133
x11Colour MidnightBlue         = opaque $ sRGB24 25  25  112
x11Colour MintCream            = opaque $ sRGB24 245 255 250
x11Colour MistyRose            = opaque $ sRGB24 255 228 225
x11Colour MistyRose1           = opaque $ sRGB24 255 228 225
x11Colour MistyRose2           = opaque $ sRGB24 238 213 210
x11Colour MistyRose3           = opaque $ sRGB24 205 183 181
x11Colour MistyRose4           = opaque $ sRGB24 139 125 123
x11Colour Moccasin             = opaque $ sRGB24 255 228 181
x11Colour NavajoWhite          = opaque $ sRGB24 255 222 173
x11Colour NavajoWhite1         = opaque $ sRGB24 255 222 173
x11Colour NavajoWhite2         = opaque $ sRGB24 238 207 161
x11Colour NavajoWhite3         = opaque $ sRGB24 205 179 139
x11Colour NavajoWhite4         = opaque $ sRGB24 139 121 94
x11Colour Navy                 = opaque $ sRGB24 0   0   128
x11Colour NavyBlue             = opaque $ sRGB24 0   0   128
x11Colour OldLace              = opaque $ sRGB24 253 245 230
x11Colour OliveDrab            = opaque $ sRGB24 107 142 35
x11Colour OliveDrab1           = opaque $ sRGB24 192 255 62
x11Colour OliveDrab2           = opaque $ sRGB24 179 238 58
x11Colour OliveDrab3           = opaque $ sRGB24 154 205 50
x11Colour OliveDrab4           = opaque $ sRGB24 105 139 34
x11Colour Orange               = opaque $ sRGB24 255 165 0
x11Colour Orange1              = opaque $ sRGB24 255 165 0
x11Colour Orange2              = opaque $ sRGB24 238 154 0
x11Colour Orange3              = opaque $ sRGB24 205 133 0
x11Colour Orange4              = opaque $ sRGB24 139 90  0
x11Colour OrangeRed            = opaque $ sRGB24 255 69  0
x11Colour OrangeRed1           = opaque $ sRGB24 255 69  0
x11Colour OrangeRed2           = opaque $ sRGB24 238 64  0
x11Colour OrangeRed3           = opaque $ sRGB24 205 55  0
x11Colour OrangeRed4           = opaque $ sRGB24 139 37  0
x11Colour Orchid               = opaque $ sRGB24 218 112 214
x11Colour Orchid1              = opaque $ sRGB24 255 131 250
x11Colour Orchid2              = opaque $ sRGB24 238 122 233
x11Colour Orchid3              = opaque $ sRGB24 205 105 201
x11Colour Orchid4              = opaque $ sRGB24 139 71  137
x11Colour PaleGoldenrod        = opaque $ sRGB24 238 232 170
x11Colour PaleGreen            = opaque $ sRGB24 152 251 152
x11Colour PaleGreen1           = opaque $ sRGB24 154 255 154
x11Colour PaleGreen2           = opaque $ sRGB24 144 238 144
x11Colour PaleGreen3           = opaque $ sRGB24 124 205 124
x11Colour PaleGreen4           = opaque $ sRGB24 84  139 84
x11Colour PaleTurquoise        = opaque $ sRGB24 175 238 238
x11Colour PaleTurquoise1       = opaque $ sRGB24 187 255 255
x11Colour PaleTurquoise2       = opaque $ sRGB24 174 238 238
x11Colour PaleTurquoise3       = opaque $ sRGB24 150 205 205
x11Colour PaleTurquoise4       = opaque $ sRGB24 102 139 139
x11Colour PaleVioletRed        = opaque $ sRGB24 219 112 147
x11Colour PaleVioletRed1       = opaque $ sRGB24 255 130 171
x11Colour PaleVioletRed2       = opaque $ sRGB24 238 121 159
x11Colour PaleVioletRed3       = opaque $ sRGB24 205 104 137
x11Colour PaleVioletRed4       = opaque $ sRGB24 139 71  93
x11Colour PapayaWhip           = opaque $ sRGB24 255 239 213
x11Colour PeachPuff            = opaque $ sRGB24 255 218 185
x11Colour PeachPuff1           = opaque $ sRGB24 255 218 185
x11Colour PeachPuff2           = opaque $ sRGB24 238 203 173
x11Colour PeachPuff3           = opaque $ sRGB24 205 175 149
x11Colour PeachPuff4           = opaque $ sRGB24 139 119 101
x11Colour Peru                 = opaque $ sRGB24 205 133 63
x11Colour Pink                 = opaque $ sRGB24 255 192 203
x11Colour Pink1                = opaque $ sRGB24 255 181 197
x11Colour Pink2                = opaque $ sRGB24 238 169 184
x11Colour Pink3                = opaque $ sRGB24 205 145 158
x11Colour Pink4                = opaque $ sRGB24 139 99  108
x11Colour Plum                 = opaque $ sRGB24 221 160 221
x11Colour Plum1                = opaque $ sRGB24 255 187 255
x11Colour Plum2                = opaque $ sRGB24 238 174 238
x11Colour Plum3                = opaque $ sRGB24 205 150 205
x11Colour Plum4                = opaque $ sRGB24 139 102 139
x11Colour PowderBlue           = opaque $ sRGB24 176 224 230
x11Colour Purple               = opaque $ sRGB24 160 32  240
x11Colour Purple1              = opaque $ sRGB24 155 48  255
x11Colour Purple2              = opaque $ sRGB24 145 44  238
x11Colour Purple3              = opaque $ sRGB24 125 38  205
x11Colour Purple4              = opaque $ sRGB24 85  26  139
x11Colour Red                  = opaque $ sRGB24 255 0   0
x11Colour Red1                 = opaque $ sRGB24 255 0   0
x11Colour Red2                 = opaque $ sRGB24 238 0   0
x11Colour Red3                 = opaque $ sRGB24 205 0   0
x11Colour Red4                 = opaque $ sRGB24 139 0   0
x11Colour RosyBrown            = opaque $ sRGB24 188 143 143
x11Colour RosyBrown1           = opaque $ sRGB24 255 193 193
x11Colour RosyBrown2           = opaque $ sRGB24 238 180 180
x11Colour RosyBrown3           = opaque $ sRGB24 205 155 155
x11Colour RosyBrown4           = opaque $ sRGB24 139 105 105
x11Colour RoyalBlue            = opaque $ sRGB24 65  105 225
x11Colour RoyalBlue1           = opaque $ sRGB24 72  118 255
x11Colour RoyalBlue2           = opaque $ sRGB24 67  110 238
x11Colour RoyalBlue3           = opaque $ sRGB24 58  95  205
x11Colour RoyalBlue4           = opaque $ sRGB24 39  64  139
x11Colour SaddleBrown          = opaque $ sRGB24 139 69  19
x11Colour Salmon               = opaque $ sRGB24 250 128 114
x11Colour Salmon1              = opaque $ sRGB24 255 140 105
x11Colour Salmon2              = opaque $ sRGB24 238 130 98
x11Colour Salmon3              = opaque $ sRGB24 205 112 84
x11Colour Salmon4              = opaque $ sRGB24 139 76  57
x11Colour SandyBrown           = opaque $ sRGB24 244 164 96
x11Colour SeaGreen             = opaque $ sRGB24 46  139 87
x11Colour SeaGreen1            = opaque $ sRGB24 84  255 159
x11Colour SeaGreen2            = opaque $ sRGB24 78  238 148
x11Colour SeaGreen3            = opaque $ sRGB24 67  205 128
x11Colour SeaGreen4            = opaque $ sRGB24 46  139 87
x11Colour SeaShell             = opaque $ sRGB24 255 245 238
x11Colour SeaShell1            = opaque $ sRGB24 255 245 238
x11Colour SeaShell2            = opaque $ sRGB24 238 229 222
x11Colour SeaShell3            = opaque $ sRGB24 205 197 191
x11Colour SeaShell4            = opaque $ sRGB24 139 134 130
x11Colour Sienna               = opaque $ sRGB24 160 82  45
x11Colour Sienna1              = opaque $ sRGB24 255 130 71
x11Colour Sienna2              = opaque $ sRGB24 238 121 66
x11Colour Sienna3              = opaque $ sRGB24 205 104 57
x11Colour Sienna4              = opaque $ sRGB24 139 71  38
x11Colour SkyBlue              = opaque $ sRGB24 135 206 235
x11Colour SkyBlue1             = opaque $ sRGB24 135 206 255
x11Colour SkyBlue2             = opaque $ sRGB24 126 192 238
x11Colour SkyBlue3             = opaque $ sRGB24 108 166 205
x11Colour SkyBlue4             = opaque $ sRGB24 74  112 139
x11Colour SlateBlue            = opaque $ sRGB24 106 90  205
x11Colour SlateBlue1           = opaque $ sRGB24 131 111 255
x11Colour SlateBlue2           = opaque $ sRGB24 122 103 238
x11Colour SlateBlue3           = opaque $ sRGB24 105 89  205
x11Colour SlateBlue4           = opaque $ sRGB24 71  60  139
x11Colour SlateGray            = opaque $ sRGB24 112 128 144
x11Colour SlateGray1           = opaque $ sRGB24 198 226 255
x11Colour SlateGray2           = opaque $ sRGB24 185 211 238
x11Colour SlateGray3           = opaque $ sRGB24 159 182 205
x11Colour SlateGray4           = opaque $ sRGB24 108 123 139
x11Colour Snow                 = opaque $ sRGB24 255 250 250
x11Colour Snow1                = opaque $ sRGB24 255 250 250
x11Colour Snow2                = opaque $ sRGB24 238 233 233
x11Colour Snow3                = opaque $ sRGB24 205 201 201
x11Colour Snow4                = opaque $ sRGB24 139 137 137
x11Colour SpringGreen          = opaque $ sRGB24 0   255 127
x11Colour SpringGreen1         = opaque $ sRGB24 0   255 127
x11Colour SpringGreen2         = opaque $ sRGB24 0   238 118
x11Colour SpringGreen3         = opaque $ sRGB24 0   205 102
x11Colour SpringGreen4         = opaque $ sRGB24 0   139 69
x11Colour SteelBlue            = opaque $ sRGB24 70  130 180
x11Colour SteelBlue1           = opaque $ sRGB24 99  184 255
x11Colour SteelBlue2           = opaque $ sRGB24 92  172 238
x11Colour SteelBlue3           = opaque $ sRGB24 79  148 205
x11Colour SteelBlue4           = opaque $ sRGB24 54  100 139
x11Colour Tan                  = opaque $ sRGB24 210 180 140
x11Colour Tan1                 = opaque $ sRGB24 255 165 79
x11Colour Tan2                 = opaque $ sRGB24 238 154 73
x11Colour Tan3                 = opaque $ sRGB24 205 133 63
x11Colour Tan4                 = opaque $ sRGB24 139 90  43
x11Colour Thistle              = opaque $ sRGB24 216 191 216
x11Colour Thistle1             = opaque $ sRGB24 255 225 255
x11Colour Thistle2             = opaque $ sRGB24 238 210 238
x11Colour Thistle3             = opaque $ sRGB24 205 181 205
x11Colour Thistle4             = opaque $ sRGB24 139 123 139
x11Colour Tomato               = opaque $ sRGB24 255 99  71
x11Colour Tomato1              = opaque $ sRGB24 255 99  71
x11Colour Tomato2              = opaque $ sRGB24 238 92  66
x11Colour Tomato3              = opaque $ sRGB24 205 79  57
x11Colour Tomato4              = opaque $ sRGB24 139 54  38
x11Colour Transparent          = transparent
x11Colour Turquoise            = opaque $ sRGB24 64  224 208
x11Colour Turquoise1           = opaque $ sRGB24 0   245 255
x11Colour Turquoise2           = opaque $ sRGB24 0   229 238
x11Colour Turquoise3           = opaque $ sRGB24 0   197 205
x11Colour Turquoise4           = opaque $ sRGB24 0   134 139
x11Colour Violet               = opaque $ sRGB24 238 130 238
x11Colour VioletRed            = opaque $ sRGB24 208 32  144
x11Colour VioletRed1           = opaque $ sRGB24 255 62  150
x11Colour VioletRed2           = opaque $ sRGB24 238 58  140
x11Colour VioletRed3           = opaque $ sRGB24 205 50  120
x11Colour VioletRed4           = opaque $ sRGB24 139 34  82
x11Colour Wheat                = opaque $ sRGB24 245 222 179
x11Colour Wheat1               = opaque $ sRGB24 255 231 186
x11Colour Wheat2               = opaque $ sRGB24 238 216 174
x11Colour Wheat3               = opaque $ sRGB24 205 186 150
x11Colour Wheat4               = opaque $ sRGB24 139 126 102
x11Colour White                = opaque $ sRGB24 255 255 255
x11Colour WhiteSmoke           = opaque $ sRGB24 245 245 245
x11Colour Yellow               = opaque $ sRGB24 255 255 0
x11Colour Yellow1              = opaque $ sRGB24 255 255 0
x11Colour Yellow2              = opaque $ sRGB24 238 238 0
x11Colour Yellow3              = opaque $ sRGB24 205 205 0
x11Colour Yellow4              = opaque $ sRGB24 139 139 0
x11Colour YellowGreen          = opaque $ sRGB24 154 205 50

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
