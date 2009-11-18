{- |
   Module      : Data.GraphViz.Attributes.Colors
   Description : Helper functions for converting to Dot format.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines the various colors, etc. for Graphviz.  For
   information on colors in general, see:
     <http://graphviz.org/doc/info/attrs.html#k:color>
   For named colors, see:
     <http://graphviz.org/doc/info/colors.html>

   Note that the Brewer Colors are covered by the following license:
     <http://graphviz.org/doc/info/colors.html#brewer_license>

   When parsing and printing 'Color' values (more specifically, the
   Brewer color values), the actual 'ColorScheme' being used isn't
   taken into account.  Also, the \"\/foo\/bar\" style of overriding
   the 'ColorScheme' being used isn't supported for parsing purposes
   (and will result in a parsing failure).
-}
module Data.GraphViz.Attributes.Colors
       ( ColorScheme(..)
       , BrewerName(..)
       , Color(..)
       , X11Color(..)
       ) where

import Data.GraphViz.Types.Parsing
import Data.GraphViz.Types.Printing

import Data.Char(isDigit, isHexDigit)
import Numeric(showHex, readHex)
import Data.Word(Word8)
import Control.Monad(liftM, liftM2)

-- -----------------------------------------------------------------------------

-- | This represents the color schemes that Graphviz accepts.  As
--   mentioned above, these are /not/ used for actual parsing or
--   printing.
data ColorScheme = X11
                 | BrewerScheme BrewerName Word8 -- ^ The value of the
                                                 --   scheme number
                                                 --   depends on
                                                 --   the name.
                 deriving (Eq, Ord, Show, Read)

instance PrintDot ColorScheme where
    unqtDot X11 = unqtDot "X11"
    unqtDot (BrewerScheme n l) = unqtDot n <> unqtDot l

instance ParseDot ColorScheme where
    parseUnqt = stringRep X11 "X11"
                `onFail`
                liftM2 BrewerScheme parseUnqt parseUnqt

-- | All of these have a minimum level value of @3@, with a maximum
--   of @9@ unless otherwise specified.
data BrewerName = Accent   -- ^ @8@
                | Blues
                | Brbg     -- ^ @11@
                | Bugn
                | Bupu
                | Dark2    -- ^ @8@
                | Gnbu
                | Greens
                | Greys
                | Oranges
                | Orrd
                | Paired   -- ^ @12@
                | Pastel1
                | Pastel2  -- ^ @8@
                | Piyg     -- ^ @11@
                | Prgn     -- ^ @11@
                | Pubu
                | Pubugn
                | Puor     -- ^ @11@; note that the last two are listed
                           --   after the @'Purd'@ values in the
                           --   documentation.
                | Purd
                | Purples
                | Rdbu     -- ^ @11@; note that the last two are listed
                           --   first.
                | Rdgy     -- ^ @11@; note that the last two are listed
                           --   after the @'Rdpu'@ values in the
                           --   documentation.
                | Rdpu
                | Rdylbu   -- ^ @11@
                | Rdylgn   -- ^ @11@
                | Reds
                | Set1
                | Set2     -- ^ @8@
                | Set3     -- ^ @12@
                | Spectral -- ^ @11@
                | Ylgn
                | Ylgnbu
                | Ylorbr
                | Ylorrd
                deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot BrewerName where
    unqtDot Accent   = unqtDot "accent"
    unqtDot Blues    = unqtDot "blues"
    unqtDot Brbg     = unqtDot "brbg"
    unqtDot Bugn     = unqtDot "bugn"
    unqtDot Bupu     = unqtDot "bupu"
    unqtDot Dark2    = unqtDot "dark2"
    unqtDot Gnbu     = unqtDot "gnbu"
    unqtDot Greens   = unqtDot "greens"
    unqtDot Greys    = unqtDot "greys"
    unqtDot Oranges  = unqtDot "oranges"
    unqtDot Orrd     = unqtDot "orrd"
    unqtDot Paired   = unqtDot "paired"
    unqtDot Pastel1  = unqtDot "pastel1"
    unqtDot Pastel2  = unqtDot "pastel2"
    unqtDot Piyg     = unqtDot "piyg"
    unqtDot Prgn     = unqtDot "prgn"
    unqtDot Pubu     = unqtDot "pubu"
    unqtDot Pubugn   = unqtDot "pubugn"
    unqtDot Puor     = unqtDot "puor"
    unqtDot Purd     = unqtDot "purd"
    unqtDot Purples  = unqtDot "purples"
    unqtDot Rdbu     = unqtDot "rdbu"
    unqtDot Rdgy     = unqtDot "rdgy"
    unqtDot Rdpu     = unqtDot "rdpu"
    unqtDot Rdylbu   = unqtDot "rdylbu"
    unqtDot Rdylgn   = unqtDot "rdylgn"
    unqtDot Reds     = unqtDot "reds"
    unqtDot Set1     = unqtDot "set1"
    unqtDot Set2     = unqtDot "set2"
    unqtDot Set3     = unqtDot "set3"
    unqtDot Spectral = unqtDot "spectral"
    unqtDot Ylgn     = unqtDot "ylgn"
    unqtDot Ylgnbu   = unqtDot "ylgnbu"
    unqtDot Ylorbr   = unqtDot "ylorbr"
    unqtDot Ylorrd   = unqtDot "ylorrd"

instance ParseDot BrewerName where
  -- The order is different from above to make sure longer names are
  -- parsed first.
  parseUnqt = oneOf [ stringRep Accent "accent"
                    , stringRep Blues "blues"
                    , stringRep Brbg "brbg"
                    , stringRep Bugn "bugn"
                    , stringRep Bupu "bupu"
                    , stringRep Dark2 "dark2"
                    , stringRep Gnbu "gnbu"
                    , stringRep Greens "greens"
                    , stringRep Greys "greys"
                    , stringRep Oranges "oranges"
                    , stringRep Orrd "orrd"
                    , stringRep Paired "paired"
                    , stringRep Pastel1 "pastel1"
                    , stringRep Pastel2 "pastel2"
                    , stringRep Piyg "piyg"
                    , stringRep Prgn "prgn"
                    , stringRep Pubugn "pubugn"
                    , stringRep Pubu "pubu"
                    , stringRep Puor "puor"
                    , stringRep Purd "purd"
                    , stringRep Purples "purples"
                    , stringRep Rdbu "rdbu"
                    , stringRep Rdgy "rdgy"
                    , stringRep Rdpu "rdpu"
                    , stringRep Rdylbu "rdylbu"
                    , stringRep Rdylgn "rdylgn"
                    , stringRep Reds "reds"
                    , stringRep Set1 "set1"
                    , stringRep Set2 "set2"
                    , stringRep Set3 "set3"
                    , stringRep Spectral "spectral"
                    , stringRep Ylgnbu "ylgnbu"
                    , stringRep Ylgn "ylgn"
                    , stringRep Ylorbr "ylorbr"
                    , stringRep Ylorrd "ylorrd"
                    ]

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
           | BrewerColor Word8 -- ^ This value should be between @1@
                               --   and the level of the 'BrewerName'
                               --   being used.
             deriving (Eq, Ord, Show, Read)

instance PrintDot Color where
    unqtDot (RGB  r g b)    = hexColor [r,g,b]
    unqtDot (RGBA r g b a)  = hexColor [r,g,b,a]
    unqtDot (HSV  h s v)    = hcat . punctuate comma $ map unqtDot [h,s,v]
    unqtDot (X11Color name) = unqtDot name
    unqtDot (BrewerColor n) = unqtDot n

    toDot (X11Color name) = toDot name
    toDot (BrewerColor n) = toDot n
    toDot c               = doubleQuotes $ unqtDot c

    unqtListToDot = hcat . punctuate colon . map unqtDot

    -- These two don't need to be quoted if they're on their own.
    listToDot [X11Color name] = toDot name
    listToDot [BrewerColor n] = toDot n
    listToDot cs              = doubleQuotes $ unqtListToDot cs

hexColor :: [Word8] -> DotCode
hexColor = (<>) (char '#') . hcat . map word8Doc

word8Doc   :: Word8 -> DotCode
word8Doc w = text $ padding ++ simple
    where
      simple = showHex w ""
      padding = replicate count '0'
      count = 2 - findCols 1 w
      findCols :: Int -> Word8 -> Int
      findCols c n
          | n < 16 = c
          | otherwise = findCols (c+1) (n `div` 16)

instance ParseDot Color where
    parseUnqt = oneOf [ parseHexBased
                      , parseHSV
                      , liftM X11Color parseUnqt
                      , liftM BrewerColor parseUnqt
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
                  , liftM BrewerColor parseUnqt
                  ]

    parseUnqtList = sepBy1 parseUnqt (character ':')

    parseList = liftM return
                -- Unquoted single color
                (oneOf [ liftM X11Color parseUnqt
                       , liftM BrewerColor parseUnqt
                       ]
                )
                `onFail`
                quotedParse parseUnqtList

-- -----------------------------------------------------------------------------

-- | The X11 colors that Graphviz uses.  Note that these are slightly
--   different from the "normal" X11 colors used (e.g. the inclusion
--   of @Crimson@).  Graphviz's list of colors also duplicated almost
--   all @*Gray*@ colors with @*Grey*@ ones; parsing of an 'X11Color'
--   which is specified using \"grey\" will succeed, even for those
--   that didn't have the duplicate spelling (e.g. @DarkSlateGray1@).
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
  parseUnqt = oneOf [ stringRep  AliceBlue "aliceblue"
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
