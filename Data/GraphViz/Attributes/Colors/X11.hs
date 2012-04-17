{-# LANGUAGE OverloadedStrings #-}

{- |
   Module      : Data.GraphViz.Attributes.Colors.X11
   Description : Specification of X11 colors.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   Graphviz's definition of X11 colors differs from the \"normal\"
   list installed on many systems at @/usr/share/X11/rgb.txt@.  For
   example, @Crimson@ is not a usual X11 color.

   Furthermore, all @Gray*@ colors are duplicated with @Grey*@ names.
   To simplify this, these duplicates have been removed but /all/
   'X11Color's with \"@Gray@\" (whether they have the duplicate
   spelling or not) in their name are also parseable as if they were
   spelt with \"@grey@\".

   The complete list of X11 colors can be found at
   <http://www.graphviz.org/doc/info/colors.html#x11>.

 -}
module Data.GraphViz.Attributes.Colors.X11
    ( X11Color(..)
    , x11Colour
    ) where

import Data.GraphViz.Parsing
import Data.GraphViz.Printing

import Data.Colour( AlphaColour, opaque, transparent)
import Data.Colour.SRGB(sRGB24)

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
  unqtDot AliceBlue            = unqtText "aliceblue"
  unqtDot AntiqueWhite         = unqtText "antiquewhite"
  unqtDot AntiqueWhite1        = unqtText "antiquewhite1"
  unqtDot AntiqueWhite2        = unqtText "antiquewhite2"
  unqtDot AntiqueWhite3        = unqtText "antiquewhite3"
  unqtDot AntiqueWhite4        = unqtText "antiquewhite4"
  unqtDot Aquamarine           = unqtText "aquamarine"
  unqtDot Aquamarine1          = unqtText "aquamarine1"
  unqtDot Aquamarine2          = unqtText "aquamarine2"
  unqtDot Aquamarine3          = unqtText "aquamarine3"
  unqtDot Aquamarine4          = unqtText "aquamarine4"
  unqtDot Azure                = unqtText "azure"
  unqtDot Azure1               = unqtText "azure1"
  unqtDot Azure2               = unqtText "azure2"
  unqtDot Azure3               = unqtText "azure3"
  unqtDot Azure4               = unqtText "azure4"
  unqtDot Beige                = unqtText "beige"
  unqtDot Bisque               = unqtText "bisque"
  unqtDot Bisque1              = unqtText "bisque1"
  unqtDot Bisque2              = unqtText "bisque2"
  unqtDot Bisque3              = unqtText "bisque3"
  unqtDot Bisque4              = unqtText "bisque4"
  unqtDot Black                = unqtText "black"
  unqtDot BlanchedAlmond       = unqtText "blanchedalmond"
  unqtDot Blue                 = unqtText "blue"
  unqtDot Blue1                = unqtText "blue1"
  unqtDot Blue2                = unqtText "blue2"
  unqtDot Blue3                = unqtText "blue3"
  unqtDot Blue4                = unqtText "blue4"
  unqtDot BlueViolet           = unqtText "blueviolet"
  unqtDot Brown                = unqtText "brown"
  unqtDot Brown1               = unqtText "brown1"
  unqtDot Brown2               = unqtText "brown2"
  unqtDot Brown3               = unqtText "brown3"
  unqtDot Brown4               = unqtText "brown4"
  unqtDot Burlywood            = unqtText "burlywood"
  unqtDot Burlywood1           = unqtText "burlywood1"
  unqtDot Burlywood2           = unqtText "burlywood2"
  unqtDot Burlywood3           = unqtText "burlywood3"
  unqtDot Burlywood4           = unqtText "burlywood4"
  unqtDot CadetBlue            = unqtText "cadetblue"
  unqtDot CadetBlue1           = unqtText "cadetblue1"
  unqtDot CadetBlue2           = unqtText "cadetblue2"
  unqtDot CadetBlue3           = unqtText "cadetblue3"
  unqtDot CadetBlue4           = unqtText "cadetblue4"
  unqtDot Chartreuse           = unqtText "chartreuse"
  unqtDot Chartreuse1          = unqtText "chartreuse1"
  unqtDot Chartreuse2          = unqtText "chartreuse2"
  unqtDot Chartreuse3          = unqtText "chartreuse3"
  unqtDot Chartreuse4          = unqtText "chartreuse4"
  unqtDot Chocolate            = unqtText "chocolate"
  unqtDot Chocolate1           = unqtText "chocolate1"
  unqtDot Chocolate2           = unqtText "chocolate2"
  unqtDot Chocolate3           = unqtText "chocolate3"
  unqtDot Chocolate4           = unqtText "chocolate4"
  unqtDot Coral                = unqtText "coral"
  unqtDot Coral1               = unqtText "coral1"
  unqtDot Coral2               = unqtText "coral2"
  unqtDot Coral3               = unqtText "coral3"
  unqtDot Coral4               = unqtText "coral4"
  unqtDot CornFlowerBlue       = unqtText "cornflowerblue"
  unqtDot CornSilk             = unqtText "cornsilk"
  unqtDot CornSilk1            = unqtText "cornsilk1"
  unqtDot CornSilk2            = unqtText "cornsilk2"
  unqtDot CornSilk3            = unqtText "cornsilk3"
  unqtDot CornSilk4            = unqtText "cornsilk4"
  unqtDot Crimson              = unqtText "crimson"
  unqtDot Cyan                 = unqtText "cyan"
  unqtDot Cyan1                = unqtText "cyan1"
  unqtDot Cyan2                = unqtText "cyan2"
  unqtDot Cyan3                = unqtText "cyan3"
  unqtDot Cyan4                = unqtText "cyan4"
  unqtDot DarkGoldenrod        = unqtText "darkgoldenrod"
  unqtDot DarkGoldenrod1       = unqtText "darkgoldenrod1"
  unqtDot DarkGoldenrod2       = unqtText "darkgoldenrod2"
  unqtDot DarkGoldenrod3       = unqtText "darkgoldenrod3"
  unqtDot DarkGoldenrod4       = unqtText "darkgoldenrod4"
  unqtDot DarkGreen            = unqtText "darkgreen"
  unqtDot Darkkhaki            = unqtText "darkkhaki"
  unqtDot DarkOliveGreen       = unqtText "darkolivegreen"
  unqtDot DarkOliveGreen1      = unqtText "darkolivegreen1"
  unqtDot DarkOliveGreen2      = unqtText "darkolivegreen2"
  unqtDot DarkOliveGreen3      = unqtText "darkolivegreen3"
  unqtDot DarkOliveGreen4      = unqtText "darkolivegreen4"
  unqtDot DarkOrange           = unqtText "darkorange"
  unqtDot DarkOrange1          = unqtText "darkorange1"
  unqtDot DarkOrange2          = unqtText "darkorange2"
  unqtDot DarkOrange3          = unqtText "darkorange3"
  unqtDot DarkOrange4          = unqtText "darkorange4"
  unqtDot DarkOrchid           = unqtText "darkorchid"
  unqtDot DarkOrchid1          = unqtText "darkorchid1"
  unqtDot DarkOrchid2          = unqtText "darkorchid2"
  unqtDot DarkOrchid3          = unqtText "darkorchid3"
  unqtDot DarkOrchid4          = unqtText "darkorchid4"
  unqtDot DarkSalmon           = unqtText "darksalmon"
  unqtDot DarkSeaGreen         = unqtText "darkseagreen"
  unqtDot DarkSeaGreen1        = unqtText "darkseagreen1"
  unqtDot DarkSeaGreen2        = unqtText "darkseagreen2"
  unqtDot DarkSeaGreen3        = unqtText "darkseagreen3"
  unqtDot DarkSeaGreen4        = unqtText "darkseagreen4"
  unqtDot DarkSlateBlue        = unqtText "darkslateblue"
  unqtDot DarkSlateGray        = unqtText "darkslategray"
  unqtDot DarkSlateGray1       = unqtText "darkslategray1"
  unqtDot DarkSlateGray2       = unqtText "darkslategray2"
  unqtDot DarkSlateGray3       = unqtText "darkslategray3"
  unqtDot DarkSlateGray4       = unqtText "darkslategray4"
  unqtDot DarkTurquoise        = unqtText "darkturquoise"
  unqtDot DarkViolet           = unqtText "darkviolet"
  unqtDot DeepPink             = unqtText "deeppink"
  unqtDot DeepPink1            = unqtText "deeppink1"
  unqtDot DeepPink2            = unqtText "deeppink2"
  unqtDot DeepPink3            = unqtText "deeppink3"
  unqtDot DeepPink4            = unqtText "deeppink4"
  unqtDot DeepSkyBlue          = unqtText "deepskyblue"
  unqtDot DeepSkyBlue1         = unqtText "deepskyblue1"
  unqtDot DeepSkyBlue2         = unqtText "deepskyblue2"
  unqtDot DeepSkyBlue3         = unqtText "deepskyblue3"
  unqtDot DeepSkyBlue4         = unqtText "deepskyblue4"
  unqtDot DimGray              = unqtText "dimgray"
  unqtDot DodgerBlue           = unqtText "dodgerblue"
  unqtDot DodgerBlue1          = unqtText "dodgerblue1"
  unqtDot DodgerBlue2          = unqtText "dodgerblue2"
  unqtDot DodgerBlue3          = unqtText "dodgerblue3"
  unqtDot DodgerBlue4          = unqtText "dodgerblue4"
  unqtDot Firebrick            = unqtText "firebrick"
  unqtDot Firebrick1           = unqtText "firebrick1"
  unqtDot Firebrick2           = unqtText "firebrick2"
  unqtDot Firebrick3           = unqtText "firebrick3"
  unqtDot Firebrick4           = unqtText "firebrick4"
  unqtDot FloralWhite          = unqtText "floralwhite"
  unqtDot ForestGreen          = unqtText "forestgreen"
  unqtDot Gainsboro            = unqtText "gainsboro"
  unqtDot GhostWhite           = unqtText "ghostwhite"
  unqtDot Gold                 = unqtText "gold"
  unqtDot Gold1                = unqtText "gold1"
  unqtDot Gold2                = unqtText "gold2"
  unqtDot Gold3                = unqtText "gold3"
  unqtDot Gold4                = unqtText "gold4"
  unqtDot Goldenrod            = unqtText "goldenrod"
  unqtDot Goldenrod1           = unqtText "goldenrod1"
  unqtDot Goldenrod2           = unqtText "goldenrod2"
  unqtDot Goldenrod3           = unqtText "goldenrod3"
  unqtDot Goldenrod4           = unqtText "goldenrod4"
  unqtDot Gray                 = unqtText "gray"
  unqtDot Gray0                = unqtText "gray0"
  unqtDot Gray1                = unqtText "gray1"
  unqtDot Gray2                = unqtText "gray2"
  unqtDot Gray3                = unqtText "gray3"
  unqtDot Gray4                = unqtText "gray4"
  unqtDot Gray5                = unqtText "gray5"
  unqtDot Gray6                = unqtText "gray6"
  unqtDot Gray7                = unqtText "gray7"
  unqtDot Gray8                = unqtText "gray8"
  unqtDot Gray9                = unqtText "gray9"
  unqtDot Gray10               = unqtText "gray10"
  unqtDot Gray11               = unqtText "gray11"
  unqtDot Gray12               = unqtText "gray12"
  unqtDot Gray13               = unqtText "gray13"
  unqtDot Gray14               = unqtText "gray14"
  unqtDot Gray15               = unqtText "gray15"
  unqtDot Gray16               = unqtText "gray16"
  unqtDot Gray17               = unqtText "gray17"
  unqtDot Gray18               = unqtText "gray18"
  unqtDot Gray19               = unqtText "gray19"
  unqtDot Gray20               = unqtText "gray20"
  unqtDot Gray21               = unqtText "gray21"
  unqtDot Gray22               = unqtText "gray22"
  unqtDot Gray23               = unqtText "gray23"
  unqtDot Gray24               = unqtText "gray24"
  unqtDot Gray25               = unqtText "gray25"
  unqtDot Gray26               = unqtText "gray26"
  unqtDot Gray27               = unqtText "gray27"
  unqtDot Gray28               = unqtText "gray28"
  unqtDot Gray29               = unqtText "gray29"
  unqtDot Gray30               = unqtText "gray30"
  unqtDot Gray31               = unqtText "gray31"
  unqtDot Gray32               = unqtText "gray32"
  unqtDot Gray33               = unqtText "gray33"
  unqtDot Gray34               = unqtText "gray34"
  unqtDot Gray35               = unqtText "gray35"
  unqtDot Gray36               = unqtText "gray36"
  unqtDot Gray37               = unqtText "gray37"
  unqtDot Gray38               = unqtText "gray38"
  unqtDot Gray39               = unqtText "gray39"
  unqtDot Gray40               = unqtText "gray40"
  unqtDot Gray41               = unqtText "gray41"
  unqtDot Gray42               = unqtText "gray42"
  unqtDot Gray43               = unqtText "gray43"
  unqtDot Gray44               = unqtText "gray44"
  unqtDot Gray45               = unqtText "gray45"
  unqtDot Gray46               = unqtText "gray46"
  unqtDot Gray47               = unqtText "gray47"
  unqtDot Gray48               = unqtText "gray48"
  unqtDot Gray49               = unqtText "gray49"
  unqtDot Gray50               = unqtText "gray50"
  unqtDot Gray51               = unqtText "gray51"
  unqtDot Gray52               = unqtText "gray52"
  unqtDot Gray53               = unqtText "gray53"
  unqtDot Gray54               = unqtText "gray54"
  unqtDot Gray55               = unqtText "gray55"
  unqtDot Gray56               = unqtText "gray56"
  unqtDot Gray57               = unqtText "gray57"
  unqtDot Gray58               = unqtText "gray58"
  unqtDot Gray59               = unqtText "gray59"
  unqtDot Gray60               = unqtText "gray60"
  unqtDot Gray61               = unqtText "gray61"
  unqtDot Gray62               = unqtText "gray62"
  unqtDot Gray63               = unqtText "gray63"
  unqtDot Gray64               = unqtText "gray64"
  unqtDot Gray65               = unqtText "gray65"
  unqtDot Gray66               = unqtText "gray66"
  unqtDot Gray67               = unqtText "gray67"
  unqtDot Gray68               = unqtText "gray68"
  unqtDot Gray69               = unqtText "gray69"
  unqtDot Gray70               = unqtText "gray70"
  unqtDot Gray71               = unqtText "gray71"
  unqtDot Gray72               = unqtText "gray72"
  unqtDot Gray73               = unqtText "gray73"
  unqtDot Gray74               = unqtText "gray74"
  unqtDot Gray75               = unqtText "gray75"
  unqtDot Gray76               = unqtText "gray76"
  unqtDot Gray77               = unqtText "gray77"
  unqtDot Gray78               = unqtText "gray78"
  unqtDot Gray79               = unqtText "gray79"
  unqtDot Gray80               = unqtText "gray80"
  unqtDot Gray81               = unqtText "gray81"
  unqtDot Gray82               = unqtText "gray82"
  unqtDot Gray83               = unqtText "gray83"
  unqtDot Gray84               = unqtText "gray84"
  unqtDot Gray85               = unqtText "gray85"
  unqtDot Gray86               = unqtText "gray86"
  unqtDot Gray87               = unqtText "gray87"
  unqtDot Gray88               = unqtText "gray88"
  unqtDot Gray89               = unqtText "gray89"
  unqtDot Gray90               = unqtText "gray90"
  unqtDot Gray91               = unqtText "gray91"
  unqtDot Gray92               = unqtText "gray92"
  unqtDot Gray93               = unqtText "gray93"
  unqtDot Gray94               = unqtText "gray94"
  unqtDot Gray95               = unqtText "gray95"
  unqtDot Gray96               = unqtText "gray96"
  unqtDot Gray97               = unqtText "gray97"
  unqtDot Gray98               = unqtText "gray98"
  unqtDot Gray99               = unqtText "gray99"
  unqtDot Gray100              = unqtText "gray100"
  unqtDot Green                = unqtText "green"
  unqtDot Green1               = unqtText "green1"
  unqtDot Green2               = unqtText "green2"
  unqtDot Green3               = unqtText "green3"
  unqtDot Green4               = unqtText "green4"
  unqtDot GreenYellow          = unqtText "greenyellow"
  unqtDot HoneyDew             = unqtText "honeydew"
  unqtDot HoneyDew1            = unqtText "honeydew1"
  unqtDot HoneyDew2            = unqtText "honeydew2"
  unqtDot HoneyDew3            = unqtText "honeydew3"
  unqtDot HoneyDew4            = unqtText "honeydew4"
  unqtDot HotPink              = unqtText "hotpink"
  unqtDot HotPink1             = unqtText "hotpink1"
  unqtDot HotPink2             = unqtText "hotpink2"
  unqtDot HotPink3             = unqtText "hotpink3"
  unqtDot HotPink4             = unqtText "hotpink4"
  unqtDot IndianRed            = unqtText "indianred"
  unqtDot IndianRed1           = unqtText "indianred1"
  unqtDot IndianRed2           = unqtText "indianred2"
  unqtDot IndianRed3           = unqtText "indianred3"
  unqtDot IndianRed4           = unqtText "indianred4"
  unqtDot Indigo               = unqtText "indigo"
  unqtDot Ivory                = unqtText "ivory"
  unqtDot Ivory1               = unqtText "ivory1"
  unqtDot Ivory2               = unqtText "ivory2"
  unqtDot Ivory3               = unqtText "ivory3"
  unqtDot Ivory4               = unqtText "ivory4"
  unqtDot Khaki                = unqtText "khaki"
  unqtDot Khaki1               = unqtText "khaki1"
  unqtDot Khaki2               = unqtText "khaki2"
  unqtDot Khaki3               = unqtText "khaki3"
  unqtDot Khaki4               = unqtText "khaki4"
  unqtDot Lavender             = unqtText "lavender"
  unqtDot LavenderBlush        = unqtText "lavenderblush"
  unqtDot LavenderBlush1       = unqtText "lavenderblush1"
  unqtDot LavenderBlush2       = unqtText "lavenderblush2"
  unqtDot LavenderBlush3       = unqtText "lavenderblush3"
  unqtDot LavenderBlush4       = unqtText "lavenderblush4"
  unqtDot LawnGreen            = unqtText "lawngreen"
  unqtDot LemonChiffon         = unqtText "lemonchiffon"
  unqtDot LemonChiffon1        = unqtText "lemonchiffon1"
  unqtDot LemonChiffon2        = unqtText "lemonchiffon2"
  unqtDot LemonChiffon3        = unqtText "lemonchiffon3"
  unqtDot LemonChiffon4        = unqtText "lemonchiffon4"
  unqtDot LightBlue            = unqtText "lightblue"
  unqtDot LightBlue1           = unqtText "lightblue1"
  unqtDot LightBlue2           = unqtText "lightblue2"
  unqtDot LightBlue3           = unqtText "lightblue3"
  unqtDot LightBlue4           = unqtText "lightblue4"
  unqtDot LightCoral           = unqtText "lightcoral"
  unqtDot LightCyan            = unqtText "lightcyan"
  unqtDot LightCyan1           = unqtText "lightcyan1"
  unqtDot LightCyan2           = unqtText "lightcyan2"
  unqtDot LightCyan3           = unqtText "lightcyan3"
  unqtDot LightCyan4           = unqtText "lightcyan4"
  unqtDot LightGoldenrod       = unqtText "lightgoldenrod"
  unqtDot LightGoldenrod1      = unqtText "lightgoldenrod1"
  unqtDot LightGoldenrod2      = unqtText "lightgoldenrod2"
  unqtDot LightGoldenrod3      = unqtText "lightgoldenrod3"
  unqtDot LightGoldenrod4      = unqtText "lightgoldenrod4"
  unqtDot LightGoldenrodYellow = unqtText "lightgoldenrodyellow"
  unqtDot LightGray            = unqtText "lightgray"
  unqtDot LightPink            = unqtText "lightpink"
  unqtDot LightPink1           = unqtText "lightpink1"
  unqtDot LightPink2           = unqtText "lightpink2"
  unqtDot LightPink3           = unqtText "lightpink3"
  unqtDot LightPink4           = unqtText "lightpink4"
  unqtDot LightSalmon          = unqtText "lightsalmon"
  unqtDot LightSalmon1         = unqtText "lightsalmon1"
  unqtDot LightSalmon2         = unqtText "lightsalmon2"
  unqtDot LightSalmon3         = unqtText "lightsalmon3"
  unqtDot LightSalmon4         = unqtText "lightsalmon4"
  unqtDot LightSeaGreen        = unqtText "lightseagreen"
  unqtDot LightSkyBlue         = unqtText "lightskyblue"
  unqtDot LightSkyBlue1        = unqtText "lightskyblue1"
  unqtDot LightSkyBlue2        = unqtText "lightskyblue2"
  unqtDot LightSkyBlue3        = unqtText "lightskyblue3"
  unqtDot LightSkyBlue4        = unqtText "lightskyblue4"
  unqtDot LightSlateBlue       = unqtText "lightslateblue"
  unqtDot LightSlateGray       = unqtText "lightslategray"
  unqtDot LightSteelBlue       = unqtText "lightsteelblue"
  unqtDot LightSteelBlue1      = unqtText "lightsteelblue1"
  unqtDot LightSteelBlue2      = unqtText "lightsteelblue2"
  unqtDot LightSteelBlue3      = unqtText "lightsteelblue3"
  unqtDot LightSteelBlue4      = unqtText "lightsteelblue4"
  unqtDot LightYellow          = unqtText "lightyellow"
  unqtDot LightYellow1         = unqtText "lightyellow1"
  unqtDot LightYellow2         = unqtText "lightyellow2"
  unqtDot LightYellow3         = unqtText "lightyellow3"
  unqtDot LightYellow4         = unqtText "lightyellow4"
  unqtDot LimeGreen            = unqtText "limegreen"
  unqtDot Linen                = unqtText "linen"
  unqtDot Magenta              = unqtText "magenta"
  unqtDot Magenta1             = unqtText "magenta1"
  unqtDot Magenta2             = unqtText "magenta2"
  unqtDot Magenta3             = unqtText "magenta3"
  unqtDot Magenta4             = unqtText "magenta4"
  unqtDot Maroon               = unqtText "maroon"
  unqtDot Maroon1              = unqtText "maroon1"
  unqtDot Maroon2              = unqtText "maroon2"
  unqtDot Maroon3              = unqtText "maroon3"
  unqtDot Maroon4              = unqtText "maroon4"
  unqtDot MediumAquamarine     = unqtText "mediumaquamarine"
  unqtDot MediumBlue           = unqtText "mediumblue"
  unqtDot MediumOrchid         = unqtText "mediumorchid"
  unqtDot MediumOrchid1        = unqtText "mediumorchid1"
  unqtDot MediumOrchid2        = unqtText "mediumorchid2"
  unqtDot MediumOrchid3        = unqtText "mediumorchid3"
  unqtDot MediumOrchid4        = unqtText "mediumorchid4"
  unqtDot MediumPurple         = unqtText "mediumpurple"
  unqtDot MediumPurple1        = unqtText "mediumpurple1"
  unqtDot MediumPurple2        = unqtText "mediumpurple2"
  unqtDot MediumPurple3        = unqtText "mediumpurple3"
  unqtDot MediumPurple4        = unqtText "mediumpurple4"
  unqtDot MediumSeaGreen       = unqtText "mediumseagreen"
  unqtDot MediumSlateBlue      = unqtText "mediumslateblue"
  unqtDot MediumSpringGreen    = unqtText "mediumspringgreen"
  unqtDot MediumTurquoise      = unqtText "mediumturquoise"
  unqtDot MediumVioletRed      = unqtText "mediumvioletred"
  unqtDot MidnightBlue         = unqtText "midnightblue"
  unqtDot MintCream            = unqtText "mintcream"
  unqtDot MistyRose            = unqtText "mistyrose"
  unqtDot MistyRose1           = unqtText "mistyrose1"
  unqtDot MistyRose2           = unqtText "mistyrose2"
  unqtDot MistyRose3           = unqtText "mistyrose3"
  unqtDot MistyRose4           = unqtText "mistyrose4"
  unqtDot Moccasin             = unqtText "moccasin"
  unqtDot NavajoWhite          = unqtText "navajowhite"
  unqtDot NavajoWhite1         = unqtText "navajowhite1"
  unqtDot NavajoWhite2         = unqtText "navajowhite2"
  unqtDot NavajoWhite3         = unqtText "navajowhite3"
  unqtDot NavajoWhite4         = unqtText "navajowhite4"
  unqtDot Navy                 = unqtText "navy"
  unqtDot NavyBlue             = unqtText "navyblue"
  unqtDot OldLace              = unqtText "oldlace"
  unqtDot OliveDrab            = unqtText "olivedrab"
  unqtDot OliveDrab1           = unqtText "olivedrab1"
  unqtDot OliveDrab2           = unqtText "olivedrab2"
  unqtDot OliveDrab3           = unqtText "olivedrab3"
  unqtDot OliveDrab4           = unqtText "olivedrab4"
  unqtDot Orange               = unqtText "orange"
  unqtDot Orange1              = unqtText "orange1"
  unqtDot Orange2              = unqtText "orange2"
  unqtDot Orange3              = unqtText "orange3"
  unqtDot Orange4              = unqtText "orange4"
  unqtDot OrangeRed            = unqtText "orangered"
  unqtDot OrangeRed1           = unqtText "orangered1"
  unqtDot OrangeRed2           = unqtText "orangered2"
  unqtDot OrangeRed3           = unqtText "orangered3"
  unqtDot OrangeRed4           = unqtText "orangered4"
  unqtDot Orchid               = unqtText "orchid"
  unqtDot Orchid1              = unqtText "orchid1"
  unqtDot Orchid2              = unqtText "orchid2"
  unqtDot Orchid3              = unqtText "orchid3"
  unqtDot Orchid4              = unqtText "orchid4"
  unqtDot PaleGoldenrod        = unqtText "palegoldenrod"
  unqtDot PaleGreen            = unqtText "palegreen"
  unqtDot PaleGreen1           = unqtText "palegreen1"
  unqtDot PaleGreen2           = unqtText "palegreen2"
  unqtDot PaleGreen3           = unqtText "palegreen3"
  unqtDot PaleGreen4           = unqtText "palegreen4"
  unqtDot PaleTurquoise        = unqtText "paleturquoise"
  unqtDot PaleTurquoise1       = unqtText "paleturquoise1"
  unqtDot PaleTurquoise2       = unqtText "paleturquoise2"
  unqtDot PaleTurquoise3       = unqtText "paleturquoise3"
  unqtDot PaleTurquoise4       = unqtText "paleturquoise4"
  unqtDot PaleVioletRed        = unqtText "palevioletred"
  unqtDot PaleVioletRed1       = unqtText "palevioletred1"
  unqtDot PaleVioletRed2       = unqtText "palevioletred2"
  unqtDot PaleVioletRed3       = unqtText "palevioletred3"
  unqtDot PaleVioletRed4       = unqtText "palevioletred4"
  unqtDot PapayaWhip           = unqtText "papayawhip"
  unqtDot PeachPuff            = unqtText "peachpuff"
  unqtDot PeachPuff1           = unqtText "peachpuff1"
  unqtDot PeachPuff2           = unqtText "peachpuff2"
  unqtDot PeachPuff3           = unqtText "peachpuff3"
  unqtDot PeachPuff4           = unqtText "peachpuff4"
  unqtDot Peru                 = unqtText "peru"
  unqtDot Pink                 = unqtText "pink"
  unqtDot Pink1                = unqtText "pink1"
  unqtDot Pink2                = unqtText "pink2"
  unqtDot Pink3                = unqtText "pink3"
  unqtDot Pink4                = unqtText "pink4"
  unqtDot Plum                 = unqtText "plum"
  unqtDot Plum1                = unqtText "plum1"
  unqtDot Plum2                = unqtText "plum2"
  unqtDot Plum3                = unqtText "plum3"
  unqtDot Plum4                = unqtText "plum4"
  unqtDot PowderBlue           = unqtText "powderblue"
  unqtDot Purple               = unqtText "purple"
  unqtDot Purple1              = unqtText "purple1"
  unqtDot Purple2              = unqtText "purple2"
  unqtDot Purple3              = unqtText "purple3"
  unqtDot Purple4              = unqtText "purple4"
  unqtDot Red                  = unqtText "red"
  unqtDot Red1                 = unqtText "red1"
  unqtDot Red2                 = unqtText "red2"
  unqtDot Red3                 = unqtText "red3"
  unqtDot Red4                 = unqtText "red4"
  unqtDot RosyBrown            = unqtText "rosybrown"
  unqtDot RosyBrown1           = unqtText "rosybrown1"
  unqtDot RosyBrown2           = unqtText "rosybrown2"
  unqtDot RosyBrown3           = unqtText "rosybrown3"
  unqtDot RosyBrown4           = unqtText "rosybrown4"
  unqtDot RoyalBlue            = unqtText "royalblue"
  unqtDot RoyalBlue1           = unqtText "royalblue1"
  unqtDot RoyalBlue2           = unqtText "royalblue2"
  unqtDot RoyalBlue3           = unqtText "royalblue3"
  unqtDot RoyalBlue4           = unqtText "royalblue4"
  unqtDot SaddleBrown          = unqtText "saddlebrown"
  unqtDot Salmon               = unqtText "salmon"
  unqtDot Salmon1              = unqtText "salmon1"
  unqtDot Salmon2              = unqtText "salmon2"
  unqtDot Salmon3              = unqtText "salmon3"
  unqtDot Salmon4              = unqtText "salmon4"
  unqtDot SandyBrown           = unqtText "sandybrown"
  unqtDot SeaGreen             = unqtText "seagreen"
  unqtDot SeaGreen1            = unqtText "seagreen1"
  unqtDot SeaGreen2            = unqtText "seagreen2"
  unqtDot SeaGreen3            = unqtText "seagreen3"
  unqtDot SeaGreen4            = unqtText "seagreen4"
  unqtDot SeaShell             = unqtText "seashell"
  unqtDot SeaShell1            = unqtText "seashell1"
  unqtDot SeaShell2            = unqtText "seashell2"
  unqtDot SeaShell3            = unqtText "seashell3"
  unqtDot SeaShell4            = unqtText "seashell4"
  unqtDot Sienna               = unqtText "sienna"
  unqtDot Sienna1              = unqtText "sienna1"
  unqtDot Sienna2              = unqtText "sienna2"
  unqtDot Sienna3              = unqtText "sienna3"
  unqtDot Sienna4              = unqtText "sienna4"
  unqtDot SkyBlue              = unqtText "skyblue"
  unqtDot SkyBlue1             = unqtText "skyblue1"
  unqtDot SkyBlue2             = unqtText "skyblue2"
  unqtDot SkyBlue3             = unqtText "skyblue3"
  unqtDot SkyBlue4             = unqtText "skyblue4"
  unqtDot SlateBlue            = unqtText "slateblue"
  unqtDot SlateBlue1           = unqtText "slateblue1"
  unqtDot SlateBlue2           = unqtText "slateblue2"
  unqtDot SlateBlue3           = unqtText "slateblue3"
  unqtDot SlateBlue4           = unqtText "slateblue4"
  unqtDot SlateGray            = unqtText "slategray"
  unqtDot SlateGray1           = unqtText "slategray1"
  unqtDot SlateGray2           = unqtText "slategray2"
  unqtDot SlateGray3           = unqtText "slategray3"
  unqtDot SlateGray4           = unqtText "slategray4"
  unqtDot Snow                 = unqtText "snow"
  unqtDot Snow1                = unqtText "snow1"
  unqtDot Snow2                = unqtText "snow2"
  unqtDot Snow3                = unqtText "snow3"
  unqtDot Snow4                = unqtText "snow4"
  unqtDot SpringGreen          = unqtText "springgreen"
  unqtDot SpringGreen1         = unqtText "springgreen1"
  unqtDot SpringGreen2         = unqtText "springgreen2"
  unqtDot SpringGreen3         = unqtText "springgreen3"
  unqtDot SpringGreen4         = unqtText "springgreen4"
  unqtDot SteelBlue            = unqtText "steelblue"
  unqtDot SteelBlue1           = unqtText "steelblue1"
  unqtDot SteelBlue2           = unqtText "steelblue2"
  unqtDot SteelBlue3           = unqtText "steelblue3"
  unqtDot SteelBlue4           = unqtText "steelblue4"
  unqtDot Tan                  = unqtText "tan"
  unqtDot Tan1                 = unqtText "tan1"
  unqtDot Tan2                 = unqtText "tan2"
  unqtDot Tan3                 = unqtText "tan3"
  unqtDot Tan4                 = unqtText "tan4"
  unqtDot Thistle              = unqtText "thistle"
  unqtDot Thistle1             = unqtText "thistle1"
  unqtDot Thistle2             = unqtText "thistle2"
  unqtDot Thistle3             = unqtText "thistle3"
  unqtDot Thistle4             = unqtText "thistle4"
  unqtDot Tomato               = unqtText "tomato"
  unqtDot Tomato1              = unqtText "tomato1"
  unqtDot Tomato2              = unqtText "tomato2"
  unqtDot Tomato3              = unqtText "tomato3"
  unqtDot Tomato4              = unqtText "tomato4"
  unqtDot Transparent          = unqtText "transparent"
  unqtDot Turquoise            = unqtText "turquoise"
  unqtDot Turquoise1           = unqtText "turquoise1"
  unqtDot Turquoise2           = unqtText "turquoise2"
  unqtDot Turquoise3           = unqtText "turquoise3"
  unqtDot Turquoise4           = unqtText "turquoise4"
  unqtDot Violet               = unqtText "violet"
  unqtDot VioletRed            = unqtText "violetred"
  unqtDot VioletRed1           = unqtText "violetred1"
  unqtDot VioletRed2           = unqtText "violetred2"
  unqtDot VioletRed3           = unqtText "violetred3"
  unqtDot VioletRed4           = unqtText "violetred4"
  unqtDot Wheat                = unqtText "wheat"
  unqtDot Wheat1               = unqtText "wheat1"
  unqtDot Wheat2               = unqtText "wheat2"
  unqtDot Wheat3               = unqtText "wheat3"
  unqtDot Wheat4               = unqtText "wheat4"
  unqtDot White                = unqtText "white"
  unqtDot WhiteSmoke           = unqtText "whitesmoke"
  unqtDot Yellow               = unqtText "yellow"
  unqtDot Yellow1              = unqtText "yellow1"
  unqtDot Yellow2              = unqtText "yellow2"
  unqtDot Yellow3              = unqtText "yellow3"
  unqtDot Yellow4              = unqtText "yellow4"
  unqtDot YellowGreen          = unqtText "yellowgreen"

instance ParseDot X11Color where
  parseUnqt = stringValue [ ("aliceblue", AliceBlue)
                          , ("antiquewhite", AntiqueWhite)
                          , ("antiquewhite1", AntiqueWhite1)
                          , ("antiquewhite2", AntiqueWhite2)
                          , ("antiquewhite3", AntiqueWhite3)
                          , ("antiquewhite4", AntiqueWhite4)
                          , ("aquamarine", Aquamarine)
                          , ("aquamarine1", Aquamarine1)
                          , ("aquamarine2", Aquamarine2)
                          , ("aquamarine3", Aquamarine3)
                          , ("aquamarine4", Aquamarine4)
                          , ("azure", Azure)
                          , ("azure1", Azure1)
                          , ("azure2", Azure2)
                          , ("azure3", Azure3)
                          , ("azure4", Azure4)
                          , ("beige", Beige)
                          , ("bisque", Bisque)
                          , ("bisque1", Bisque1)
                          , ("bisque2", Bisque2)
                          , ("bisque3", Bisque3)
                          , ("bisque4", Bisque4)
                          , ("black", Black)
                          , ("blanchedalmond", BlanchedAlmond)
                          , ("blue", Blue)
                          , ("blue1", Blue1)
                          , ("blue2", Blue2)
                          , ("blue3", Blue3)
                          , ("blue4", Blue4)
                          , ("blueviolet", BlueViolet)
                          , ("brown", Brown)
                          , ("brown1", Brown1)
                          , ("brown2", Brown2)
                          , ("brown3", Brown3)
                          , ("brown4", Brown4)
                          , ("burlywood", Burlywood)
                          , ("burlywood1", Burlywood1)
                          , ("burlywood2", Burlywood2)
                          , ("burlywood3", Burlywood3)
                          , ("burlywood4", Burlywood4)
                          , ("cadetblue", CadetBlue)
                          , ("cadetblue1", CadetBlue1)
                          , ("cadetblue2", CadetBlue2)
                          , ("cadetblue3", CadetBlue3)
                          , ("cadetblue4", CadetBlue4)
                          , ("chartreuse", Chartreuse)
                          , ("chartreuse1", Chartreuse1)
                          , ("chartreuse2", Chartreuse2)
                          , ("chartreuse3", Chartreuse3)
                          , ("chartreuse4", Chartreuse4)
                          , ("chocolate", Chocolate)
                          , ("chocolate1", Chocolate1)
                          , ("chocolate2", Chocolate2)
                          , ("chocolate3", Chocolate3)
                          , ("chocolate4", Chocolate4)
                          , ("coral", Coral)
                          , ("coral1", Coral1)
                          , ("coral2", Coral2)
                          , ("coral3", Coral3)
                          , ("coral4", Coral4)
                          , ("cornflowerblue", CornFlowerBlue)
                          , ("cornsilk", CornSilk)
                          , ("cornsilk1", CornSilk1)
                          , ("cornsilk2", CornSilk2)
                          , ("cornsilk3", CornSilk3)
                          , ("cornsilk4", CornSilk4)
                          , ("crimson", Crimson)
                          , ("cyan", Cyan)
                          , ("cyan1", Cyan1)
                          , ("cyan2", Cyan2)
                          , ("cyan3", Cyan3)
                          , ("cyan4", Cyan4)
                          , ("darkgoldenrod", DarkGoldenrod)
                          , ("darkgoldenrod1", DarkGoldenrod1)
                          , ("darkgoldenrod2", DarkGoldenrod2)
                          , ("darkgoldenrod3", DarkGoldenrod3)
                          , ("darkgoldenrod4", DarkGoldenrod4)
                          , ("darkgreen", DarkGreen)
                          , ("darkkhaki", Darkkhaki)
                          , ("darkolivegreen", DarkOliveGreen)
                          , ("darkolivegreen1", DarkOliveGreen1)
                          , ("darkolivegreen2", DarkOliveGreen2)
                          , ("darkolivegreen3", DarkOliveGreen3)
                          , ("darkolivegreen4", DarkOliveGreen4)
                          , ("darkorange", DarkOrange)
                          , ("darkorange1", DarkOrange1)
                          , ("darkorange2", DarkOrange2)
                          , ("darkorange3", DarkOrange3)
                          , ("darkorange4", DarkOrange4)
                          , ("darkorchid", DarkOrchid)
                          , ("darkorchid1", DarkOrchid1)
                          , ("darkorchid2", DarkOrchid2)
                          , ("darkorchid3", DarkOrchid3)
                          , ("darkorchid4", DarkOrchid4)
                          , ("darksalmon", DarkSalmon)
                          , ("darkseagreen", DarkSeaGreen)
                          , ("darkseagreen1", DarkSeaGreen1)
                          , ("darkseagreen2", DarkSeaGreen2)
                          , ("darkseagreen3", DarkSeaGreen3)
                          , ("darkseagreen4", DarkSeaGreen4)
                          , ("darkslateblue", DarkSlateBlue)
                          , ("darkslategray", DarkSlateGray)
                          , ("darkslategrey", DarkSlateGray)
                          , ("darkslategray1", DarkSlateGray1)
                          , ("darkslategrey1", DarkSlateGray1)
                          , ("darkslategray2", DarkSlateGray2)
                          , ("darkslategrey2", DarkSlateGray2)
                          , ("darkslategray3", DarkSlateGray3)
                          , ("darkslategrey3", DarkSlateGray3)
                          , ("darkslategray4", DarkSlateGray4)
                          , ("darkslategrey4", DarkSlateGray4)
                          , ("darkturquoise", DarkTurquoise)
                          , ("darkviolet", DarkViolet)
                          , ("deeppink", DeepPink)
                          , ("deeppink1", DeepPink1)
                          , ("deeppink2", DeepPink2)
                          , ("deeppink3", DeepPink3)
                          , ("deeppink4", DeepPink4)
                          , ("deepskyblue", DeepSkyBlue)
                          , ("deepskyblue1", DeepSkyBlue1)
                          , ("deepskyblue2", DeepSkyBlue2)
                          , ("deepskyblue3", DeepSkyBlue3)
                          , ("deepskyblue4", DeepSkyBlue4)
                          , ("dimgray", DimGray)
                          , ("dimgrey", DimGray)
                          , ("dodgerblue", DodgerBlue)
                          , ("dodgerblue1", DodgerBlue1)
                          , ("dodgerblue2", DodgerBlue2)
                          , ("dodgerblue3", DodgerBlue3)
                          , ("dodgerblue4", DodgerBlue4)
                          , ("firebrick", Firebrick)
                          , ("firebrick1", Firebrick1)
                          , ("firebrick2", Firebrick2)
                          , ("firebrick3", Firebrick3)
                          , ("firebrick4", Firebrick4)
                          , ("floralwhite", FloralWhite)
                          , ("forestgreen", ForestGreen)
                          , ("gainsboro", Gainsboro)
                          , ("ghostwhite", GhostWhite)
                          , ("gold", Gold)
                          , ("gold1", Gold1)
                          , ("gold2", Gold2)
                          , ("gold3", Gold3)
                          , ("gold4", Gold4)
                          , ("goldenrod", Goldenrod)
                          , ("goldenrod1", Goldenrod1)
                          , ("goldenrod2", Goldenrod2)
                          , ("goldenrod3", Goldenrod3)
                          , ("goldenrod4", Goldenrod4)
                          , ("gray", Gray)
                          , ("grey", Gray)
                          , ("gray0", Gray0)
                          , ("grey0", Gray0)
                          , ("gray1", Gray1)
                          , ("grey1", Gray1)
                          , ("gray2", Gray2)
                          , ("grey2", Gray2)
                          , ("gray3", Gray3)
                          , ("grey3", Gray3)
                          , ("gray4", Gray4)
                          , ("grey4", Gray4)
                          , ("gray5", Gray5)
                          , ("grey5", Gray5)
                          , ("gray6", Gray6)
                          , ("grey6", Gray6)
                          , ("gray7", Gray7)
                          , ("grey7", Gray7)
                          , ("gray8", Gray8)
                          , ("grey8", Gray8)
                          , ("gray9", Gray9)
                          , ("grey9", Gray9)
                          , ("gray10", Gray10)
                          , ("grey10", Gray10)
                          , ("gray11", Gray11)
                          , ("grey11", Gray11)
                          , ("gray12", Gray12)
                          , ("grey12", Gray12)
                          , ("gray13", Gray13)
                          , ("grey13", Gray13)
                          , ("gray14", Gray14)
                          , ("grey14", Gray14)
                          , ("gray15", Gray15)
                          , ("grey15", Gray15)
                          , ("gray16", Gray16)
                          , ("grey16", Gray16)
                          , ("gray17", Gray17)
                          , ("grey17", Gray17)
                          , ("gray18", Gray18)
                          , ("grey18", Gray18)
                          , ("gray19", Gray19)
                          , ("grey19", Gray19)
                          , ("gray20", Gray20)
                          , ("grey20", Gray20)
                          , ("gray21", Gray21)
                          , ("grey21", Gray21)
                          , ("gray22", Gray22)
                          , ("grey22", Gray22)
                          , ("gray23", Gray23)
                          , ("grey23", Gray23)
                          , ("gray24", Gray24)
                          , ("grey24", Gray24)
                          , ("gray25", Gray25)
                          , ("grey25", Gray25)
                          , ("gray26", Gray26)
                          , ("grey26", Gray26)
                          , ("gray27", Gray27)
                          , ("grey27", Gray27)
                          , ("gray28", Gray28)
                          , ("grey28", Gray28)
                          , ("gray29", Gray29)
                          , ("grey29", Gray29)
                          , ("gray30", Gray30)
                          , ("grey30", Gray30)
                          , ("gray31", Gray31)
                          , ("grey31", Gray31)
                          , ("gray32", Gray32)
                          , ("grey32", Gray32)
                          , ("gray33", Gray33)
                          , ("grey33", Gray33)
                          , ("gray34", Gray34)
                          , ("grey34", Gray34)
                          , ("gray35", Gray35)
                          , ("grey35", Gray35)
                          , ("gray36", Gray36)
                          , ("grey36", Gray36)
                          , ("gray37", Gray37)
                          , ("grey37", Gray37)
                          , ("gray38", Gray38)
                          , ("grey38", Gray38)
                          , ("gray39", Gray39)
                          , ("grey39", Gray39)
                          , ("gray40", Gray40)
                          , ("grey40", Gray40)
                          , ("gray41", Gray41)
                          , ("grey41", Gray41)
                          , ("gray42", Gray42)
                          , ("grey42", Gray42)
                          , ("gray43", Gray43)
                          , ("grey43", Gray43)
                          , ("gray44", Gray44)
                          , ("grey44", Gray44)
                          , ("gray45", Gray45)
                          , ("grey45", Gray45)
                          , ("gray46", Gray46)
                          , ("grey46", Gray46)
                          , ("gray47", Gray47)
                          , ("grey47", Gray47)
                          , ("gray48", Gray48)
                          , ("grey48", Gray48)
                          , ("gray49", Gray49)
                          , ("grey49", Gray49)
                          , ("gray50", Gray50)
                          , ("grey50", Gray50)
                          , ("gray51", Gray51)
                          , ("grey51", Gray51)
                          , ("gray52", Gray52)
                          , ("grey52", Gray52)
                          , ("gray53", Gray53)
                          , ("grey53", Gray53)
                          , ("gray54", Gray54)
                          , ("grey54", Gray54)
                          , ("gray55", Gray55)
                          , ("grey55", Gray55)
                          , ("gray56", Gray56)
                          , ("grey56", Gray56)
                          , ("gray57", Gray57)
                          , ("grey57", Gray57)
                          , ("gray58", Gray58)
                          , ("grey58", Gray58)
                          , ("gray59", Gray59)
                          , ("grey59", Gray59)
                          , ("gray60", Gray60)
                          , ("grey60", Gray60)
                          , ("gray61", Gray61)
                          , ("grey61", Gray61)
                          , ("gray62", Gray62)
                          , ("grey62", Gray62)
                          , ("gray63", Gray63)
                          , ("grey63", Gray63)
                          , ("gray64", Gray64)
                          , ("grey64", Gray64)
                          , ("gray65", Gray65)
                          , ("grey65", Gray65)
                          , ("gray66", Gray66)
                          , ("grey66", Gray66)
                          , ("gray67", Gray67)
                          , ("grey67", Gray67)
                          , ("gray68", Gray68)
                          , ("grey68", Gray68)
                          , ("gray69", Gray69)
                          , ("grey69", Gray69)
                          , ("gray70", Gray70)
                          , ("grey70", Gray70)
                          , ("gray71", Gray71)
                          , ("grey71", Gray71)
                          , ("gray72", Gray72)
                          , ("grey72", Gray72)
                          , ("gray73", Gray73)
                          , ("grey73", Gray73)
                          , ("gray74", Gray74)
                          , ("grey74", Gray74)
                          , ("gray75", Gray75)
                          , ("grey75", Gray75)
                          , ("gray76", Gray76)
                          , ("grey76", Gray76)
                          , ("gray77", Gray77)
                          , ("grey77", Gray77)
                          , ("gray78", Gray78)
                          , ("grey78", Gray78)
                          , ("gray79", Gray79)
                          , ("grey79", Gray79)
                          , ("gray80", Gray80)
                          , ("grey80", Gray80)
                          , ("gray81", Gray81)
                          , ("grey81", Gray81)
                          , ("gray82", Gray82)
                          , ("grey82", Gray82)
                          , ("gray83", Gray83)
                          , ("grey83", Gray83)
                          , ("gray84", Gray84)
                          , ("grey84", Gray84)
                          , ("gray85", Gray85)
                          , ("grey85", Gray85)
                          , ("gray86", Gray86)
                          , ("grey86", Gray86)
                          , ("gray87", Gray87)
                          , ("grey87", Gray87)
                          , ("gray88", Gray88)
                          , ("grey88", Gray88)
                          , ("gray89", Gray89)
                          , ("grey89", Gray89)
                          , ("gray90", Gray90)
                          , ("grey90", Gray90)
                          , ("gray91", Gray91)
                          , ("grey91", Gray91)
                          , ("gray92", Gray92)
                          , ("grey92", Gray92)
                          , ("gray93", Gray93)
                          , ("grey93", Gray93)
                          , ("gray94", Gray94)
                          , ("grey94", Gray94)
                          , ("gray95", Gray95)
                          , ("grey95", Gray95)
                          , ("gray96", Gray96)
                          , ("grey96", Gray96)
                          , ("gray97", Gray97)
                          , ("grey97", Gray97)
                          , ("gray98", Gray98)
                          , ("grey98", Gray98)
                          , ("gray99", Gray99)
                          , ("grey99", Gray99)
                          , ("gray100", Gray100)
                          , ("grey100", Gray100)
                          , ("green", Green)
                          , ("green1", Green1)
                          , ("green2", Green2)
                          , ("green3", Green3)
                          , ("green4", Green4)
                          , ("greenyellow", GreenYellow)
                          , ("honeydew", HoneyDew)
                          , ("honeydew1", HoneyDew1)
                          , ("honeydew2", HoneyDew2)
                          , ("honeydew3", HoneyDew3)
                          , ("honeydew4", HoneyDew4)
                          , ("hotpink", HotPink)
                          , ("hotpink1", HotPink1)
                          , ("hotpink2", HotPink2)
                          , ("hotpink3", HotPink3)
                          , ("hotpink4", HotPink4)
                          , ("indianred", IndianRed)
                          , ("indianred1", IndianRed1)
                          , ("indianred2", IndianRed2)
                          , ("indianred3", IndianRed3)
                          , ("indianred4", IndianRed4)
                          , ("indigo", Indigo)
                          , ("ivory", Ivory)
                          , ("ivory1", Ivory1)
                          , ("ivory2", Ivory2)
                          , ("ivory3", Ivory3)
                          , ("ivory4", Ivory4)
                          , ("khaki", Khaki)
                          , ("khaki1", Khaki1)
                          , ("khaki2", Khaki2)
                          , ("khaki3", Khaki3)
                          , ("khaki4", Khaki4)
                          , ("lavender", Lavender)
                          , ("lavenderblush", LavenderBlush)
                          , ("lavenderblush1", LavenderBlush1)
                          , ("lavenderblush2", LavenderBlush2)
                          , ("lavenderblush3", LavenderBlush3)
                          , ("lavenderblush4", LavenderBlush4)
                          , ("lawngreen", LawnGreen)
                          , ("lemonchiffon", LemonChiffon)
                          , ("lemonchiffon1", LemonChiffon1)
                          , ("lemonchiffon2", LemonChiffon2)
                          , ("lemonchiffon3", LemonChiffon3)
                          , ("lemonchiffon4", LemonChiffon4)
                          , ("lightblue", LightBlue)
                          , ("lightblue1", LightBlue1)
                          , ("lightblue2", LightBlue2)
                          , ("lightblue3", LightBlue3)
                          , ("lightblue4", LightBlue4)
                          , ("lightcoral", LightCoral)
                          , ("lightcyan", LightCyan)
                          , ("lightcyan1", LightCyan1)
                          , ("lightcyan2", LightCyan2)
                          , ("lightcyan3", LightCyan3)
                          , ("lightcyan4", LightCyan4)
                          , ("lightgoldenrod", LightGoldenrod)
                          , ("lightgoldenrod1", LightGoldenrod1)
                          , ("lightgoldenrod2", LightGoldenrod2)
                          , ("lightgoldenrod3", LightGoldenrod3)
                          , ("lightgoldenrod4", LightGoldenrod4)
                          , ("lightgoldenrodyellow", LightGoldenrodYellow)
                          , ("lightgray", LightGray)
                          , ("lightgrey", LightGray)
                          , ("lightpink", LightPink)
                          , ("lightpink1", LightPink1)
                          , ("lightpink2", LightPink2)
                          , ("lightpink3", LightPink3)
                          , ("lightpink4", LightPink4)
                          , ("lightsalmon", LightSalmon)
                          , ("lightsalmon1", LightSalmon1)
                          , ("lightsalmon2", LightSalmon2)
                          , ("lightsalmon3", LightSalmon3)
                          , ("lightsalmon4", LightSalmon4)
                          , ("lightseagreen", LightSeaGreen)
                          , ("lightskyblue", LightSkyBlue)
                          , ("lightskyblue1", LightSkyBlue1)
                          , ("lightskyblue2", LightSkyBlue2)
                          , ("lightskyblue3", LightSkyBlue3)
                          , ("lightskyblue4", LightSkyBlue4)
                          , ("lightslateblue", LightSlateBlue)
                          , ("lightslategray", LightSlateGray)
                          , ("lightslategrey", LightSlateGray)
                          , ("lightsteelblue", LightSteelBlue)
                          , ("lightsteelblue1", LightSteelBlue1)
                          , ("lightsteelblue2", LightSteelBlue2)
                          , ("lightsteelblue3", LightSteelBlue3)
                          , ("lightsteelblue4", LightSteelBlue4)
                          , ("lightyellow", LightYellow)
                          , ("lightyellow1", LightYellow1)
                          , ("lightyellow2", LightYellow2)
                          , ("lightyellow3", LightYellow3)
                          , ("lightyellow4", LightYellow4)
                          , ("limegreen", LimeGreen)
                          , ("linen", Linen)
                          , ("magenta", Magenta)
                          , ("magenta1", Magenta1)
                          , ("magenta2", Magenta2)
                          , ("magenta3", Magenta3)
                          , ("magenta4", Magenta4)
                          , ("maroon", Maroon)
                          , ("maroon1", Maroon1)
                          , ("maroon2", Maroon2)
                          , ("maroon3", Maroon3)
                          , ("maroon4", Maroon4)
                          , ("mediumaquamarine", MediumAquamarine)
                          , ("mediumblue", MediumBlue)
                          , ("mediumorchid", MediumOrchid)
                          , ("mediumorchid1", MediumOrchid1)
                          , ("mediumorchid2", MediumOrchid2)
                          , ("mediumorchid3", MediumOrchid3)
                          , ("mediumorchid4", MediumOrchid4)
                          , ("mediumpurple", MediumPurple)
                          , ("mediumpurple1", MediumPurple1)
                          , ("mediumpurple2", MediumPurple2)
                          , ("mediumpurple3", MediumPurple3)
                          , ("mediumpurple4", MediumPurple4)
                          , ("mediumseagreen", MediumSeaGreen)
                          , ("mediumslateblue", MediumSlateBlue)
                          , ("mediumspringgreen", MediumSpringGreen)
                          , ("mediumturquoise", MediumTurquoise)
                          , ("mediumvioletred", MediumVioletRed)
                          , ("midnightblue", MidnightBlue)
                          , ("mintcream", MintCream)
                          , ("mistyrose", MistyRose)
                          , ("mistyrose1", MistyRose1)
                          , ("mistyrose2", MistyRose2)
                          , ("mistyrose3", MistyRose3)
                          , ("mistyrose4", MistyRose4)
                          , ("moccasin", Moccasin)
                          , ("navajowhite", NavajoWhite)
                          , ("navajowhite1", NavajoWhite1)
                          , ("navajowhite2", NavajoWhite2)
                          , ("navajowhite3", NavajoWhite3)
                          , ("navajowhite4", NavajoWhite4)
                          , ("navy", Navy)
                          , ("navyblue", NavyBlue)
                          , ("oldlace", OldLace)
                          , ("olivedrab", OliveDrab)
                          , ("olivedrab1", OliveDrab1)
                          , ("olivedrab2", OliveDrab2)
                          , ("olivedrab3", OliveDrab3)
                          , ("olivedrab4", OliveDrab4)
                          , ("orange", Orange)
                          , ("orange1", Orange1)
                          , ("orange2", Orange2)
                          , ("orange3", Orange3)
                          , ("orange4", Orange4)
                          , ("orangered", OrangeRed)
                          , ("orangered1", OrangeRed1)
                          , ("orangered2", OrangeRed2)
                          , ("orangered3", OrangeRed3)
                          , ("orangered4", OrangeRed4)
                          , ("orchid", Orchid)
                          , ("orchid1", Orchid1)
                          , ("orchid2", Orchid2)
                          , ("orchid3", Orchid3)
                          , ("orchid4", Orchid4)
                          , ("palegoldenrod", PaleGoldenrod)
                          , ("palegreen", PaleGreen)
                          , ("palegreen1", PaleGreen1)
                          , ("palegreen2", PaleGreen2)
                          , ("palegreen3", PaleGreen3)
                          , ("palegreen4", PaleGreen4)
                          , ("paleturquoise", PaleTurquoise)
                          , ("paleturquoise1", PaleTurquoise1)
                          , ("paleturquoise2", PaleTurquoise2)
                          , ("paleturquoise3", PaleTurquoise3)
                          , ("paleturquoise4", PaleTurquoise4)
                          , ("palevioletred", PaleVioletRed)
                          , ("palevioletred1", PaleVioletRed1)
                          , ("palevioletred2", PaleVioletRed2)
                          , ("palevioletred3", PaleVioletRed3)
                          , ("palevioletred4", PaleVioletRed4)
                          , ("papayawhip", PapayaWhip)
                          , ("peachpuff", PeachPuff)
                          , ("peachpuff1", PeachPuff1)
                          , ("peachpuff2", PeachPuff2)
                          , ("peachpuff3", PeachPuff3)
                          , ("peachpuff4", PeachPuff4)
                          , ("peru", Peru)
                          , ("pink", Pink)
                          , ("pink1", Pink1)
                          , ("pink2", Pink2)
                          , ("pink3", Pink3)
                          , ("pink4", Pink4)
                          , ("plum", Plum)
                          , ("plum1", Plum1)
                          , ("plum2", Plum2)
                          , ("plum3", Plum3)
                          , ("plum4", Plum4)
                          , ("powderblue", PowderBlue)
                          , ("purple", Purple)
                          , ("purple1", Purple1)
                          , ("purple2", Purple2)
                          , ("purple3", Purple3)
                          , ("purple4", Purple4)
                          , ("red", Red)
                          , ("red1", Red1)
                          , ("red2", Red2)
                          , ("red3", Red3)
                          , ("red4", Red4)
                          , ("rosybrown", RosyBrown)
                          , ("rosybrown1", RosyBrown1)
                          , ("rosybrown2", RosyBrown2)
                          , ("rosybrown3", RosyBrown3)
                          , ("rosybrown4", RosyBrown4)
                          , ("royalblue", RoyalBlue)
                          , ("royalblue1", RoyalBlue1)
                          , ("royalblue2", RoyalBlue2)
                          , ("royalblue3", RoyalBlue3)
                          , ("royalblue4", RoyalBlue4)
                          , ("saddlebrown", SaddleBrown)
                          , ("salmon", Salmon)
                          , ("salmon1", Salmon1)
                          , ("salmon2", Salmon2)
                          , ("salmon3", Salmon3)
                          , ("salmon4", Salmon4)
                          , ("sandybrown", SandyBrown)
                          , ("seagreen", SeaGreen)
                          , ("seagreen1", SeaGreen1)
                          , ("seagreen2", SeaGreen2)
                          , ("seagreen3", SeaGreen3)
                          , ("seagreen4", SeaGreen4)
                          , ("seashell", SeaShell)
                          , ("seashell1", SeaShell1)
                          , ("seashell2", SeaShell2)
                          , ("seashell3", SeaShell3)
                          , ("seashell4", SeaShell4)
                          , ("sienna", Sienna)
                          , ("sienna1", Sienna1)
                          , ("sienna2", Sienna2)
                          , ("sienna3", Sienna3)
                          , ("sienna4", Sienna4)
                          , ("skyblue", SkyBlue)
                          , ("skyblue1", SkyBlue1)
                          , ("skyblue2", SkyBlue2)
                          , ("skyblue3", SkyBlue3)
                          , ("skyblue4", SkyBlue4)
                          , ("slateblue", SlateBlue)
                          , ("slateblue1", SlateBlue1)
                          , ("slateblue2", SlateBlue2)
                          , ("slateblue3", SlateBlue3)
                          , ("slateblue4", SlateBlue4)
                          , ("slategray", SlateGray)
                          , ("slategrey", SlateGray)
                          , ("slategray1", SlateGray1)
                          , ("slategrey1", SlateGray1)
                          , ("slategray2", SlateGray2)
                          , ("slategrey2", SlateGray2)
                          , ("slategray3", SlateGray3)
                          , ("slategrey3", SlateGray3)
                          , ("slategray4", SlateGray4)
                          , ("slategrey4", SlateGray4)
                          , ("snow", Snow)
                          , ("snow1", Snow1)
                          , ("snow2", Snow2)
                          , ("snow3", Snow3)
                          , ("snow4", Snow4)
                          , ("springgreen", SpringGreen)
                          , ("springgreen1", SpringGreen1)
                          , ("springgreen2", SpringGreen2)
                          , ("springgreen3", SpringGreen3)
                          , ("springgreen4", SpringGreen4)
                          , ("steelblue", SteelBlue)
                          , ("steelblue1", SteelBlue1)
                          , ("steelblue2", SteelBlue2)
                          , ("steelblue3", SteelBlue3)
                          , ("steelblue4", SteelBlue4)
                          , ("tan", Tan)
                          , ("tan1", Tan1)
                          , ("tan2", Tan2)
                          , ("tan3", Tan3)
                          , ("tan4", Tan4)
                          , ("thistle", Thistle)
                          , ("thistle1", Thistle1)
                          , ("thistle2", Thistle2)
                          , ("thistle3", Thistle3)
                          , ("thistle4", Thistle4)
                          , ("tomato", Tomato)
                          , ("tomato1", Tomato1)
                          , ("tomato2", Tomato2)
                          , ("tomato3", Tomato3)
                          , ("tomato4", Tomato4)
                          , ("transparent", Transparent)
                          , ("invis", Transparent)
                          , ("none", Transparent)
                          , ("turquoise", Turquoise)
                          , ("turquoise1", Turquoise1)
                          , ("turquoise2", Turquoise2)
                          , ("turquoise3", Turquoise3)
                          , ("turquoise4", Turquoise4)
                          , ("violet", Violet)
                          , ("violetred", VioletRed)
                          , ("violetred1", VioletRed1)
                          , ("violetred2", VioletRed2)
                          , ("violetred3", VioletRed3)
                          , ("violetred4", VioletRed4)
                          , ("wheat", Wheat)
                          , ("wheat1", Wheat1)
                          , ("wheat2", Wheat2)
                          , ("wheat3", Wheat3)
                          , ("wheat4", Wheat4)
                          , ("white", White)
                          , ("whitesmoke", WhiteSmoke)
                          , ("yellow", Yellow)
                          , ("yellow1", Yellow1)
                          , ("yellow2", Yellow2)
                          , ("yellow3", Yellow3)
                          , ("yellow4", Yellow4)
                          , ("yellowgreen", YellowGreen)
                          ]

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
