{-# LANGUAGE OverloadedStrings #-}

{- |
   Module      : Data.GraphViz.Attributes.Colors.SVG
   Description : Specification of SVG colors.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   Graphviz comes with an SVG color scheme:
   <http://www.graphviz.org/doc/info/colors.html#svg>

   However, in general use you probably want to use
   "Data.GraphViz.Attributes.Colors.X11" instead, unless you are only
   generating SVG images.

 -}
module Data.GraphViz.Attributes.Colors.SVG
    ( SVGColor(..)
    , svgColour
    ) where

import Data.GraphViz.Parsing
import Data.GraphViz.Printing

import Data.Colour(Colour)
import Data.Colour.SRGB(sRGB24)

-- -----------------------------------------------------------------------------

-- | The SVG colors that Graphviz uses.  Graphviz's list of colors
--   also duplicated all @*Gray*@ colors with @*Grey*@ ones; parsing
--   of an 'SVGColor' which is specified using \"grey\" will succeed.
data SVGColor = AliceBlue
              | AntiqueWhite
              | Aqua
              | Aquamarine
              | Azure
              | Beige
              | Bisque
              | Black
              | BlanchedAlmond
              | Blue
              | BlueViolet
              | Brown
              | Burlywood
              | CadetBlue
              | Chartreuse
              | Chocolate
              | Coral
              | CornflowerBlue
              | Cornsilk
              | Crimson
              | Cyan
              | DarkBlue
              | DarkCyan
              | DarkGoldenrod
              | DarkGray
              | DarkGreen
              | DarkKhaki
              | DarkMagenta
              | DarkOliveGreen
              | DarkOrange
              | DarkOrchid
              | DarkRed
              | DarkSalmon
              | DarkSeaGreen
              | DarkSlateBlue
              | DarkSlateGray
              | DarkTurquoise
              | DarkViolet
              | DeepPink
              | DeepSkyBlue
              | DimGray
              | DodgerBlue
              | Firebrick
              | FloralWhite
              | ForestGreen
              | Fuchsia
              | Gainsboro
              | GhostWhite
              | Gold
              | Goldenrod
              | Gray
              | Green
              | GreenYellow
              | Honeydew
              | HotPink
              | IndianRed
              | Indigo
              | Ivory
              | Khaki
              | Lavender
              | LavenderBlush
              | LawnGreen
              | LemonChiffon
              | LightBlue
              | LightCoral
              | LightCyan
              | LightGoldenrodYellow
              | LightGray
              | LightGreen
              | LightPink
              | LightSalmon
              | LightSeaGreen
              | LightSkyBlue
              | LightSlateGray
              | LightSteelBlue
              | LightYellow
              | Lime
              | LimeGreen
              | Linen
              | Magenta
              | Maroon
              | MediumAquamarine
              | MediumBlue
              | MediumOrchid
              | MediumPurple
              | MediumSeaGreen
              | MediumSlateBlue
              | MediumSpringGreen
              | MediumTurquoise
              | MediumVioletRed
              | MidnightBlue
              | MintCream
              | MistyRose
              | Moccasin
              | NavajoWhite
              | Navy
              | OldLace
              | Olive
              | OliveDrab
              | Orange
              | OrangeRed
              | Orchid
              | PaleGoldenrod
              | PaleGreen
              | PaleTurquoise
              | PaleVioletRed
              | PapayaWhip
              | PeachPuff
              | Peru
              | Pink
              | Plum
              | PowderBlue
              | Purple
              | Red
              | RosyBrown
              | RoyalBlue
              | SaddleBrown
              | Salmon
              | SandyBrown
              | SeaGreen
              | SeaShell
              | Sienna
              | Silver
              | SkyBlue
              | SlateBlue
              | SlateGray
              | Snow
              | SpringGreen
              | SteelBlue
              | Tan
              | Teal
              | Thistle
              | Tomato
              | Turquoise
              | Violet
              | Wheat
              | White
              | WhiteSmoke
              | Yellow
              | YellowGreen
              deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot SVGColor where
  unqtDot AliceBlue            = unqtText "aliceblue"
  unqtDot AntiqueWhite         = unqtText "antiquewhite"
  unqtDot Aqua                 = unqtText "aqua"
  unqtDot Aquamarine           = unqtText "aquamarine"
  unqtDot Azure                = unqtText "azure"
  unqtDot Beige                = unqtText "beige"
  unqtDot Bisque               = unqtText "bisque"
  unqtDot Black                = unqtText "black"
  unqtDot BlanchedAlmond       = unqtText "blanchedalmond"
  unqtDot Blue                 = unqtText "blue"
  unqtDot BlueViolet           = unqtText "blueviolet"
  unqtDot Brown                = unqtText "brown"
  unqtDot Burlywood            = unqtText "burlywood"
  unqtDot CadetBlue            = unqtText "cadetblue"
  unqtDot Chartreuse           = unqtText "chartreuse"
  unqtDot Chocolate            = unqtText "chocolate"
  unqtDot Coral                = unqtText "coral"
  unqtDot CornflowerBlue       = unqtText "cornflowerblue"
  unqtDot Cornsilk             = unqtText "cornsilk"
  unqtDot Crimson              = unqtText "crimson"
  unqtDot Cyan                 = unqtText "cyan"
  unqtDot DarkBlue             = unqtText "darkblue"
  unqtDot DarkCyan             = unqtText "darkcyan"
  unqtDot DarkGoldenrod        = unqtText "darkgoldenrod"
  unqtDot DarkGray             = unqtText "darkgray"
  unqtDot DarkGreen            = unqtText "darkgreen"
  unqtDot DarkKhaki            = unqtText "darkkhaki"
  unqtDot DarkMagenta          = unqtText "darkmagenta"
  unqtDot DarkOliveGreen       = unqtText "darkolivegreen"
  unqtDot DarkOrange           = unqtText "darkorange"
  unqtDot DarkOrchid           = unqtText "darkorchid"
  unqtDot DarkRed              = unqtText "darkred"
  unqtDot DarkSalmon           = unqtText "darksalmon"
  unqtDot DarkSeaGreen         = unqtText "darkseagreen"
  unqtDot DarkSlateBlue        = unqtText "darkslateblue"
  unqtDot DarkSlateGray        = unqtText "darkslategray"
  unqtDot DarkTurquoise        = unqtText "darkturquoise"
  unqtDot DarkViolet           = unqtText "darkviolet"
  unqtDot DeepPink             = unqtText "deeppink"
  unqtDot DeepSkyBlue          = unqtText "deepskyblue"
  unqtDot DimGray              = unqtText "dimgray"
  unqtDot DodgerBlue           = unqtText "dodgerblue"
  unqtDot Firebrick            = unqtText "firebrick"
  unqtDot FloralWhite          = unqtText "floralwhite"
  unqtDot ForestGreen          = unqtText "forestgreen"
  unqtDot Fuchsia              = unqtText "fuchsia"
  unqtDot Gainsboro            = unqtText "gainsboro"
  unqtDot GhostWhite           = unqtText "ghostwhite"
  unqtDot Gold                 = unqtText "gold"
  unqtDot Goldenrod            = unqtText "goldenrod"
  unqtDot Gray                 = unqtText "gray"
  unqtDot Green                = unqtText "green"
  unqtDot GreenYellow          = unqtText "greenyellow"
  unqtDot Honeydew             = unqtText "honeydew"
  unqtDot HotPink              = unqtText "hotpink"
  unqtDot IndianRed            = unqtText "indianred"
  unqtDot Indigo               = unqtText "indigo"
  unqtDot Ivory                = unqtText "ivory"
  unqtDot Khaki                = unqtText "khaki"
  unqtDot Lavender             = unqtText "lavender"
  unqtDot LavenderBlush        = unqtText "lavenderblush"
  unqtDot LawnGreen            = unqtText "lawngreen"
  unqtDot LemonChiffon         = unqtText "lemonchiffon"
  unqtDot LightBlue            = unqtText "lightblue"
  unqtDot LightCoral           = unqtText "lightcoral"
  unqtDot LightCyan            = unqtText "lightcyan"
  unqtDot LightGoldenrodYellow = unqtText "lightgoldenrodyellow"
  unqtDot LightGray            = unqtText "lightgray"
  unqtDot LightGreen           = unqtText "lightgreen"
  unqtDot LightPink            = unqtText "lightpink"
  unqtDot LightSalmon          = unqtText "lightsalmon"
  unqtDot LightSeaGreen        = unqtText "lightseagreen"
  unqtDot LightSkyBlue         = unqtText "lightskyblue"
  unqtDot LightSlateGray       = unqtText "lightslategray"
  unqtDot LightSteelBlue       = unqtText "lightsteelblue"
  unqtDot LightYellow          = unqtText "lightyellow"
  unqtDot Lime                 = unqtText "lime"
  unqtDot LimeGreen            = unqtText "limegreen"
  unqtDot Linen                = unqtText "linen"
  unqtDot Magenta              = unqtText "magenta"
  unqtDot Maroon               = unqtText "maroon"
  unqtDot MediumAquamarine     = unqtText "mediumaquamarine"
  unqtDot MediumBlue           = unqtText "mediumblue"
  unqtDot MediumOrchid         = unqtText "mediumorchid"
  unqtDot MediumPurple         = unqtText "mediumpurple"
  unqtDot MediumSeaGreen       = unqtText "mediumseagreen"
  unqtDot MediumSlateBlue      = unqtText "mediumslateblue"
  unqtDot MediumSpringGreen    = unqtText "mediumspringgreen"
  unqtDot MediumTurquoise      = unqtText "mediumturquoise"
  unqtDot MediumVioletRed      = unqtText "mediumvioletred"
  unqtDot MidnightBlue         = unqtText "midnightblue"
  unqtDot MintCream            = unqtText "mintcream"
  unqtDot MistyRose            = unqtText "mistyrose"
  unqtDot Moccasin             = unqtText "moccasin"
  unqtDot NavajoWhite          = unqtText "navajowhite"
  unqtDot Navy                 = unqtText "navy"
  unqtDot OldLace              = unqtText "oldlace"
  unqtDot Olive                = unqtText "olive"
  unqtDot OliveDrab            = unqtText "olivedrab"
  unqtDot Orange               = unqtText "orange"
  unqtDot OrangeRed            = unqtText "orangered"
  unqtDot Orchid               = unqtText "orchid"
  unqtDot PaleGoldenrod        = unqtText "palegoldenrod"
  unqtDot PaleGreen            = unqtText "palegreen"
  unqtDot PaleTurquoise        = unqtText "paleturquoise"
  unqtDot PaleVioletRed        = unqtText "palevioletred"
  unqtDot PapayaWhip           = unqtText "papayawhip"
  unqtDot PeachPuff            = unqtText "peachpuff"
  unqtDot Peru                 = unqtText "peru"
  unqtDot Pink                 = unqtText "pink"
  unqtDot Plum                 = unqtText "plum"
  unqtDot PowderBlue           = unqtText "powderblue"
  unqtDot Purple               = unqtText "purple"
  unqtDot Red                  = unqtText "red"
  unqtDot RosyBrown            = unqtText "rosybrown"
  unqtDot RoyalBlue            = unqtText "royalblue"
  unqtDot SaddleBrown          = unqtText "saddlebrown"
  unqtDot Salmon               = unqtText "salmon"
  unqtDot SandyBrown           = unqtText "sandybrown"
  unqtDot SeaGreen             = unqtText "seagreen"
  unqtDot SeaShell             = unqtText "seashell"
  unqtDot Sienna               = unqtText "sienna"
  unqtDot Silver               = unqtText "silver"
  unqtDot SkyBlue              = unqtText "skyblue"
  unqtDot SlateBlue            = unqtText "slateblue"
  unqtDot SlateGray            = unqtText "slategray"
  unqtDot Snow                 = unqtText "snow"
  unqtDot SpringGreen          = unqtText "springgreen"
  unqtDot SteelBlue            = unqtText "steelblue"
  unqtDot Tan                  = unqtText "tan"
  unqtDot Teal                 = unqtText "teal"
  unqtDot Thistle              = unqtText "thistle"
  unqtDot Tomato               = unqtText "tomato"
  unqtDot Turquoise            = unqtText "turquoise"
  unqtDot Violet               = unqtText "violet"
  unqtDot Wheat                = unqtText "wheat"
  unqtDot White                = unqtText "white"
  unqtDot WhiteSmoke           = unqtText "whitesmoke"
  unqtDot Yellow               = unqtText "yellow"
  unqtDot YellowGreen          = unqtText "yellowgreen"

instance ParseDot SVGColor where
  parseUnqt = stringValue [ ("aliceblue", AliceBlue)
                          , ("antiquewhite", AntiqueWhite)
                          , ("aqua", Aqua)
                          , ("aquamarine", Aquamarine)
                          , ("azure", Azure)
                          , ("beige", Beige)
                          , ("bisque", Bisque)
                          , ("black", Black)
                          , ("blanchedalmond", BlanchedAlmond)
                          , ("blue", Blue)
                          , ("blueviolet", BlueViolet)
                          , ("brown", Brown)
                          , ("burlywood", Burlywood)
                          , ("cadetblue", CadetBlue)
                          , ("chartreuse", Chartreuse)
                          , ("chocolate", Chocolate)
                          , ("coral", Coral)
                          , ("cornflowerblue", CornflowerBlue)
                          , ("cornsilk", Cornsilk)
                          , ("crimson", Crimson)
                          , ("cyan", Cyan)
                          , ("darkblue", DarkBlue)
                          , ("darkcyan", DarkCyan)
                          , ("darkgoldenrod", DarkGoldenrod)
                          , ("darkgray", DarkGray)
                          , ("darkgrey", DarkGray)
                          , ("darkgreen", DarkGreen)
                          , ("darkkhaki", DarkKhaki)
                          , ("darkmagenta", DarkMagenta)
                          , ("darkolivegreen", DarkOliveGreen)
                          , ("darkorange", DarkOrange)
                          , ("darkorchid", DarkOrchid)
                          , ("darkred", DarkRed)
                          , ("darksalmon", DarkSalmon)
                          , ("darkseagreen", DarkSeaGreen)
                          , ("darkslateblue", DarkSlateBlue)
                          , ("darkslategray", DarkSlateGray)
                          , ("darkslategrey", DarkSlateGray)
                          , ("darkturquoise", DarkTurquoise)
                          , ("darkviolet", DarkViolet)
                          , ("deeppink", DeepPink)
                          , ("deepskyblue", DeepSkyBlue)
                          , ("dimgray", DimGray)
                          , ("dimgrey", DimGray)
                          , ("dodgerblue", DodgerBlue)
                          , ("firebrick", Firebrick)
                          , ("floralwhite", FloralWhite)
                          , ("forestgreen", ForestGreen)
                          , ("fuchsia", Fuchsia)
                          , ("gainsboro", Gainsboro)
                          , ("ghostwhite", GhostWhite)
                          , ("gold", Gold)
                          , ("goldenrod", Goldenrod)
                          , ("gray", Gray)
                          , ("grey", Gray)
                          , ("green", Green)
                          , ("greenyellow", GreenYellow)
                          , ("honeydew", Honeydew)
                          , ("hotpink", HotPink)
                          , ("indianred", IndianRed)
                          , ("indigo", Indigo)
                          , ("ivory", Ivory)
                          , ("khaki", Khaki)
                          , ("lavender", Lavender)
                          , ("lavenderblush", LavenderBlush)
                          , ("lawngreen", LawnGreen)
                          , ("lemonchiffon", LemonChiffon)
                          , ("lightblue", LightBlue)
                          , ("lightcoral", LightCoral)
                          , ("lightcyan", LightCyan)
                          , ("lightgoldenrodyellow", LightGoldenrodYellow)
                          , ("lightgray", LightGray)
                          , ("lightgrey", LightGray)
                          , ("lightgreen", LightGreen)
                          , ("lightpink", LightPink)
                          , ("lightsalmon", LightSalmon)
                          , ("lightseagreen", LightSeaGreen)
                          , ("lightskyblue", LightSkyBlue)
                          , ("lightslategray", LightSlateGray)
                          , ("lightslategrey", LightSlateGray)
                          , ("lightsteelblue", LightSteelBlue)
                          , ("lightyellow", LightYellow)
                          , ("lime", Lime)
                          , ("limegreen", LimeGreen)
                          , ("linen", Linen)
                          , ("magenta", Magenta)
                          , ("maroon", Maroon)
                          , ("mediumaquamarine", MediumAquamarine)
                          , ("mediumblue", MediumBlue)
                          , ("mediumorchid", MediumOrchid)
                          , ("mediumpurple", MediumPurple)
                          , ("mediumseagreen", MediumSeaGreen)
                          , ("mediumslateblue", MediumSlateBlue)
                          , ("mediumspringgreen", MediumSpringGreen)
                          , ("mediumturquoise", MediumTurquoise)
                          , ("mediumvioletred", MediumVioletRed)
                          , ("midnightblue", MidnightBlue)
                          , ("mintcream", MintCream)
                          , ("mistyrose", MistyRose)
                          , ("moccasin", Moccasin)
                          , ("navajowhite", NavajoWhite)
                          , ("navy", Navy)
                          , ("oldlace", OldLace)
                          , ("olive", Olive)
                          , ("olivedrab", OliveDrab)
                          , ("orange", Orange)
                          , ("orangered", OrangeRed)
                          , ("orchid", Orchid)
                          , ("palegoldenrod", PaleGoldenrod)
                          , ("palegreen", PaleGreen)
                          , ("paleturquoise", PaleTurquoise)
                          , ("palevioletred", PaleVioletRed)
                          , ("papayawhip", PapayaWhip)
                          , ("peachpuff", PeachPuff)
                          , ("peru", Peru)
                          , ("pink", Pink)
                          , ("plum", Plum)
                          , ("powderblue", PowderBlue)
                          , ("purple", Purple)
                          , ("red", Red)
                          , ("rosybrown", RosyBrown)
                          , ("royalblue", RoyalBlue)
                          , ("saddlebrown", SaddleBrown)
                          , ("salmon", Salmon)
                          , ("sandybrown", SandyBrown)
                          , ("seagreen", SeaGreen)
                          , ("seashell", SeaShell)
                          , ("sienna", Sienna)
                          , ("silver", Silver)
                          , ("skyblue", SkyBlue)
                          , ("slateblue", SlateBlue)
                          , ("slategray", SlateGray)
                          , ("slategrey", SlateGray)
                          , ("snow", Snow)
                          , ("springgreen", SpringGreen)
                          , ("steelblue", SteelBlue)
                          , ("tan", Tan)
                          , ("teal", Teal)
                          , ("thistle", Thistle)
                          , ("tomato", Tomato)
                          , ("turquoise", Turquoise)
                          , ("violet", Violet)
                          , ("wheat", Wheat)
                          , ("white", White)
                          , ("whitesmoke", WhiteSmoke)
                          , ("yellow", Yellow)
                          , ("yellowgreen", YellowGreen)
                          ]

-- | Convert an 'SVGColor' to its equivalent 'Colour' value.
svgColour                      :: SVGColor -> Colour Double
svgColour AliceBlue            = sRGB24 240 248 255
svgColour AntiqueWhite         = sRGB24 250 235 215
svgColour Aqua                 = sRGB24 0   255 255
svgColour Aquamarine           = sRGB24 127 255 212
svgColour Azure                = sRGB24 240 255 255
svgColour Beige                = sRGB24 245 245 220
svgColour Bisque               = sRGB24 255 228 196
svgColour Black                = sRGB24 0   0   0
svgColour BlanchedAlmond       = sRGB24 255 235 205
svgColour Blue                 = sRGB24 0   0   255
svgColour BlueViolet           = sRGB24 138 43  226
svgColour Brown                = sRGB24 165 42  42
svgColour Burlywood            = sRGB24 222 184 135
svgColour CadetBlue            = sRGB24 95  158 160
svgColour Chartreuse           = sRGB24 127 255 0
svgColour Chocolate            = sRGB24 210 105 30
svgColour Coral                = sRGB24 255 127 80
svgColour CornflowerBlue       = sRGB24 100 149 237
svgColour Cornsilk             = sRGB24 255 248 220
svgColour Crimson              = sRGB24 220 20  60
svgColour Cyan                 = sRGB24 0   255 255
svgColour DarkBlue             = sRGB24 0   0   139
svgColour DarkCyan             = sRGB24 0   139 139
svgColour DarkGoldenrod        = sRGB24 184 134 11
svgColour DarkGray             = sRGB24 169 169 169
svgColour DarkGreen            = sRGB24 0   100 0
svgColour DarkKhaki            = sRGB24 189 183 107
svgColour DarkMagenta          = sRGB24 139 0   139
svgColour DarkOliveGreen       = sRGB24 85  107 47
svgColour DarkOrange           = sRGB24 255 140 0
svgColour DarkOrchid           = sRGB24 153 50  204
svgColour DarkRed              = sRGB24 139 0   0
svgColour DarkSalmon           = sRGB24 233 150 122
svgColour DarkSeaGreen         = sRGB24 143 188 143
svgColour DarkSlateBlue        = sRGB24 72  61  139
svgColour DarkSlateGray        = sRGB24 47  79  79
svgColour DarkTurquoise        = sRGB24 0   206 209
svgColour DarkViolet           = sRGB24 148 0   211
svgColour DeepPink             = sRGB24 255 20  147
svgColour DeepSkyBlue          = sRGB24 0   191 255
svgColour DimGray              = sRGB24 105 105 105
svgColour DodgerBlue           = sRGB24 30  144 255
svgColour Firebrick            = sRGB24 178 34  34
svgColour FloralWhite          = sRGB24 255 250 240
svgColour ForestGreen          = sRGB24 34  139 34
svgColour Fuchsia              = sRGB24 255 0   255
svgColour Gainsboro            = sRGB24 220 220 220
svgColour GhostWhite           = sRGB24 248 248 255
svgColour Gold                 = sRGB24 255 215 0
svgColour Goldenrod            = sRGB24 218 165 32
svgColour Gray                 = sRGB24 128 128 128
svgColour Green                = sRGB24 0   128 0
svgColour GreenYellow          = sRGB24 173 255 47
svgColour Honeydew             = sRGB24 240 255 240
svgColour HotPink              = sRGB24 255 105 180
svgColour IndianRed            = sRGB24 205 92  92
svgColour Indigo               = sRGB24 75  0   130
svgColour Ivory                = sRGB24 255 255 240
svgColour Khaki                = sRGB24 240 230 140
svgColour Lavender             = sRGB24 230 230 250
svgColour LavenderBlush        = sRGB24 255 240 245
svgColour LawnGreen            = sRGB24 124 252 0
svgColour LemonChiffon         = sRGB24 255 250 205
svgColour LightBlue            = sRGB24 173 216 230
svgColour LightCoral           = sRGB24 240 128 128
svgColour LightCyan            = sRGB24 224 255 255
svgColour LightGoldenrodYellow = sRGB24 250 250 210
svgColour LightGray            = sRGB24 211 211 211
svgColour LightGreen           = sRGB24 144 238 144
svgColour LightPink            = sRGB24 255 182 193
svgColour LightSalmon          = sRGB24 255 160 122
svgColour LightSeaGreen        = sRGB24 32  178 170
svgColour LightSkyBlue         = sRGB24 135 206 250
svgColour LightSlateGray       = sRGB24 119 136 153
svgColour LightSteelBlue       = sRGB24 176 196 222
svgColour LightYellow          = sRGB24 255 255 224
svgColour Lime                 = sRGB24 0   255 0
svgColour LimeGreen            = sRGB24 50  205 50
svgColour Linen                = sRGB24 250 240 230
svgColour Magenta              = sRGB24 255 0   255
svgColour Maroon               = sRGB24 128 0   0
svgColour MediumAquamarine     = sRGB24 102 205 170
svgColour MediumBlue           = sRGB24 0   0   205
svgColour MediumOrchid         = sRGB24 186 85  211
svgColour MediumPurple         = sRGB24 147 112 219
svgColour MediumSeaGreen       = sRGB24 60  179 113
svgColour MediumSlateBlue      = sRGB24 123 104 238
svgColour MediumSpringGreen    = sRGB24 0   250 154
svgColour MediumTurquoise      = sRGB24 72  209 204
svgColour MediumVioletRed      = sRGB24 199 21  133
svgColour MidnightBlue         = sRGB24 25  25  112
svgColour MintCream            = sRGB24 245 255 250
svgColour MistyRose            = sRGB24 255 228 225
svgColour Moccasin             = sRGB24 255 228 181
svgColour NavajoWhite          = sRGB24 255 222 173
svgColour Navy                 = sRGB24 0   0   128
svgColour OldLace              = sRGB24 253 245 230
svgColour Olive                = sRGB24 128 128 0
svgColour OliveDrab            = sRGB24 107 142 35
svgColour Orange               = sRGB24 255 165 0
svgColour OrangeRed            = sRGB24 255 69  0
svgColour Orchid               = sRGB24 218 112 214
svgColour PaleGoldenrod        = sRGB24 238 232 170
svgColour PaleGreen            = sRGB24 152 251 152
svgColour PaleTurquoise        = sRGB24 175 238 238
svgColour PaleVioletRed        = sRGB24 219 112 147
svgColour PapayaWhip           = sRGB24 255 239 213
svgColour PeachPuff            = sRGB24 255 218 185
svgColour Peru                 = sRGB24 205 133 63
svgColour Pink                 = sRGB24 255 192 203
svgColour Plum                 = sRGB24 221 160 221
svgColour PowderBlue           = sRGB24 176 224 230
svgColour Purple               = sRGB24 128 0   128
svgColour Red                  = sRGB24 255 0   0
svgColour RosyBrown            = sRGB24 188 143 143
svgColour RoyalBlue            = sRGB24 65  105 225
svgColour SaddleBrown          = sRGB24 139 69  19
svgColour Salmon               = sRGB24 250 128 114
svgColour SandyBrown           = sRGB24 244 164 96
svgColour SeaGreen             = sRGB24 46  139 87
svgColour SeaShell             = sRGB24 255 245 238
svgColour Sienna               = sRGB24 160 82  45
svgColour Silver               = sRGB24 192 192 192
svgColour SkyBlue              = sRGB24 135 206 235
svgColour SlateBlue            = sRGB24 106 90  205
svgColour SlateGray            = sRGB24 112 128 144
svgColour Snow                 = sRGB24 255 250 250
svgColour SpringGreen          = sRGB24 0   255 127
svgColour SteelBlue            = sRGB24 70  130 180
svgColour Tan                  = sRGB24 210 180 140
svgColour Teal                 = sRGB24 0   128 128
svgColour Thistle              = sRGB24 216 191 216
svgColour Tomato               = sRGB24 255 99  71
svgColour Turquoise            = sRGB24 64  224 208
svgColour Violet               = sRGB24 238 130 238
svgColour Wheat                = sRGB24 245 222 179
svgColour White                = sRGB24 255 255 255
svgColour WhiteSmoke           = sRGB24 245 245 245
svgColour Yellow               = sRGB24 255 255 0
svgColour YellowGreen          = sRGB24 154 205 50
