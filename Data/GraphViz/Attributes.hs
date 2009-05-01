{-# LANGUAGE RecordPuns
           , PatternSignatures
           #-}

 {- GraphViz ------------------------------------------------------\
 |                                                                 |
 | Copyright (c) 2008, Matthew Sackman (matthew@wellquite.org)     |
 |                                                                 |
 | DisTract is freely distributable under the terms of a 3-Clause  |
 | BSD-style license.                                              |
 |                                                                 |
 \-----------------------------------------------------------------}

module Data.GraphViz.Attributes where

import Prelude hiding (LT)

import Data.Word
import Numeric
import Text.ParserCombinators.PolyLazy
import Control.Monad
import Data.Maybe

import Data.GraphViz.ParserCombinators

data ArrowType = Normal   | Inv
               | Dot      | InvDot
               | ODot     | InvODot
               | NoArrow  | Tee
               | Empty    | InvEmpty
               | Diamond  | ODiamond
               | EDiamond | Crow
               | Box      | OBox
               | Open     | HalfOpen
               | Vee
                 deriving (Eq)

instance Show ArrowType where
    show Normal = "normal"
    show Inv = "inv"
    show Dot = "dot"
    show InvDot = "invdot"
    show ODot = "odot"
    show InvODot = "invodot"
    show NoArrow = "none"
    show Tee = "tee"
    show Empty = "empty"
    show InvEmpty = "invempty"
    show Diamond = "diamond"
    show ODiamond = "odiamond"
    show EDiamond = "ediamond"
    show Crow = "crow"
    show Box = "box"
    show OBox = "obox"
    show Open = "open"
    show HalfOpen = "halfopen"
    show Vee = "vee"

readArrowType :: Parser Char ArrowType
readArrowType
              = oneOf [ optionalQuotedString "normal" >> return Normal   
                      , optionalQuotedString "inv" >> return Inv      
                      , optionalQuotedString "dot" >> return Dot      
                      , optionalQuotedString "invdot" >> return InvDot   
                      , optionalQuotedString "odot" >> return ODot     
                      , optionalQuotedString "invodot" >> return InvODot  
                      , optionalQuotedString "noarrow" >> return NoArrow  
                      , optionalQuotedString "tee" >> return Tee      
                      , optionalQuotedString "empty" >> return Empty    
                      , optionalQuotedString "invempty" >> return InvEmpty 
                      , optionalQuotedString "diamond" >> return Diamond  
                      , optionalQuotedString "odiamond" >> return ODiamond 
                      , optionalQuotedString "ediamond" >> return EDiamond 
                      , optionalQuotedString "crow" >> return Crow     
                      , optionalQuotedString "box" >> return Box      
                      , optionalQuotedString "obox" >> return OBox     
                      , optionalQuotedString "open" >> return Open     
                      , optionalQuotedString "halfopen" >> return HalfOpen 
                      , optionalQuotedString "vee" >> return Vee      
                      ]                                          

data ColorType = RGB { red :: Word8
                      , green :: Word8
                      , blue :: Word8
                      }
                | RGBA { red :: Word8
                       , green :: Word8
                       , blue :: Word8
                       , alpha :: Word8
                       }
                  deriving (Eq)

instance Show ColorType where
    show (RGB { red, green, blue })
        = show $ '#' : foldr showWord8Pad "" [red, green, blue]
    show (RGBA { red, green, blue, alpha })
        = show $ '#' : foldr showWord8Pad "" [red, green, blue, alpha]

showWord8Pad :: Word8 -> String -> String
showWord8Pad w s = padding ++ simple ++ s
    where
      simple = showHex w ""
      padding = replicate count '0'
      count = (2 - (findCols 1 w))
      findCols :: Int -> Word8 -> Int
      findCols c n
          | n < 16 = c
          | otherwise = findCols (c+1) (n `div` 16)

readColorType :: Parser Char ColorType
readColorType
              = do { string "\"#"
                   ; digits <- many $ noneOf ['"']
                   ; char '"'
                   ; let c = readHexPairs digits
                   ; return $ case c of
                                [r,g,b]
                                    -> RGB r g b
                                [r,g,b,a]
                                    -> RGBA r g b a
                                _ -> error $ "Unexpected pairs: " ++ show c
                   }
    where
          readHexPairs :: String -> [Word8]
          readHexPairs [] = []
          readHexPairs (h1:h2:h')
              = let [(n, [])] = readHex [h1,h2] in n : readHexPairs h'
          readHexPairs c = error $ "Error in readHexPairs: " ++ (show c)

data DirType = Forward | Back | Both | None
               deriving (Eq)

instance Show DirType where
    show Forward = "forward"
    show Back = "back"
    show Both = "both"
    show None = "none"

readDirType :: Parser Char DirType
readDirType
              = oneOf [optionalQuotedString "forward" >> return Forward
                      ,optionalQuotedString "back" >> return Back
                      ,optionalQuotedString "both" >> return Both
                      ,optionalQuotedString "none" >> return None
                      ]

data OutputMode = BreadthFirst | NodesFirst | EdgesFirst
                  deriving (Eq)

instance Show OutputMode where
    show BreadthFirst = "breadthfirst"
    show NodesFirst = "nodesfirst"
    show EdgesFirst = "edgesfirst"

readOutputMode :: Parser Char OutputMode
readOutputMode
              = oneOf [ optionalQuotedString "breadthfirst" >> return BreadthFirst
                      , optionalQuotedString "nodesfirst" >> return NodesFirst
                      , optionalQuotedString "edgesfirst" >> return EdgesFirst
                      ]

data PageDir = BL | BR | TL | TR | RB | RT | LB | LT
               deriving (Eq)

instance Show PageDir where
    show BL = "BL"
    show BR = "BR"
    show TL = "TL"
    show TR = "TR"
    show RB = "RB"
    show RT = "RT"
    show LB = "LB"
    show LT = "LT"

readPageDir :: Parser Char PageDir
readPageDir
              = oneOf [ optionalQuotedString "BL" >> return BL
                      , optionalQuotedString "BR" >> return BR
                      , optionalQuotedString "TL" >> return TL
                      , optionalQuotedString "TR" >> return TR
                      , optionalQuotedString "RB" >> return RB
                      , optionalQuotedString "RT" >> return RT
                      , optionalQuotedString "LB" >> return LB
                      , optionalQuotedString "LT" >> return LT
                      ]

data ShapeType
    = BoxShape
    | Polygon
    | Ellipse
    | Circle
    | PointShape
    | Egg
    | Triangle
    | Plaintext
    | DiamondShape
    | Trapezium
    | Parallelogram
    | House
    | Pentagon
    | Hexagon
    | Septagon
    | Octagon
    | Doublecircle
    | Doubleoctagon
    | Tripleoctagon
    | Invtriangle
    | Invtrapezium
    | Invhouse
    | Mdiamond
    | Msquare
    | Mcircle
    | Rectangle
    | NoShape
    | Note
    | Tab
    | Folder
    | Box3d
    | Component
      deriving (Eq)

instance Show ShapeType where
    show BoxShape = "box"
    show Polygon = "polygon"
    show Ellipse = "ellipse"
    show Circle = "circle"
    show PointShape = "point"
    show Egg = "egg"
    show Triangle = "triangle"
    show Plaintext = "plaintext"
    show DiamondShape = "diamond"
    show Trapezium = "trapezium"
    show Parallelogram = "parallelogram"
    show House = "house"
    show Pentagon = "pentagon"
    show Hexagon = "hexagon"
    show Septagon = "septagon"
    show Octagon = "octagon"
    show Doublecircle = "doublecircle"
    show Doubleoctagon = "doubleoctagon"
    show Tripleoctagon = "tripleoctagon"
    show Invtriangle = "invtriangle"
    show Invtrapezium = "invtrapezium"
    show Invhouse = "invhouse"
    show Mdiamond = "mdiamond"
    show Msquare = "msquare"
    show Mcircle = "mcircle"
    show Rectangle = "rectangle"
    show NoShape = "none"
    show Note = "note"
    show Tab = "tab"
    show Folder = "folder"
    show Box3d = "box3d"
    show Component = "component"

readShapeType :: Parser Char ShapeType
readShapeType
              = oneOf [ optionalQuotedString "box" >> return BoxShape
                      , optionalQuotedString "polygon" >> return Polygon
                      , optionalQuotedString "ellipse" >> return Ellipse
                      , optionalQuotedString "circle" >> return Circle
                      , optionalQuotedString "point" >> return PointShape
                      , optionalQuotedString "egg" >> return Egg
                      , optionalQuotedString "triangle" >> return Triangle
                      , optionalQuotedString "plaintext" >> return Plaintext
                      , optionalQuotedString "diamond" >> return DiamondShape
                      , optionalQuotedString "trapezium" >> return Trapezium
                      , optionalQuotedString "parallelogram" >> return Parallelogram
                      , optionalQuotedString "house" >> return House
                      , optionalQuotedString "pentagon" >> return Pentagon
                      , optionalQuotedString "hexagon" >> return Hexagon
                      , optionalQuotedString "septagon" >> return Septagon
                      , optionalQuotedString "octagon" >> return Octagon
                      , optionalQuotedString "doublecircle" >> return Doublecircle
                      , optionalQuotedString "doubleoctagon" >> return Doubleoctagon
                      , optionalQuotedString "tripleoctagon" >> return Tripleoctagon
                      , optionalQuotedString "invtriangle" >> return Invtriangle
                      , optionalQuotedString "invtrapezium" >> return Invtrapezium
                      , optionalQuotedString "invhouse" >> return Invhouse
                      , optionalQuotedString "mdiamond" >> return Mdiamond
                      , optionalQuotedString "msquare" >> return Msquare
                      , optionalQuotedString "mcircle" >> return Mcircle
                      , optionalQuotedString "rectangle" >> return Rectangle
                      , optionalQuotedString "none" >> return NoShape
                      , optionalQuotedString "note" >> return Note
                      , optionalQuotedString "tab" >> return Tab
                      , optionalQuotedString "folder" >> return Folder
                      , optionalQuotedString "box3d" >> return Box3d
                      , optionalQuotedString "component" >> return Component
                      ]

data StyleType = Filled | Invisible | Diagonals | Rounded | Dashed | Dotted | Solid | Bold
                 deriving (Eq)

instance Show StyleType where
    show Filled = "filled"
    show Invisible = "invisible"
    show Diagonals = "diagonals"
    show Rounded = "rounded"
    show Dashed = "dashed"
    show Dotted = "dotted"
    show Solid = "solid"
    show Bold = "bold"

readStyleType :: Parser Char StyleType
readStyleType
              = oneOf [ optionalQuotedString "filled" >> return Filled
                      , optionalQuotedString "invisible" >> return Invisible
                      , optionalQuotedString "diagonals" >> return Diagonals
                      , optionalQuotedString "rounded" >> return Rounded
                      , optionalQuotedString "dashed" >> return Dashed
                      , optionalQuotedString "dotted" >> return Dotted
                      , optionalQuotedString "solid" >> return Solid
                      , optionalQuotedString "bold" >> return Bold
                      ]

data Point = Point Int Int
           | PointD Double Double
             deriving (Eq)

newtype PointList = PointList [Point]
                    deriving (Eq)

instance Show Point where
    show (Point x y) = show $ (show x) ++ (',':(show y))
    show (PointD x y) = show $ (show x) ++ (',':(show y))

instance Show PointList where
    show (PointList points) = show $ case foldr s "" points of
                                       [] -> ""
                                       str -> tail str
        where
          s (Point x y) acc = ' ':((show x) ++ (',':((show y) ++ acc)))
          s (PointD x y) acc = ' ':((show x) ++ (',':((show y) ++ acc)))

readPoint :: Parser Char Point
readPoint = char '"' >> oneOf [readPointI, readPointD]
    where
      readPointI = do { x <- number
                      ; char ','
                      ; y <- number
                      ; char '"'
                      ; return $ Point x y
                      }
      readPointD = do { x <- floatingNumber
                      ; char ','
                      ; y <- floatingNumber
                      ; char '"'
                      ; return $ PointD x y
                      }

readPointList :: Parser Char PointList
readPointList
    = do { char '"'
         ; points <- many pointPair
         ; char '"'
         ; return $ PointList points
         }
    where
          pointPair
              = do { x <- number
                   ; char ','
                   ; y <- number
                   ; optional (char ' ')
                   ; return $ Point x y
                   }

data Rect = Rect Point Point
            deriving (Eq)

instance Show Rect where
    show (Rect (Point x1 y1) (Point x2 y2))
        = show $ (show x1) ++ (',': ((show y1) ++ (',': ((show x2) ++ (',': (show y2))))))
    show (Rect (Point x1 y1) (PointD x2 y2))
        = show $ (show x1) ++ (',': ((show y1) ++ (',': ((show x2) ++ (',': (show y2))))))
    show (Rect (PointD x1 y1) (Point x2 y2))
        = show $ (show x1) ++ (',': ((show y1) ++ (',': ((show x2) ++ (',': (show y2))))))
    show (Rect (PointD x1 y1) (PointD x2 y2))
        = show $ (show x1) ++ (',': ((show y1) ++ (',': ((show x2) ++ (',': (show y2))))))

readRect :: Parser Char Rect
readRect = do { char '"'
              ; x1 <- number
              ; char ','
              ; y1 <- number
              ; char ','
              ; x2 <- number
              ; char ','
              ; y2 <- number
              ; char '"'
              ; return (Rect (Point x1 y1) (Point x2 y2))
              }

data ScaleType = Scale | NoScale | FitX | FitY
                 deriving (Eq)

instance Show ScaleType where
    show Scale = "true"
    show NoScale = "false"
    show FitX = "width"
    show FitY = "height"

readScaleType :: Parser Char ScaleType
readScaleType = oneOf [ optionalQuotedString "true" >> return Scale
                      , optionalQuotedString "false" >> return NoScale
                      , optionalQuotedString "width" >> return FitX
                      , optionalQuotedString "height" >> return FitY
                      ]

data Justification = JLeft | JRight | JCenter
                     deriving (Eq)

instance Show Justification where
    show JLeft = "l"
    show JRight = "r"
    show JCenter = "c"

readJustification :: Parser Char Justification
readJustification = oneOf [ optionalQuotedString "l" >> return JLeft
                          , optionalQuotedString "r" >> return JRight
                          , optionalQuotedString "c" >> return JCenter
                          ]

data VerticalPlacement = VTop | VCenter | VBottom
                         deriving (Eq)

instance Show VerticalPlacement where
    show VTop = "t"
    show VCenter = "c"
    show VBottom = "b"

readVerticalPlacement :: Parser Char VerticalPlacement
readVerticalPlacement
              = oneOf [ optionalQuotedString "t" >> return VTop
                      , optionalQuotedString "c" >> return VCenter
                      , optionalQuotedString "b" >> return VBottom
                      ]

data Attribute
    = ArrowHead ArrowType
    | ArrowSize Double
    | ArrowTail ArrowType
    | Bb Rect
    | BgColor ColorType
    | Center Bool
    | Color ColorType
    | Concentrate Bool
    | Constraint Bool
    | Decorate Bool
    | DefaultDist Double
    | Dir DirType
    | Dpi Double
    | FillColor ColorType
    | FixedSize Bool
    | FontColor ColorType
    | FontName String
    | FontSize Double
    | Group String
    | HeadClip Bool
    | HeadLabel String
    | Height Double
    | Image String
    | ImageScale ScaleType
    | Label String
    | LabelAngle Double
    | LabelDistance Double
    | LabelFloat Bool
    | LabelFontColor ColorType
    | LabelFontName String
    | LabelFontSize Double
    | LabelJust Justification
    | LabelLoc VerticalPlacement
    | Landscape Bool
    | Len Double
    | Margin Double Double
    | MinDist Double
    | Minlen Double
    | Nodesep Double
    | NoJustify Bool
    | Normalize Bool
    | Orientation Double
    | OutputOrder OutputMode
    | Overlap Bool
    | Pad Double Double
    | Page Double Double
    | PageDir PageDir
    | PenColor ColorType
    | Pos PointList
    | Quantum Double
    | RankDir PageDir
    | RankSep Double
    | Ratio Double
    | Regular Bool
    | Rotate Double
    | SameHead String
    | SameTail String
    | Sep Double
    | Shape ShapeType
    | Sides Int
    | Size Double Double
    | Skew Double
    | Splines (Maybe Bool)
    | Style StyleType
    | TailClip Bool
    | TailLabel String
    | Weight Double
    | Width Double
    | Unknown String String
      deriving (Eq)

instance Show Attribute where
    show (ArrowHead arrowtype) = "arrowhead=" ++ (show arrowtype)
    show (ArrowSize double) = "arrowsize=" ++ (show double)
    show (ArrowTail arrowtype) = "arrowtail=" ++ (show arrowtype)
    show (Bb rect) = "bb=" ++ (show rect)
    show (BgColor colortype) = "bgcolor=" ++ (show colortype)
    show (Center bool) = "center=" ++ (show bool)
    show (Color colortype) = "color=" ++ (show colortype)
    show (Concentrate bool) = "concentrate=" ++ (show bool)
    show (Constraint bool) = "constraint=" ++ (show bool)
    show (Decorate bool) = "decorate=" ++ (show bool)
    show (DefaultDist double) = "defaultdist=" ++ (show double)
    show (Dir dirtype) = "dir=" ++ (show dirtype)
    show (Dpi double) = "dpi=" ++ (show double)
    show (FillColor colortype) = "fillcolor=" ++ (show colortype)
    show (FixedSize bool) = "fixedsize=" ++ (show bool)
    show (FontColor colortype) = "fontcolor=" ++ (show colortype)
    show (FontName string) = "fontname=" ++ (show string)
    show (FontSize double) = "fontsize=" ++ (show double)
    show (Group string) = "group=" ++ (show string)
    show (HeadClip bool) = "headclip=" ++ (show bool)
    show (HeadLabel string) = "headlabel=" ++ (show string)
    show (Height double) = "height=" ++ (show double)
    show (Image string) = "image=" ++ (show string)
    show (ImageScale scaletype) = "imagescale=" ++ (show scaletype)
    show (Label string) = "label=" ++ (show string)
    show (LabelAngle double) = "labelangle=" ++ (show double)
    show (LabelDistance double) = "labeldistance=" ++ (show double)
    show (LabelFloat bool) = "labelfloat=" ++ (show bool)
    show (LabelFontColor colortype) = "labelfontcolor=" ++ (show colortype)
    show (LabelFontName string) = "labelfontname=" ++ (show string)
    show (LabelFontSize double) = "labelfontsize=" ++ (show double)
    show (LabelJust justification) = "labeljust=" ++ (show justification)
    show (LabelLoc verticalplacement) = "labelloc=" ++ (show verticalplacement)
    show (Landscape bool) = "landscape=" ++ (show bool)
    show (Len double) = "len=" ++ (show double)
    show (Margin double1 double2) = "margin=" ++ (show $ PointD double1 double2)
    show (MinDist double) = "mindist=" ++ (show double)
    show (Minlen double) = "minlen=" ++ (show double)
    show (Nodesep double) = "nodesep=" ++ (show double)
    show (NoJustify bool) = "nojustify=" ++ (show bool)
    show (Normalize bool) = "normalize=" ++ (show bool)
    show (Orientation double) = "orientation=" ++ (show double)
    show (OutputOrder outputmode) = "outputorder=" ++ (show outputmode)
    show (Overlap bool) = "overlap=" ++ (show bool)
    show (Pad double1 double2) = "pad=" ++ (show $ PointD double1 double2)
    show (Page double1 double2) = "page=" ++ (show $ PointD double1 double2)
    show (PageDir pagedir) = "pagedir=" ++ (show pagedir)
    show (PenColor colortype) = "pencolor=" ++ (show colortype)
    show (Pos pointlist) = "pos=" ++ (show pointlist)
    show (Quantum double) = "quantum=" ++ (show double)
    show (RankDir pagedir) = "rankdir=" ++ (show pagedir)
    show (RankSep double) = "ranksep=" ++ (show double)
    show (Ratio double) = "ratio=" ++ (show double)
    show (Regular bool) = "regular=" ++ (show bool)
    show (Rotate double) = "rotate=" ++ (show double)
    show (SameHead string) = "samehead=" ++ (show string)
    show (SameTail string) = "sametail=" ++ (show string)
    show (Sep double) = "sep=" ++ (show double)
    show (Shape shapetype) = "shape=" ++ (show shapetype)
    show (Sides int) = "sides=" ++ (show int)
    show (Size double1 double2) = "size=" ++ (show $ PointD double1 double2)
    show (Skew double) = "skew=" ++ (show double)
    show (Splines maybebool) = "splines=" ++ (show (fromMaybe [] (liftM show maybebool)))
    show (Style styletype) = "style=" ++ (show styletype)
    show (TailClip bool) = "tailclip=" ++ (show bool)
    show (TailLabel string) = "taillabel=" ++ (show string)
    show (Weight double) = "weight=" ++ (show double)
    show (Width double) = "width=" ++ (show double)
    show (Unknown key value) = (show key) ++ ('=':(show . show $ value))

readBool :: Parser Char Bool
readBool = oneOf [ (optionalQuotedString "true" >> return True)
                 , (optionalQuotedString "false" >> return False)
                 , (optionalQuotedString "True" >> return True)
                 , (optionalQuotedString "False" >> return False)
                 ]

readString :: Parser Char String
readString = oneOf [quoted, nonquoted]
    where
      quoted = do { char '"'
                  ; str <- liftM concat . many . oneOf $ [nonescaped, escapedPair]
                  ; char '"'
                  ; return str
                  }
      nonquoted = many $ noneOf ['"', ' ', ',', '\t', '\n', '\r', ']']
      escapedPair = do { char '\\'
                       ; oneOf [ char '"' >> return "\""
                               , char 't' >> return "\t"
                               , char 'n' >> return "\n"
                               , char 'r' >> return "\r"
                               , satisfy (const True) >>= \c -> return ['\\', c]
                               ]
                       }
      nonescaped = liftM (: []) . noneOf $ ['\\', '"']

readAttribute :: Parser Char Attribute
readAttribute
              = oneOf
                [ string "arrowhead=" >> readArrowType >>= return . ArrowHead
                , string "arrowsize=" >> optionalQuoted floatingNumber >>= return . ArrowSize
                , string "arrowtail=" >> readArrowType >>= return . ArrowTail
                , string "bb=" >> readRect >>= return . Bb
                , string "bgcolor=" >> readColorType >>= return . BgColor
                , string "center=" >> readBool >>= return . Center
                , string "color=" >> readColorType >>= return . Color
                , string "concentrate=" >> readBool >>= return . Concentrate
                , string "constraint=" >> readBool >>= return . Constraint
                , string "decorate=" >> readBool >>= return . Decorate
                , string "defaultdist=" >> optionalQuoted floatingNumber >>= return . DefaultDist
                , string "dir=" >> readDirType >>= return . Dir
                , string "dpi=" >> optionalQuoted floatingNumber >>= return . Dpi
                , string "fillcolor=" >> readColorType >>= return . FillColor
                , string "fixedsize=" >> readBool >>= return . FixedSize
                , string "fontcolor=" >> readColorType >>= return . FontColor
                , string "fontname=" >> readString >>= return . FontName
                , string "fontsize=" >> optionalQuoted floatingNumber >>= return . FontSize
                , string "group=" >> readString >>= return . Group
                , string "headclip=" >> readBool >>= return . HeadClip
                , string "headlabel=" >> readString >>= return . HeadLabel
                , string "height=" >> optionalQuoted floatingNumber >>= return . Height
                , string "image=" >> readString >>= return . Image
                , string "imagescale=" >> readScaleType >>= return . ImageScale
                , string "label=" >> readString >>= return . Label
                , string "labelangle=" >> optionalQuoted floatingNumber >>= return . LabelAngle
                , string "labeldistance=" >> optionalQuoted floatingNumber >>= return . LabelDistance
                , string "labelfloat=" >> readBool >>= return . LabelFloat
                , string "labelfontcolor=" >> readColorType >>= return . LabelFontColor
                , string "labelfontname=" >> readString >>= return . LabelFontName
                , string "labelfontsize=" >> optionalQuoted floatingNumber >>= return . LabelFontSize
                , string "labeljust=" >> readJustification >>= return . LabelJust
                , string "labelloc=" >> readVerticalPlacement >>= return . LabelLoc
                , string "landscape=" >> readBool >>= return . Landscape
                , string "len=" >> optionalQuoted floatingNumber >>= return . Len
                , string "margin=" >> readPoint >>= \(PointD x y) -> return $ Margin x y
                , string "mindist=" >> optionalQuoted floatingNumber >>= return . MinDist
                , string "minlen=" >> optionalQuoted floatingNumber >>= return . Minlen
                , string "nodesep=" >> optionalQuoted floatingNumber >>= return . Nodesep
                , string "nojustify=" >> readBool >>= return . NoJustify
                , string "normalize=" >> readBool >>= return . Normalize
                , string "orientation=" >> optionalQuoted floatingNumber >>= return . Orientation
                , string "outputorder=" >> readOutputMode >>= return . OutputOrder
                , string "overlap=" >> readBool >>= return . Overlap
                , string "pad=" >> readPoint >>= \(PointD x y) -> return $ Pad x y
                , string "page=" >> readPoint >>= \(PointD x y) -> return $ Page x y
                , string "pagedir=" >> readPageDir >>= return . PageDir
                , string "pencolor=" >> readColorType >>= return . PenColor
                , string "pos=" >> readPointList >>= return . Pos
                , string "quantum=" >> optionalQuoted floatingNumber >>= return . Quantum
                , string "rankdir=" >> readPageDir >>= return . RankDir
                , string "ranksep=" >> optionalQuoted floatingNumber >>= return . RankSep
                , string "ratio=" >> optionalQuoted floatingNumber >>= return . Ratio
                , string "regular=" >> readBool >>= return . Regular
                , string "rotate=" >> optionalQuoted floatingNumber >>= return . Rotate
                , string "samehead=" >> readString >>= return . SameHead
                , string "sametail=" >> readString >>= return . SameTail
                , string "sep=" >> optionalQuoted floatingNumber >>= return . Sep
                , string "shape=" >> readShapeType >>= return . Shape
                , string "sides=" >> optionalQuoted number >>= return . Sides
                , string "size=" >> readPoint >>= \(PointD x y) -> return $ Size x y
                , string "skew=" >> optionalQuoted floatingNumber >>= return . Skew
                , string "splines=" >> (oneOf [(string "\"\"" >> return Nothing), readBool >>= return . Just]) >>= return . Splines
                , string "style=" >> readStyleType >>= return . Style
                , string "tailclip=" >> readBool >>= return . TailClip
                , string "taillabel=" >> readString >>= return . TailLabel
                , string "weight=" >> optionalQuoted floatingNumber >>= return . Weight
                , string "width=" >> optionalQuoted floatingNumber >>= return . Width
                , many (noneOf ['=', '"', ' ', ',', '\t', '\n', '\r', ']']) >>= \key -> char '=' >> readString >>= return . Unknown key
                ]

readAttributesList :: Parser Char [Attribute]
readAttributesList = do { char '['
                        ; as <- many (optional whitespace >> readAttribute >>= \a -> optional whitespace >> optional (char ',') >> return a)
                        ; optional whitespace
                        ; char ']'
                        ; return as
                        }
