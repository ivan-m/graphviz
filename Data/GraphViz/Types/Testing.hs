{- |
   Module      : Data.GraphViz.Types.Testing
   Description : Testing suite for Graphviz.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

-}
module Data.GraphViz.Types.Testing where

import Data.GraphViz.Types.Printing(PrintDot(..), renderDot)
import Data.GraphViz.Types.Parsing(ParseDot(..), runParser, quoteChar)

import Data.GraphViz.Attributes

import Test.QuickCheck

import Control.Monad(liftM, liftM2, liftM3, liftM4)
import Data.Word(Word8)

-- -----------------------------------------------------------------------------



printParse   :: (ParseDot a, PrintDot a, Eq a) => a -> Bool
printParse a = (fst . parseIt . printIt) a == a

printParseList    :: (ParseDot a, PrintDot a, Eq a) => [a] -> Property
printParseList as =  not (null as) ==> printParse as

printIt :: (PrintDot a) => a -> String
printIt = renderDot . toDot

parseIt :: (ParseDot a) => String -> (a, String)
parseIt = runParser parse

-- -----------------------------------------------------------------------------
-- Defining Arbitrary instances

instance Arbitrary Attribute where
    arbitrary = oneof [ liftM Damping arbitrary
                      , liftM K arbitrary
                      , liftM URL arbitrary
                      , liftM ArrowHead arbitrary
                      , liftM ArrowSize arbitrary
                      , liftM ArrowTail arbitrary
                      , liftM Aspect arbitrary
                      , liftM Bb arbitrary
                      , liftM BgColor arbitrary
                      , liftM Center arbitrary
                      , liftM Charset arbString
                      , liftM ClusterRank arbitrary
                      , liftM Color arbList
                      , liftM ColorScheme arbitrary
                      , liftM Comment arbString
                      , liftM Compound arbitrary
                      , liftM Concentrate arbitrary
                      , liftM Constraint arbitrary
                      , liftM Decorate arbitrary
                      , liftM DefaultDist arbitrary
                      , liftM Dim arbitrary
                      , liftM Dimen arbitrary
                      , liftM Dir arbitrary
                      , liftM DirEdgeConstraints arbitrary
                      , liftM Distortion arbitrary
                      , liftM DPI arbitrary
                      , liftM EdgeURL arbitrary
                      , liftM EdgeTarget arbString
                      , liftM EdgeTooltip arbString
                      , liftM Epsilon arbitrary
                      , liftM ESep arbitrary
                      , liftM FillColor arbitrary
                      , liftM FixedSize arbitrary
                      , liftM FontColor arbitrary
                      , liftM FontName arbString
                      , liftM FontNames arbString
                      , liftM FontPath arbString
                      , liftM FontSize arbitrary
                      , liftM Group arbString
                      , liftM HeadURL arbitrary
                      , liftM HeadClip arbitrary
                      , liftM HeadLabel arbitrary
                      , liftM HeadPort arbitrary
                      , liftM HeadTarget arbString
                      , liftM HeadTooltip arbString
                      , liftM Height arbitrary
                      , liftM ID arbitrary
                      , liftM Image arbString
                      , liftM ImageScale arbitrary
                      , liftM Label arbitrary
                      , liftM LabelURL arbitrary
                      , liftM LabelAngle arbitrary
                      , liftM LabelDistance arbitrary
                      , liftM LabelFloat arbitrary
                      , liftM LabelFontColor arbitrary
                      , liftM LabelFontName arbString
                      , liftM LabelFontSize arbitrary
                      , liftM LabelJust arbitrary
                      , liftM LabelLoc arbitrary
                      , liftM LabelTarget arbString
                      , liftM LabelTooltip arbString
                      , liftM Landscape arbitrary
                      , liftM Layer arbitrary
                      , liftM Layers arbitrary
                      , liftM LayerSep arbString
                      , liftM Layout arbString
                      , liftM Len arbitrary
                      , liftM Levels arbitrary
                      , liftM LevelsGap arbitrary
                      , liftM LHead arbString
                      , liftM LPos arbitrary
                      , liftM LTail arbString
                      , liftM Margin arbitrary
                      , liftM MaxIter arbitrary
                      , liftM MCLimit arbitrary
                      , liftM MinDist arbitrary
                      , liftM MinLen arbitrary
                      , liftM Mode arbitrary
                      , liftM Model arbitrary
                      , liftM Mosek arbitrary
                      , liftM NodeSep arbitrary
                      , liftM NoJustify arbitrary
                      , liftM Normalize arbitrary
                      , liftM Nslimit arbitrary
                      , liftM Nslimit1 arbitrary
                      , liftM Ordering arbString
                      , liftM Orientation arbitrary
                      , liftM OrientationGraph arbString
                      , liftM OutputOrder arbitrary
                      , liftM Overlap arbitrary
                      , liftM OverlapScaling arbitrary
                      , liftM PackMode arbitrary
                      , liftM Pack arbitrary
                      , liftM Pad arbitrary
                      , liftM Page arbitrary
                      , liftM PageDir arbitrary
                      , liftM PenColor arbitrary
                      , liftM PenWidth arbitrary
                      , liftM Peripheries arbitrary
                      , liftM Pin arbitrary
                      , liftM Pos arbitrary
                      , liftM QuadTree arbitrary
                      , liftM Quantum arbitrary
                      , liftM Rank arbitrary
                      , liftM RankDir arbitrary
                      , liftM Ranksep arbitrary
                      , liftM Ratio arbitrary
                      , liftM Rects arbitrary
                      , liftM Regular arbitrary
                      , liftM ReMinCross arbitrary
                      , liftM RepulsiveForce arbitrary
                      , liftM Root arbitrary
                      , liftM Rotate arbitrary
                      , liftM SameHead arbString
                      , liftM SameTail arbString
                      , liftM SamplePoints arbitrary
                      , liftM SearchSize arbitrary
                      , liftM Sep arbitrary
                      , liftM Shape arbitrary
                      , liftM ShapeFile arbString
                      , liftM ShowBoxes arbitrary
                      , liftM Sides arbitrary
                      , liftM Size arbitrary
                      , liftM Skew arbitrary
                      , liftM Smoothing arbitrary
                      , liftM SortV arbitrary
                      , liftM Splines arbitrary
                      , liftM Start arbitrary
                      , liftM Style arbList
                      , liftM StyleSheet arbString
                      , liftM TailURL arbitrary
                      , liftM TailClip arbitrary
                      , liftM TailLabel arbitrary
                      , liftM TailPort arbitrary
                      , liftM TailTarget arbString
                      , liftM TailTooltip arbString
                      , liftM Target arbString
                      , liftM Tooltip arbString
                      , liftM TrueColor arbitrary
                      , liftM Vertices arbList
                      , liftM ViewPort arbitrary
                      , liftM VoroMargin arbitrary
                      , liftM Weight arbitrary
                      , liftM Width arbitrary
                      , liftM Z arbitrary
                      ]

    shrink (Damping v)            = map Damping            $ shrink v
    shrink (K v)                  = map K                  $ shrink v
    shrink (URL v)                = map URL                $ shrink v
    shrink (ArrowHead v)          = map ArrowHead          $ shrink v
    shrink (ArrowSize v)          = map ArrowSize          $ shrink v
    shrink (ArrowTail v)          = map ArrowTail          $ shrink v
    shrink (Aspect v)             = map Aspect             $ shrink v
    shrink (Bb v)                 = map Bb                 $ shrink v
    shrink (BgColor v)            = map BgColor            $ shrink v
    shrink (Center v)             = map Center             $ shrink v
    shrink (Charset v)            = map Charset            $ shrink v
    shrink (ClusterRank v)        = map ClusterRank        $ shrink v
    shrink (Color v)              = map Color              $ nonEmptyShrinks v
    shrink (ColorScheme v)        = map ColorScheme        $ shrink v
    shrink (Comment v)            = map Comment            $ shrink v
    shrink (Compound v)           = map Compound           $ shrink v
    shrink (Concentrate v)        = map Concentrate        $ shrink v
    shrink (Constraint v)         = map Constraint         $ shrink v
    shrink (Decorate v)           = map Decorate           $ shrink v
    shrink (DefaultDist v)        = map DefaultDist        $ shrink v
    shrink (Dim v)                = map Dim                $ shrink v
    shrink (Dimen v)              = map Dimen              $ shrink v
    shrink (Dir v)                = map Dir                $ shrink v
    shrink (DirEdgeConstraints v) = map DirEdgeConstraints $ shrink v
    shrink (Distortion v)         = map Distortion         $ shrink v
    shrink (DPI v)                = map DPI                $ shrink v
    shrink (EdgeURL v)            = map EdgeURL            $ shrink v
    shrink (EdgeTarget v)         = map EdgeTarget         $ shrink v
    shrink (EdgeTooltip v)        = map EdgeTooltip        $ shrink v
    shrink (Epsilon v)            = map Epsilon            $ shrink v
    shrink (ESep v)               = map ESep               $ shrink v
    shrink (FillColor v)          = map FillColor          $ shrink v
    shrink (FixedSize v)          = map FixedSize          $ shrink v
    shrink (FontColor v)          = map FontColor          $ shrink v
    shrink (FontName v)           = map FontName           $ shrink v
    shrink (FontNames v)          = map FontNames          $ shrink v
    shrink (FontPath v)           = map FontPath           $ shrink v
    shrink (FontSize v)           = map FontSize           $ shrink v
    shrink (Group v)              = map Group              $ shrink v
    shrink (HeadURL v)            = map HeadURL            $ shrink v
    shrink (HeadClip v)           = map HeadClip           $ shrink v
    shrink (HeadLabel v)          = map HeadLabel          $ shrink v
    shrink (HeadPort v)           = map HeadPort           $ shrink v
    shrink (HeadTarget v)         = map HeadTarget         $ shrink v
    shrink (HeadTooltip v)        = map HeadTooltip        $ shrink v
    shrink (Height v)             = map Height             $ shrink v
    shrink (ID v)                 = map ID                 $ shrink v
    shrink (Image v)              = map Image              $ shrink v
    shrink (ImageScale v)         = map ImageScale         $ shrink v
    shrink (Label v)              = map Label              $ shrink v
    shrink (LabelURL v)           = map LabelURL           $ shrink v
    shrink (LabelAngle v)         = map LabelAngle         $ shrink v
    shrink (LabelDistance v)      = map LabelDistance      $ shrink v
    shrink (LabelFloat v)         = map LabelFloat         $ shrink v
    shrink (LabelFontColor v)     = map LabelFontColor     $ shrink v
    shrink (LabelFontName v)      = map LabelFontName      $ shrink v
    shrink (LabelFontSize v)      = map LabelFontSize      $ shrink v
    shrink (LabelJust v)          = map LabelJust          $ shrink v
    shrink (LabelLoc v)           = map LabelLoc           $ shrink v
    shrink (LabelTarget v)        = map LabelTarget        $ shrink v
    shrink (LabelTooltip v)       = map LabelTooltip       $ shrink v
    shrink (Landscape v)          = map Landscape          $ shrink v
    shrink (Layer v)              = map Layer              $ shrink v
    shrink (Layers v)             = map Layers             $ shrink v
    shrink (LayerSep v)           = map LayerSep           $ shrink v
    shrink (Layout v)             = map Layout             $ shrink v
    shrink (Len v)                = map Len                $ shrink v
    shrink (Levels v)             = map Levels             $ shrink v
    shrink (LevelsGap v)          = map LevelsGap          $ shrink v
    shrink (LHead v)              = map LHead              $ shrink v
    shrink (LPos v)               = map LPos               $ shrink v
    shrink (LTail v)              = map LTail              $ shrink v
    shrink (Margin v)             = map Margin             $ shrink v
    shrink (MaxIter v)            = map MaxIter            $ shrink v
    shrink (MCLimit v)            = map MCLimit            $ shrink v
    shrink (MinDist v)            = map MinDist            $ shrink v
    shrink (MinLen v)             = map MinLen             $ shrink v
    shrink (Mode v)               = map Mode               $ shrink v
    shrink (Model v)              = map Model              $ shrink v
    shrink (Mosek v)              = map Mosek              $ shrink v
    shrink (NodeSep v)            = map NodeSep            $ shrink v
    shrink (NoJustify v)          = map NoJustify          $ shrink v
    shrink (Normalize v)          = map Normalize          $ shrink v
    shrink (Nslimit v)            = map Nslimit            $ shrink v
    shrink (Nslimit1 v)           = map Nslimit1           $ shrink v
    shrink (Ordering v)           = map Ordering           $ shrink v
    shrink (Orientation v)        = map Orientation        $ shrink v
    shrink (OrientationGraph v)   = map OrientationGraph   $ shrink v
    shrink (OutputOrder v)        = map OutputOrder        $ shrink v
    shrink (Overlap v)            = map Overlap            $ shrink v
    shrink (OverlapScaling v)     = map OverlapScaling     $ shrink v
    shrink (PackMode v)           = map PackMode           $ shrink v
    shrink (Pack v)               = map Pack               $ shrink v
    shrink (Pad v)                = map Pad                $ shrink v
    shrink (Page v)               = map Page               $ shrink v
    shrink (PageDir v)            = map PageDir            $ shrink v
    shrink (PenColor v)           = map PenColor           $ shrink v
    shrink (PenWidth v)           = map PenWidth           $ shrink v
    shrink (Peripheries v)        = map Peripheries        $ shrink v
    shrink (Pin v)                = map Pin                $ shrink v
    shrink (Pos v)                = map Pos                $ shrink v
    shrink (QuadTree v)           = map QuadTree           $ shrink v
    shrink (Quantum v)            = map Quantum            $ shrink v
    shrink (Rank v)               = map Rank               $ shrink v
    shrink (RankDir v)            = map RankDir            $ shrink v
    shrink (Ranksep v)            = map Ranksep            $ shrink v
    shrink (Ratio v)              = map Ratio              $ shrink v
    shrink (Rects v)              = map Rects              $ shrink v
    shrink (Regular v)            = map Regular            $ shrink v
    shrink (ReMinCross v)         = map ReMinCross         $ shrink v
    shrink (RepulsiveForce v)     = map RepulsiveForce     $ shrink v
    shrink (Root v)               = map Root               $ shrink v
    shrink (Rotate v)             = map Rotate             $ shrink v
    shrink (SameHead v)           = map SameHead           $ shrink v
    shrink (SameTail v)           = map SameTail           $ shrink v
    shrink (SamplePoints v)       = map SamplePoints       $ shrink v
    shrink (SearchSize v)         = map SearchSize         $ shrink v
    shrink (Sep v)                = map Sep                $ shrink v
    shrink (Shape v)              = map Shape              $ shrink v
    shrink (ShapeFile v)          = map ShapeFile          $ shrink v
    shrink (ShowBoxes v)          = map ShowBoxes          $ shrink v
    shrink (Sides v)              = map Sides              $ shrink v
    shrink (Size v)               = map Size               $ shrink v
    shrink (Skew v)               = map Skew               $ shrink v
    shrink (Smoothing v)          = map Smoothing          $ shrink v
    shrink (SortV v)              = map SortV              $ shrink v
    shrink (Splines v)            = map Splines            $ shrink v
    shrink (Start v)              = map Start              $ shrink v
    shrink (Style v)              = map Style              $ nonEmptyShrinks v
    shrink (StyleSheet v)         = map StyleSheet         $ shrink v
    shrink (TailURL v)            = map TailURL            $ shrink v
    shrink (TailClip v)           = map TailClip           $ shrink v
    shrink (TailLabel v)          = map TailLabel          $ shrink v
    shrink (TailPort v)           = map TailPort           $ shrink v
    shrink (TailTarget v)         = map TailTarget         $ shrink v
    shrink (TailTooltip v)        = map TailTooltip        $ shrink v
    shrink (Target v)             = map Target             $ shrink v
    shrink (Tooltip v)            = map Tooltip            $ shrink v
    shrink (TrueColor v)          = map TrueColor          $ shrink v
    shrink (Vertices v)           = map Vertices           $ nonEmptyShrinks v
    shrink (ViewPort v)           = map ViewPort           $ shrink v
    shrink (VoroMargin v)         = map VoroMargin         $ shrink v
    shrink (Weight v)             = map Weight             $ shrink v
    shrink (Width v)              = map Width              $ shrink v
    shrink (Z v)                  = map Z                  $ shrink v
{- delete to here -}

instance Arbitrary Word8 where
  arbitrary = arbitraryBoundedIntegral

instance Arbitrary URL where
  arbitrary = liftM UStr
              $ suchThat arbString (all notAngled)
    where
      -- Ensure it doesn't have any angled brackets
      notAngled = flip notElem ['<', '>']

instance Arbitrary ArrowType where
  arbitrary = liftM AType
              -- Arrow specifications have between 1 and 4 elements.
              $ resize 4 arbList

instance Arbitrary ArrowShape where
  arbitrary = arbBounded

instance Arbitrary ArrowModifier where
  arbitrary = liftM2 ArrMod arbitrary arbitrary

instance Arbitrary ArrowFill where
  arbitrary = arbBounded

instance Arbitrary ArrowSide where
  arbitrary = arbBounded

instance Arbitrary AspectType where
  arbitrary = oneof [ liftM  RatioOnly arbitrary
                    , liftM2 RatioPassCount arbitrary posArbitrary
                    ]

instance Arbitrary Rect where
  arbitrary = liftM2 Rect arbitrary arbitrary

instance Arbitrary Point where
  -- Pretty sure points have to be positive...
  arbitrary = oneof [ liftM2 Point  posArbitrary posArbitrary
                    , liftM2 PointD posArbitrary posArbitrary
                    ]

instance Arbitrary ClusterMode where
  arbitrary = arbBounded

instance Arbitrary DirType where
  arbitrary = arbBounded

instance Arbitrary DEConstraints where
  arbitrary = arbBounded

instance Arbitrary DPoint where
  arbitrary = oneof [ liftM DVal arbitrary
                    , liftM PVal arbitrary
                    ]

instance Arbitrary ModeType where
  arbitrary = arbBounded

instance Arbitrary Model where
  arbitrary = arbBounded

instance Arbitrary Label where
  arbitrary = oneof [ liftM StrLabel arbString
                    , liftM URLLabel arbitrary
                    ]

instance Arbitrary Overlap where
  arbitrary = oneof [ simpleOverlap
                    , liftM PrismOverlap arbitrary
                    ]
    where
      -- Have to do this by hand since Overlap can't have Bounded and
      -- Enum instances
      simpleOverlap = elements [ KeepOverlaps
                               , RemoveOverlaps
                               , ScaleOverlaps
                               , ScaleXYOverlaps
                               , CompressOverlap
                               , VpscOverlap
                               , IpsepOverlap
                               ]

instance Arbitrary LayerList where
  arbitrary = do fst <- arbLayerName
                 oths <- listOf $ liftM2 (,) arbLayerSep arbLayerName
                 return $ LL fst oths

  shrink (LL _ [(_,fst')]) = [LL fst' []]
  shrink (LL fst oths) = map (LL fst) $ shrink oths

instance Arbitrary LayerRange where
  arbitrary = oneof [ liftM  LRID arbitrary
                    , liftM3 LRS arbitrary arbLayerSep arbitrary
                    ]
    where
      arbLayerSep = listOf1 (elements defLayerSep)

  shrink LRID{}        = []
  shrink (LRS l1 _ l2) = [LRID l1, LRID l2]

instance Arbitrary LayerID where
  arbitrary = oneof [ return AllLayers
                    , liftM LRInt arbitrary
                    , liftM LRName arbLayerName
                    ]

instance Arbitrary OutputMode where
  arbitrary = arbBounded

instance Arbitrary Pack where
  arbitrary = oneof [ return DoPack
                    , return DontPack
                    , liftM PackMargin arbitrary
                    ]

instance Arbitrary PackMode where
  arbitrary = oneof [ return PackNode
                    , return PackClust
                    , return PackGraph
                    , liftM3 PackArray arbitrary arbitrary arbitrary
                    ]

instance Arbitrary Pos where
  arbitrary = oneof [ liftM PointPos arbitrary
                      -- A single spline with only one point overall
                      -- is just a point...
                    , liftM SplinePos $ suchThat arbList isValid
                    ]
    where
      isValid [Spline Nothing Nothing [_]] = False
      isValid _                            = True

instance Arbitrary Spline where
  arbitrary = liftM3 Spline arbitrary arbitrary
              -- list of points must have length of 1 mod 3
              $ suchThat arbitrary ((==) 1 . flip mod 3 . length)

instance Arbitrary EdgeType where
  arbitrary = arbBounded

instance Arbitrary PageDir where
  arbitrary = arbBounded

instance Arbitrary QuadType where
  arbitrary = arbBounded

instance Arbitrary Root where
  arbitrary = oneof [ return IsCentral
                    , return NotCentral
                    , liftM NodeName arbString
                    ]

instance Arbitrary RankType where
  arbitrary = arbBounded

instance Arbitrary RankDir where
  arbitrary = arbBounded

instance Arbitrary Shape where
  arbitrary = arbBounded

instance Arbitrary SmoothType where
  arbitrary = arbBounded

instance Arbitrary StartType where
  arbitrary = oneof [ liftM  StartStyle arbitrary
                    , liftM  StartSeed arbitrary
                    , liftM2 StartStyleSeed arbitrary arbitrary
                    ]

instance Arbitrary STStyle where
  arbitrary = arbBounded

instance Arbitrary StyleItem where
  arbitrary = liftM2 SItem arbitrary (listOf arbStyleName)

instance Arbitrary StyleName where
  arbitrary = oneof [ defaultStyles
                    , liftM DD arbStyleName
                    ]
    where
      defaultStyles = elements [ Dashed
                               , Dotted
                               , Bold
                               , Invisible
                               , Filled
                               , Diagonals
                               , Rounded
                               ]

instance Arbitrary PortPos where
  arbitrary = liftM PP arbitrary

instance Arbitrary CompassPoint where
  arbitrary = arbBounded

instance Arbitrary ViewPort where
  arbitrary = liftM4 VP arbitrary arbitrary arbitrary arbitrary

instance Arbitrary FocusType where
  arbitrary = oneof [ liftM XY arbitrary
                    , liftM NodeFocus arbString
                    ]

instance Arbitrary VerticalPlacement where
  arbitrary = arbBounded

instance Arbitrary ScaleType where
  arbitrary = arbBounded

instance Arbitrary Justification where
  arbitrary = arbBounded

instance Arbitrary Ratios where
  arbitrary = oneof [ liftM AspectRatio posArbitrary
                    , namedRats
                    ]
    where
      namedRats = elements [ FillRatio
                           , CompressRatio
                           , ExpandRatio
                           , AutoRatio
                           ]

instance Arbitrary ColorScheme where
  arbitrary = oneof [ return X11
                    , liftM2 BrewerScheme arbitrary arbitrary
                    ]

instance Arbitrary BrewerName where
  arbitrary = arbBounded

instance Arbitrary Color where
  arbitrary = oneof [ liftM3 RGB  arbitrary arbitrary arbitrary
                    , liftM4 RGBA arbitrary arbitrary arbitrary arbitrary
                    , liftM3 HSV  zeroOne zeroOne zeroOne
                    , liftM X11Color arbitrary
                      -- Not quite right as the values can get too
                      -- high/low, but should suffice for
                      -- printing/parsing purposes.
                    , liftM BrewerColor arbitrary
                    ]
    where
      zeroOne = choose (0,1)

instance Arbitrary X11Color where
  arbitrary = arbBounded

-- -----------------------------------------------------------------------------
-- Helper Functions

fromPositive              :: Positive a -> a
fromPositive (Positive a) = a

posArbitrary :: (Arbitrary a, Num a, Ord a) => Gen a
posArbitrary = liftM fromPositive arbitrary

arbString :: Gen String
arbString = arbList

arbBounded :: (Bounded a, Enum a) => Gen a
arbBounded = elements [minBound .. maxBound]

arbLayerSep :: Gen [Char]
arbLayerSep = listOf1 (elements defLayerSep)

arbLayerName :: Gen String
arbLayerName = suchThat arbString
               $ liftM2 (&&) (all notLayerSep) (all ((/=) quoteChar))

arbStyleName :: Gen String
arbStyleName = suchThat arbString (all notBrackCom)
  where
    notBrackCom = flip notElem ['(', ')', ',', ' ']

arbList :: (Arbitrary a) => Gen [a]
arbList = listOf1 arbitrary

nonEmptyShrinks :: (Arbitrary a) => [a] -> [[a]]
nonEmptyShrinks = filter (not . null) . shrink
