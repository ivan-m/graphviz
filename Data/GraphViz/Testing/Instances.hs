{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
   Module      : Data.GraphViz.Testing.Instances
   Description : 'Arbitrary' instances for graphviz.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines the 'Arbitrary' instances for the various types
   used to represent Graphviz Dot code.

   Note that they do not generally generate /sensible/ values for the
   various types; in particular, there's no guarantee that the
   'Attributes' chosen for a particular value type are indeed legal
   for that type.
-}
module Data.GraphViz.Testing.Instances() where

import Data.GraphViz.Types.Parsing(isNumString)

import Data.GraphViz.Attributes
import Data.GraphViz.Types

import Test.QuickCheck

import Data.Char(toLower)
import Data.List(nub)
import Control.Monad(liftM, liftM2, liftM3, liftM4, guard)
import Data.Word(Word8)

-- -----------------------------------------------------------------------------
-- Defining Arbitrary instances for the overall types

instance (Eq a, Arbitrary a) => Arbitrary (DotGraph a) where
  arbitrary = liftM4 DotGraph arbitrary arbitrary arbitrary arbitrary

  shrink (DotGraph str dir gid stmts) = map (DotGraph str dir gid)
                                        $ shrink stmts

instance Arbitrary GraphID where
  arbitrary = oneof [ liftM Str arbString
                    , liftM Int arbitrary
                    , liftM Dbl $ suchThat arbitrary notInt
                    , liftM HTML arbitrary
                    ]

  shrink (Str s) = map Str $ shrinkString s
  shrink (Int i) = map Int $ shrink i
  shrink (Dbl d) = map Dbl $ filter notInt $ shrink d
  shrink (HTML u) = map HTML $ shrink u

instance (Eq a, Arbitrary a) => Arbitrary (DotStatements a) where
  arbitrary = arbDS True

  shrink ds@(DotStmts gas sgs ns es) = do gas' <- shrinkL gas
                                          sgs' <- shrinkL sgs
                                          ns' <- shrinkL ns
                                          es' <- shrinkL es
                                          returnCheck ds
                                            $ DotStmts gas' sgs' ns' es'

-- | If 'True', generate 'DotSubGraph's; otherwise don't.
arbDS         :: (Arbitrary a, Eq a) => Bool -> Gen (DotStatements a)
arbDS haveSGs = liftM4 DotStmts arbitrary genSGs arbitrary arbitrary
  where
    genSGs = if haveSGs
             then resize 2 arbitrary
             else return []

instance Arbitrary GlobalAttributes where
  arbitrary = oneof [ liftM GraphAttrs arbList
                    , liftM NodeAttrs  arbList
                    , liftM EdgeAttrs  arbList
                    ]

  shrink (GraphAttrs atts) = map GraphAttrs $ nonEmptyShrinks atts
  shrink (NodeAttrs  atts) = map NodeAttrs  $ nonEmptyShrinks atts
  shrink (EdgeAttrs  atts) = map EdgeAttrs  $ nonEmptyShrinks atts

instance (Eq a, Arbitrary a) => Arbitrary (DotSubGraph a) where
  arbitrary = liftM3 DotSG arbitrary arbitrary (arbDS False)

  shrink (DotSG isCl mid stmts) = map (DotSG isCl mid) $ shrink stmts

instance (Arbitrary a) => Arbitrary (DotNode a) where
  arbitrary = liftM2 DotNode arbitrary arbitrary

  shrink (DotNode n as) = map (DotNode n) $ shrinkList as

instance (Arbitrary a) => Arbitrary (DotEdge a) where
  arbitrary = liftM4 DotEdge arbitrary arbitrary arbitrary arbitrary

  shrink (DotEdge f t isDir as) = map (DotEdge f t isDir) $ shrinkList as

-- -----------------------------------------------------------------------------
-- Defining Arbitrary instances for Attributes

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
                      , liftM ColorScheme arbitrary
                      , liftM Color arbList
                      , liftM Comment arbString
                      , liftM Compound arbitrary
                      , liftM Concentrate arbitrary
                      , liftM Constraint arbitrary
                      , liftM Decorate arbitrary
                      , liftM DefaultDist arbitrary
                      , liftM Dimen arbitrary
                      , liftM Dim arbitrary
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
                      , liftM Label arbitrary
                      , liftM Landscape arbitrary
                      , liftM LayerSep arbString
                      , liftM Layers arbitrary
                      , liftM Layer arbitrary
                      , liftM Layout arbString
                      , liftM Len arbitrary
                      , liftM LevelsGap arbitrary
                      , liftM Levels arbitrary
                      , liftM LHead arbString
                      , liftM LPos arbitrary
                      , liftM LTail arbString
                      , liftM Margin arbitrary
                      , liftM MaxIter arbitrary
                      , liftM MCLimit arbitrary
                      , liftM MinDist arbitrary
                      , liftM MinLen arbitrary
                      , liftM Model arbitrary
                      , liftM Mode arbitrary
                      , liftM Mosek arbitrary
                      , liftM NodeSep arbitrary
                      , liftM NoJustify arbitrary
                      , liftM Normalize arbitrary
                      , liftM Nslimit1 arbitrary
                      , liftM Nslimit arbitrary
                      , liftM Ordering arbString
                      , liftM Orientation arbitrary
                      , liftM OutputOrder arbitrary
                      , liftM OverlapScaling arbitrary
                      , liftM Overlap arbitrary
                      , liftM PackMode arbitrary
                      , liftM Pack arbitrary
                      , liftM Pad arbitrary
                      , liftM PageDir arbitrary
                      , liftM Page arbitrary
                      , liftM PenColor arbitrary
                      , liftM PenWidth arbitrary
                      , liftM Peripheries arbitrary
                      , liftM Pin arbitrary
                      , liftM Pos arbitrary
                      , liftM QuadTree arbitrary
                      , liftM Quantum arbitrary
                      , liftM RankDir arbitrary
                      , liftM RankSep arbitrary
                      , liftM Rank arbitrary
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
                      , liftM ShapeFile arbString
                      , liftM Shape arbitrary
                      , liftM ShowBoxes arbitrary
                      , liftM Sides arbitrary
                      , liftM Size arbitrary
                      , liftM Skew arbitrary
                      , liftM Smoothing arbitrary
                      , liftM SortV arbitrary
                      , liftM Splines arbitrary
                      , liftM Start arbitrary
                      , liftM StyleSheet arbString
                      , liftM Style arbList
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
    shrink (Charset v)            = map Charset            $ shrinkString v
    shrink (ClusterRank v)        = map ClusterRank        $ shrink v
    shrink (ColorScheme v)        = map ColorScheme        $ shrink v
    shrink (Color v)              = map Color              $ nonEmptyShrinks v
    shrink (Comment v)            = map Comment            $ shrinkString v
    shrink (Compound v)           = map Compound           $ shrink v
    shrink (Concentrate v)        = map Concentrate        $ shrink v
    shrink (Constraint v)         = map Constraint         $ shrink v
    shrink (Decorate v)           = map Decorate           $ shrink v
    shrink (DefaultDist v)        = map DefaultDist        $ shrink v
    shrink (Dimen v)              = map Dimen              $ shrink v
    shrink (Dim v)                = map Dim                $ shrink v
    shrink (Dir v)                = map Dir                $ shrink v
    shrink (DirEdgeConstraints v) = map DirEdgeConstraints $ shrink v
    shrink (Distortion v)         = map Distortion         $ shrink v
    shrink (DPI v)                = map DPI                $ shrink v
    shrink (EdgeURL v)            = map EdgeURL            $ shrink v
    shrink (EdgeTarget v)         = map EdgeTarget         $ shrinkString v
    shrink (EdgeTooltip v)        = map EdgeTooltip        $ shrinkString v
    shrink (Epsilon v)            = map Epsilon            $ shrink v
    shrink (ESep v)               = map ESep               $ shrink v
    shrink (FillColor v)          = map FillColor          $ shrink v
    shrink (FixedSize v)          = map FixedSize          $ shrink v
    shrink (FontColor v)          = map FontColor          $ shrink v
    shrink (FontName v)           = map FontName           $ shrinkString v
    shrink (FontNames v)          = map FontNames          $ shrinkString v
    shrink (FontPath v)           = map FontPath           $ shrinkString v
    shrink (FontSize v)           = map FontSize           $ shrink v
    shrink (Group v)              = map Group              $ shrinkString v
    shrink (HeadURL v)            = map HeadURL            $ shrink v
    shrink (HeadClip v)           = map HeadClip           $ shrink v
    shrink (HeadLabel v)          = map HeadLabel          $ shrink v
    shrink (HeadPort v)           = map HeadPort           $ shrink v
    shrink (HeadTarget v)         = map HeadTarget         $ shrinkString v
    shrink (HeadTooltip v)        = map HeadTooltip        $ shrinkString v
    shrink (Height v)             = map Height             $ shrink v
    shrink (ID v)                 = map ID                 $ shrink v
    shrink (Image v)              = map Image              $ shrinkString v
    shrink (ImageScale v)         = map ImageScale         $ shrink v
    shrink (LabelURL v)           = map LabelURL           $ shrink v
    shrink (LabelAngle v)         = map LabelAngle         $ shrink v
    shrink (LabelDistance v)      = map LabelDistance      $ shrink v
    shrink (LabelFloat v)         = map LabelFloat         $ shrink v
    shrink (LabelFontColor v)     = map LabelFontColor     $ shrink v
    shrink (LabelFontName v)      = map LabelFontName      $ shrinkString v
    shrink (LabelFontSize v)      = map LabelFontSize      $ shrink v
    shrink (LabelJust v)          = map LabelJust          $ shrink v
    shrink (LabelLoc v)           = map LabelLoc           $ shrink v
    shrink (LabelTarget v)        = map LabelTarget        $ shrinkString v
    shrink (LabelTooltip v)       = map LabelTooltip       $ shrinkString v
    shrink (Label v)              = map Label              $ shrink v
    shrink (Landscape v)          = map Landscape          $ shrink v
    shrink (LayerSep v)           = map LayerSep           $ shrinkString v
    shrink (Layers v)             = map Layers             $ shrink v
    shrink (Layer v)              = map Layer              $ shrink v
    shrink (Layout v)             = map Layout             $ shrinkString v
    shrink (Len v)                = map Len                $ shrink v
    shrink (LevelsGap v)          = map LevelsGap          $ shrink v
    shrink (Levels v)             = map Levels             $ shrink v
    shrink (LHead v)              = map LHead              $ shrinkString v
    shrink (LPos v)               = map LPos               $ shrink v
    shrink (LTail v)              = map LTail              $ shrinkString v
    shrink (Margin v)             = map Margin             $ shrink v
    shrink (MaxIter v)            = map MaxIter            $ shrink v
    shrink (MCLimit v)            = map MCLimit            $ shrink v
    shrink (MinDist v)            = map MinDist            $ shrink v
    shrink (MinLen v)             = map MinLen             $ shrink v
    shrink (Model v)              = map Model              $ shrink v
    shrink (Mode v)               = map Mode               $ shrink v
    shrink (Mosek v)              = map Mosek              $ shrink v
    shrink (NodeSep v)            = map NodeSep            $ shrink v
    shrink (NoJustify v)          = map NoJustify          $ shrink v
    shrink (Normalize v)          = map Normalize          $ shrink v
    shrink (Nslimit1 v)           = map Nslimit1           $ shrink v
    shrink (Nslimit v)            = map Nslimit            $ shrink v
    shrink (Ordering v)           = map Ordering           $ shrinkString v
    shrink (Orientation v)        = map Orientation        $ shrink v
    shrink (OutputOrder v)        = map OutputOrder        $ shrink v
    shrink (OverlapScaling v)     = map OverlapScaling     $ shrink v
    shrink (Overlap v)            = map Overlap            $ shrink v
    shrink (PackMode v)           = map PackMode           $ shrink v
    shrink (Pack v)               = map Pack               $ shrink v
    shrink (Pad v)                = map Pad                $ shrink v
    shrink (PageDir v)            = map PageDir            $ shrink v
    shrink (Page v)               = map Page               $ shrink v
    shrink (PenColor v)           = map PenColor           $ shrink v
    shrink (PenWidth v)           = map PenWidth           $ shrink v
    shrink (Peripheries v)        = map Peripheries        $ shrink v
    shrink (Pin v)                = map Pin                $ shrink v
    shrink (Pos v)                = map Pos                $ shrink v
    shrink (QuadTree v)           = map QuadTree           $ shrink v
    shrink (Quantum v)            = map Quantum            $ shrink v
    shrink (RankDir v)            = map RankDir            $ shrink v
    shrink (RankSep v)            = map RankSep            $ shrink v
    shrink (Rank v)               = map Rank               $ shrink v
    shrink (Ratio v)              = map Ratio              $ shrink v
    shrink (Rects v)              = map Rects              $ shrink v
    shrink (Regular v)            = map Regular            $ shrink v
    shrink (ReMinCross v)         = map ReMinCross         $ shrink v
    shrink (RepulsiveForce v)     = map RepulsiveForce     $ shrink v
    shrink (Root v)               = map Root               $ shrink v
    shrink (Rotate v)             = map Rotate             $ shrink v
    shrink (SameHead v)           = map SameHead           $ shrinkString v
    shrink (SameTail v)           = map SameTail           $ shrinkString v
    shrink (SamplePoints v)       = map SamplePoints       $ shrink v
    shrink (SearchSize v)         = map SearchSize         $ shrink v
    shrink (Sep v)                = map Sep                $ shrink v
    shrink (ShapeFile v)          = map ShapeFile          $ shrinkString v
    shrink (Shape v)              = map Shape              $ shrink v
    shrink (ShowBoxes v)          = map ShowBoxes          $ shrink v
    shrink (Sides v)              = map Sides              $ shrink v
    shrink (Size v)               = map Size               $ shrink v
    shrink (Skew v)               = map Skew               $ shrink v
    shrink (Smoothing v)          = map Smoothing          $ shrink v
    shrink (SortV v)              = map SortV              $ shrink v
    shrink (Splines v)            = map Splines            $ shrink v
    shrink (Start v)              = map Start              $ shrink v
    shrink (StyleSheet v)         = map StyleSheet         $ shrinkString v
    shrink (Style v)              = map Style              $ nonEmptyShrinks v
    shrink (TailURL v)            = map TailURL            $ shrink v
    shrink (TailClip v)           = map TailClip           $ shrink v
    shrink (TailLabel v)          = map TailLabel          $ shrink v
    shrink (TailPort v)           = map TailPort           $ shrink v
    shrink (TailTarget v)         = map TailTarget         $ shrinkString v
    shrink (TailTooltip v)        = map TailTooltip        $ shrinkString v
    shrink (Target v)             = map Target             $ shrinkString v
    shrink (Tooltip v)            = map Tooltip            $ shrinkString v
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
  shrink = shrinkIntegral

instance Arbitrary URL where
  arbitrary = liftM UStr
              $ suchThat arbString (not . null)

  shrink (UStr ustr) = map UStr $ nonEmptyShrinks ustr

instance Arbitrary ArrowType where
  arbitrary = liftM AType
              -- Arrow specifications have between 1 and 4 elements.
              $ resize 4 arbList

  shrink (AType as) = map AType $ nonEmptyShrinks as

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

  shrink (RatioOnly d) = map RatioOnly $ shrink d
  shrink (RatioPassCount d i) = do ds <- shrink d
                                   is <- shrink i
                                   return $ RatioPassCount ds is

instance Arbitrary Rect where
  arbitrary = liftM2 Rect arbitrary arbitrary

  shrink (Rect p1 p2) = do p1s <- shrink p1
                           p2s <- shrink p2
                           return $ Rect p1s p2s

instance Arbitrary Point where
  -- Pretty sure points have to be positive...
  arbitrary = oneof [ liftM2 Point  posArbitrary posArbitrary
                    , liftM (uncurry PointD)
                      $ suchThat (liftM2 (,) posArbitrary posArbitrary)
                                 notBothInt
                    ]

  shrink (Point  v1 v2) = do v1s <- shrink v1
                             v2s <- shrink v2
                             return $ Point v1s v2s
  shrink (PointD v1 v2) = do v1s <- shrink v1
                             v2s <- shrink v2
                             guard $ notBothInt (v1s,v2s)
                             return $ PointD v1s v2s

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

  shrink (DVal d) = map DVal $ shrink d
  shrink (PVal p) = map PVal $ shrink p

instance Arbitrary ModeType where
  arbitrary = arbBounded

instance Arbitrary Model where
  arbitrary = arbBounded

instance Arbitrary Label where
  arbitrary = oneof [ liftM StrLabel arbString
                    , liftM URLLabel arbitrary
                    ]

  shrink (StrLabel str) = map StrLabel $ shrinkString str
  shrink (URLLabel url) = map URLLabel $ shrink url

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

  shrink (PrismOverlap mi) = map PrismOverlap $ shrink mi
  shrink _                 = []

instance Arbitrary LayerList where
  arbitrary = do fs <- arbLayerName
                 oths <- listOf $ liftM2 (,) arbLayerSep arbLayerName
                 return $ LL fs oths

  -- This should be improved to try actually shrinking the seps and names.
  -- shrink (LL _ [(_,fs')]) = [LL fs' []]
  -- shrink (LL fs oths)     = map (LL fs) $ shrink oths

instance Arbitrary LayerRange where
  arbitrary = oneof [ liftM  LRID arbitrary
                    , liftM3 LRS arbitrary arbLayerSep arbitrary
                    ]

  shrink (LRID nm)     = map LRID $ shrink nm
  shrink (LRS l1 _ l2) = [LRID l1, LRID l2]

instance Arbitrary LayerID where
  arbitrary = oneof [ return AllLayers
                    , liftM LRInt arbitrary
                    , liftM LRName $ suchThat arbLayerName lrnameCheck
                    ]

  shrink AllLayers   = []
  shrink (LRInt i)   = map LRInt $ shrink i
  shrink (LRName nm) = map LRName
                       . filter lrnameCheck
                       $ shrinkString nm

lrnameCheck :: String -> Bool
lrnameCheck = (/=) "all"

instance Arbitrary OutputMode where
  arbitrary = arbBounded

instance Arbitrary Pack where
  arbitrary = oneof [ return DoPack
                    , return DontPack
                    , liftM PackMargin arbitrary
                    ]

  shrink (PackMargin m) = map PackMargin $ shrink m
  shrink _              = []

instance Arbitrary PackMode where
  arbitrary = oneof [ return PackNode
                    , return PackClust
                    , return PackGraph
                    , liftM3 PackArray arbitrary arbitrary arbitrary
                    ]

  shrink (PackArray c u mi) = map (PackArray c u) $ shrink mi
  shrink _                  = []

instance Arbitrary Pos where
  arbitrary = oneof [ liftM PointPos arbitrary
                      -- A single spline with only one point overall
                      -- is just a point...
                    , liftM SplinePos $ suchThat arbList validSplineList
                    ]
    where

  shrink (PointPos p)   = map PointPos $ shrink p
  shrink (SplinePos ss) = map SplinePos . filter validSplineList
                          $ nonEmptyShrinks ss

validSplineList                              :: [Spline] -> Bool
validSplineList [Spline Nothing Nothing [_]] = False
validSplineList _                            = True

instance Arbitrary Spline where
  arbitrary = liftM3 Spline arbitrary arbitrary
              -- list of points must have length of 1 mod 3
              $ suchThat arbitrary ((==) 1 . flip mod 3 . length)

  shrink (Spline Nothing Nothing [p]) = map (Spline Nothing Nothing . return)
                                        $ shrink p
  -- We're not going to be shrinking the points in the list; just
  -- making sure that its length is === 1 mod 3
  shrink (Spline ms me ps) = do mss <- shrinkM ms
                                mes <- shrinkM me
                                pss <- rem2 ps
                                return $ Spline mss mes pss
    where

      rem1 []     = []
      rem1 (a:as) = as : map (a:) (rem1 as)

      rem2 = nub . concatMap rem1 . rem1

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

  shrink (NodeName nm) = map NodeName $ shrinkString nm
  shrink _             = []

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

  shrink StartStyle{} = [] -- No shrinks for STStyle
  shrink (StartSeed ss) = map StartSeed $ shrink ss
  shrink (StartStyleSeed st ss) = map (StartStyleSeed st) $ shrink ss

instance Arbitrary STStyle where
  arbitrary = arbBounded

instance Arbitrary StyleItem where
  arbitrary = liftM2 SItem arbitrary (listOf arbStyleName)

  -- Can't use this because of what shrink on the individual strings
  -- might do.
  -- shrink (SItem sn opts) = map (SItem sn) $ shrink opts

instance Arbitrary StyleName where
  arbitrary = oneof [ defaultStyles
                    , liftM DD $ suchThat arbStyleName notDefault
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
      notDefault = flip notElem [ "dashed"
                                , "dotted"
                                , "solid"
                                , "bold"
                                , "invis"
                                , "filled"
                                , "diagonals"
                                , "rounded"
                                ]

instance Arbitrary PortPos where
  arbitrary = liftM PP arbitrary

instance Arbitrary CompassPoint where
  arbitrary = arbBounded

instance Arbitrary ViewPort where
  arbitrary = liftM4 VP arbitrary arbitrary arbitrary arbitrary

  shrink (VP w h z f) = case sVPs of
                          [_] -> []
                          _   -> sVPs
    where
      sVPs = do ws <- shrink w
                hs <- shrink h
                zs <- shrink z
                fs <- shrinkM f
                return $ VP ws hs zs fs

instance Arbitrary FocusType where
  arbitrary = oneof [ liftM XY arbitrary
                    , liftM NodeFocus $ suchThat arbString (all ((/=) ','))
                    ]

  shrink (XY p)          = map XY $ shrink p
  shrink (NodeFocus str) = map NodeFocus $ shrinkString str

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

  shrink (AspectRatio r) = map (AspectRatio . fromPositive)
                           . shrink $ Positive r
  shrink _               = []

instance Arbitrary ColorScheme where
  arbitrary = oneof [ return X11
                    , liftM2 BrewerScheme arbitrary arbitrary
                    ]

  shrink (BrewerScheme nm v) = map (BrewerScheme nm) $ shrink v
  shrink _                   = []

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

  shrink (RGB r g b)     = do rs <- shrink r
                              gs <- shrink g
                              bs <- shrink b
                              return $ RGB rs gs bs
  shrink (RGBA r g b a)  = RGB r g b
                           : do rs <- shrink r
                                gs <- shrink g
                                bs <- shrink b
                                as <- shrink a
                                return $ RGBA rs gs bs as
  shrink (BrewerColor c) = map BrewerColor $ shrink c
  shrink _               = [] -- Shrinking 0<=h,s,v<=1 does nothing

instance Arbitrary X11Color where
  arbitrary = arbBounded

-- -----------------------------------------------------------------------------
-- Helper Functions

fromPositive              :: Positive a -> a
fromPositive (Positive a) = a

posArbitrary :: (Arbitrary a, Num a, Ord a) => Gen a
posArbitrary = liftM fromPositive arbitrary

arbString :: Gen String
arbString = suchThat (liftM (map toLower) genStr) isValid
  where
    genStr = listOf1 $ elements strChr
    strChr = ['a'..'z'] ++ ['0'..'9'] ++ ['\'', '"', ' ', '\t', '(', ')'
                                         , ',', ':', '.']
    isValid "true"  = False
    isValid "false" = False
    isValid str     = notNumStr str

shrinkString :: String -> [String]
shrinkString = filter notNumStr . nonEmptyShrinks

notNumStr :: String -> Bool
notNumStr = not . isNumString

arbBounded :: (Bounded a, Enum a) => Gen a
arbBounded = elements [minBound .. maxBound]

arbLayerSep :: Gen [Char]
arbLayerSep = listOf1 (elements defLayerSep)

arbLayerName :: Gen String
arbLayerName = suchThat arbString (all notLayerSep)

arbStyleName :: Gen String
arbStyleName = suchThat arbString (all notBrackCom)
  where
    notBrackCom = flip notElem ['(', ')', ',', ' ']

arbList :: (Arbitrary a) => Gen [a]
arbList = listOf1 arbitrary

nonEmptyShrinks :: (Arbitrary a) => [a] -> [[a]]
nonEmptyShrinks = filter (not . null) . shrinkList

-- Shrink lists with more than one value only by removing values, not
-- by shrinking individual items.
shrinkList     :: (Arbitrary a) => [a] -> [[a]]
shrinkList [a] = map return $ shrink a
shrinkList as  = rm (length as) as
  where
    rm 0 _  = []
    rm 1 _  = [[]]
    rm n xs = xs1
            : xs2
            : ( [ xs1' ++ xs2 | xs1' <- rm n1 xs1, not (null xs1') ]
                `ilv` [ xs1 ++ xs2' | xs2' <- rm n2 xs2, not (null xs2') ]
              )
     where
      n1  = n `div` 2
      xs1 = take n1 xs
      n2  = n - n1
      xs2 = drop n1 xs

    []     `ilv` ys     = ys
    xs     `ilv` []     = xs
    (x:xs) `ilv` (y:ys) = x : y : (xs `ilv` ys)

-- When a Maybe value is a sub-component, and we need shrink to return
-- a value.
shrinkM         :: (Arbitrary a) => Maybe a -> [Maybe a]
shrinkM Nothing = [Nothing]
shrinkM j       = shrink j

shrinkL    :: (Arbitrary a) => [a] -> [[a]]
shrinkL xs = case shrinkList xs of
               []  -> [xs]
               xs' -> xs'

notInt   :: Double -> Bool
notInt d = fromIntegral (round d :: Int) /= d

notBothInt         :: (Double, Double) -> Bool
notBothInt (p1,p2) = notInt p1 && notInt p2

returnCheck     :: (Eq a) => a -> a -> [a]
returnCheck o n = if o == n
                  then []
                  else [n]
