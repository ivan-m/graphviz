{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP, OverloadedStrings #-}

{- |
   Module      : Data.GraphViz.Testing.Instances.Attributes
   Description : Attribute instances for Arbitrary.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com
 -}
module Data.GraphViz.Testing.Instances.Attributes
       ( arbGraphAttrs
       , arbSubGraphAttrs
       , arbClusterAttrs
       , arbNodeAttrs
       , arbEdgeAttrs
       ) where

import Data.GraphViz.Testing.Instances.Helpers

import           Data.GraphViz.Attributes.Colors.Brewer
import           Data.GraphViz.Attributes.Colors.SVG    (SVGColor)
import           Data.GraphViz.Attributes.Colors.X11    (X11Color)
import           Data.GraphViz.Attributes.Complete
import qualified Data.GraphViz.Attributes.HTML          as Html
import           Data.GraphViz.Attributes.Internal      (compassLookup)
import           Data.GraphViz.Internal.State           (initialState,
                                                         layerListSep, layerSep)
import           Data.GraphViz.Internal.Util            (bool)

import Test.QuickCheck

import           Control.Monad   (liftM, liftM2, liftM3, liftM4)
import           Data.List       (delete, groupBy, nub)
import qualified Data.Map        as Map
import           Data.Text.Lazy  (Text)
import qualified Data.Text.Lazy  as T
import           System.FilePath (searchPathSeparator)

#if !MIN_VERSION_QuickCheck(2,9,0)
import Data.GraphViz.Internal.Util (createVersion)
import Data.Version                (Version(..))
#endif

-- -----------------------------------------------------------------------------
-- Defining Arbitrary instances for Attributes

arbGraphAttrs :: Gen Attributes
arbGraphAttrs = arbAttrs usedByGraphs

arbSubGraphAttrs :: Gen Attributes
arbSubGraphAttrs = arbAttrs usedBySubGraphs

arbClusterAttrs :: Gen Attributes
arbClusterAttrs = arbAttrs usedByClusters

arbNodeAttrs :: Gen Attributes
arbNodeAttrs = arbAttrs usedByNodes

arbEdgeAttrs :: Gen Attributes
arbEdgeAttrs = arbAttrs usedByEdges

arbAttrs   :: (Attribute -> Bool) -> Gen Attributes
arbAttrs p = liftM (filter p) arbList

instance Arbitrary Attribute where
  arbitrary = oneof [ liftM Damping arbitrary
                    , liftM K arbitrary
                    , liftM URL arbitrary
                    , liftM Area arbitrary
                    , liftM ArrowHead arbitrary
                    , liftM ArrowSize arbitrary
                    , liftM ArrowTail arbitrary
                    , liftM Background arbitrary
                    , liftM BoundingBox arbitrary
                    , liftM BgColor arbList
                    , liftM Center arbitrary
                    , liftM ClusterRank arbitrary
                    , liftM Color arbList
                    , liftM ColorScheme arbitrary
                    , liftM Comment arbitrary
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
                    , liftM EdgeTarget arbitrary
                    , liftM EdgeTooltip arbitrary
                    , liftM Epsilon arbitrary
                    , liftM ESep arbitrary
                    , liftM FillColor arbList
                    , liftM FixedSize arbitrary
                    , liftM FontColor arbitrary
                    , liftM FontName arbitrary
                    , liftM FontNames arbitrary
                    , liftM FontPath arbitrary
                    , liftM FontSize arbitrary
                    , liftM ForceLabels arbitrary
                    , liftM GradientAngle arbitrary
                    , liftM Group arbitrary
                    , liftM HeadURL arbitrary
                    , liftM Head_LP arbitrary
                    , liftM HeadClip arbitrary
                    , liftM HeadLabel arbitrary
                    , liftM HeadPort arbitrary
                    , liftM HeadTarget arbitrary
                    , liftM HeadTooltip arbitrary
                    , liftM Height arbitrary
                    , liftM ID arbitrary
                    , liftM Image arbitrary
                    , liftM ImagePath arbitrary
                    , liftM ImageScale arbitrary
                    , liftM InputScale arbitrary
                    , liftM Label arbitrary
                    , liftM LabelURL arbitrary
                    , liftM LabelScheme arbitrary
                    , liftM LabelAngle arbitrary
                    , liftM LabelDistance arbitrary
                    , liftM LabelFloat arbitrary
                    , liftM LabelFontColor arbitrary
                    , liftM LabelFontName arbitrary
                    , liftM LabelFontSize arbitrary
                    , liftM LabelJust arbitrary
                    , liftM LabelLoc arbitrary
                    , liftM LabelTarget arbitrary
                    , liftM LabelTooltip arbitrary
                    , liftM Landscape arbitrary
                    , liftM Layer arbitrary
                    , liftM LayerListSep arbitrary
                    , liftM Layers arbitrary
                    , liftM LayerSelect arbitrary
                    , liftM LayerSep arbitrary
                    , liftM Layout arbitrary
                    , liftM Len arbitrary
                    , liftM Levels arbitrary
                    , liftM LevelsGap arbitrary
                    , liftM LHead arbitrary
                    , liftM LHeight arbitrary
                    , liftM LPos arbitrary
                    , liftM LTail arbitrary
                    , liftM LWidth arbitrary
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
                    , liftM NoTranslate arbitrary
                    , liftM Nslimit arbitrary
                    , liftM Nslimit1 arbitrary
                    , liftM Ordering arbitrary
                    , liftM Orientation arbitrary
                    , liftM OutputOrder arbitrary
                    , liftM Overlap arbitrary
                    , liftM OverlapScaling arbitrary
                    , liftM OverlapShrink arbitrary
                    , liftM Pack arbitrary
                    , liftM PackMode arbitrary
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
                    , liftM RankSep arbList
                    , liftM Ratio arbitrary
                    , liftM Rects arbList
                    , liftM Regular arbitrary
                    , liftM ReMinCross arbitrary
                    , liftM RepulsiveForce arbitrary
                    , liftM Root arbitrary
                    , liftM Rotate arbitrary
                    , liftM Rotation arbitrary
                    , liftM SameHead arbitrary
                    , liftM SameTail arbitrary
                    , liftM SamplePoints arbitrary
                    , liftM Scale arbitrary
                    , liftM SearchSize arbitrary
                    , liftM Sep arbitrary
                    , liftM Shape arbitrary
                    , liftM ShowBoxes arbitrary
                    , liftM Sides arbitrary
                    , liftM Size arbitrary
                    , liftM Skew arbitrary
                    , liftM Smoothing arbitrary
                    , liftM SortV arbitrary
                    , liftM Splines arbitrary
                    , liftM Start arbitrary
                    , liftM Style arbList
                    , liftM StyleSheet arbitrary
                    , liftM TailURL arbitrary
                    , liftM Tail_LP arbitrary
                    , liftM TailClip arbitrary
                    , liftM TailLabel arbitrary
                    , liftM TailPort arbitrary
                    , liftM TailTarget arbitrary
                    , liftM TailTooltip arbitrary
                    , liftM Target arbitrary
                    , liftM Tooltip arbitrary
                    , liftM TrueColor arbitrary
                    , liftM Vertices arbList
                    , liftM ViewPort arbitrary
                    , liftM VoroMargin arbitrary
                    , liftM Weight arbitrary
                    , liftM Width arbitrary
                    , liftM XDotVersion arbitrary
                    , liftM XLabel arbitrary
                    , liftM XLP arbitrary
                    , liftM2 UnknownAttribute (suchThat arbIDString validUnknown) arbitrary
                    ]

  shrink (Damping v)            = map Damping             $ shrink v
  shrink (K v)                  = map K                   $ shrink v
  shrink (URL v)                = map URL                 $ shrink v
  shrink (Area v)               = map Area                $ shrink v
  shrink (ArrowHead v)          = map ArrowHead           $ shrink v
  shrink (ArrowSize v)          = map ArrowSize           $ shrink v
  shrink (ArrowTail v)          = map ArrowTail           $ shrink v
  shrink (Background v)         = map Background          $ shrink v
  shrink (BoundingBox v)        = map BoundingBox         $ shrink v
  shrink (BgColor v)            = map BgColor             $ nonEmptyShrinks v
  shrink (Center v)             = map Center              $ shrink v
  shrink (ClusterRank v)        = map ClusterRank         $ shrink v
  shrink (Color v)              = map Color               $ nonEmptyShrinks v
  shrink (ColorScheme v)        = map ColorScheme         $ shrink v
  shrink (Comment v)            = map Comment             $ shrink v
  shrink (Compound v)           = map Compound            $ shrink v
  shrink (Concentrate v)        = map Concentrate         $ shrink v
  shrink (Constraint v)         = map Constraint          $ shrink v
  shrink (Decorate v)           = map Decorate            $ shrink v
  shrink (DefaultDist v)        = map DefaultDist         $ shrink v
  shrink (Dim v)                = map Dim                 $ shrink v
  shrink (Dimen v)              = map Dimen               $ shrink v
  shrink (Dir v)                = map Dir                 $ shrink v
  shrink (DirEdgeConstraints v) = map DirEdgeConstraints  $ shrink v
  shrink (Distortion v)         = map Distortion          $ shrink v
  shrink (DPI v)                = map DPI                 $ shrink v
  shrink (EdgeURL v)            = map EdgeURL             $ shrink v
  shrink (EdgeTarget v)         = map EdgeTarget          $ shrink v
  shrink (EdgeTooltip v)        = map EdgeTooltip         $ shrink v
  shrink (Epsilon v)            = map Epsilon             $ shrink v
  shrink (ESep v)               = map ESep                $ shrink v
  shrink (FillColor v)          = map FillColor           $ nonEmptyShrinks v
  shrink (FixedSize v)          = map FixedSize           $ shrink v
  shrink (FontColor v)          = map FontColor           $ shrink v
  shrink (FontName v)           = map FontName            $ shrink v
  shrink (FontNames v)          = map FontNames           $ shrink v
  shrink (FontPath v)           = map FontPath            $ shrink v
  shrink (FontSize v)           = map FontSize            $ shrink v
  shrink (ForceLabels v)        = map ForceLabels         $ shrink v
  shrink (GradientAngle v)      = map GradientAngle       $ shrink v
  shrink (Group v)              = map Group               $ shrink v
  shrink (HeadURL v)            = map HeadURL             $ shrink v
  shrink (Head_LP v)            = map Head_LP             $ shrink v
  shrink (HeadClip v)           = map HeadClip            $ shrink v
  shrink (HeadLabel v)          = map HeadLabel           $ shrink v
  shrink (HeadPort v)           = map HeadPort            $ shrink v
  shrink (HeadTarget v)         = map HeadTarget          $ shrink v
  shrink (HeadTooltip v)        = map HeadTooltip         $ shrink v
  shrink (Height v)             = map Height              $ shrink v
  shrink (ID v)                 = map ID                  $ shrink v
  shrink (Image v)              = map Image               $ shrink v
  shrink (ImagePath v)          = map ImagePath           $ shrink v
  shrink (ImageScale v)         = map ImageScale          $ shrink v
  shrink (InputScale v)         = map InputScale          $ shrink v
  shrink (Label v)              = map Label               $ shrink v
  shrink (LabelURL v)           = map LabelURL            $ shrink v
  shrink (LabelScheme v)        = map LabelScheme         $ shrink v
  shrink (LabelAngle v)         = map LabelAngle          $ shrink v
  shrink (LabelDistance v)      = map LabelDistance       $ shrink v
  shrink (LabelFloat v)         = map LabelFloat          $ shrink v
  shrink (LabelFontColor v)     = map LabelFontColor      $ shrink v
  shrink (LabelFontName v)      = map LabelFontName       $ shrink v
  shrink (LabelFontSize v)      = map LabelFontSize       $ shrink v
  shrink (LabelJust v)          = map LabelJust           $ shrink v
  shrink (LabelLoc v)           = map LabelLoc            $ shrink v
  shrink (LabelTarget v)        = map LabelTarget         $ shrink v
  shrink (LabelTooltip v)       = map LabelTooltip        $ shrink v
  shrink (Landscape v)          = map Landscape           $ shrink v
  shrink (Layer v)              = map Layer               $ shrink v
  shrink (LayerListSep v)       = map LayerListSep        $ shrink v
  shrink (Layers v)             = map Layers              $ shrink v
  shrink (LayerSelect v)        = map LayerSelect         $ shrink v
  shrink (LayerSep v)           = map LayerSep            $ shrink v
  shrink (Layout v)             = map Layout              $ shrink v
  shrink (Len v)                = map Len                 $ shrink v
  shrink (Levels v)             = map Levels              $ shrink v
  shrink (LevelsGap v)          = map LevelsGap           $ shrink v
  shrink (LHead v)              = map LHead               $ shrink v
  shrink (LHeight v)            = map LHeight             $ shrink v
  shrink (LPos v)               = map LPos                $ shrink v
  shrink (LTail v)              = map LTail               $ shrink v
  shrink (LWidth v)             = map LWidth              $ shrink v
  shrink (Margin v)             = map Margin              $ shrink v
  shrink (MaxIter v)            = map MaxIter             $ shrink v
  shrink (MCLimit v)            = map MCLimit             $ shrink v
  shrink (MinDist v)            = map MinDist             $ shrink v
  shrink (MinLen v)             = map MinLen              $ shrink v
  shrink (Mode v)               = map Mode                $ shrink v
  shrink (Model v)              = map Model               $ shrink v
  shrink (Mosek v)              = map Mosek               $ shrink v
  shrink (NodeSep v)            = map NodeSep             $ shrink v
  shrink (NoJustify v)          = map NoJustify           $ shrink v
  shrink (Normalize v)          = map Normalize           $ shrink v
  shrink (NoTranslate v)        = map NoTranslate         $ shrink v
  shrink (Nslimit v)            = map Nslimit             $ shrink v
  shrink (Nslimit1 v)           = map Nslimit1            $ shrink v
  shrink (Ordering v)           = map Ordering            $ shrink v
  shrink (Orientation v)        = map Orientation         $ shrink v
  shrink (OutputOrder v)        = map OutputOrder         $ shrink v
  shrink (Overlap v)            = map Overlap             $ shrink v
  shrink (OverlapScaling v)     = map OverlapScaling      $ shrink v
  shrink (OverlapShrink v)      = map OverlapShrink       $ shrink v
  shrink (Pack v)               = map Pack                $ shrink v
  shrink (PackMode v)           = map PackMode            $ shrink v
  shrink (Pad v)                = map Pad                 $ shrink v
  shrink (Page v)               = map Page                $ shrink v
  shrink (PageDir v)            = map PageDir             $ shrink v
  shrink (PenColor v)           = map PenColor            $ shrink v
  shrink (PenWidth v)           = map PenWidth            $ shrink v
  shrink (Peripheries v)        = map Peripheries         $ shrink v
  shrink (Pin v)                = map Pin                 $ shrink v
  shrink (Pos v)                = map Pos                 $ shrink v
  shrink (QuadTree v)           = map QuadTree            $ shrink v
  shrink (Quantum v)            = map Quantum             $ shrink v
  shrink (Rank v)               = map Rank                $ shrink v
  shrink (RankDir v)            = map RankDir             $ shrink v
  shrink (RankSep v)            = map RankSep             $ nonEmptyShrinks v
  shrink (Ratio v)              = map Ratio               $ shrink v
  shrink (Rects v)              = map Rects               $ nonEmptyShrinks v
  shrink (Regular v)            = map Regular             $ shrink v
  shrink (ReMinCross v)         = map ReMinCross          $ shrink v
  shrink (RepulsiveForce v)     = map RepulsiveForce      $ shrink v
  shrink (Root v)               = map Root                $ shrink v
  shrink (Rotate v)             = map Rotate              $ shrink v
  shrink (Rotation v)           = map Rotation            $ shrink v
  shrink (SameHead v)           = map SameHead            $ shrink v
  shrink (SameTail v)           = map SameTail            $ shrink v
  shrink (SamplePoints v)       = map SamplePoints        $ shrink v
  shrink (Scale v)              = map Scale               $ shrink v
  shrink (SearchSize v)         = map SearchSize          $ shrink v
  shrink (Sep v)                = map Sep                 $ shrink v
  shrink (Shape v)              = map Shape               $ shrink v
  shrink (ShowBoxes v)          = map ShowBoxes           $ shrink v
  shrink (Sides v)              = map Sides               $ shrink v
  shrink (Size v)               = map Size                $ shrink v
  shrink (Skew v)               = map Skew                $ shrink v
  shrink (Smoothing v)          = map Smoothing           $ shrink v
  shrink (SortV v)              = map SortV               $ shrink v
  shrink (Splines v)            = map Splines             $ shrink v
  shrink (Start v)              = map Start               $ shrink v
  shrink (Style v)              = map Style               $ nonEmptyShrinks v
  shrink (StyleSheet v)         = map StyleSheet          $ shrink v
  shrink (TailURL v)            = map TailURL             $ shrink v
  shrink (Tail_LP v)            = map Tail_LP             $ shrink v
  shrink (TailClip v)           = map TailClip            $ shrink v
  shrink (TailLabel v)          = map TailLabel           $ shrink v
  shrink (TailPort v)           = map TailPort            $ shrink v
  shrink (TailTarget v)         = map TailTarget          $ shrink v
  shrink (TailTooltip v)        = map TailTooltip         $ shrink v
  shrink (Target v)             = map Target              $ shrink v
  shrink (Tooltip v)            = map Tooltip             $ shrink v
  shrink (TrueColor v)          = map TrueColor           $ shrink v
  shrink (Vertices v)           = map Vertices            $ nonEmptyShrinks v
  shrink (ViewPort v)           = map ViewPort            $ shrink v
  shrink (VoroMargin v)         = map VoroMargin          $ shrink v
  shrink (Weight v)             = map Weight              $ shrink v
  shrink (Width v)              = map Width               $ shrink v
  shrink (XDotVersion v)        = map XDotVersion         $ shrink v
  shrink (XLabel v)             = map XLabel              $ shrink v
  shrink (XLP v)                = map XLP                 $ shrink v
  shrink (UnknownAttribute a v) = liftM2 UnknownAttribute (liftM (filter validUnknown) shrink a) (shrink v)
{- delete to here -}

instance Arbitrary GraphvizCommand where
  arbitrary = arbBounded

instance Arbitrary ArrowType where
  arbitrary = liftM AType
              -- Arrow specifications have between 1 and 4 elements.
              $ sized (\ s -> resize (min s 4) arbList)

  shrink (AType as) = map AType $ nonEmptyShrinks as

instance Arbitrary ArrowShape where
  arbitrary = arbBounded

instance Arbitrary ArrowModifier where
  arbitrary = liftM2 ArrMod arbitrary arbitrary

instance Arbitrary ArrowFill where
  arbitrary = arbBounded

instance Arbitrary ArrowSide where
  arbitrary = arbBounded

instance Arbitrary LabelScheme where
  arbitrary = arbBounded

instance Arbitrary Rect where
  arbitrary = liftM2 Rect point2D point2D

  shrink (Rect p1 p2) = do p1s <- shrink p1
                           p2s <- shrink p2
                           return $ Rect p1s p2s

instance Arbitrary Point where
  -- Pretty sure points have to be positive...
  arbitrary = liftM4 Point posArbitrary posArbitrary posZ arbitrary
    where
      posZ = liftArbitrary posArbitrary

  shrink p = do x' <- shrink $ xCoord p
                y' <- shrink $ yCoord p
                z' <- shrinkM $ zCoord p
                return $ Point x' y' z' False

point2D :: Gen Point
point2D = liftM2 createPoint posArbitrary posArbitrary

instance Arbitrary ClusterMode where
  arbitrary = arbBounded

instance Arbitrary DirType where
  arbitrary = arbBounded

instance Arbitrary DEConstraints where
  arbitrary = arbBounded

instance Arbitrary DPoint where
  arbitrary = oneof [ liftM DVal arbitrary
                    , liftM PVal point2D
                    ]

  shrink (DVal d) = map DVal $ shrink d
  shrink (PVal p) = map PVal $ shrink p

instance Arbitrary SVGFontNames where
  arbitrary = arbBounded

instance Arbitrary GraphSize where
  arbitrary = liftM3 GSize arbitrary arbitrary arbitrary

  shrink gs = do w' <- shrink $ width gs
                 h' <- shrinkM $ height gs
                 return $ GSize w' h' False

instance Arbitrary ModeType where
  arbitrary = arbBounded

instance Arbitrary Model where
  arbitrary = arbBounded

instance Arbitrary Label where
  arbitrary = oneof [ liftM StrLabel arbitrary
                    , liftM HtmlLabel arbitrary
                    , liftM RecordLabel $ suchThat arbList notStr
                    ]

  shrink (StrLabel str)   = map StrLabel $ shrink str
  shrink (HtmlLabel html) = map HtmlLabel $ shrink html
  shrink (RecordLabel fs) = map RecordLabel . filter notStr $ listShrink fs

notStr                :: RecordFields -> Bool
notStr [FieldLabel{}] = False -- Just in case
notStr _              = True

arbField     :: Bool -> Int -> Gen RecordField
arbField b s = resize s'
               . oneof
               . bool id ((:) genFlipped) b
               $ [ liftM2 LabelledTarget arbitrary arbitrary
                 , liftM PortName arbitrary
                 , liftM FieldLabel arbitrary
                 ]
  where
    genFlipped = liftM FlipFields
                 $ listOf1 (sized $ arbField False)
    s' = min 3 s

instance Arbitrary RecordField where
  arbitrary = sized (arbField True)

  shrink (LabelledTarget f l) = [PortName f, FieldLabel l]
  shrink (PortName f)         = map PortName $ shrink f
  shrink (FieldLabel l)       = map FieldLabel $ shrink l
  shrink (FlipFields fs)      = map FlipFields $ listShrink fs

instance Arbitrary Overlap where
  arbitrary = oneof [ simpleOverlap
                    , liftM PrismOverlap arbitrary
                    ]
    where
      -- Have to do this by hand since Overlap can't have Bounded and
      -- Enum instances
      simpleOverlap = elements [ KeepOverlaps
                               , ScaleOverlaps
                               , ScaleXYOverlaps
                               , VoronoiOverlap
                               , CompressOverlap
                               , VpscOverlap
                               , IpsepOverlap
                               ]

  shrink (PrismOverlap mi) = map PrismOverlap $ shrink mi
  shrink _                 = []

instance Arbitrary LayerSep where
  -- Since Arbitrary isn't stateful, we can't generate an arbitrary
  -- one because of arbLayerName
  arbitrary = return . LSep . T.pack $ layerSep initialState

instance Arbitrary LayerListSep where
  -- Since Arbitrary isn't stateful, we can't generate an arbitrary
  -- one because of arbLayerName
  arbitrary = return . LLSep . T.pack $ layerListSep initialState

instance Arbitrary LayerList where
  arbitrary = liftM LL $ listOf1 arbName
    where
      arbName = suchThat arbitrary isLayerName

      isLayerName LRName{} = True
      isLayerName _        = False

  shrink (LL ll) = map LL $ nonEmptyShrinks ll

instance Arbitrary LayerRangeElem where
  arbitrary = oneof [ liftM LRID arbitrary
                    , liftM2 LRS arbitrary arbitrary
                    ]

  shrink (LRID nm)   = map LRID $ shrink nm
  shrink (LRS l1 l2) = [LRID l1, LRID l2]

instance Arbitrary LayerID where
  arbitrary = oneof [ return AllLayers
                    , liftM LRInt arbitrary
                    , liftM LRName $ suchThat arbLayerName lrnameCheck
                    ]

  shrink AllLayers   = []
  shrink (LRInt i)   = map LRInt $ shrink i
  shrink (LRName nm) = map LRName
                       . filter lrnameCheck
                       $ shrink nm

lrnameCheck :: Text -> Bool
lrnameCheck = (/=) "all"

instance Arbitrary Order where
  arbitrary = arbBounded

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
                    , liftM NodeName arbitrary
                    ]

  shrink (NodeName nm) = map NodeName $ shrink nm
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

  shrink StartStyle{}           = [] -- No shrinks for STStyle
  shrink (StartSeed ss)         = map StartSeed $ shrink ss
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
                               , Striped
                               , Wedged
                               , Diagonals
                               , Rounded
                               , Tapered
                               , Radial
                               ]
      notDefault = flip notElem [ "dashed"
                                , "dotted"
                                , "solid"
                                , "bold"
                                , "invis"
                                , "filled"
                                , "striped"
                                , "wedged"
                                , "diagonals"
                                , "rounded"
                                , "tapered"
                                , "radial"
                                ]

instance Arbitrary PortPos where
  arbitrary = oneof [ liftM2 LabelledPort arbitrary arbitrary
                    , liftM CompassPoint arbitrary
                    ]

  shrink (LabelledPort pn mc) = map (flip LabelledPort mc) $ shrink pn
  shrink _                    = []

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
                    , liftM NodeFocus $ suchThat arbitrary (T.all ((/=) ','))
                    ]

  shrink (XY p)          = map XY $ shrink p
  shrink (NodeFocus str) = map NodeFocus $ shrink str

instance Arbitrary VerticalPlacement where
  arbitrary = arbBounded

instance Arbitrary Paths where
  arbitrary = liftM Paths $ listOf1 arbFilePath

  shrink (Paths ps) = map Paths $ nonEmptyShrinks' ps

arbFilePath :: Gen FilePath
arbFilePath = suchThat arbString (searchPathSeparator `notElem`)

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
                    , liftM Brewer arbitrary
                    ]

  shrink (Brewer bs) = map Brewer $ shrink bs
  shrink _           = []

instance Arbitrary BrewerScheme where
  arbitrary = liftM2 BScheme arbitrary arbitrary -- Not /quite/ right, but close enough

  shrink (BScheme nm l) = map (BScheme nm) $ shrink l

instance Arbitrary BrewerName where
  arbitrary = arbBounded

instance Arbitrary Color where
  arbitrary = oneof [ liftM3 RGB  arbitrary arbitrary arbitrary
                    , liftM4 RGBA arbitrary arbitrary arbitrary arbitrary
                    , liftM3 HSV  zeroOne zeroOne zeroOne
                    , liftM X11Color arbitrary
                    , liftM SVGColor arbitrary
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

-- | No guarantees are made as to sanity of generated weightings.
instance Arbitrary WeightedColor where
  arbitrary = liftM2 WC arbitrary arbitrary

  -- No color shrinks to itself, so no sanity checking needed.
  shrink (WC c mw) = do c' <- shrink c
                        mw' <- shrink mw
                        return $ WC c' mw'

instance Arbitrary X11Color where
  arbitrary = arbBounded

instance Arbitrary SVGColor where
  arbitrary = arbBounded

-- | Not quite right as the values can get too high/low, but should
--   suffice for printing/parsing purposes.
instance Arbitrary BrewerColor where
  arbitrary = liftM2 BC arbitrary arbitrary

  shrink (BC s c) = map (BC s) $ shrink c

instance Arbitrary Html.Label where
  arbitrary = sized $ arbHtml True

  shrink (Html.Text txts) = map Html.Text $ listShrink txts
  shrink (Html.Table tbl) = map Html.Table $ shrink tbl

-- Note: for the most part, Html.Label values are very repetitive (and
-- furthermore, they end up chewing a large amount of memory).  As
-- such, use resize to limit how large the actual Html.Label values
-- become.
arbHtml         :: Bool -> Int -> Gen Html.Label
arbHtml table s = resize' $ frequency options
  where
    s' = min 2 s
    resize' = if not table
              then resize s'
              else id
    allowTable = if table
                 then (:) (1, arbTbl)
                 else id
    arbTbl = liftM Html.Table arbitrary
    options = allowTable [ (20, liftM Html.Text . sized $ arbHtmlTexts table) ]

arbHtmlTexts       :: Bool -> Int -> Gen Html.Text
arbHtmlTexts fnt s = liftM simplifyHtmlText
                     . resize s'
                     . listOf1
                     . sized
                     $ arbHtmlText fnt
  where
    s' = min s 5

-- When parsing, all textual characters are parsed together; thus,
-- make sure we generate them like that.
simplifyHtmlText :: Html.Text -> Html.Text
simplifyHtmlText = map head . groupBy sameType
  where
    sameType Html.Str{}           Html.Str{}           = True
    sameType Html.Newline{}       Html.Newline{}       = True
    sameType Html.Font{}          Html.Font{}          = True
    sameType (Html.Format fmt1 _) (Html.Format fmt2 _) = fmt1 == fmt2
    sameType _                    _                    = False

instance Arbitrary Html.TextItem where
  arbitrary = sized $ arbHtmlText True

  shrink (Html.Str str)        = map Html.Str . filter (not . T.null) . map T.strip $ shrink str
  shrink (Html.Newline as)     = map Html.Newline $ shrinkHtmlAttrs as
  shrink hf@(Html.Font as txt) = do as' <- shrinkHtmlAttrs as
                                    txt' <- shrinkL txt
                                    returnCheck hf $ Html.Font as' txt'
  shrink (Html.Format _ txt)   = txt

arbHtmlText        :: Bool -> Int -> Gen Html.TextItem
arbHtmlText font s = frequency options
  where
    allowFonts = if font
                 then (++) recHtmlText
                 else id
    s' = min 2 s
    arbRec = resize s' . sized $ arbHtmlTexts False
    recHtmlText = [ (1, liftM2 Html.Font arbHtmlAttrs arbRec)
                  , (3, liftM2 Html.Format arbitrary arbRec)
                  ]
    options = allowFonts [ (10, liftM Html.Str (suchThat (liftM T.strip arbitrary) (not . T.null)))
                         , (10, liftM Html.Newline arbHtmlAttrs)
                         ]

instance Arbitrary Html.Format where
  arbitrary = arbBounded

instance Arbitrary Html.Table where
  arbitrary = liftM3 Html.HTable (liftArbitrary arbHtmlAttrs) arbHtmlAttrs (sized arbRows)
    where
      arbRows s = resize (min s 10) arbList

  shrink (Html.HTable fas as rs) = liftM3 Html.HTable shrinkFont (shrinkHtmlAttrs as) (shrinkL rs)
    where
      shrinkFont = liftShrink shrinkHtmlAttrs fas

#if !MIN_VERSION_QuickCheck(2,10,0)
liftArbitrary :: Gen a -> Gen (Maybe a)
liftArbitrary gen = frequency [(1, return Nothing), (3, liftM Just gen)]

liftShrink :: (a -> [a]) -> Maybe a -> [Maybe a]
liftShrink shr (Just x) = Nothing : map Just (shr x)
liftShrink _   Nothing  = []
#endif

instance Arbitrary Html.Row where
  arbitrary = frequency [ (5, liftM Html.Cells arbList)
                        , (1, return Html.HorizontalRule)
                        ]

  shrink hr@(Html.Cells cs) = delete hr . map Html.Cells $ shrinkL cs
  shrink _                  = []

instance Arbitrary Html.Cell where
  arbitrary = frequency [ (5, liftM2 Html.LabelCell arbitrary . sized $ arbHtml False)
                        , (3, liftM2 Html.ImgCell arbitrary arbitrary)
                        , (1, return Html.VerticalRule)
                        ]

  shrink lc@(Html.LabelCell as h) = do as' <- shrink as
                                       h' <- shrink h
                                       returnCheck lc $ Html.LabelCell as' h'
  shrink (Html.ImgCell as ic) = map (Html.ImgCell as) $ shrink ic
  shrink _                    = []

instance Arbitrary Html.Img where
  arbitrary = liftM Html.Img arbitrary

arbHtmlAttrs :: Gen Html.Attributes
arbHtmlAttrs = sized (\s -> resize (min 5 s) arbitrary)

shrinkHtmlAttrs :: Html.Attributes -> [Html.Attributes]
shrinkHtmlAttrs = listShrink

instance Arbitrary Html.Attribute where
  arbitrary = oneof [ liftM Html.Align arbitrary
                    , liftM Html.BAlign arbitrary
                    , liftM Html.BGColor arbitrary
                    , liftM Html.Border arbitrary
                    , liftM Html.CellBorder arbitrary
                    , liftM Html.CellPadding arbitrary
                    , liftM Html.CellSpacing arbitrary
                    , liftM Html.Color arbitrary
                    , liftM Html.ColSpan arbitrary
                    , liftM Html.Columns arbitrary
                    , liftM Html.Face arbitrary
                    , liftM Html.FixedSize arbitrary
                    , liftM Html.GradientAngle arbitrary
                    , liftM Html.Height arbitrary
                    , liftM Html.HRef arbitrary
                    , liftM Html.ID arbitrary
                    , liftM Html.PointSize arbitrary
                    , liftM Html.Port arbitrary
                    , liftM Html.Rows arbitrary
                    , liftM Html.RowSpan arbitrary
                    , liftM Html.Scale arbitrary
                    , liftM Html.Sides (fmap nub (sized (\s -> resize (min s 4) arbitrary))) -- Will never have more than 4 values
                    , liftM Html.Src arbString
                    , liftM Html.Style arbitrary
                    , liftM Html.Target arbitrary
                    , liftM Html.Title arbitrary
                    , liftM Html.VAlign arbitrary
                    , liftM Html.Width arbitrary
                    ]

  shrink (Html.Align v)         = map Html.Align         $ shrink v
  shrink (Html.BAlign v)        = map Html.BAlign        $ shrink v
  shrink (Html.BGColor v)       = map Html.BGColor       $ shrink v
  shrink (Html.Border v)        = map Html.Border        $ shrink v
  shrink (Html.CellBorder v)    = map Html.CellBorder    $ shrink v
  shrink (Html.CellPadding v)   = map Html.CellPadding   $ shrink v
  shrink (Html.CellSpacing v)   = map Html.CellSpacing   $ shrink v
  shrink (Html.Color v)         = map Html.Color         $ shrink v
  shrink (Html.ColSpan v)       = map Html.ColSpan       $ shrink v
  shrink (Html.Columns v)       = map Html.Columns       $ shrink v
  shrink (Html.Face v)          = map Html.Face          $ shrink v
  shrink (Html.FixedSize v)     = map Html.FixedSize     $ shrink v
  shrink (Html.GradientAngle v) = map Html.GradientAngle $ shrink v
  shrink (Html.Height v)        = map Html.Height        $ shrink v
  shrink (Html.HRef v)          = map Html.HRef          $ shrink v
  shrink (Html.ID v)            = map Html.ID            $ shrink v
  shrink (Html.PointSize v)     = map Html.PointSize     $ shrink v
  shrink (Html.Port v)          = map Html.Port          $ shrink v
  shrink (Html.Rows v)          = map Html.Rows          $ shrink v
  shrink (Html.RowSpan v)       = map Html.RowSpan       $ shrink v
  shrink (Html.Scale v)         = map Html.Scale         $ shrink v
  shrink (Html.Sides v)         = map Html.Sides         $ listShrink' v
  shrink (Html.Src v)           = map Html.Src           $ shrinkString v
  shrink (Html.Style v)         = map Html.Style         $ shrink v
  shrink (Html.Target v)        = map Html.Target        $ shrink v
  shrink (Html.Title v)         = map Html.Title         $ shrink v
  shrink (Html.VAlign v)        = map Html.VAlign        $ shrink v
  shrink (Html.Width v)         = map Html.Width         $ shrink v

instance Arbitrary Html.Scale where
  arbitrary = arbBounded

instance Arbitrary Html.Align where
  arbitrary = arbBounded

instance Arbitrary Html.VAlign where
  arbitrary = arbBounded

instance Arbitrary Html.CellFormat where
  arbitrary = arbBounded

instance Arbitrary Html.Side where
  arbitrary = arbBounded

instance Arbitrary Html.Style where
  arbitrary = arbBounded

instance Arbitrary PortName where
  arbitrary = liftM PN
              $ suchThat arbitrary (liftM2 (&&) (T.all (/=':')) notCP)

  shrink = map PN . filter notCP . shrink . portName

notCP :: Text -> Bool
notCP = flip Map.notMember compassLookup

instance Arbitrary Number where
  arbitrary = frequency [ (3, liftM Dbl $ suchThat arbitrary notInt)
                        , (1, liftM Int arbitrary)
                        ]

  shrink (Int i) = map Int $ shrink i
  shrink (Dbl d) = map Dbl $ filter notInt $ shrink d

instance Arbitrary Normalized where
  arbitrary = oneof [ elements [IsNormalized, NotNormalized]
                    , liftM NormalizedAngle arbitrary
                    ]

  shrink (NormalizedAngle a) = map NormalizedAngle $ shrink a
  shrink _                   = []

#if !MIN_VERSION_QuickCheck(2,9,0)
instance Arbitrary Version where
  arbitrary = liftM (createVersion . map getPositive) arbList

  shrink = map createVersion . nonEmptyShrinks . versionBranch
#endif

instance Arbitrary NodeSize where
  arbitrary = arbBounded

{-

As of Graphviz 2.36.0 this was commented out; as such it might come
back, so leave this here in case we need it again.

instance Arbitrary AspectType where
  arbitrary = oneof [ liftM  RatioOnly arbitrary
                    , liftM2 RatioPassCount arbitrary posArbitrary
                    ]

  shrink (RatioOnly d) = map RatioOnly $ shrink d
  shrink (RatioPassCount d i) = do ds <- shrink d
                                   is <- shrink i
                                   return $ RatioPassCount ds is

-}
