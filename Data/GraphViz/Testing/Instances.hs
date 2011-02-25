{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Data.GraphViz.Parsing(isNumString)

import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Internal(compassLookup)
import Data.GraphViz.Types
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Util(bool)
import Data.GraphViz.State(initialState, layerSep)

import Test.QuickCheck

import Data.List(nub, delete, groupBy)
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import qualified Data.Text.Lazy as T
import Data.Text.Lazy(Text)
import Control.Monad(liftM, liftM2, liftM3, liftM4)

-- -----------------------------------------------------------------------------
-- Defining Arbitrary instances for the overall types

instance (Eq a, Arbitrary a) => Arbitrary (DotGraph a) where
  arbitrary = liftM4 DotGraph arbitrary arbitrary arbitrary arbitrary

  shrink (DotGraph str dir gid stmts) = map (DotGraph str dir gid)
                                        $ shrink stmts

instance Arbitrary GraphID where
  arbitrary = oneof [ liftM Str arbitrary
                    , liftM Int arbitrary
                    , liftM Dbl $ suchThat arbitrary notInt
                    ]

  shrink (Str s) = map Str $ shrink s
  shrink (Int i) = map Int $ shrink i
  shrink (Dbl d) = map Dbl $ filter notInt $ shrink d

instance (Eq a, Arbitrary a) => Arbitrary (DotStatements a) where
  arbitrary = sized (arbDS True)

  shrink ds@(DotStmts gas sgs ns es) = do gas' <- shrinkL gas
                                          sgs' <- shrinkL sgs
                                          ns' <- shrinkL ns
                                          es' <- shrinkL es
                                          returnCheck ds
                                            $ DotStmts gas' sgs' ns' es'

-- | If 'True', generate 'DotSubGraph's; otherwise don't.
arbDS           :: (Arbitrary a, Eq a) => Bool -> Int -> Gen (DotStatements a)
arbDS haveSGs s = liftM4 DotStmts arbitrary genSGs arbitrary arbitrary
  where
    s' = min s 2
    genSGs = if haveSGs
             then resize s' arbitrary
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
  arbitrary = liftM3 DotSG arbitrary arbitrary (sized $ arbDS False)

  shrink (DotSG isCl mid stmts) = map (DotSG isCl mid) $ shrink stmts

instance (Arbitrary a) => Arbitrary (DotNode a) where
  arbitrary = liftM2 DotNode arbitrary arbitrary

  shrink (DotNode n as) = map (DotNode n) $ shrinkList as

instance (Arbitrary a) => Arbitrary (DotEdge a) where
  arbitrary = liftM3 DotEdge arbitrary arbitrary arbitrary

  shrink (DotEdge f t as) = map (DotEdge f t) $ shrinkList as

-- -----------------------------------------------------------------------------
-- Defining Arbitrary instances for the generalised types

instance (Eq a, Arbitrary a) => Arbitrary (GDotGraph a) where
  arbitrary = liftM4 GDotGraph arbitrary arbitrary arbitrary genGDStmts

  shrink (GDotGraph str dir gid stmts) = map (GDotGraph str dir gid)
                                         $ shrinkGDStmts stmts

genGDStmts :: (Eq a, Arbitrary a) => Gen (GDotStatements a)
genGDStmts = sized (arbGDS True)

shrinkGDStmts :: (Eq a, Arbitrary a) => GDotStatements a -> [GDotStatements a]
shrinkGDStmts gds
  | len == 1  = map Seq.singleton . shrink $ Seq.index gds 0
  | otherwise = [gds1, gds2]
    where
      len = Seq.length gds
      -- Halve the sequence
      (gds1, gds2) = (len `div` 2) `Seq.splitAt` gds

instance (Eq a, Arbitrary a) => Arbitrary (GDotStatement a) where
  -- Don't want as many sub-graphs as nodes, etc.
  arbitrary = frequency [ (3, liftM GA arbitrary)
                        , (1, liftM SG arbitrary)
                        , (5, liftM DN arbitrary)
                        , (7, liftM DE arbitrary)
                        ]

  shrink (GA ga) = map GA $ shrink ga
  shrink (SG sg) = map SG $ shrink sg
  shrink (DN dn) = map DN $ shrink dn
  shrink (DE de) = map DE $ shrink de

-- | If 'True', generate 'GDotSubGraph's; otherwise don't.
arbGDS           :: (Arbitrary a, Eq a) => Bool -> Int -> Gen (GDotStatements a)
arbGDS haveSGs s = liftM (Seq.fromList . checkSGs) (resize s' arbList)
  where
    checkSGs = if haveSGs
               then id
               else filter notSG
    notSG SG{} = False
    notSG _    = True

    s' = min s 10


instance (Eq a, Arbitrary a) => Arbitrary (GDotSubGraph a) where
  arbitrary = liftM3 GDotSG arbitrary arbitrary (sized $ arbGDS False)

  shrink (GDotSG isCl mid stmts) = map (GDotSG isCl mid) $ shrinkGDStmts stmts

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
                      , liftM ClusterRank arbitrary
                      , liftM ColorScheme arbitrary
                      , liftM Color arbList
                      , liftM Comment arbitrary
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
                      , liftM EdgeTarget arbitrary
                      , liftM EdgeTooltip arbitrary
                      , liftM Epsilon arbitrary
                      , liftM ESep arbitrary
                      , liftM FillColor arbitrary
                      , liftM FixedSize arbitrary
                      , liftM FontColor arbitrary
                      , liftM FontName arbitrary
                      , liftM FontNames arbitrary
                      , liftM FontPath arbitrary
                      , liftM FontSize arbitrary
                      , liftM Group arbitrary
                      , liftM HeadURL arbitrary
                      , liftM HeadClip arbitrary
                      , liftM HeadLabel arbitrary
                      , liftM HeadPort arbitrary
                      , liftM HeadTarget arbitrary
                      , liftM HeadTooltip arbitrary
                      , liftM Height arbitrary
                      , liftM ID arbitrary
                      , liftM Image arbitrary
                      , liftM ImageScale arbitrary
                      , liftM LabelURL arbitrary
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
                      , liftM Label arbitrary
                      , liftM Landscape arbitrary
                      , liftM LayerSep arbitrary
                      , liftM Layers arbitrary
                      , liftM Layer arbitrary
                      , liftM Layout arbitrary
                      , liftM Len arbitrary
                      , liftM LevelsGap arbitrary
                      , liftM Levels arbitrary
                      , liftM LHead arbitrary
                      , liftM LPos arbitrary
                      , liftM LTail arbitrary
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
                      , liftM Ordering arbitrary
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
                      , liftM RankSep arbList
                      , liftM Rank arbitrary
                      , liftM Ratio arbitrary
                      , liftM Rects arbitrary
                      , liftM Regular arbitrary
                      , liftM ReMinCross arbitrary
                      , liftM RepulsiveForce arbitrary
                      , liftM Root arbitrary
                      , liftM Rotate arbitrary
                      , liftM SameHead arbitrary
                      , liftM SameTail arbitrary
                      , liftM SamplePoints arbitrary
                      , liftM SearchSize arbitrary
                      , liftM Sep arbitrary
                      , liftM ShapeFile arbitrary
                      , liftM Shape arbitrary
                      , liftM ShowBoxes arbitrary
                      , liftM Sides arbitrary
                      , liftM Size arbitrary
                      , liftM Skew arbitrary
                      , liftM Smoothing arbitrary
                      , liftM SortV arbitrary
                      , liftM Splines arbitrary
                      , liftM Start arbitrary
                      , liftM StyleSheet arbitrary
                      , liftM Style arbList
                      , liftM TailURL arbitrary
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
                      , liftM Z arbitrary
                      , liftM2 UnknownAttribute (suchThat arbIDString validUnknown) arbitrary
                      ]

    shrink (Damping v)            = map Damping             $ shrink v
    shrink (K v)                  = map K                   $ shrink v
    shrink (URL v)                = map URL                 $ shrink v
    shrink (ArrowHead v)          = map ArrowHead           $ shrink v
    shrink (ArrowSize v)          = map ArrowSize           $ shrink v
    shrink (ArrowTail v)          = map ArrowTail           $ shrink v
    shrink (Aspect v)             = map Aspect              $ shrink v
    shrink (Bb v)                 = map Bb                  $ shrink v
    shrink (BgColor v)            = map BgColor             $ shrink v
    shrink (Center v)             = map Center              $ shrink v
    shrink (ClusterRank v)        = map ClusterRank         $ shrink v
    shrink (ColorScheme v)        = map ColorScheme         $ shrink v
    shrink (Color v)              = map Color               $ nonEmptyShrinks v
    shrink (Comment v)            = map Comment             $ shrink v
    shrink (Compound v)           = map Compound            $ shrink v
    shrink (Concentrate v)        = map Concentrate         $ shrink v
    shrink (Constraint v)         = map Constraint          $ shrink v
    shrink (Decorate v)           = map Decorate            $ shrink v
    shrink (DefaultDist v)        = map DefaultDist         $ shrink v
    shrink (Dimen v)              = map Dimen               $ shrink v
    shrink (Dim v)                = map Dim                 $ shrink v
    shrink (Dir v)                = map Dir                 $ shrink v
    shrink (DirEdgeConstraints v) = map DirEdgeConstraints  $ shrink v
    shrink (Distortion v)         = map Distortion          $ shrink v
    shrink (DPI v)                = map DPI                 $ shrink v
    shrink (EdgeURL v)            = map EdgeURL             $ shrink v
    shrink (EdgeTarget v)         = map EdgeTarget          $ shrink v
    shrink (EdgeTooltip v)        = map EdgeTooltip         $ shrink v
    shrink (Epsilon v)            = map Epsilon             $ shrink v
    shrink (ESep v)               = map ESep                $ shrink v
    shrink (FillColor v)          = map FillColor           $ shrink v
    shrink (FixedSize v)          = map FixedSize           $ shrink v
    shrink (FontColor v)          = map FontColor           $ shrink v
    shrink (FontName v)           = map FontName            $ shrink v
    shrink (FontNames v)          = map FontNames           $ shrink v
    shrink (FontPath v)           = map FontPath            $ shrink v
    shrink (FontSize v)           = map FontSize            $ shrink v
    shrink (Group v)              = map Group               $ shrink v
    shrink (HeadURL v)            = map HeadURL             $ shrink v
    shrink (HeadClip v)           = map HeadClip            $ shrink v
    shrink (HeadLabel v)          = map HeadLabel           $ shrink v
    shrink (HeadPort v)           = map HeadPort            $ shrink v
    shrink (HeadTarget v)         = map HeadTarget          $ shrink v
    shrink (HeadTooltip v)        = map HeadTooltip         $ shrink v
    shrink (Height v)             = map Height              $ shrink v
    shrink (ID v)                 = map ID                  $ shrink v
    shrink (Image v)              = map Image               $ shrink v
    shrink (ImageScale v)         = map ImageScale          $ shrink v
    shrink (LabelURL v)           = map LabelURL            $ shrink v
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
    shrink (Label v)              = map Label               $ shrink v
    shrink (Landscape v)          = map Landscape           $ shrink v
    shrink (LayerSep v)           = map LayerSep            $ shrink v
    shrink (Layers v)             = map Layers              $ shrink v
    shrink (Layer v)              = map Layer               $ shrink v
    shrink (Layout v)             = map Layout              $ shrink v
    shrink (Len v)                = map Len                 $ shrink v
    shrink (LevelsGap v)          = map LevelsGap           $ shrink v
    shrink (Levels v)             = map Levels              $ shrink v
    shrink (LHead v)              = map LHead               $ shrink v
    shrink (LPos v)               = map LPos                $ shrink v
    shrink (LTail v)              = map LTail               $ shrink v
    shrink (Margin v)             = map Margin              $ shrink v
    shrink (MaxIter v)            = map MaxIter             $ shrink v
    shrink (MCLimit v)            = map MCLimit             $ shrink v
    shrink (MinDist v)            = map MinDist             $ shrink v
    shrink (MinLen v)             = map MinLen              $ shrink v
    shrink (Model v)              = map Model               $ shrink v
    shrink (Mode v)               = map Mode                $ shrink v
    shrink (Mosek v)              = map Mosek               $ shrink v
    shrink (NodeSep v)            = map NodeSep             $ shrink v
    shrink (NoJustify v)          = map NoJustify           $ shrink v
    shrink (Normalize v)          = map Normalize           $ shrink v
    shrink (Nslimit1 v)           = map Nslimit1            $ shrink v
    shrink (Nslimit v)            = map Nslimit             $ shrink v
    shrink (Ordering v)           = map Ordering            $ shrink v
    shrink (Orientation v)        = map Orientation         $ shrink v
    shrink (OutputOrder v)        = map OutputOrder         $ shrink v
    shrink (OverlapScaling v)     = map OverlapScaling      $ shrink v
    shrink (Overlap v)            = map Overlap             $ shrink v
    shrink (PackMode v)           = map PackMode            $ shrink v
    shrink (Pack v)               = map Pack                $ shrink v
    shrink (Pad v)                = map Pad                 $ shrink v
    shrink (PageDir v)            = map PageDir             $ shrink v
    shrink (Page v)               = map Page                $ shrink v
    shrink (PenColor v)           = map PenColor            $ shrink v
    shrink (PenWidth v)           = map PenWidth            $ shrink v
    shrink (Peripheries v)        = map Peripheries         $ shrink v
    shrink (Pin v)                = map Pin                 $ shrink v
    shrink (Pos v)                = map Pos                 $ shrink v
    shrink (QuadTree v)           = map QuadTree            $ shrink v
    shrink (Quantum v)            = map Quantum             $ shrink v
    shrink (RankDir v)            = map RankDir             $ shrink v
    shrink (RankSep v)            = map RankSep             $ nonEmptyShrinks v
    shrink (Rank v)               = map Rank                $ shrink v
    shrink (Ratio v)              = map Ratio               $ shrink v
    shrink (Rects v)              = map Rects               $ shrink v
    shrink (Regular v)            = map Regular             $ shrink v
    shrink (ReMinCross v)         = map ReMinCross          $ shrink v
    shrink (RepulsiveForce v)     = map RepulsiveForce      $ shrink v
    shrink (Root v)               = map Root                $ shrink v
    shrink (Rotate v)             = map Rotate              $ shrink v
    shrink (SameHead v)           = map SameHead            $ shrink v
    shrink (SameTail v)           = map SameTail            $ shrink v
    shrink (SamplePoints v)       = map SamplePoints        $ shrink v
    shrink (SearchSize v)         = map SearchSize          $ shrink v
    shrink (Sep v)                = map Sep                 $ shrink v
    shrink (ShapeFile v)          = map ShapeFile           $ shrink v
    shrink (Shape v)              = map Shape               $ shrink v
    shrink (ShowBoxes v)          = map ShowBoxes           $ shrink v
    shrink (Sides v)              = map Sides               $ shrink v
    shrink (Size v)               = map Size                $ shrink v
    shrink (Skew v)               = map Skew                $ shrink v
    shrink (Smoothing v)          = map Smoothing           $ shrink v
    shrink (SortV v)              = map SortV               $ shrink v
    shrink (Splines v)            = map Splines             $ shrink v
    shrink (Start v)              = map Start               $ shrink v
    shrink (StyleSheet v)         = map StyleSheet          $ shrink v
    shrink (Style v)              = map Style               $ nonEmptyShrinks v
    shrink (TailURL v)            = map TailURL             $ shrink v
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
    shrink (Z v)                  = map Z                   $ shrink v
    shrink (UnknownAttribute a v) = liftM2 UnknownAttribute (liftM (filter validUnknown) shrink a) (shrink v)

validUnknown                      :: Text -> Bool
validUnknown "Damping"            = False
validUnknown "K"                  = False
validUnknown "URL"                = False
validUnknown "href"               = False
validUnknown "arrowhead"          = False
validUnknown "arrowsize"          = False
validUnknown "arrowtail"          = False
validUnknown "aspect"             = False
validUnknown "bb"                 = False
validUnknown "bgcolor"            = False
validUnknown "center"             = False
validUnknown "clusterrank"        = False
validUnknown "colorscheme"        = False
validUnknown "color"              = False
validUnknown "comment"            = False
validUnknown "compound"           = False
validUnknown "concentrate"        = False
validUnknown "constraint"         = False
validUnknown "decorate"           = False
validUnknown "defaultdist"        = False
validUnknown "dimen"              = False
validUnknown "dim"                = False
validUnknown "dir"                = False
validUnknown "diredgeconstraints" = False
validUnknown "distortion"         = False
validUnknown "dpi"                = False
validUnknown "resolution"         = False
validUnknown "edgeURL"            = False
validUnknown "edgehref"           = False
validUnknown "edgetarget"         = False
validUnknown "edgetooltip"        = False
validUnknown "epsilon"            = False
validUnknown "esep"               = False
validUnknown "fillcolor"          = False
validUnknown "fixedsize"          = False
validUnknown "fontcolor"          = False
validUnknown "fontname"           = False
validUnknown "fontnames"          = False
validUnknown "fontpath"           = False
validUnknown "fontsize"           = False
validUnknown "group"              = False
validUnknown "headURL"            = False
validUnknown "headhref"           = False
validUnknown "headclip"           = False
validUnknown "headlabel"          = False
validUnknown "headport"           = False
validUnknown "headtarget"         = False
validUnknown "headtooltip"        = False
validUnknown "height"             = False
validUnknown "id"                 = False
validUnknown "image"              = False
validUnknown "imagescale"         = False
validUnknown "labelURL"           = False
validUnknown "labelhref"          = False
validUnknown "labelangle"         = False
validUnknown "labeldistance"      = False
validUnknown "labelfloat"         = False
validUnknown "labelfontcolor"     = False
validUnknown "labelfontname"      = False
validUnknown "labelfontsize"      = False
validUnknown "labeljust"          = False
validUnknown "labelloc"           = False
validUnknown "labeltarget"        = False
validUnknown "labeltooltip"       = False
validUnknown "label"              = False
validUnknown "landscape"          = False
validUnknown "layersep"           = False
validUnknown "layers"             = False
validUnknown "layer"              = False
validUnknown "layout"             = False
validUnknown "len"                = False
validUnknown "levelsgap"          = False
validUnknown "levels"             = False
validUnknown "lhead"              = False
validUnknown "lp"                 = False
validUnknown "ltail"              = False
validUnknown "margin"             = False
validUnknown "maxiter"            = False
validUnknown "mclimit"            = False
validUnknown "mindist"            = False
validUnknown "minlen"             = False
validUnknown "model"              = False
validUnknown "mode"               = False
validUnknown "mosek"              = False
validUnknown "nodesep"            = False
validUnknown "nojustify"          = False
validUnknown "normalize"          = False
validUnknown "nslimit1"           = False
validUnknown "nslimit"            = False
validUnknown "ordering"           = False
validUnknown "orientation"        = False
validUnknown "outputorder"        = False
validUnknown "overlap_scaling"    = False
validUnknown "overlap"            = False
validUnknown "packmode"           = False
validUnknown "pack"               = False
validUnknown "pad"                = False
validUnknown "pagedir"            = False
validUnknown "page"               = False
validUnknown "pencolor"           = False
validUnknown "penwidth"           = False
validUnknown "peripheries"        = False
validUnknown "pin"                = False
validUnknown "pos"                = False
validUnknown "quadtree"           = False
validUnknown "quantum"            = False
validUnknown "rankdir"            = False
validUnknown "ranksep"            = False
validUnknown "rank"               = False
validUnknown "ratio"              = False
validUnknown "rects"              = False
validUnknown "regular"            = False
validUnknown "remincross"         = False
validUnknown "repulsiveforce"     = False
validUnknown "root"               = False
validUnknown "rotate"             = False
validUnknown "samehead"           = False
validUnknown "sametail"           = False
validUnknown "samplepoints"       = False
validUnknown "searchsize"         = False
validUnknown "sep"                = False
validUnknown "shapefile"          = False
validUnknown "shape"              = False
validUnknown "showboxes"          = False
validUnknown "sides"              = False
validUnknown "size"               = False
validUnknown "skew"               = False
validUnknown "smoothing"          = False
validUnknown "sortv"              = False
validUnknown "splines"            = False
validUnknown "start"              = False
validUnknown "stylesheet"         = False
validUnknown "style"              = False
validUnknown "tailURL"            = False
validUnknown "tailhref"           = False
validUnknown "tailclip"           = False
validUnknown "taillabel"          = False
validUnknown "tailport"           = False
validUnknown "tailtarget"         = False
validUnknown "tailtooltip"        = False
validUnknown "target"             = False
validUnknown "tooltip"            = False
validUnknown "truecolor"          = False
validUnknown "vertices"           = False
validUnknown "viewport"           = False
validUnknown "voro_margin"        = False
validUnknown "weight"             = False
validUnknown "width"              = False
validUnknown "z"                  = False
validUnknown "charset"            = False
validUnknown _                    = True
{- delete to here -}

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

instance Arbitrary AspectType where
  arbitrary = oneof [ liftM  RatioOnly arbitrary
                    , liftM2 RatioPassCount arbitrary posArbitrary
                    ]

  shrink (RatioOnly d) = map RatioOnly $ shrink d
  shrink (RatioPassCount d i) = do ds <- shrink d
                                   is <- shrink i
                                   return $ RatioPassCount ds is

instance Arbitrary Rect where
  arbitrary = liftM2 Rect point2D point2D

  shrink (Rect p1 p2) = do p1s <- shrink p1
                           p2s <- shrink p2
                           return $ Rect p1s p2s

instance Arbitrary Point where
  -- Pretty sure points have to be positive...
  arbitrary = liftM4 Point posArbitrary posArbitrary posZ arbitrary
    where
      posZ = frequency [(1, return Nothing), (3, liftM Just posArbitrary)]

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
  shrink (RecordLabel fs) = map RecordLabel . filter notStr $ shrinkList fs

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
  shrink (FlipFields fs)      = map FlipFields $ shrinkList fs

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

instance Arbitrary LayerSep where
  -- Since Arbitrary isn't stateful, we can't generate an arbitrary
  -- one because of arbLayerName
  arbitrary = return . LSep . T.pack $ layerSep initialState

instance Arbitrary LayerList where
  arbitrary = liftM LL $ listOf1 arbName
    where
      arbName = suchThat arbitrary isLayerName

      isLayerName LRName{} = True
      isLayerName _        = False

  shrink (LL ll) = map LL $ nonEmptyShrinks ll

instance Arbitrary LayerRange where
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
                      -- Not quite right as the values can get too
                      -- high/low, but should suffice for
                      -- printing/parsing purposes.
                    , liftM2 BrewerColor arbitrary arbitrary
                    ]
    where
      zeroOne = choose (0,1)

  shrink (RGB r g b)       = do rs <- shrink r
                                gs <- shrink g
                                bs <- shrink b
                                return $ RGB rs gs bs
  shrink (RGBA r g b a)    = RGB r g b
                             : do rs <- shrink r
                                  gs <- shrink g
                                  bs <- shrink b
                                  as <- shrink a
                                  return $ RGBA rs gs bs as
  shrink (BrewerColor s c) = map (BrewerColor s) $ shrink c
  shrink _                 = [] -- Shrinking 0<=h,s,v<=1 does nothing

instance Arbitrary X11Color where
  arbitrary = arbBounded

instance Arbitrary HtmlLabel where
  arbitrary = sized $ arbHtml True

  shrink ht@(HtmlText txts) = delete ht . map HtmlText $ shrinkL txts
  shrink (HtmlTable tbl)    = map HtmlTable $ shrink tbl

-- Note: for the most part, HtmlLabel values are very repetitive (and
-- furthermore, they end up chewing a large amount of memory).  As
-- such, use resize to limit how large the actual HtmlLabel values
-- become.
arbHtml         :: Bool -> Int -> Gen HtmlLabel
arbHtml table s = resize' $ frequency options
  where
    s' = min 2 s
    resize' = if not table
              then resize s'
              else id
    allowTable = if table
                 then (:) (1, arbTbl)
                 else id
    arbTbl = liftM HtmlTable arbitrary
    options = allowTable [ (20, liftM HtmlText . sized $ arbHtmlTexts table) ]

arbHtmlTexts       :: Bool -> Int -> Gen HtmlText
arbHtmlTexts fnt s = liftM simplifyHtmlText
                     . resize s'
                     . listOf1
                     . sized
                     $ arbHtmlText fnt
  where
    s' = min s 10

-- When parsing, all textual characters are parsed together; thus,
-- make sure we generate them like that.
simplifyHtmlText :: HtmlText -> HtmlText
simplifyHtmlText = map head . groupBy sameType
  where
    sameType HtmlStr{}     HtmlStr{}     = True
    sameType HtmlNewline{} HtmlNewline{} = True
    sameType HtmlFont{}    HtmlFont{}    = True
    sameType _             _             = False

instance Arbitrary HtmlTextItem where
  arbitrary = sized $ arbHtmlText True

  shrink (HtmlStr str) = map HtmlStr $ shrink str
  shrink (HtmlNewline as) = map HtmlNewline $ shrink as
  shrink hf@(HtmlFont as txt) = do as' <- shrink as
                                   txt' <- shrinkL txt
                                   returnCheck hf $ HtmlFont as' txt'

arbHtmlText        :: Bool -> Int -> Gen HtmlTextItem
arbHtmlText font s = frequency options
  where
    allowFonts = if font
                 then (:) (1, arbFont)
                 else id
    s' = min 2 s
    arbFont = liftM2 HtmlFont arbitrary . resize s' . sized $ arbHtmlTexts False
    options = allowFonts [ (10, liftM HtmlStr arbitrary)
                         , (10, liftM HtmlNewline arbitrary)
                         ]

instance Arbitrary HtmlTable where
  arbitrary = liftM3 HTable arbitrary arbitrary (sized arbRows)
    where
      arbRows s = resize (min s 10) arbList

  shrink (HTable fas as rs) = map (HTable fas as) $ shrinkL rs

instance Arbitrary HtmlRow where
  arbitrary = liftM HtmlRow arbList

  shrink hr@(HtmlRow cs) = delete hr . map HtmlRow $ shrinkL cs

instance Arbitrary HtmlCell where
  arbitrary = oneof [ liftM2 HtmlLabelCell arbitrary . sized $ arbHtml False
                    , liftM2 HtmlImgCell arbitrary arbitrary
                    ]

  shrink lc@(HtmlLabelCell as h) = do as' <- shrink as
                                      h' <- shrink h
                                      returnCheck lc $ HtmlLabelCell as' h'
  shrink (HtmlImgCell as ic) = map (HtmlImgCell as) $ shrink ic

instance Arbitrary HtmlImg where
  arbitrary = liftM HtmlImg arbitrary

instance Arbitrary HtmlAttribute where
  arbitrary = oneof [ liftM HtmlAlign arbitrary
                    , liftM HtmlBAlign arbitrary
                    , liftM HtmlBGColor arbitrary
                    , liftM HtmlBorder arbitrary
                    , liftM HtmlCellBorder arbitrary
                    , liftM HtmlCellPadding arbitrary
                    , liftM HtmlCellSpacing arbitrary
                    , liftM HtmlColor arbitrary
                    , liftM HtmlColSpan arbitrary
                    , liftM HtmlFace arbitrary
                    , liftM HtmlFixedSize arbitrary
                    , liftM HtmlHeight arbitrary
                    , liftM HtmlHRef arbitrary
                    , liftM HtmlPointSize arbitrary
                    , liftM HtmlPort arbitrary
                    , liftM HtmlRowSpan arbitrary
                    , liftM HtmlScale arbitrary
                    , liftM HtmlSrc arbString
                    , liftM HtmlTarget arbitrary
                    , liftM HtmlTitle arbitrary
                    , liftM HtmlVAlign arbitrary
                    , liftM HtmlWidth arbitrary
                    ]

  shrink (HtmlAlign v)       = map HtmlAlign       $ shrink v
  shrink (HtmlBAlign v)      = map HtmlBAlign      $ shrink v
  shrink (HtmlBGColor v)     = map HtmlBGColor     $ shrink v
  shrink (HtmlBorder v)      = map HtmlBorder      $ shrink v
  shrink (HtmlCellBorder v)  = map HtmlCellBorder  $ shrink v
  shrink (HtmlCellPadding v) = map HtmlCellPadding $ shrink v
  shrink (HtmlCellSpacing v) = map HtmlCellSpacing $ shrink v
  shrink (HtmlColor v)       = map HtmlColor       $ shrink v
  shrink (HtmlColSpan v)     = map HtmlColSpan     $ shrink v
  shrink (HtmlFace v)        = map HtmlFace        $ shrink v
  shrink (HtmlFixedSize v)   = map HtmlFixedSize   $ shrink v
  shrink (HtmlHeight v)      = map HtmlHeight      $ shrink v
  shrink (HtmlHRef v)        = map HtmlHRef        $ shrink v
  shrink (HtmlPointSize v)   = map HtmlPointSize   $ shrink v
  shrink (HtmlPort v)        = map HtmlPort        $ shrink v
  shrink (HtmlRowSpan v)     = map HtmlRowSpan     $ shrink v
  shrink (HtmlScale v)       = map HtmlScale       $ shrink v
  shrink (HtmlSrc v)         = map HtmlSrc         $ shrinkString v
  shrink (HtmlTarget v)      = map HtmlTarget      $ shrink v
  shrink (HtmlTitle v)       = map HtmlTitle       $ shrink v
  shrink (HtmlVAlign v)      = map HtmlVAlign      $ shrink v
  shrink (HtmlWidth v)       = map HtmlWidth       $ shrink v

instance Arbitrary HtmlScale where
  arbitrary = arbBounded

instance Arbitrary HtmlAlign where
  arbitrary = arbBounded

instance Arbitrary HtmlVAlign where
  arbitrary = arbBounded

instance Arbitrary PortName where
  arbitrary = liftM PN
              $ suchThat arbitrary (liftM2 (&&) (T.all (/=':')) notCP)

  shrink = map PN . filter notCP . shrink . portName

notCP :: Text -> Bool
notCP = flip Map.notMember compassLookup

-- -----------------------------------------------------------------------------
-- Helper Functions

instance Arbitrary Text where
  arbitrary = arbText

  shrink = filter validString
           . map T.pack . nonEmptyShrinks' . T.unpack

arbText :: Gen Text
arbText = suchThat genStr notBool
    where
      genStr = liftM2 T.cons (elements notDigits)
                             (liftM T.concat . listOf $ elements strChr)
      notDigits = ['a'..'z'] ++ ['\'', '"', ' ', '(', ')', ',', ':', '\\']
      strChr = map T.singleton $ notDigits ++ '.' : ['0'..'9']

arbString :: Gen String
arbString = liftM T.unpack arbitrary

fromPositive              :: Positive a -> a
fromPositive (Positive a) = a

posArbitrary :: (Arbitrary a, Num a, Ord a) => Gen a
posArbitrary = liftM fromPositive arbitrary

arbIDString :: Gen Text
arbIDString = suchThat genStr notBool
  where
    genStr = liftM2 T.cons (elements frst)
                           (liftM T.pack . listOf $ elements rest)
    frst = ['a'..'z'] ++ ['_']
    rest = frst ++ ['0'.. '9']

validString :: Text -> Bool
validString = liftM2 (&&) notBool notNumStr

notBool         :: Text -> Bool
notBool "true"  = False
notBool "false" = False
notBool _       = True

shrinkString :: String -> [String]
shrinkString = map T.unpack . shrink . T.pack

notNumStr :: Text -> Bool
notNumStr = not . isNumString

arbBounded :: (Bounded a, Enum a) => Gen a
arbBounded = elements [minBound .. maxBound]

arbLayerName :: Gen Text
arbLayerName = suchThat arbitrary (T.all notLayerSep)
  where
    defLayerSep = layerSep initialState
    notLayerSep = (`notElem` defLayerSep)

arbStyleName :: Gen Text
arbStyleName = suchThat arbitrary (T.all notBrackCom)
  where
    notBrackCom = flip notElem ['(', ')', ',', ' ']

arbList :: (Arbitrary a) => Gen [a]
arbList = listOf1 arbitrary

nonEmptyShrinks :: (Arbitrary a) => [a] -> [[a]]
nonEmptyShrinks = filter (not . null) . shrinkList

nonEmptyShrinks' :: [a] -> [[a]]
nonEmptyShrinks' = filter (not . null) . shrinkList'

-- Shrink lists with more than one value only by removing values, not
-- by shrinking individual items.
shrinkList     :: (Arbitrary a) => [a] -> [[a]]
shrinkList [a] = map return $ shrink a
shrinkList as  = shrinkList' as

-- Just shrink the size.
shrinkList'     :: [a] -> [[a]]
shrinkList' as  = rm (length as) as
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

returnCheck     :: (Eq a) => a -> a -> [a]
returnCheck o n = if o == n
                  then []
                  else [n]
