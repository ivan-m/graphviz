{- |
   Module      : Data.GraphViz.Attributes
   Description : Definition of the GraphViz attributes.
   Copyright   : (c) Matthew Sackman, Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines the various attributes that different parts of
   a GraphViz graph can have.  These attributes are based on the
   documentation found at:
     <http://graphviz.org/doc/info/attrs.html>
   For more information on usage, etc. please see that document.

   A summary of known current constraints/limitations/differences:

   * Parsing of Quoted Strings might not always work if they are a
     sub-part of another Attribute (e.g. a quoted name in LayerList).
     In fact, parsing with quotes is iffy for everything; specifically
     when they are and aren't allowed.

   * ColorScheme is ignored when parsing Color values

   * ColorList and PointfList are defined as actual lists (but
     LayerList is not).

   * A lot of values have a possible value of "none".  These now have
     custom constructors.  In fact, most constructors have been
     expanded upon to give an idea of what they represent rather than
     using generic terms.

   * PointF and Point have been combined, and feature support for pure
     Int-based co-ordinates as well as Double ones (i.e. no floating
     point-only points for Point).  The optional '!' and third value
     for Point are not available.

   * Rect uses two Point values.

   * The two labelloc attributes have been combined.

   * The defined LayerSep is not used to parse LayerRange or
     LayerList; the default is instead used.

   * SplineType has been replaced with [Spline].

   * Only polygon-based shapes are available.

   * Device-dependent StyleType values are not available.

   * PortPos only has CompassPoint option, not
     PortName[:CompassPoint] (since record shapes aren't allowed, and
     parsing HTML-like labels could be problematic).

   * Not every Attribute is fully documented/described.  In
     particular, a lot of them are listed as having a String value,
     when actually only certain Strings are allowed.
  -}

module Data.GraphViz.Attributes where

import Data.GraphViz.ParserCombinators

import Data.Char(isDigit, isHexDigit)
import Data.Word
import Numeric
import Control.Monad
import Data.Maybe

-- -----------------------------------------------------------------------------

{- |

   These attributes have been implemented in a /permissive/ manner:
   that is, rather than split them up based on which type of value
   they are allowed, they have all been included in the one data type,
   with functions to determine if they are indeed valid for what
   they're being applied to.

   To interpret the "/Valid for/" listings:

     [@G@] Valid for Graphs.

     [@C@] Valid for Clusters.

     [@S@] Valid for Sub-Graphs (and also Clusters).

     [@N@] Valid for Nodes.

     [@E@] Valid for Edges.

   Note also that the default values are taken from the specification
   page listed above, and might not correspond fully with the names of
   the permitted values.
-}
data Attribute
    = Damping Double                          -- ^ /Valid for/: G; /Default/: 0.99; /Minimum/: 0.0; /Notes/: neato only
    | K Double                                -- ^ /Valid for/: GC; /Default/: 0.3; /Minimum/: 0; /Notes/: sfdp, fdp only
    | URL URL                                 -- ^ /Valid for/: ENGC; /Default/: <none>; /Notes/: svg, postscript, map only
    | ArrowHead ArrowType                     -- ^ /Valid for/: E; /Default/: Normal
    | ArrowSize Double                        -- ^ /Valid for/: E; /Default/: 1.0; /Minimum/: 0.0
    | ArrowTail ArrowType                     -- ^ /Valid for/: E; /Default/: Normal
    | Aspect AspectType                       -- ^ /Valid for/: G; /Notes/: dot only
    | Bb Rect                                 -- ^ /Valid for/: G; /Notes/: write only
    | BgColor Color                           -- ^ /Valid for/: GC; /Default/: <none>
    | Center Bool                             -- ^ /Valid for/: G; /Default/: false
    | Charset String                          -- ^ /Valid for/: G; /Default/: "UTF-8"
    | ClusterRank ClusterMode                 -- ^ /Valid for/: G; /Default/: local; /Notes/: dot only
    | Color (Either Color [Color])            -- ^ /Valid for/: ENC; /Default/: black
    | ColorScheme String                      -- ^ /Valid for/: ENCG; /Default/: \"\"
    | Comment String                          -- ^ /Valid for/: ENG; /Default/: \"\"
    | Compound Bool                           -- ^ /Valid for/: G; /Default/: false; /Notes/: dot only
    | Concentrate Bool                        -- ^ /Valid for/: G; /Default/: false
    | Constraint Bool                         -- ^ /Valid for/: E; /Default/: true; /Notes/: dot only
    | Decorate Bool                           -- ^ /Valid for/: E; /Default/: false
    | DefaultDist Double                      -- ^ /Valid for/: G; /Default/: 1+(avg. len)*sqrt(|V|); /Minimum/: epsilon; /Notes/: neato only
    | Dim Int                                 -- ^ /Valid for/: G; /Default/: 2; /Minimum/: 2; /Notes/: sfdp, fdp, neato only
    | Dimen Int                               -- ^ /Valid for/: G; /Default/: 2; /Minimum/: 2; /Notes/: sfdp, fdp, neato only
    | Dir DirType                             -- ^ /Valid for/: E; /Default/: forward(directed)/none(undirected)
    | DirEdgeConstraints (Either String Bool) -- ^ /Valid for/: G; /Default/: false; /Notes/: neato only
    | Distortion Double                       -- ^ /Valid for/: N; /Default/: 0.0; /Minimum/: -100.0
    | DPI Double                              -- ^ /Valid for/: G; /Default/: 96.0 | 0.0; /Notes/: svg, bitmap output only
    | EdgeURL URL                             -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: svg, map only
    | EdgeHref URL                            -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: svg, map only
    | EdgeTarget String                       -- ^ /Valid for/: E; /Default/: <none>; /Notes/: svg, map only
    | EdgeTooltip String                      -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: svg, cmap only
    | Epsilon Double                          -- ^ /Valid for/: G; /Default/: .0001 * # nodes(mode == KK) | .0001(mode == major); /Notes/: neato only
    | ESep (Either Double Point)              -- ^ /Valid for/: G; /Default/: +3; /Notes/: not dot
    | FillColor Color                         -- ^ /Valid for/: NC; /Default/: lightgrey(nodes) | black(clusters)
    | FixedSize Bool                          -- ^ /Valid for/: N; /Default/: false
    | FontColor Color                         -- ^ /Valid for/: ENGC; /Default/: black
    | FontName String                         -- ^ /Valid for/: ENGC; /Default/: "Times-Roman"
    | FontNames String                        -- ^ /Valid for/: G; /Default/: \"\"; /Notes/: svg only
    | FontPath String                         -- ^ /Valid for/: G; /Default/: system-dependent
    | FontSize Double                         -- ^ /Valid for/: ENGC; /Default/: 14.0; /Minimum/: 1.0
    | Group String                            -- ^ /Valid for/: N; /Default/: \"\"; /Notes/: dot only
    | HeadURL URL                             -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: svg, map only
    | HeadClip Bool                           -- ^ /Valid for/: E; /Default/: true
    | HeadHref URL                            -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: svg, map only
    | HeadLabel (Either String URL)           -- ^ /Valid for/: E; /Default/: \"\"
    | HeadPort PortPos                        -- ^ /Valid for/: E; /Default/: center
    | HeadTarget QuotedString                 -- ^ /Valid for/: E; /Default/: <none>; /Notes/: svg, map only
    | HeadTooltip QuotedString                -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: svg, cmap only
    | Height Double                           -- ^ /Valid for/: N; /Default/: 0.5; /Minimum/: 0.02
    | Href URL                                -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: svg, postscript, map only
    | ID (Either String URL)                  -- ^ /Valid for/: GNE; /Default/: \"\"; /Notes/: svg, postscript, map only
    | Image String                            -- ^ /Valid for/: N; /Default/: \"\"
    | ImageScale ScaleType                    -- ^ /Valid for/: N; /Default/: false
    | Label (Either String URL)               -- ^ /Valid for/: ENGC; /Default/: "\N" (nodes) | \"\" (otherwise)
    | LabelURL URL                            -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: svg, map only
    | LabelAngle Double                       -- ^ /Valid for/: E; /Default/: -25.0; /Minimum/: -180.0
    | LabelDistance Double                    -- ^ /Valid for/: E; /Default/: 1.0; /Minimum/: 0.0
    | LabelFloat Bool                         -- ^ /Valid for/: E; /Default/: false
    | LabelFontColor Color                    -- ^ /Valid for/: E; /Default/: black
    | LabelFontName String                    -- ^ /Valid for/: E; /Default/: "Times-Roman"
    | LabelFontSize Double                    -- ^ /Valid for/: E; /Default/: 14.0; /Minimum/: 1.0
    | LabelHref URL                           -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: svg, map only
    | LabelJust Justification                 -- ^ /Valid for/: GC; /Default/: "c"
    | LabelLoc VerticalPlacement              -- ^ /Valid for/: GCN; /Default/: "t"(clusters) | "b"(root graphs) | "c"(clusters)
    | LabelTarget String                      -- ^ /Valid for/: E; /Default/: <none>; /Notes/: svg, map only
    | LabelTooltip String                     -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: svg, cmap only
    | Landscape Bool                          -- ^ /Valid for/: G; /Default/: false
    | Layer LayerRange                        -- ^ /Valid for/: EN; /Default/: \"\"
    | Layers LayerList                        -- ^ /Valid for/: G; /Default/: \"\"
    | LayerSep String                         -- ^ /Valid for/: G; /Default/: " :\\t"
    | Layout String                           -- ^ /Valid for/: G; /Default/: \"\"
    | Len Double                              -- ^ /Valid for/: E; /Default/: 1.0(neato)/0.3(fdp); /Notes/: fdp, neato only
    | Levels Int                              -- ^ /Valid for/: G; /Default/: MAXINT; /Minimum/: 0.0; /Notes/: sfdp only
    | LevelsGap Double                        -- ^ /Valid for/: G; /Default/: 0.0; /Notes/: neato only
    | LHead String                            -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: dot only
    | LP Point                                -- ^ /Valid for/: EGC; /Notes/: write only
    | LTail String                            -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: dot only
    | Margin (Either Double Point)            -- ^ /Valid for/: NG; /Default/: <device-dependent>
    | MaxIter Int                             -- ^ /Valid for/: G; /Default/: 100 * # nodes(mode == KK) | 200(mode == major) | 600(fdp); /Notes/: fdp, neato only
    | MCLimit Double                          -- ^ /Valid for/: G; /Default/: 1.0; /Notes/: dot only
    | MinDist Double                          -- ^ /Valid for/: G; /Default/: 1.0; /Minimum/: 0.0; /Notes/: circo only
    | MinLen Int                              -- ^ /Valid for/: E; /Default/: 1; /Minimum/: 0; /Notes/: dot only
    | Mode String                             -- ^ /Valid for/: G; /Default/: "major"; /Notes/: neato only
    | Model String                            -- ^ /Valid for/: G; /Default/: "shortpath"; /Notes/: neato only
    | Mosek Bool                              -- ^ /Valid for/: G; /Default/: false; /Notes/: neato only; requires the Mosek software
    | NodeSep Double                          -- ^ /Valid for/: G; /Default/: 0.25; /Minimum/: 0.02; /Notes/: dot only
    | NoJustify Bool                          -- ^ /Valid for/: GCNE; /Default/: false
    | Normalize Bool                          -- ^ /Valid for/: G; /Default/: false; /Notes/: not dot
    | Nslimit Double                          -- ^ /Valid for/: G; /Notes/: dot only
    | Nslimit1 Double                         -- ^ /Valid for/: G; /Notes/: dot only
    | Ordering String                         -- ^ /Valid for/: G; /Default/: \"\"; /Notes/: dot only
    | Orientation Double                      -- ^ /Valid for/: N; /Default/: 0.0; /Minimum/: 360.0
    | OrientationGraph String                 -- ^ /Valid for/: G; /Default/: \"\"; /Notes/: Landscape if "[lL]*" and rotate not defined
    | OutputOrder OutputMode                  -- ^ /Valid for/: G; /Default/: breadthfirst
    | Overlap (Either String Bool)            -- ^ /Valid for/: G; /Default/: true; /Notes/: not dot
    | OverlapScaling Double                   -- ^ /Valid for/: G; /Default/: -4; /Minimum/: -1.0e10; /Notes/: prism only
    | Pack (Either Bool Int)                  -- ^ /Valid for/: G; /Default/: false; /Notes/: not dot
    | PackMode PackMode                       -- ^ /Valid for/: G; /Default/: node; /Notes/: not dot
    | Pad (Either Double Point)               -- ^ /Valid for/: G; /Default/: 0.0555 (4 points)
    | Page Point                              -- ^ /Valid for/: G
    | PageDir PageDir                         -- ^ /Valid for/: G; /Default/: BL
    | PenColor Color                          -- ^ /Valid for/: C; /Default/: black
    | PenWidth Double                         -- ^ /Valid for/: CNE; /Default/: 1.0; /Minimum/: 0.0
    | Peripheries Int                         -- ^ /Valid for/: NC; /Default/: shape default(nodes) | 1(clusters); /Minimum/: 0
    | Pin Bool                                -- ^ /Valid for/: N; /Default/: false; /Notes/: fdp, neato only
    | Pos (Either Point [Spline])             -- ^ /Valid for/: EN
    | QuadTree (Either QuadType Bool)         -- ^ /Valid for/: G; /Default/: "normal"; /Notes/: sfdp only
    | Quantum Double                          -- ^ /Valid for/: G; /Default/: 0.0; /Minimum/: 0.0
    | Rank RankType                           -- ^ /Valid for/: S; /Notes/: dot only
    | RankDir RankDir                         -- ^ /Valid for/: G; /Default/: TB; /Notes/: dot only
    | Ranksep Double                          -- ^ /Valid for/: G; /Default/: 0.5(dot) | 1.0(twopi); /Minimum/: 0.02; /Notes/: twopi, dot only
    | Ratio Ratios                            -- ^ /Valid for/: G
    | Rects Rect                              -- ^ /Valid for/: N; /Notes/: write only
    | Regular Bool                            -- ^ /Valid for/: N; /Default/: false
    | ReMinCross Bool                         -- ^ /Valid for/: G; /Default/: false; /Notes/: dot only
    | RepulsiveForce Double                   -- ^ /Valid for/: G; /Default/: 1.0; /Minimum/: 0.0; /Notes/: sfdp only
    | Resolution Double                       -- ^ /Valid for/: G; /Default/: 96.0 | 0.0; /Notes/: svg, bitmap output only
    | Root (Either String Bool)               -- ^ /Valid for/: GN; /Default/: \"\"(graphs) | false(nodes); /Notes/: circo, twopi only
    | Rotate Int                              -- ^ /Valid for/: G; /Default/: 0
    | SameHead String                         -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: dot only
    | SameTail String                         -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: dot only
    | SamplePoints Int                        -- ^ /Valid for/: N; /Default/: 8(output) | 20(overlap and image maps)
    | SearchSize Int                          -- ^ /Valid for/: G; /Default/: 30; /Notes/: dot only
    | Sep (Either Double Point)               -- ^ /Valid for/: G; /Default/: +4; /Notes/: not dot
    | Shape Shape                             -- ^ /Valid for/: N; /Default/: ellipse
    | ShapeFile String                        -- ^ /Valid for/: N; /Default/: \"\"
    | ShowBoxes Int                           -- ^ /Valid for/: ENG; /Default/: 0; /Minimum/: 0; /Notes/: dot only
    | Sides Int                               -- ^ /Valid for/: N; /Default/: 4; /Minimum/: 0
    | Size Point                              -- ^ /Valid for/: G
    | Skew Double                             -- ^ /Valid for/: N; /Default/: 0.0; /Minimum/: -100.0
    | Smoothing SmoothType                    -- ^ /Valid for/: G; /Default/: "none"; /Notes/: sfdp only
    | SortV Int                               -- ^ /Valid for/: GCN; /Default/: 0; /Minimum/: 0
    | Splines (Either Bool String)            -- ^ /Valid for/: G
    | Start StartType                         -- ^ /Valid for/: G; /Default/: \"\"; /Notes/: fdp, neato only
    | Style Style                             -- ^ /Valid for/: ENC
    | StyleSheet String                       -- ^ /Valid for/: G; /Default/: \"\"; /Notes/: svg only
    | TailURL URL                             -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: svg, map only
    | TailClip Bool                           -- ^ /Valid for/: E; /Default/: true
    | TailHref URL                            -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: svg, map only
    | TailLabel (Either String URL)           -- ^ /Valid for/: E; /Default/: \"\"
    | TailPort PortPos                        -- ^ /Valid for/: E; /Default/: center
    | TailTarget String                       -- ^ /Valid for/: E; /Default/: <none>; /Notes/: svg, map only
    | TailTooltip String                      -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: svg, cmap only
    | Target String                           -- ^ /Valid for/: ENGC; /Default/: <none>; /Notes/: svg, map only
    | Tooltip String                          -- ^ /Valid for/: NEC; /Default/: \"\"; /Notes/: svg, cmap only
    | TrueColor Bool                          -- ^ /Valid for/: G; /Notes/: bitmap output only
    | Vertices [Point]                        -- ^ /Valid for/: N; /Notes/: write only
    | ViewPort ViewPort                       -- ^ /Valid for/: G; /Default/: \"\"
    | VoroMargin Double                       -- ^ /Valid for/: G; /Default/: 0.05; /Minimum/: 0.0; /Notes/: not dot
    | Weight Double                           -- ^ /Valid for/: E; /Default/: 1.0; /Minimum/: 0(dot) | 1(neato,fdp,sfdp)
    | Width Double                            -- ^ /Valid for/: N; /Default/: 0.75; /Minimum/: 0.01
    | Z Double                                -- ^ /Valid for/: N; /Default/: 0.0; /Minimum/: -MAXFLOAT | -1000
      deriving (Eq, Read)

instance Show Attribute where
    show (Damping v)            = "Damping=" ++ show v
    show (K v)                  = "K=" ++ show v
    show (URL v)                = "URL=" ++ show v
    show (ArrowHead v)          = "arrowhead=" ++ show v
    show (ArrowSize v)          = "arrowsize=" ++ show v
    show (ArrowTail v)          = "arrowtail=" ++ show v
    show (Aspect v)             = "aspect=" ++ show v
    show (Bb v)                 = "bb=" ++ show v
    show (BgColor v)            = "bgcolor=" ++ show v
    show (Center v)             = "center=" ++ show v
    show (Charset v)            = "charset=" ++ v
    show (ClusterRank v)        = "clusterrank=" ++ show v
    show (Color v)              = "color=" ++ show v
    show (ColorScheme v)        = "colorscheme=" ++ v
    show (Comment v)            = "comment=" ++ v
    show (Compound v)           = "compound=" ++ show v
    show (Concentrate v)        = "concentrate=" ++ show v
    show (Constraint v)         = "constraint=" ++ show v
    show (Decorate v)           = "decorate=" ++ show v
    show (DefaultDist v)        = "defaultdist=" ++ show v
    show (Dim v)                = "dim=" ++ show v
    show (Dimen v)              = "dimen=" ++ show v
    show (Dir v)                = "dir=" ++ show v
    show (DirEdgeConstraints v) = "diredgeconstraints=" ++ show v
    show (Distortion v)         = "distortion=" ++ show v
    show (DPI v)                = "dpi=" ++ show v
    show (EdgeURL v)            = "edgeURL=" ++ show v
    show (EdgeHref v)           = "edgehref=" ++ show v
    show (EdgeTarget v)         = "edgetarget=" ++ v
    show (EdgeTooltip v)        = "edgetooltip=" ++ v
    show (Epsilon v)            = "epsilon=" ++ show v
    show (ESep v)               = "esep=" ++ show v
    show (FillColor v)          = "fillcolor=" ++ show v
    show (FixedSize v)          = "fixedsize=" ++ show v
    show (FontColor v)          = "fontcolor=" ++ show v
    show (FontName v)           = "fontname=" ++ v
    show (FontNames v)          = "fontnames=" ++ v
    show (FontPath v)           = "fontpath=" ++ v
    show (FontSize v)           = "fontsize=" ++ show v
    show (Group v)              = "group=" ++ v
    show (HeadURL v)            = "headURL=" ++ show v
    show (HeadClip v)           = "headclip=" ++ show v
    show (HeadHref v)           = "headhref=" ++ show v
    show (HeadLabel v)          = "headlabel=" ++ show v
    show (HeadPort v)           = "headport=" ++ show v
    show (HeadTarget v)         = "headtarget=" ++ show v
    show (HeadTooltip v)        = "headtooltip=" ++ show v
    show (Height v)             = "height=" ++ show v
    show (Href v)               = "href=" ++ show v
    show (ID v)                 = "id=" ++ show v
    show (Image v)              = "image=" ++ v
    show (ImageScale v)         = "imagescale=" ++ show v
    show (Label v)              = "label=" ++ show v
    show (LabelURL v)           = "labelURL=" ++ show v
    show (LabelAngle v)         = "labelangle=" ++ show v
    show (LabelDistance v)      = "labeldistance=" ++ show v
    show (LabelFloat v)         = "labelfloat=" ++ show v
    show (LabelFontColor v)     = "labelfontcolor=" ++ show v
    show (LabelFontName v)      = "labelfontname=" ++ v
    show (LabelFontSize v)      = "labelfontsize=" ++ show v
    show (LabelHref v)          = "labelhref=" ++ show v
    show (LabelJust v)          = "labeljust=" ++ show v
    show (LabelLoc v)           = "labelloc=" ++ show v
    show (LabelTarget v)        = "labeltarget=" ++ v
    show (LabelTooltip v)       = "labeltooltip=" ++ v
    show (Landscape v)          = "landscape=" ++ show v
    show (Layer v)              = "layer=" ++ show v
    show (Layers v)             = "layers=" ++ show v
    show (LayerSep v)           = "layersep=" ++ v
    show (Layout v)             = "layout=" ++ v
    show (Len v)                = "len=" ++ show v
    show (Levels v)             = "levels=" ++ show v
    show (LevelsGap v)          = "levelsgap=" ++ show v
    show (LHead v)              = "lhead=" ++ v
    show (LP v)                 = "lp=" ++ show v
    show (LTail v)              = "ltail=" ++ v
    show (Margin v)             = "margin=" ++ show v
    show (MaxIter v)            = "maxiter=" ++ show v
    show (MCLimit v)            = "mclimit=" ++ show v
    show (MinDist v)            = "mindist=" ++ show v
    show (MinLen v)             = "minlen=" ++ show v
    show (Mode v)               = "mode=" ++ v
    show (Model v)              = "model=" ++ v
    show (Mosek v)              = "mosek=" ++ show v
    show (NodeSep v)            = "nodesep=" ++ show v
    show (NoJustify v)          = "nojustify=" ++ show v
    show (Normalize v)          = "normalize=" ++ show v
    show (Nslimit v)            = "nslimit=" ++ show v
    show (Nslimit1 v)           = "nslimit1=" ++ show v
    show (Ordering v)           = "ordering=" ++ v
    show (Orientation v)        = "orientation=" ++ show v
    show (OrientationGraph v)   = "orientation=" ++ v
    show (OutputOrder v)        = "outputorder=" ++ show v
    show (Overlap v)            = "overlap=" ++ show v
    show (OverlapScaling v)     = "overlap_scaling=" ++ show v
    show (Pack v)               = "pack=" ++ show v
    show (PackMode v)           = "packmode=" ++ show v
    show (Pad v)                = "pad=" ++ show v
    show (Page v)               = "page=" ++ show v
    show (PageDir v)            = "pagedir=" ++ show v
    show (PenColor v)           = "pencolor=" ++ show v
    show (PenWidth v)           = "penwidth=" ++ show v
    show (Peripheries v)        = "peripheries=" ++ show v
    show (Pin v)                = "pin=" ++ show v
    show (Pos v)                = "pos=" ++ show v
    show (QuadTree v)           = "quadtree=" ++ show v
    show (Quantum v)            = "quantum=" ++ show v
    show (Rank v)               = "rank=" ++ show v
    show (RankDir v)            = "rankdir=" ++ show v
    show (Ranksep v)            = "ranksep=" ++ show v
    show (Ratio v)              = "ratio=" ++ show v
    show (Rects v)              = "rects=" ++ show v
    show (Regular v)            = "regular=" ++ show v
    show (ReMinCross v)         = "remincross=" ++ show v
    show (RepulsiveForce v)     = "repulsiveforce=" ++ show v
    show (Resolution v)         = "resolution=" ++ show v
    show (Root v)               = "root=" ++ show v
    show (Rotate v)             = "rotate=" ++ show v
    show (SameHead v)           = "samehead=" ++ v
    show (SameTail v)           = "sametail=" ++ v
    show (SamplePoints v)       = "samplepoints=" ++ show v
    show (SearchSize v)         = "searchsize=" ++ show v
    show (Sep v)                = "sep=" ++ show v
    show (Shape v)              = "shape=" ++ show v
    show (ShapeFile v)          = "shapefile=" ++ v
    show (ShowBoxes v)          = "showboxes=" ++ show v
    show (Sides v)              = "sides=" ++ show v
    show (Size v)               = "size=" ++ show v
    show (Skew v)               = "skew=" ++ show v
    show (Smoothing v)          = "smoothing=" ++ show v
    show (SortV v)              = "sortv=" ++ show v
    show (Splines v)            = "splines=" ++ show v
    show (Start v)              = "start=" ++ show v
    show (Style v)              = "style=" ++ show v
    show (StyleSheet v)         = "stylesheet=" ++ v
    show (TailURL v)            = "tailURL=" ++ show v
    show (TailClip v)           = "tailclip=" ++ show v
    show (TailHref v)           = "tailhref=" ++ show v
    show (TailLabel v)          = "taillabel=" ++ show v
    show (TailPort v)           = "tailport=" ++ show v
    show (TailTarget v)         = "tailtarget=" ++ v
    show (TailTooltip v)        = "tailtooltip=" ++ v
    show (Target v)             = "target=" ++ v
    show (Tooltip v)            = "tooltip=" ++ v
    show (TrueColor v)          = "truecolor=" ++ show v
    show (Vertices v)           = "vertices=" ++ show v
    show (ViewPort v)           = "viewport=" ++ show v
    show (VoroMargin v)         = "voro_margin=" ++ show v
    show (Weight v)             = "weight=" ++ show v
    show (Width v)              = "width=" ++ show v
    show (Z v)                  = "z=" ++ show v

instance Parseable Attribute where
    parse = oneOf [ liftM Damping            $ parseField "Damping"
                  , liftM K                  $ parseField "K"
                  , liftM URL                $ parseField "URL"
                  , liftM ArrowHead          $ parseField "arrowhead"
                  , liftM ArrowSize          $ parseField "arrowsize"
                  , liftM ArrowTail          $ parseField "arrowtail"
                  , liftM Aspect             $ parseField "aspect"
                  , liftM Bb                 $ parseField "bb"
                  , liftM BgColor            $ parseField "bgcolor"
                  , liftM Center             $ parseBoolField "center"
                  , liftM Charset            $ parseField "charset"
                  , liftM ClusterRank        $ parseField "clusterrank"
                  , liftM Color              $ parseField "color"
                  , liftM ColorScheme        $ parseField "colorscheme"
                  , liftM Comment            $ parseField "comment"
                  , liftM Compound           $ parseBoolField "compound"
                  , liftM Concentrate        $ parseBoolField "concentrate"
                  , liftM Constraint         $ parseBoolField "constraint"
                  , liftM Decorate           $ parseBoolField "decorate"
                  , liftM DefaultDist        $ parseField "defaultdist"
                  , liftM Dim                $ parseField "dim"
                  , liftM Dimen              $ parseField "dimen"
                  , liftM Dir                $ parseField "dir"
                  , liftM DirEdgeConstraints $ parseField "diredgeconstraints"
                  , liftM Distortion         $ parseField "distortion"
                  , liftM DPI                $ parseField "dpi"
                  , liftM EdgeURL            $ parseField "edgeURL"
                  , liftM EdgeHref           $ parseField "edgehref"
                  , liftM EdgeTarget         $ parseField "edgetarget"
                  , liftM EdgeTooltip        $ parseField "edgetooltip"
                  , liftM Epsilon            $ parseField "epsilon"
                  , liftM ESep               $ parseField "esep"
                  , liftM FillColor          $ parseField "fillcolor"
                  , liftM FixedSize          $ parseBoolField "fixedsize"
                  , liftM FontColor          $ parseField "fontcolor"
                  , liftM FontName           $ parseField "fontname"
                  , liftM FontNames          $ parseField "fontnames"
                  , liftM FontPath           $ parseField "fontpath"
                  , liftM FontSize           $ parseField "fontsize"
                  , liftM Group              $ parseField "group"
                  , liftM HeadURL            $ parseField "headURL"
                  , liftM HeadClip           $ parseBoolField "headclip"
                  , liftM HeadHref           $ parseField "headhref"
                  , liftM HeadLabel          $ parseField "headlabel"
                  , liftM HeadPort           $ parseField "headport"
                  , liftM HeadTarget         $ parseField "headtarget"
                  , liftM HeadTooltip        $ parseField "headtooltip"
                  , liftM Height             $ parseField "height"
                  , liftM Href               $ parseField "href"
                  , liftM ID                 $ parseField "id"
                  , liftM Image              $ parseField "image"
                  , liftM ImageScale         $ parseField "imagescale"
                  , liftM Label              $ parseField "label"
                  , liftM LabelURL           $ parseField "labelURL"
                  , liftM LabelAngle         $ parseField "labelangle"
                  , liftM LabelDistance      $ parseField "labeldistance"
                  , liftM LabelFloat         $ parseBoolField "labelfloat"
                  , liftM LabelFontColor     $ parseField "labelfontcolor"
                  , liftM LabelFontName      $ parseField "labelfontname"
                  , liftM LabelFontSize      $ parseField "labelfontsize"
                  , liftM LabelHref          $ parseField "labelhref"
                  , liftM LabelJust          $ parseField "labeljust"
                  , liftM LabelLoc           $ parseField "labelloc"
                  , liftM LabelTarget        $ parseField "labeltarget"
                  , liftM LabelTooltip       $ parseField "labeltooltip"
                  , liftM Landscape          $ parseBoolField "landscape"
                  , liftM Layer              $ parseField "layer"
                  , liftM Layers             $ parseField "layers"
                  , liftM LayerSep           $ parseField "layersep"
                  , liftM Layout             $ parseField "layout"
                  , liftM Len                $ parseField "len"
                  , liftM Levels             $ parseField "levels"
                  , liftM LevelsGap          $ parseField "levelsgap"
                  , liftM LHead              $ parseField "lhead"
                  , liftM LP                 $ parseField "lp"
                  , liftM LTail              $ parseField "ltail"
                  , liftM Margin             $ parseField "margin"
                  , liftM MaxIter            $ parseField "maxiter"
                  , liftM MCLimit            $ parseField "mclimit"
                  , liftM MinDist            $ parseField "mindist"
                  , liftM MinLen             $ parseField "minlen"
                  , liftM Mode               $ parseField "mode"
                  , liftM Model              $ parseField "model"
                  , liftM Mosek              $ parseBoolField "mosek"
                  , liftM NodeSep            $ parseField "nodesep"
                  , liftM NoJustify          $ parseBoolField "nojustify"
                  , liftM Normalize          $ parseBoolField "normalize"
                  , liftM Nslimit            $ parseField "nslimit"
                  , liftM Nslimit1           $ parseField "nslimit1"
                  , liftM Ordering           $ parseField "ordering"
                  , liftM Orientation        $ parseField "orientation"
                  , liftM OrientationGraph   $ parseField "orientation"
                  , liftM OutputOrder        $ parseField "outputorder"
                  , liftM Overlap            $ parseField "overlap"
                  , liftM OverlapScaling     $ parseField "overlap_scaling"
                  , liftM Pack               $ parseField "pack"
                  , liftM PackMode           $ parseField "packmode"
                  , liftM Pad                $ parseField "pad"
                  , liftM Page               $ parseField "page"
                  , liftM PageDir            $ parseField "pagedir"
                  , liftM PenColor           $ parseField "pencolor"
                  , liftM PenWidth           $ parseField "penwidth"
                  , liftM Peripheries        $ parseField "peripheries"
                  , liftM Pin                $ parseBoolField "pin"
                  , liftM Pos                $ parseField "pos"
                  , liftM QuadTree           $ parseField "quadtree"
                  , liftM Quantum            $ parseField "quantum"
                  , liftM Rank               $ parseField "rank"
                  , liftM RankDir            $ parseField "rankdir"
                  , liftM Ranksep            $ parseField "ranksep"
                  , liftM Ratio              $ parseField "ratio"
                  , liftM Rects              $ parseField "rects"
                  , liftM Regular            $ parseBoolField "regular"
                  , liftM ReMinCross         $ parseBoolField "remincross"
                  , liftM RepulsiveForce     $ parseField "repulsiveforce"
                  , liftM Resolution         $ parseField "resolution"
                  , liftM Root               $ parseField "root"
                  , liftM Rotate             $ parseField "rotate"
                  , liftM SameHead           $ parseField "samehead"
                  , liftM SameTail           $ parseField "sametail"
                  , liftM SamplePoints       $ parseField "samplepoints"
                  , liftM SearchSize         $ parseField "searchsize"
                  , liftM Sep                $ parseField "sep"
                  , liftM Shape              $ parseField "shape"
                  , liftM ShapeFile          $ parseField "shapefile"
                  , liftM ShowBoxes          $ parseField "showboxes"
                  , liftM Sides              $ parseField "sides"
                  , liftM Size               $ parseField "size"
                  , liftM Skew               $ parseField "skew"
                  , liftM Smoothing          $ parseField "smoothing"
                  , liftM SortV              $ parseField "sortv"
                  , liftM Splines            $ parseField "splines"
                  , liftM Start              $ parseField "start"
                  , liftM Style              $ parseField "style"
                  , liftM StyleSheet         $ parseField "stylesheet"
                  , liftM TailURL            $ parseField "tailURL"
                  , liftM TailClip           $ parseBoolField "tailclip"
                  , liftM TailHref           $ parseField "tailhref"
                  , liftM TailLabel          $ parseField "taillabel"
                  , liftM TailPort           $ parseField "tailport"
                  , liftM TailTarget         $ parseField "tailtarget"
                  , liftM TailTooltip        $ parseField "tailtooltip"
                  , liftM Target             $ parseField "target"
                  , liftM Tooltip            $ parseField "tooltip"
                  , liftM TrueColor          $ parseBoolField "truecolor"
                  , liftM Vertices           $ parseField "vertices"
                  , liftM ViewPort           $ parseField "viewport"
                  , liftM VoroMargin         $ parseField "voro_margin"
                  , liftM Weight             $ parseField "weight"
                  , liftM Width              $ parseField "width"
                  , liftM Z                  $ parseField "z"
                  ]

-- | Determine if this Attribute is valid for use with Graphs.
usedByGraphs                      :: Attribute -> Bool
usedByGraphs Damping{}            = True
usedByGraphs K{}                  = True
usedByGraphs URL{}                = True
usedByGraphs Aspect{}             = True
usedByGraphs Bb{}                 = True
usedByGraphs BgColor{}            = True
usedByGraphs Center{}             = True
usedByGraphs Charset{}            = True
usedByGraphs ClusterRank{}        = True
usedByGraphs ColorScheme{}        = True
usedByGraphs Comment{}            = True
usedByGraphs Compound{}           = True
usedByGraphs Concentrate{}        = True
usedByGraphs DefaultDist{}        = True
usedByGraphs Dim{}                = True
usedByGraphs Dimen{}              = True
usedByGraphs DirEdgeConstraints{} = True
usedByGraphs DPI{}                = True
usedByGraphs Epsilon{}            = True
usedByGraphs ESep{}               = True
usedByGraphs FontColor{}          = True
usedByGraphs FontName{}           = True
usedByGraphs FontNames{}          = True
usedByGraphs FontPath{}           = True
usedByGraphs FontSize{}           = True
usedByGraphs ID{}                 = True
usedByGraphs Label{}              = True
usedByGraphs LabelJust{}          = True
usedByGraphs LabelLoc{}           = True
usedByGraphs Landscape{}          = True
usedByGraphs Layers{}             = True
usedByGraphs LayerSep{}           = True
usedByGraphs Layout{}             = True
usedByGraphs Levels{}             = True
usedByGraphs LevelsGap{}          = True
usedByGraphs LP{}                 = True
usedByGraphs Margin{}             = True
usedByGraphs MaxIter{}            = True
usedByGraphs MCLimit{}            = True
usedByGraphs MinDist{}            = True
usedByGraphs Mode{}               = True
usedByGraphs Model{}              = True
usedByGraphs Mosek{}              = True
usedByGraphs NodeSep{}            = True
usedByGraphs NoJustify{}          = True
usedByGraphs Normalize{}          = True
usedByGraphs Nslimit{}            = True
usedByGraphs Nslimit1{}           = True
usedByGraphs Ordering{}           = True
usedByGraphs OrientationGraph{}   = True
usedByGraphs OutputOrder{}        = True
usedByGraphs Overlap{}            = True
usedByGraphs OverlapScaling{}     = True
usedByGraphs Pack{}               = True
usedByGraphs PackMode{}           = True
usedByGraphs Pad{}                = True
usedByGraphs Page{}               = True
usedByGraphs PageDir{}            = True
usedByGraphs QuadTree{}           = True
usedByGraphs Quantum{}            = True
usedByGraphs RankDir{}            = True
usedByGraphs Ranksep{}            = True
usedByGraphs Ratio{}              = True
usedByGraphs ReMinCross{}         = True
usedByGraphs RepulsiveForce{}     = True
usedByGraphs Resolution{}         = True
usedByGraphs Root{}               = True
usedByGraphs Rotate{}             = True
usedByGraphs SearchSize{}         = True
usedByGraphs Sep{}                = True
usedByGraphs ShowBoxes{}          = True
usedByGraphs Size{}               = True
usedByGraphs Smoothing{}          = True
usedByGraphs SortV{}              = True
usedByGraphs Splines{}            = True
usedByGraphs Start{}              = True
usedByGraphs StyleSheet{}         = True
usedByGraphs Target{}             = True
usedByGraphs TrueColor{}          = True
usedByGraphs ViewPort{}           = True
usedByGraphs VoroMargin{}         = True
usedByGraphs _                    = False

-- | Determine if this Attribute is valid for use with Clusters.
usedByClusters               :: Attribute -> Bool
usedByClusters K{}           = True
usedByClusters URL{}         = True
usedByClusters BgColor{}     = True
usedByClusters Color{}       = True
usedByClusters ColorScheme{} = True
usedByClusters FillColor{}   = True
usedByClusters FontColor{}   = True
usedByClusters FontName{}    = True
usedByClusters FontSize{}    = True
usedByClusters Label{}       = True
usedByClusters LabelJust{}   = True
usedByClusters LabelLoc{}    = True
usedByClusters LP{}          = True
usedByClusters NoJustify{}   = True
usedByClusters PenColor{}    = True
usedByClusters PenWidth{}    = True
usedByClusters Peripheries{} = True
usedByClusters Rank{}        = True
usedByClusters SortV{}       = True
usedByClusters Style{}       = True
usedByClusters Target{}      = True
usedByClusters Tooltip{}     = True
usedByClusters _             = False

-- | Determine if this Attribute is valid for use with SubGraphs.
usedBySubGraphs        :: Attribute -> Bool
usedBySubGraphs Rank{} = True
usedBySubGraphs _      = False

-- | Determine if this Attribute is valid for use with Nodes.
usedByNodes                :: Attribute -> Bool
usedByNodes URL{}          = True
usedByNodes Color{}        = True
usedByNodes ColorScheme{}  = True
usedByNodes Comment{}      = True
usedByNodes Distortion{}   = True
usedByNodes FillColor{}    = True
usedByNodes FixedSize{}    = True
usedByNodes FontColor{}    = True
usedByNodes FontName{}     = True
usedByNodes FontSize{}     = True
usedByNodes Group{}        = True
usedByNodes Height{}       = True
usedByNodes ID{}           = True
usedByNodes Image{}        = True
usedByNodes ImageScale{}   = True
usedByNodes Label{}        = True
usedByNodes LabelLoc{}     = True
usedByNodes Layer{}        = True
usedByNodes Margin{}       = True
usedByNodes NoJustify{}    = True
usedByNodes Orientation{}  = True
usedByNodes PenWidth{}     = True
usedByNodes Peripheries{}  = True
usedByNodes Pin{}          = True
usedByNodes Pos{}          = True
usedByNodes Rects{}        = True
usedByNodes Regular{}      = True
usedByNodes Root{}         = True
usedByNodes SamplePoints{} = True
usedByNodes Shape{}        = True
usedByNodes ShapeFile{}    = True
usedByNodes ShowBoxes{}    = True
usedByNodes Sides{}        = True
usedByNodes Skew{}         = True
usedByNodes SortV{}        = True
usedByNodes Style{}        = True
usedByNodes Target{}       = True
usedByNodes Tooltip{}      = True
usedByNodes Vertices{}     = True
usedByNodes Width{}        = True
usedByNodes Z{}            = True
usedByNodes _              = False

-- | Determine if this Attribute is valid for use with Edges.
usedByEdges                  :: Attribute -> Bool
usedByEdges URL{}            = True
usedByEdges ArrowHead{}      = True
usedByEdges ArrowSize{}      = True
usedByEdges ArrowTail{}      = True
usedByEdges Color{}          = True
usedByEdges ColorScheme{}    = True
usedByEdges Comment{}        = True
usedByEdges Constraint{}     = True
usedByEdges Decorate{}       = True
usedByEdges Dir{}            = True
usedByEdges EdgeURL{}        = True
usedByEdges EdgeHref{}       = True
usedByEdges EdgeTarget{}     = True
usedByEdges EdgeTooltip{}    = True
usedByEdges FontColor{}      = True
usedByEdges FontName{}       = True
usedByEdges FontSize{}       = True
usedByEdges HeadURL{}        = True
usedByEdges HeadClip{}       = True
usedByEdges HeadHref{}       = True
usedByEdges HeadLabel{}      = True
usedByEdges HeadPort{}       = True
usedByEdges HeadTarget{}     = True
usedByEdges HeadTooltip{}    = True
usedByEdges Href{}           = True
usedByEdges ID{}             = True
usedByEdges Label{}          = True
usedByEdges LabelURL{}       = True
usedByEdges LabelAngle{}     = True
usedByEdges LabelDistance{}  = True
usedByEdges LabelFloat{}     = True
usedByEdges LabelFontColor{} = True
usedByEdges LabelFontName{}  = True
usedByEdges LabelFontSize{}  = True
usedByEdges LabelHref{}      = True
usedByEdges LabelTarget{}    = True
usedByEdges LabelTooltip{}   = True
usedByEdges Layer{}          = True
usedByEdges Len{}            = True
usedByEdges LHead{}          = True
usedByEdges LP{}             = True
usedByEdges LTail{}          = True
usedByEdges MinLen{}         = True
usedByEdges NoJustify{}      = True
usedByEdges PenWidth{}       = True
usedByEdges Pos{}            = True
usedByEdges SameHead{}       = True
usedByEdges SameTail{}       = True
usedByEdges ShowBoxes{}      = True
usedByEdges Style{}          = True
usedByEdges TailURL{}        = True
usedByEdges TailClip{}       = True
usedByEdges TailHref{}       = True
usedByEdges TailLabel{}      = True
usedByEdges TailPort{}       = True
usedByEdges TailTarget{}     = True
usedByEdges TailTooltip{}    = True
usedByEdges Target{}         = True
usedByEdges Tooltip{}        = True
usedByEdges Weight{}         = True
usedByEdges _                = False

-- -----------------------------------------------------------------------------

newtype URL = UStr { url :: String }
    deriving (Eq, Read)

instance Show URL where
    show u = '<' : url u ++ ">"

instance Parseable URL where
    parse = do char open
               cnt <- many1 $ satisfy ((/=) close)
               char close
               return $ UStr cnt
        where
          open = '<'
          close = '>'

-- -----------------------------------------------------------------------------

data ArrowType = Normal   | Inv
               | DotArrow | InvDot
               | ODot     | InvODot
               | NoArrow  | Tee
               | Empty    | InvEmpty
               | Diamond  | ODiamond
               | EDiamond | Crow
               | Box      | OBox
               | Open     | HalfOpen
               | Vee
                 deriving (Eq, Read)

instance Show ArrowType where
    show Normal   = "normal"
    show Inv      = "inv"
    show DotArrow = "dot"
    show InvDot   = "invdot"
    show ODot     = "odot"
    show InvODot  = "invodot"
    show NoArrow  = "none"
    show Tee      = "tee"
    show Empty    = "empty"
    show InvEmpty = "invempty"
    show Diamond  = "diamond"
    show ODiamond = "odiamond"
    show EDiamond = "ediamond"
    show Crow     = "crow"
    show Box      = "box"
    show OBox     = "obox"
    show Open     = "open"
    show HalfOpen = "halfopen"
    show Vee      = "vee"

instance Parseable ArrowType where
    parse = optionalQuoted
            $ oneOf [ string "normal"   >> return Normal
                    , string "inv"      >> return Inv
                    , string "dot"      >> return DotArrow
                    , string "invdot"   >> return InvDot
                    , string "odot"     >> return ODot
                    , string "invodot"  >> return InvODot
                    , string "none"     >> return NoArrow
                    , string "tee"      >> return Tee
                    , string "empty"    >> return Empty
                    , string "invempty" >> return InvEmpty
                    , string "diamond"  >> return Diamond
                    , string "odiamond" >> return ODiamond
                    , string "ediamond" >> return EDiamond
                    , string "crow"     >> return Crow
                    , string "box"      >> return Box
                    , string "obox"     >> return OBox
                    , string "open"     >> return Open
                    , string "halfopen" >> return HalfOpen
                    , string "vee"      >> return Vee
                    ]

-- -----------------------------------------------------------------------------

data AspectType = RatioOnly Double
                | RatioPassCount Double Int
                  deriving (Eq, Read)

instance Show AspectType where
    show (RatioOnly r)        = show r
    show (RatioPassCount r p) = show $ show r ++ ',' : show p

instance Parseable AspectType where
    parse = oneOf [ liftM RatioOnly parse
                  , quotedParse $ do r <- parse
                                     char ','
                                     whitespace'
                                     p <- parse
                                     return $ RatioPassCount r p
                  ]

-- -----------------------------------------------------------------------------

data Rect = Rect Point Point
            deriving (Eq, Read)

instance Show Rect where
    show (Rect p1 p2) = show $ show p1 ++ ',' : show p2

instance Parseable Rect where
    parse = liftM (uncurry Rect) . quotedParse
            $ commaSep' parsePoint parsePoint

-- -----------------------------------------------------------------------------

data Color = RGB { red   :: Word8
                 , green :: Word8
                 , blue  :: Word8
                 }
           | RGBA { red   :: Word8
                  , green :: Word8
                  , blue  :: Word8
                  , alpha :: Word8
                  }
           | HSV { hue        :: Int
                 , saturation :: Int
                 , value      :: Int
                 }
           | ColorName String
             deriving (Eq, Read)

instance Show Color where
    show = show . showColor

    showList cs s = show $ go cs
        where
          go []      = s
          go [c]     = showColor c ++ s
          go (c:cs') = showColor c ++ ':' : go cs'

showColor :: Color -> String
showColor (RGB r g b)      = show $ '#' : foldr showWord8Pad "" [r,g,b]
showColor (RGBA r g b a)   = show $ '#' : foldr showWord8Pad "" [r,g,b,a]
showColor (HSV h s v)      = show $ show h ++ " " ++ show s ++ " " ++ show v
showColor (ColorName name) = name

showWord8Pad :: Word8 -> String -> String
showWord8Pad w s = padding ++ simple ++ s
    where
      simple = showHex w ""
      padding = replicate count '0'
      count = 2 - findCols 1 w
      findCols :: Int -> Word8 -> Int
      findCols c n
          | n < 16 = c
          | otherwise = findCols (c+1) (n `div` 16)

instance Parseable Color where
    parse = quotedParse parseColor

    parseList = quotedParse $ sepBy1 parseColor (char ':')

parseColor :: Parse Color
parseColor = oneOf [ parseHexBased
                   , parseHSV
                   , liftM ColorName parse -- Should we check it is a colour?
                   ]
    where
      parseHexBased
          = do char '#'
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

-- -----------------------------------------------------------------------------

data ClusterMode = Local
                 | Global
                 | NoCluster
                   deriving (Eq, Read)

instance Show ClusterMode where
    show Local     = "local"
    show Global    = "global"
    show NoCluster = "none"

instance Parseable ClusterMode where
    parse = optionalQuoted
            . oneOf
            $ [ string "local"  >> return Local
              , string "global" >> return Global
              , string "none"   >> return NoCluster
              ]

-- -----------------------------------------------------------------------------

data DirType = Forward | Back | Both | NoDir
               deriving (Eq, Read)

instance Show DirType where
    show Forward = "forward"
    show Back    = "back"
    show Both    = "both"
    show NoDir   = "none"

instance Parseable DirType where
    parse = optionalQuoted
            $ oneOf [ string "forward" >> return Forward
                    , string "back"    >> return Back
                    , string "both"    >> return Both
                    , string "none"    >> return NoDir
                    ]

-- -----------------------------------------------------------------------------

data Point = Point Int Int
           | PointD Double Double
             deriving (Eq, Read)

instance Show Point where
    show = show . showPoint

    showList ps s = unwords (map showPoint ps) ++ s

showPoint :: Point -> String
showPoint (Point  x y) = show x ++ ',' : show y
showPoint (PointD x y) = show x ++ ',' : show y

instance Parseable Point where
    parse = quotedParse parsePoint

    parseList = quotedParse $ sepBy1 parsePoint whitespace

parsePoint :: Parse Point
parsePoint = oneOf [ liftM (uncurry Point)  commaSep
                   , liftM (uncurry PointD) commaSep
                   ]

-- -----------------------------------------------------------------------------

data LayerRange = LRID LayerID
                | LRS LayerID String LayerID
                  deriving (Eq, Read)

instance Show LayerRange where
    show (LRID lid)        = show lid
    show (LRS id1 sep id2) = show $ show id1 ++ sep ++ show id2

instance Parseable LayerRange where
    parse = oneOf [ liftM LRID parse
                  , do id1 <- parse
                       sep <- parseLayerSep
                       id2 <- parse
                       return $ LRS id1 sep id2
                  ]

parseLayerSep :: Parse String
parseLayerSep = many1 . oneOf
                $ map char defLayerSep

defLayerSep :: [Char]
defLayerSep = [' ', ':', '\t']

parseLayerName :: Parse String
parseLayerName = many1 $ satisfy (flip notElem defLayerSep)

data LayerID = AllLayers
             | LRInt Int
             | LRName String
               deriving (Eq, Read)

instance Show LayerID where
    show AllLayers   = "all"
    show (LRInt n)   = show n
    show (LRName nm) = nm

instance Parseable LayerID where
    parse = oneOf [ optionalQuotedString "all" >> return AllLayers
                  , liftM LRInt $ optionalQuoted parse
                  , liftM LRName parseLayerName
                  ]

-- | The list represent (Separator, Name)
data LayerList = LL String [(String, String)]
                 deriving (Eq, Read)

instance Show LayerList where
    show (LL l1 ols) = l1 ++ concatMap (uncurry (++)) ols

instance Parseable LayerList where
    parse = do l1 <- parseLayerName
               ols <- many $ do sep <- parseLayerSep
                                lnm <- parseLayerName
                                return (sep, lnm)
               return $ LL l1 ols

-- -----------------------------------------------------------------------------

data OutputMode = BreadthFirst | NodesFirst | EdgesFirst
                  deriving (Eq, Read)

instance Show OutputMode where
    show BreadthFirst = "breadthfirst"
    show NodesFirst = "nodesfirst"
    show EdgesFirst = "edgesfirst"

instance Parseable OutputMode where
    parse = optionalQuoted
            $ oneOf [ string "breadthfirst" >> return BreadthFirst
                    , string "nodesfirst"   >> return NodesFirst
                    , string "edgesfirst"   >> return EdgesFirst
                    ]

-- -----------------------------------------------------------------------------

data PackMode = PackNode
              | PackClust
              | PackGraph
              | PackArray Bool Bool (Maybe Int) -- ^ Sort by cols, sort
                                                -- by user, number of
                                                -- rows/cols
                deriving (Eq, Read)

instance Show PackMode where
    show PackNode           = "node"
    show PackClust          = "clust"
    show PackGraph          = "graph"
    show (PackArray c u mi) = addNum . isU . isC . isUnder
                              $ "array"
        where
          addNum = maybe id (flip (++) . show) mi
          isUnder = if c || u
                    then flip (++) "_"
                    else id
          isC = if c
                then flip (++) "c"
                else id
          isU = if u
                then flip (++) "u"
                else id

instance Parseable PackMode where
    parse = optionalQuoted
            $ oneOf [ string "node" >> return PackNode
                    , string "clust" >> return PackClust
                    , string "graph" >> return PackGraph
                    , do string "array"
                         mcu <- optional $ do char '_'
                                              many1 $ satisfy (not . isDigit)
                         let c = hasChar mcu 'c'
                             u = hasChar mcu 'u'
                         mi <- optional parse
                         return $ PackArray c u mi
                    ]
        where
          hasChar ms c = maybe False (elem c) ms

-- -----------------------------------------------------------------------------

-- | Upper-case first character is major order;
--   lower-case second character is minor order.
data PageDir = Bl | Br | Tl | Tr | Rb | Rt | Lb | Lt
               deriving (Eq, Read)

instance Show PageDir where
    show Bl = "BL"
    show Br = "BR"
    show Tl = "TL"
    show Tr = "TR"
    show Rb = "RB"
    show Rt = "RT"
    show Lb = "LB"
    show Lt = "LT"

instance Parseable PageDir where
    parse = optionalQuoted
            $ oneOf [ string "BL" >> return Bl
                    , string "BR" >> return Br
                    , string "TL" >> return Tl
                    , string "TR" >> return Tr
                    , string "RB" >> return Rb
                    , string "RT" >> return Rt
                    , string "LB" >> return Lb
                    , string "LT" >> return Lt
                    ]

-- -----------------------------------------------------------------------------

-- | The number of points in the list must be equivalent to 1 mod 3;
--   note that this is not checked.
data Spline = Spline (Maybe Point) (Maybe Point) [Point]
              deriving (Eq, Read)

instance Show Spline where
    show = show . showSpline

    showList ss o = show $ go ss
        where
          go []      = o
          go [s]     = showSpline s ++ o
          go (s:ss') = showSpline s ++ ';' : go ss'

showSpline                   :: Spline -> String
showSpline (Spline ms me ps) = addS . addE
                               . unwords
                               $ map showPoint ps
    where
      addP t = maybe id (\p -> (++) $ t : ',' : show p)
      addS = addP 's' ms
      addE = addP 'e' me

instance Parseable Spline where
    parse = quotedParse parseSpline

    parseList = quotedParse $ sepBy1 parseSpline (char ';')

parseSpline :: Parse Spline
parseSpline = do ms <- parseP 's'
                 whitespace
                 me <- parseP 'e'
                 whitespace
                 ps <- sepBy1 parsePoint whitespace
                 return $ Spline ms me ps
    where
      parseP t = optional $ do char t
                               char ';'
                               parse

-- -----------------------------------------------------------------------------

data QuadType = NormalQT
              | FastQT
              | NoQT
                deriving (Eq, Read)

instance Show QuadType where
    show NormalQT = "normal"
    show FastQT   = "fast"
    show NoQT     = "none"

instance Parseable QuadType where
    parse = optionalQuoted
            $ oneOf [ string "normal" >> return NormalQT
                    , string "fast"   >> return FastQT
                    , string "none"   >> return NoQT
                    ]

-- -----------------------------------------------------------------------------

data RankType = SameRank
              | MinRank
              | SourceRank
              | MaxRank
              | SinkRank
                deriving (Eq, Read)

instance Show RankType where
    show SameRank   = "same"
    show MinRank    = "min"
    show SourceRank = "source"
    show MaxRank    = "max"
    show SinkRank   = "sink"

instance Parseable RankType where
    parse = optionalQuoted
            $ oneOf [ string "same"   >> return SameRank
                    , string "min"    >> return MinRank
                    , string "source" >> return SourceRank
                    , string "max"    >> return MaxRank
                    , string "sink"   >> return SinkRank
                    ]

-- -----------------------------------------------------------------------------

data RankDir = FromTop
             | FromLeft
             | FromBottom
             | FromRight
               deriving (Eq, Read)

instance Show RankDir where
    show FromTop    = "TB"
    show FromLeft   = "LR"
    show FromBottom = "BT"
    show FromRight  = "RL"

instance Parseable RankDir where
    parse = optionalQuoted
            $ oneOf [ string "TB" >> return FromTop
                    , string "LR" >> return FromLeft
                    , string "BT" >> return FromBottom
                    , string "RL" >> return FromRight
                    ]

-- -----------------------------------------------------------------------------

data Shape
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
      deriving (Eq, Read)

instance Show Shape where
    show BoxShape      = "box"
    show Polygon       = "polygon"
    show Ellipse       = "ellipse"
    show Circle        = "circle"
    show PointShape    = "point"
    show Egg           = "egg"
    show Triangle      = "triangle"
    show Plaintext     = "plaintext"
    show DiamondShape  = "diamond"
    show Trapezium     = "trapezium"
    show Parallelogram = "parallelogram"
    show House         = "house"
    show Pentagon      = "pentagon"
    show Hexagon       = "hexagon"
    show Septagon      = "septagon"
    show Octagon       = "octagon"
    show Doublecircle  = "doublecircle"
    show Doubleoctagon = "doubleoctagon"
    show Tripleoctagon = "tripleoctagon"
    show Invtriangle   = "invtriangle"
    show Invtrapezium  = "invtrapezium"
    show Invhouse      = "invhouse"
    show Mdiamond      = "mdiamond"
    show Msquare       = "msquare"
    show Mcircle       = "mcircle"
    show Rectangle     = "rectangle"
    show NoShape       = "none"
    show Note          = "note"
    show Tab           = "tab"
    show Folder        = "folder"
    show Box3d         = "box3d"
    show Component     = "component"

instance Parseable Shape where
    parse = optionalQuoted
            $ oneOf [ string "box"           >> return BoxShape
                    , string "polygon"       >> return Polygon
                    , string "ellipse"       >> return Ellipse
                    , string "circle"        >> return Circle
                    , string "point"         >> return PointShape
                    , string "egg"           >> return Egg
                    , string "triangle"      >> return Triangle
                    , string "plaintext"     >> return Plaintext
                    , string "diamond"       >> return DiamondShape
                    , string "trapezium"     >> return Trapezium
                    , string "parallelogram" >> return Parallelogram
                    , string "house"         >> return House
                    , string "pentagon"      >> return Pentagon
                    , string "hexagon"       >> return Hexagon
                    , string "septagon"      >> return Septagon
                    , string "octagon"       >> return Octagon
                    , string "doublecircle"  >> return Doublecircle
                    , string "doubleoctagon" >> return Doubleoctagon
                    , string "tripleoctagon" >> return Tripleoctagon
                    , string "invtriangle"   >> return Invtriangle
                    , string "invtrapezium"  >> return Invtrapezium
                    , string "invhouse"      >> return Invhouse
                    , string "mdiamond"      >> return Mdiamond
                    , string "msquare"       >> return Msquare
                    , string "mcircle"       >> return Mcircle
                    , string "rectangle"     >> return Rectangle
                    , string "none"          >> return NoShape
                    , string "note"          >> return Note
                    , string "tab"           >> return Tab
                    , string "folder"        >> return Folder
                    , string "box3d"         >> return Box3d
                    , string "component"     >> return Component
                    ]

-- -----------------------------------------------------------------------------

data SmoothType = NoSmooth
                | AvgDist
                | GraphDist
                | PowerDist
                | RNG
                | Spring
                | TriangleSmooth
                  deriving (Eq, Read)

instance Show SmoothType where
    show NoSmooth       = "none"
    show AvgDist        = "avg_dist"
    show GraphDist      = "graph_dist"
    show PowerDist      = "power_dist"
    show RNG            = "rng"
    show Spring         = "spring"
    show TriangleSmooth = "triangle"

instance Parseable SmoothType where
    parse = optionalQuoted
            $ oneOf [ string "none"       >> return NoSmooth
                    , string "avg_dist"   >> return AvgDist
                    , string "graph_dist" >> return GraphDist
                    , string "power_dist" >> return PowerDist
                    , string "rng"        >> return RNG
                    , string "spring"     >> return Spring
                    , string "triangle"   >> return TriangleSmooth
                    ]

-- -----------------------------------------------------------------------------

-- | It it assumed that at least one of these is Just _ .
data StartType = ST (Maybe STStyle) (Maybe Int) -- Use a Word?
                 deriving (Eq, Read)

instance Show StartType where
    show (ST ms mi) = maybe id ((++) . show) ms
                      $ maybe "" show mi

instance Parseable StartType where
    parse = optionalQuoted
            $ do ms <- optional parse
                 mi <- optional parse
                 return $ ST ms mi

data STStyle = RegularStyle
             | Self
             | Random
               deriving (Eq, Read)

instance Show STStyle where
    show RegularStyle = "regular"
    show Self         = "self"
    show Random       = "random"

instance Parseable STStyle where
    parse = oneOf [ string "regular" >> return RegularStyle
                  , string "self"    >> return Self
                  , string "random"  >> return Random
                  ]

-- -----------------------------------------------------------------------------

data Style = Stl StyleName (Maybe String)
             deriving (Eq, Read)

instance Show Style where
    show (Stl nm marg) = maybe snm
                               (\arg -> show $ snm ++ '(' : arg ++ ")")
                               marg
        where
          snm = show nm

instance Parseable Style where
    parse = oneOf [ optionalQuoted $ liftM (\nm -> Stl nm Nothing) parse
                  , quotedParse $ do nm <- parse
                                     char '('
                                     arg <- many1
                                            $ satisfy (flip notElem "()")

                                     char ')'
                                     return $ Stl nm (Just arg)
                  ]

data StyleName = Dashed    -- ^ Nodes and Edges
               | Dotted    -- ^ Nodes and Edges
               | Solid     -- ^ Nodes and Edges
               | Bold      -- ^ Nodes and Edges
               | Invisible -- ^ Nodes and Edges
               | Filled    -- ^ Nodes and Clusters
               | Diagonals -- ^ Nodes only
               | Rounded   -- ^ Nodes and Clusters
                 deriving (Eq, Read)

instance Show StyleName where
    show Filled    = "filled"
    show Invisible = "invis"
    show Diagonals = "diagonals"
    show Rounded   = "rounded"
    show Dashed    = "dashed"
    show Dotted    = "dotted"
    show Solid     = "solid"
    show Bold      = "bold"

instance Parseable StyleName where
    parse = optionalQuoted
            $ oneOf [ string "filled"    >> return Filled
                    , string "invis"     >> return Invisible
                    , string "diagonals" >> return Diagonals
                    , string "rounded"   >> return Rounded
                    , string "dashed"    >> return Dashed
                    , string "dotted"    >> return Dotted
                    , string "solid"     >> return Solid
                    , string "bold"      >> return Bold
                    ]

-- -----------------------------------------------------------------------------

newtype PortPos = PP CompassPoint
    deriving (Eq, Read)

instance Show PortPos where
    show (PP cp) = show cp

instance Parseable PortPos where
    parse = liftM PP parse

data CompassPoint = North
                  | NorthEast
                  | East
                  | SouthEast
                  | South
                  | SouthWest
                  | West
                  | NorthWest
                  | CenterPoint
                  | NoCP
                    deriving (Eq, Read)

instance Show CompassPoint where
    show North       = "n"
    show NorthEast   = "ne"
    show East        = "e"
    show SouthEast   = "se"
    show South       = "s"
    show SouthWest   = "sw"
    show West        = "w"
    show NorthWest   = "nw"
    show CenterPoint = "c"
    show NoCP        = "_"

instance Parseable CompassPoint where
    parse = optionalQuoted
            $ oneOf [ string "n"  >> return North
                    , string "ne" >> return NorthEast
                    , string "e"  >> return East
                    , string "se" >> return SouthEast
                    , string "s"  >> return South
                    , string "sw" >> return SouthWest
                    , string "w"  >> return West
                    , string "nw" >> return NorthWest
                    , string "c"  >> return CenterPoint
                    , string "_"  >> return NoCP
                    ]

-- -----------------------------------------------------------------------------

data ViewPort = VP { w     :: Double
                   , h     :: Double
                   , z     :: Double
                   , focus :: Maybe FocusType
                   }
                deriving (Eq, Read)

instance Show ViewPort where
    show vp = show
              . maybe id (flip (++) . show) (focus vp)
              $ show (w vp)
              ++ ',' : show (h vp)
              ++ ',' : show (z vp)

instance Parseable ViewPort where
    parse = quotedParse
            $ do wv <- parse
                 char ','
                 hv <- parse
                 char ','
                 zv <- parse
                 mf <- optional $ char ',' >> parse
                 return $ VP wv hv zv mf

data FocusType = XY Point
               | NodeName String
                 deriving (Eq, Read)

instance Show FocusType where
    show (XY p)        = showPoint p
    show (NodeName nm) = nm

instance Parseable FocusType where
    parse = oneOf [ liftM XY parsePoint
                  , liftM NodeName stringBlock
                  ]

-- -----------------------------------------------------------------------------

-- | Note that @VCenter@ is only valid for Nodes.
data VerticalPlacement = VTop
                       | VCenter
                       | VBottom
                         deriving (Eq, Read)

instance Show VerticalPlacement where
    show VTop    = "t"
    show VCenter = "c"
    show VBottom = "b"

instance Parseable VerticalPlacement where
    parse = optionalQuoted
            $ oneOf [ string "t" >> return VTop
                    , string "c" >> return VCenter
                    , string "b" >> return VBottom
                    ]


-- -----------------------------------------------------------------------------

data ScaleType = UniformScale
               | NoScale
               | FillWidth
               | FillHeight
               | FillBoth
                 deriving (Eq, Read)

instance Show ScaleType where
    show UniformScale = "true"
    show NoScale      = "false"
    show FillWidth    = "width"
    show FillHeight   = "height"
    show FillBoth     = "both"

instance Parseable ScaleType where
    parse = optionalQuoted
            $ oneOf [ string "true"   >> return UniformScale
                    , string "false"  >> return NoScale
                    , string "width"  >> return FillWidth
                    , string "height" >> return FillHeight
                    , string "both"   >> return FillBoth
                    ]

-- -----------------------------------------------------------------------------

data Justification = JLeft
                   | JRight
                   | JCenter
                     deriving (Eq, Read)

instance Show Justification where
    show JLeft   = "l"
    show JRight  = "r"
    show JCenter = "c"

instance Parseable Justification where
    parse = optionalQuoted
            $ oneOf [ string "l" >> return JLeft
                    , string "r" >> return JRight
                    , string "c" >> return JCenter
                    ]

-- -----------------------------------------------------------------------------

data Ratios = AspectRatio Double
            | FillRatio
            | CompressRatio
            | ExpandRatio
            | AutoRatio
              deriving (Eq, Read)

instance Show Ratios where
    show (AspectRatio r) = show r
    show FillRatio       = "fill"
    show CompressRatio   = "compress"
    show ExpandRatio     = "expand"
    show AutoRatio       = "auto"

instance Parseable Ratios where
    parse = optionalQuoted
            $ oneOf [ liftM AspectRatio parse
                    , string "fill"     >> return FillRatio
                    , string "compress" >> return CompressRatio
                    , string "expand"   >> return ExpandRatio
                    , string "auto"     >> return AutoRatio
                    ]

-- -----------------------------------------------------------------------------

newtype QuotedString = QS { str :: String }
    deriving (Eq, Read)

instance Show QuotedString where
    show = show . str

instance Parseable QuotedString where
    parse = liftM (QS . tail . init) quotedString

-- -----------------------------------------------------------------------------
