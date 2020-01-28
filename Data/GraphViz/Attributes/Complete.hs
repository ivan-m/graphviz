{-# LANGUAGE CPP, OverloadedStrings #-}

{- |
   Module      : Data.GraphViz.Attributes.Complete
   Description : Definition of the Graphviz attributes.
   Copyright   : (c) Matthew Sackman, Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   If you are just using graphviz to create basic Dot graphs, then you
   probably want to use "Data.GraphViz.Attributes" rather than this
   module.

   This module defines the various attributes that different parts of
   a Graphviz graph can have.  These attributes are based on the
   documentation found at:
     <http://graphviz.org/doc/info/attrs.html>

   For more information on usage, etc. please see that document.

   A summary of known current constraints\/limitations\/differences:

   * Note that for an edge, in /Dot/ parlance if the edge goes from
     /A/ to /B/, then /A/ is the tail node and /B/ is the head node
     (since /A/ is at the tail end of the arrow).

   * @ColorList@, @DoubleList@ and @PointfList@ are defined as actual
     lists (@'LayerList'@ needs a newtype for other reasons).  All of these
     are assumed to be non-empty lists.

   * For the various @*Color@ attributes that take in a list of
     'Color' values, usually only one color is used.  The @Color@
     attribute for edges allows multiple values; for other attributes,
     two values are supported for gradient fills in Graphviz >=
     2.29.0.

   * Style is implemented as a list of 'StyleItem' values; note that
     empty lists are not allowed.

   * A lot of values have a possible value of @none@.  These now
     have custom constructors.  In fact, most constructors have been
     expanded upon to give an idea of what they represent rather than
     using generic terms.

   * 'Rect' uses two 'Point' values to denote the lower-left and
     top-right corners.

   * The two 'LabelLoc' attributes have been combined.

   * @SplineType@ has been replaced with @['Spline']@.

   * Only polygon-based 'Shape's are available.

   * Not every 'Attribute' is fully documented/described.  However,
     all those which have specific allowed values should be covered.

   * Deprecated 'Overlap' algorithms are not defined.  Furthermore,
     the ability to specify an integer prefix for use with the fdp layout
     is /not/ supported.

   * The global @Orientation@ attribute is not defined, as it is
     difficult to distinguish from the node-based 'Orientation'
     'Attribute'; also, its behaviour is duplicated by 'Rotate'.

   * The @charset@ attribute is not available, as graphviz only
     supports UTF-8 encoding (as it is not currently feasible nor needed to
     also support Latin1 encoding).

   * In Graphviz, when a node or edge has a list of attributes, the
     colorscheme which is used to identify a color can be set /after/
     that color (e.g. @[colorscheme=x11,color=grey,colorscheme=svg]@
     uses the svg colorscheme's definition of grey, which is different
     from the x11 one.  Instead, graphviz parses them in order.

 -}
module Data.GraphViz.Attributes.Complete
       ( -- * The actual /Dot/ attributes.
         -- $attributes
         Attribute(..)
       , Attributes
       , sameAttribute
       , defaultAttributeValue
       , rmUnwantedAttributes
         -- ** Validity functions on @Attribute@ values.
       , usedByGraphs
       , usedBySubGraphs
       , usedByClusters
       , usedByNodes
       , usedByEdges
       , validUnknown

         -- ** Custom attributes.
       , AttributeName
       , CustomAttribute
       , customAttribute
       , isCustom
       , isSpecifiedCustom
       , customValue
       , customName
       , findCustoms
       , findSpecifiedCustom
       , deleteCustomAttributes
       , deleteSpecifiedCustom

         -- * Value types for @Attribute@s.
       , module Data.GraphViz.Attributes.Colors

         -- ** Generic types
       , Number (..)

         -- ** Labels
       , EscString
       , Label(..)
       , VerticalPlacement(..)
       , LabelScheme(..)
       , SVGFontNames(..)
         -- *** Types representing the Dot grammar for records.
       , RecordFields
       , RecordField(..)
       , Rect(..)
       , Justification(..)

         -- ** Nodes
       , Shape(..)
       , Paths(..)
       , ScaleType(..)
       , NodeSize(..)

         -- ** Edges
       , DirType(..)
       , EdgeType(..)
         -- *** Modifying where edges point
       , PortName(..)
       , PortPos(..)
       , CompassPoint(..)
         -- *** Arrows
       , ArrowType(..)
       , ArrowShape(..)
       , ArrowModifier(..)
       , ArrowFill(..)
       , ArrowSide(..)
         -- **** @ArrowModifier@ values
       , noMods
       , openMod

         -- ** Positioning
       , Point(..)
       , createPoint
       , Pos(..)
       , Spline(..)
       , DPoint(..)
       , Normalized (..)

         -- ** Layout
       , GraphvizCommand(..)
       , GraphSize(..)
       , ClusterMode(..)
       , Model(..)
       , Overlap(..)
       , Root(..)
       , Order(..)
       , OutputMode(..)
       , Pack(..)
       , PackMode(..)
       , PageDir(..)
       , QuadType(..)
       , RankType(..)
       , RankDir(..)
       , StartType(..)
       , ViewPort(..)
       , FocusType(..)
       , Ratios(..)

         -- ** Modes
       , ModeType(..)
       , DEConstraints(..)

         -- ** Layers
       , LayerSep(..)
       , LayerListSep(..)
       , LayerRange
       , LayerRangeElem(..)
       , LayerID(..)
       , LayerList(..)

         -- ** Stylistic
       , SmoothType(..)
       , STStyle(..)
       , StyleItem(..)
       , StyleName(..)
       ) where

import Data.GraphViz.Attributes.Arrows
import Data.GraphViz.Attributes.Colors
import Data.GraphViz.Attributes.Colors.X11 (X11Color(Black))
import Data.GraphViz.Attributes.Internal
import Data.GraphViz.Attributes.Values
import Data.GraphViz.Commands.Available
import Data.GraphViz.Exception             (GraphvizException(NotCustomAttr),
                                            throw)
import Data.GraphViz.Internal.State        (getsGS, parseStrictly)
import Data.GraphViz.Internal.Util         (bool, isIDString, keywords,
                                            restIDString)
import Data.GraphViz.Parsing
import Data.GraphViz.Printing

import           Data.List      (partition)
import           Data.Maybe     (isNothing)
import qualified Data.Set       as S
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Version   (Version(..))
import           Data.Word      (Word16)

#if !MIN_VERSION_base (4,13,0)
import Data.Monoid ((<>))
#endif

-- -----------------------------------------------------------------------------

{- $attributes

   These attributes have been implemented in a /permissive/ manner:
   that is, rather than split them up based on which type of value
   they are allowed, they have all been included in the one data type,
   with functions to determine if they are indeed valid for what
   they're being applied to.

   To interpret the /Valid for/ listings:

     [@G@] Valid for Graphs.

     [@C@] Valid for Clusters.

     [@S@] Valid for Sub-Graphs (and also Clusters).

     [@N@] Valid for Nodes.

     [@E@] Valid for Edges.

   The /Default/ listings are those that the various Graphviz commands
   use if that 'Attribute' isn't specified (in cases where this is
   /none/, this is equivalent to a 'Nothing' value; that is, no value
   is used).  The /Parsing Default/ listings represent what value is
   used (i.e. corresponds to 'True') when the 'Attribute' name is
   listed on its own in /Dot/ source code.

   Please note that the 'UnknownAttribute' 'Attribute' is defined
   primarily for backwards-compatibility purposes.  It is possible to use
   it directly for custom purposes; for more information, please see
   'CustomAttribute'.  The 'deleteCustomAttributes' can be used to delete
   these values.

 -}

-- | Attributes are used to customise the layout and design of Dot
--   graphs.  Care must be taken to ensure that the attribute you use
--   is valid, as not all attributes can be used everywhere.
data Attribute
  = Damping Double                      -- ^ /Valid for/: G; /Default/: @0.99@; /Minimum/: @0.0@; /Notes/: 'Neato' only
  | K Double                            -- ^ /Valid for/: GC; /Default/: @0.3@; /Minimum/: @0@; /Notes/: 'Sfdp', 'Fdp' only
  | URL EscString                       -- ^ /Valid for/: ENGC; /Default/: none; /Notes/: svg, postscript, map only
  | Area Double                         -- ^ /Valid for/: NC; /Default/: @1.0@; /Minimum/: @>0@; /Notes/: 'Patchwork' only, requires Graphviz >= 2.30.0
  | ArrowHead ArrowType                 -- ^ /Valid for/: E; /Default/: @'normal'@
  | ArrowSize Double                    -- ^ /Valid for/: E; /Default/: @1.0@; /Minimum/: @0.0@
  | ArrowTail ArrowType                 -- ^ /Valid for/: E; /Default/: @'normal'@
  | Background Text                     -- ^ /Valid for/: G; /Default/: none; /Notes/: xdot only
  | BoundingBox Rect                    -- ^ /Valid for/: G; /Notes/: write only
  | BgColor ColorList                   -- ^ /Valid for/: GC; /Default/: @[]@
  | Center Bool                         -- ^ /Valid for/: G; /Default/: @'False'@; /Parsing Default/: 'True'
  | ClusterRank ClusterMode             -- ^ /Valid for/: G; /Default/: @'Local'@; /Notes/: 'Dot' only
  | Color ColorList                     -- ^ /Valid for/: ENC; /Default/: @['WC' ('X11Color' 'Black') Nothing]@
  | ColorScheme ColorScheme             -- ^ /Valid for/: ENCG; /Default/: @'X11'@
  | Comment Text                        -- ^ /Valid for/: ENG; /Default/: @\"\"@
  | Compound Bool                       -- ^ /Valid for/: G; /Default/: @'False'@; /Parsing Default/: 'True'; /Notes/: 'Dot' only
  | Concentrate Bool                    -- ^ /Valid for/: G; /Default/: @'False'@; /Parsing Default/: 'True'
  | Constraint Bool                     -- ^ /Valid for/: E; /Default/: @'True'@; /Parsing Default/: 'True'; /Notes/: 'Dot' only
  | Decorate Bool                       -- ^ /Valid for/: E; /Default/: @'False'@; /Parsing Default/: 'True'
  | DefaultDist Double                  -- ^ /Valid for/: G; /Default/: @1+(avg. len)*sqrt(abs(V))@ (unable to statically define); /Minimum/: The value of 'Epsilon'.; /Notes/: 'Neato' only, only if @'Pack' 'DontPack'@
  | Dim Int                             -- ^ /Valid for/: G; /Default/: @2@; /Minimum/: @2@; /Notes/: maximum of @10@; 'Sfdp', 'Fdp', 'Neato' only
  | Dimen Int                           -- ^ /Valid for/: G; /Default/: @2@; /Minimum/: @2@; /Notes/: maximum of @10@; 'Sfdp', 'Fdp', 'Neato' only
  | Dir DirType                         -- ^ /Valid for/: E; /Default/: @'Forward'@ (directed), @'NoDir'@ (undirected)
  | DirEdgeConstraints DEConstraints    -- ^ /Valid for/: G; /Default/: @'NoConstraints'@; /Parsing Default/: 'EdgeConstraints'; /Notes/: 'Neato' only
  | Distortion Double                   -- ^ /Valid for/: N; /Default/: @0.0@; /Minimum/: @-100.0@
  | DPI Double                          -- ^ /Valid for/: G; /Default/: @96.0@, @0.0@; /Notes/: svg, bitmap output only; \"resolution\" is a synonym
  | EdgeURL EscString                   -- ^ /Valid for/: E; /Default/: @\"\"@; /Notes/: svg, map only
  | EdgeTarget EscString                -- ^ /Valid for/: E; /Default/: none; /Notes/: svg, map only
  | EdgeTooltip EscString               -- ^ /Valid for/: E; /Default/: @\"\"@; /Notes/: svg, cmap only
  | Epsilon Double                      -- ^ /Valid for/: G; /Default/: @.0001 * # nodes@ (@mode == 'KK'@), @.0001@ (@mode == 'Major'@); /Notes/: 'Neato' only
  | ESep DPoint                         -- ^ /Valid for/: G; /Default/: @'DVal' 3@; /Notes/: not 'Dot'
  | FillColor ColorList                 -- ^ /Valid for/: NEC; /Default/: @['WC' ('X11Color' 'LightGray') Nothing]@ (nodes), @['WC' ('X11Color' 'Black') Nothing]@ (clusters)
  | FixedSize NodeSize                  -- ^ /Valid for/: N; /Default/: @'GrowAsNeeded'@; /Parsing Default/: 'SetNodeSize'
  | FontColor Color                     -- ^ /Valid for/: ENGC; /Default/: @'X11Color' 'Black'@
  | FontName Text                       -- ^ /Valid for/: ENGC; /Default/: @\"Times-Roman\"@
  | FontNames SVGFontNames              -- ^ /Valid for/: G; /Default/: @'SvgNames'@; /Notes/: svg only
  | FontPath Paths                      -- ^ /Valid for/: G; /Default/: system dependent
  | FontSize Double                     -- ^ /Valid for/: ENGC; /Default/: @14.0@; /Minimum/: @1.0@
  | ForceLabels Bool                    -- ^ /Valid for/: G; /Default/: @'True'@; /Parsing Default/: 'True'; /Notes/: only for 'XLabel' attributes, requires Graphviz >= 2.29.0
  | GradientAngle Int                   -- ^ /Valid for/: NCG; /Default/: 0; /Notes/: requires Graphviz >= 2.29.0
  | Group Text                          -- ^ /Valid for/: N; /Default/: @\"\"@; /Notes/: 'Dot' only
  | HeadURL EscString                   -- ^ /Valid for/: E; /Default/: @\"\"@; /Notes/: svg, map only
  | Head_LP Point                       -- ^ /Valid for/: E; /Notes/: write only, requires Graphviz >= 2.30.0
  | HeadClip Bool                       -- ^ /Valid for/: E; /Default/: @'True'@; /Parsing Default/: 'True'
  | HeadLabel Label                     -- ^ /Valid for/: E; /Default/: @'StrLabel' \"\"@
  | HeadPort PortPos                    -- ^ /Valid for/: E; /Default/: @'CompassPoint' 'CenterPoint'@
  | HeadTarget EscString                -- ^ /Valid for/: E; /Default/: none; /Notes/: svg, map only
  | HeadTooltip EscString               -- ^ /Valid for/: E; /Default/: @\"\"@; /Notes/: svg, cmap only
  | Height Double                       -- ^ /Valid for/: N; /Default/: @0.5@; /Minimum/: @0.02@
  | ID EscString                        -- ^ /Valid for/: GNE; /Default/: @\"\"@; /Notes/: svg, postscript, map only
  | Image Text                          -- ^ /Valid for/: N; /Default/: @\"\"@
  | ImagePath Paths                     -- ^ /Valid for/: G; /Default/: @'Paths' []@; /Notes/: Printing and parsing is OS-specific, requires Graphviz >= 2.29.0
  | ImageScale ScaleType                -- ^ /Valid for/: N; /Default/: @'NoScale'@; /Parsing Default/: 'UniformScale'
  | InputScale Double                   -- ^ /Valid for/: N; /Default/: none; /Notes/: 'Fdp', 'Neato' only, a value of @0@ is equivalent to being @72@, requires Graphviz >= 2.36.0
  | Label Label                         -- ^ /Valid for/: ENGC; /Default/: @'StrLabel' \"\\N\"@ (nodes), @'StrLabel' \"\"@ (otherwise)
  | LabelURL EscString                  -- ^ /Valid for/: E; /Default/: @\"\"@; /Notes/: svg, map only
  | LabelScheme LabelScheme             -- ^ /Valid for/: G; /Default/: @'NotEdgeLabel'@; /Notes/: 'Sfdp' only, requires Graphviz >= 2.28.0
  | LabelAngle Double                   -- ^ /Valid for/: E; /Default/: @-25.0@; /Minimum/: @-180.0@
  | LabelDistance Double                -- ^ /Valid for/: E; /Default/: @1.0@; /Minimum/: @0.0@
  | LabelFloat Bool                     -- ^ /Valid for/: E; /Default/: @'False'@; /Parsing Default/: 'True'
  | LabelFontColor Color                -- ^ /Valid for/: E; /Default/: @'X11Color' 'Black'@
  | LabelFontName Text                  -- ^ /Valid for/: E; /Default/: @\"Times-Roman\"@
  | LabelFontSize Double                -- ^ /Valid for/: E; /Default/: @14.0@; /Minimum/: @1.0@
  | LabelJust Justification             -- ^ /Valid for/: GC; /Default/: @'JCenter'@
  | LabelLoc VerticalPlacement          -- ^ /Valid for/: GCN; /Default/: @'VTop'@ (clusters), @'VBottom'@ (root graphs), @'VCenter'@ (nodes)
  | LabelTarget EscString               -- ^ /Valid for/: E; /Default/: none; /Notes/: svg, map only
  | LabelTooltip EscString              -- ^ /Valid for/: E; /Default/: @\"\"@; /Notes/: svg, cmap only
  | Landscape Bool                      -- ^ /Valid for/: G; /Default/: @'False'@; /Parsing Default/: 'True'
  | Layer LayerRange                    -- ^ /Valid for/: ENC; /Default/: @[]@
  | LayerListSep LayerListSep           -- ^ /Valid for/: G; /Default/: @'LLSep' \",\"@; /Notes/: requires Graphviz >= 2.30.0
  | Layers LayerList                    -- ^ /Valid for/: G; /Default/: @'LL' []@
  | LayerSelect LayerRange              -- ^ /Valid for/: G; /Default/: @[]@
  | LayerSep LayerSep                   -- ^ /Valid for/: G; /Default/: @'LSep' \" :\t\"@
  | Layout GraphvizCommand              -- ^ /Valid for/: G
  | Len Double                          -- ^ /Valid for/: E; /Default/: @1.0@ ('Neato'), @0.3@ ('Fdp'); /Notes/: 'Fdp', 'Neato' only
  | Levels Int                          -- ^ /Valid for/: G; /Default/: @'maxBound'@; /Minimum/: @0@; /Notes/: 'Sfdp' only
  | LevelsGap Double                    -- ^ /Valid for/: G; /Default/: @0.0@; /Notes/: 'Neato' only
  | LHead Text                          -- ^ /Valid for/: E; /Default/: @\"\"@; /Notes/: 'Dot' only
  | LHeight Double                      -- ^ /Valid for/: GC; /Notes/: write only, requires Graphviz >= 2.28.0
  | LPos Point                          -- ^ /Valid for/: EGC; /Notes/: write only
  | LTail Text                          -- ^ /Valid for/: E; /Default/: @\"\"@; /Notes/: 'Dot' only
  | LWidth Double                       -- ^ /Valid for/: GC; /Notes/: write only, requires Graphviz >= 2.28.0
  | Margin DPoint                       -- ^ /Valid for/: NGC; /Default/: device dependent
  | MaxIter Int                         -- ^ /Valid for/: G; /Default/: @100 * # nodes@ (@mode == 'KK'@), @200@ (@mode == 'Major'@), @600@ ('Fdp'); /Notes/: 'Fdp', 'Neato' only
  | MCLimit Double                      -- ^ /Valid for/: G; /Default/: @1.0@; /Notes/: 'Dot' only
  | MinDist Double                      -- ^ /Valid for/: G; /Default/: @1.0@; /Minimum/: @0.0@; /Notes/: 'Circo' only
  | MinLen Int                          -- ^ /Valid for/: E; /Default/: @1@; /Minimum/: @0@; /Notes/: 'Dot' only
  | Mode ModeType                       -- ^ /Valid for/: G; /Default/: @'Major'@ (actually @'Spring'@ for 'Sfdp', but this isn't used as a default in this library); /Notes/: 'Neato', 'Sfdp' only
  | Model Model                         -- ^ /Valid for/: G; /Default/: @'ShortPath'@; /Notes/: 'Neato' only
  | Mosek Bool                          -- ^ /Valid for/: G; /Default/: @'False'@; /Parsing Default/: 'True'; /Notes/: 'Neato' only; requires the Mosek software
  | NodeSep Double                      -- ^ /Valid for/: G; /Default/: @0.25@; /Minimum/: @0.02@
  | NoJustify Bool                      -- ^ /Valid for/: GCNE; /Default/: @'False'@; /Parsing Default/: 'True'
  | Normalize Normalized                -- ^ /Valid for/: G; /Default/: @'NotNormalized'@; /Parsing Default/: 'IsNormalized'; /Notes/: not 'Dot'
  | NoTranslate Bool                    -- ^ /Valid for/: G; /Default/: @'False'@; /Parsing Default/: 'True'; /Notes/: 'Neato' only, requires Graphviz >= 2.38.0
  | Nslimit Double                      -- ^ /Valid for/: G; /Notes/: 'Dot' only
  | Nslimit1 Double                     -- ^ /Valid for/: G; /Notes/: 'Dot' only
  | Ordering Order                      -- ^ /Valid for/: GN; /Default/: none; /Notes/: 'Dot' only
  | Orientation Double                  -- ^ /Valid for/: N; /Default/: @0.0@; /Minimum/: @360.0@
  | OutputOrder OutputMode              -- ^ /Valid for/: G; /Default/: @'BreadthFirst'@
  | Overlap Overlap                     -- ^ /Valid for/: G; /Default/: @'KeepOverlaps'@; /Parsing Default/: 'KeepOverlaps'; /Notes/: not 'Dot'
  | OverlapScaling Double               -- ^ /Valid for/: G; /Default/: @-4@; /Minimum/: @-1.0e10@; /Notes/: 'PrismOverlap' only
  | OverlapShrink Bool                  -- ^ /Valid for/: G; /Default/: @'True'@; /Parsing Default/: 'True'; /Notes/: 'PrismOverlap' only, requires Graphviz >= 2.36.0
  | Pack Pack                           -- ^ /Valid for/: G; /Default/: @'DontPack'@; /Parsing Default/: 'DoPack'
  | PackMode PackMode                   -- ^ /Valid for/: G; /Default/: @'PackNode'@
  | Pad DPoint                          -- ^ /Valid for/: G; /Default/: @'DVal' 0.0555@ (4 points)
  | Page Point                          -- ^ /Valid for/: G
  | PageDir PageDir                     -- ^ /Valid for/: G; /Default/: @'Bl'@
  | PenColor Color                      -- ^ /Valid for/: C; /Default/: @'X11Color' 'Black'@
  | PenWidth Double                     -- ^ /Valid for/: CNE; /Default/: @1.0@; /Minimum/: @0.0@
  | Peripheries Int                     -- ^ /Valid for/: NC; /Default/: shape default (nodes), @1@ (clusters); /Minimum/: 0
  | Pin Bool                            -- ^ /Valid for/: N; /Default/: @'False'@; /Parsing Default/: 'True'; /Notes/: 'Fdp', 'Neato' only
  | Pos Pos                             -- ^ /Valid for/: EN
  | QuadTree QuadType                   -- ^ /Valid for/: G; /Default/: @'NormalQT'@; /Parsing Default/: 'NormalQT'; /Notes/: 'Sfdp' only
  | Quantum Double                      -- ^ /Valid for/: G; /Default/: @0.0@; /Minimum/: @0.0@
  | Rank RankType                       -- ^ /Valid for/: S; /Notes/: 'Dot' only
  | RankDir RankDir                     -- ^ /Valid for/: G; /Default/: @'FromTop'@; /Notes/: 'Dot' only
  | RankSep [Double]                    -- ^ /Valid for/: G; /Default/: @[0.5]@ ('Dot'), @[1.0]@ ('Twopi'); /Minimum/: @[0.02]@; /Notes/: 'Twopi', 'Dot' only
  | Ratio Ratios                        -- ^ /Valid for/: G
  | Rects [Rect]                        -- ^ /Valid for/: N; /Notes/: write only
  | Regular Bool                        -- ^ /Valid for/: N; /Default/: @'False'@; /Parsing Default/: 'True'
  | ReMinCross Bool                     -- ^ /Valid for/: G; /Default/: @'False'@; /Parsing Default/: 'True'; /Notes/: 'Dot' only
  | RepulsiveForce Double               -- ^ /Valid for/: G; /Default/: @1.0@; /Minimum/: @0.0@; /Notes/: 'Sfdp' only
  | Root Root                           -- ^ /Valid for/: GN; /Default/: @'NodeName' \"\"@ (graphs), @'NotCentral'@ (nodes); /Parsing Default/: 'IsCentral'; /Notes/: 'Circo', 'Twopi' only
  | Rotate Int                          -- ^ /Valid for/: G; /Default/: @0@
  | Rotation Double                     -- ^ /Valid for/: G; /Default/: @0@; /Notes/: 'Sfdp' only, requires Graphviz >= 2.28.0
  | SameHead Text                       -- ^ /Valid for/: E; /Default/: @\"\"@; /Notes/: 'Dot' only
  | SameTail Text                       -- ^ /Valid for/: E; /Default/: @\"\"@; /Notes/: 'Dot' only
  | SamplePoints Int                    -- ^ /Valid for/: N; /Default/: @8@ (output), @20@ (overlap and image maps)
  | Scale DPoint                        -- ^ /Valid for/: G; /Notes/: Not 'Dot', requires Graphviz >= 2.28.0 (>= 2.38.0 for anything except 'TwoPi')
  | SearchSize Int                      -- ^ /Valid for/: G; /Default/: @30@; /Notes/: 'Dot' only
  | Sep DPoint                          -- ^ /Valid for/: G; /Default/: @'DVal' 4@; /Notes/: not 'Dot'
  | Shape Shape                         -- ^ /Valid for/: N; /Default/: @'Ellipse'@
  | ShowBoxes Int                       -- ^ /Valid for/: ENG; /Default/: @0@; /Minimum/: @0@; /Notes/: 'Dot' only; used for debugging by printing PostScript guide boxes
  | Sides Int                           -- ^ /Valid for/: N; /Default/: @4@; /Minimum/: @0@
  | Size GraphSize                      -- ^ /Valid for/: G
  | Skew Double                         -- ^ /Valid for/: N; /Default/: @0.0@; /Minimum/: @-100.0@
  | Smoothing SmoothType                -- ^ /Valid for/: G; /Default/: @'NoSmooth'@; /Notes/: 'Sfdp' only
  | SortV Word16                        -- ^ /Valid for/: GCN; /Default/: @0@; /Minimum/: @0@
  | Splines EdgeType                    -- ^ /Valid for/: G; /Default/: @'SplineEdges'@ ('Dot'), @'LineEdges'@ (other); /Parsing Default/: 'SplineEdges'
  | Start StartType                     -- ^ /Valid for/: G; /Default/: @'StartStyleSeed' 'RandomStyle' seed@ for some unknown fixed seed.; /Notes/: 'Fdp', 'Neato' only
  | Style [StyleItem]                   -- ^ /Valid for/: ENCG
  | StyleSheet Text                     -- ^ /Valid for/: G; /Default/: @\"\"@; /Notes/: svg only
  | TailURL EscString                   -- ^ /Valid for/: E; /Default/: @\"\"@; /Notes/: svg, map only
  | Tail_LP Point                       -- ^ /Valid for/: E; /Notes/: write only, requires Graphviz >= 2.30.0
  | TailClip Bool                       -- ^ /Valid for/: E; /Default/: @'True'@; /Parsing Default/: 'True'
  | TailLabel Label                     -- ^ /Valid for/: E; /Default/: @'StrLabel' \"\"@
  | TailPort PortPos                    -- ^ /Valid for/: E; /Default/: @'CompassPoint' 'CenterPoint'@
  | TailTarget EscString                -- ^ /Valid for/: E; /Default/: none; /Notes/: svg, map only
  | TailTooltip EscString               -- ^ /Valid for/: E; /Default/: @\"\"@; /Notes/: svg, cmap only
  | Target EscString                    -- ^ /Valid for/: ENGC; /Default/: none; /Notes/: svg, map only
  | Tooltip EscString                   -- ^ /Valid for/: NEC; /Default/: @\"\"@; /Notes/: svg, cmap only
  | TrueColor Bool                      -- ^ /Valid for/: G; /Parsing Default/: 'True'; /Notes/: bitmap output only
  | Vertices [Point]                    -- ^ /Valid for/: N; /Notes/: write only
  | ViewPort ViewPort                   -- ^ /Valid for/: G; /Default/: none
  | VoroMargin Double                   -- ^ /Valid for/: G; /Default/: @0.05@; /Minimum/: @0.0@; /Notes/: not 'Dot'
  | Weight Number                       -- ^ /Valid for/: E; /Default/: @'Int' 1@; /Minimum/: @'Int' 0@ ('Dot'), @'Int' 1@ ('Neato','Fdp','Sfdp'); /Notes/: as of Graphviz 2.30: weights for dot need to be 'Int's
  | Width Double                        -- ^ /Valid for/: N; /Default/: @0.75@; /Minimum/: @0.01@
  | XDotVersion Version                 -- ^ /Valid for/: G; /Notes/: xdot only, requires Graphviz >= 2.34.0, equivalent to specifying version of xdot to be used
  | XLabel Label                        -- ^ /Valid for/: EN; /Default/: @'StrLabel' \"\"@; /Notes/: requires Graphviz >= 2.29.0
  | XLP Point                           -- ^ /Valid for/: EN; /Notes/: write only, requires Graphviz >= 2.29.0
  | UnknownAttribute AttributeName Text -- ^ /Valid for/: Assumed valid for all; the fields are 'Attribute' name and value respectively.
  deriving (Eq, Ord, Show, Read)

type Attributes = [Attribute]

-- | The name for an UnknownAttribute; must satisfy  'validUnknown'.
type AttributeName = Text

instance PrintDot Attribute where
  unqtDot (Damping v)            = printField "Damping" v
  unqtDot (K v)                  = printField "K" v
  unqtDot (URL v)                = printField "URL" v
  unqtDot (Area v)               = printField "area" v
  unqtDot (ArrowHead v)          = printField "arrowhead" v
  unqtDot (ArrowSize v)          = printField "arrowsize" v
  unqtDot (ArrowTail v)          = printField "arrowtail" v
  unqtDot (Background v)         = printField "_background" v
  unqtDot (BoundingBox v)        = printField "bb" v
  unqtDot (BgColor v)            = printField "bgcolor" v
  unqtDot (Center v)             = printField "center" v
  unqtDot (ClusterRank v)        = printField "clusterrank" v
  unqtDot (Color v)              = printField "color" v
  unqtDot (ColorScheme v)        = printField "colorscheme" v
  unqtDot (Comment v)            = printField "comment" v
  unqtDot (Compound v)           = printField "compound" v
  unqtDot (Concentrate v)        = printField "concentrate" v
  unqtDot (Constraint v)         = printField "constraint" v
  unqtDot (Decorate v)           = printField "decorate" v
  unqtDot (DefaultDist v)        = printField "defaultdist" v
  unqtDot (Dim v)                = printField "dim" v
  unqtDot (Dimen v)              = printField "dimen" v
  unqtDot (Dir v)                = printField "dir" v
  unqtDot (DirEdgeConstraints v) = printField "diredgeconstraints" v
  unqtDot (Distortion v)         = printField "distortion" v
  unqtDot (DPI v)                = printField "dpi" v
  unqtDot (EdgeURL v)            = printField "edgeURL" v
  unqtDot (EdgeTarget v)         = printField "edgetarget" v
  unqtDot (EdgeTooltip v)        = printField "edgetooltip" v
  unqtDot (Epsilon v)            = printField "epsilon" v
  unqtDot (ESep v)               = printField "esep" v
  unqtDot (FillColor v)          = printField "fillcolor" v
  unqtDot (FixedSize v)          = printField "fixedsize" v
  unqtDot (FontColor v)          = printField "fontcolor" v
  unqtDot (FontName v)           = printField "fontname" v
  unqtDot (FontNames v)          = printField "fontnames" v
  unqtDot (FontPath v)           = printField "fontpath" v
  unqtDot (FontSize v)           = printField "fontsize" v
  unqtDot (ForceLabels v)        = printField "forcelabels" v
  unqtDot (GradientAngle v)      = printField "gradientangle" v
  unqtDot (Group v)              = printField "group" v
  unqtDot (HeadURL v)            = printField "headURL" v
  unqtDot (Head_LP v)            = printField "head_lp" v
  unqtDot (HeadClip v)           = printField "headclip" v
  unqtDot (HeadLabel v)          = printField "headlabel" v
  unqtDot (HeadPort v)           = printField "headport" v
  unqtDot (HeadTarget v)         = printField "headtarget" v
  unqtDot (HeadTooltip v)        = printField "headtooltip" v
  unqtDot (Height v)             = printField "height" v
  unqtDot (ID v)                 = printField "id" v
  unqtDot (Image v)              = printField "image" v
  unqtDot (ImagePath v)          = printField "imagepath" v
  unqtDot (ImageScale v)         = printField "imagescale" v
  unqtDot (InputScale v)         = printField "inputscale" v
  unqtDot (Label v)              = printField "label" v
  unqtDot (LabelURL v)           = printField "labelURL" v
  unqtDot (LabelScheme v)        = printField "label_scheme" v
  unqtDot (LabelAngle v)         = printField "labelangle" v
  unqtDot (LabelDistance v)      = printField "labeldistance" v
  unqtDot (LabelFloat v)         = printField "labelfloat" v
  unqtDot (LabelFontColor v)     = printField "labelfontcolor" v
  unqtDot (LabelFontName v)      = printField "labelfontname" v
  unqtDot (LabelFontSize v)      = printField "labelfontsize" v
  unqtDot (LabelJust v)          = printField "labeljust" v
  unqtDot (LabelLoc v)           = printField "labelloc" v
  unqtDot (LabelTarget v)        = printField "labeltarget" v
  unqtDot (LabelTooltip v)       = printField "labeltooltip" v
  unqtDot (Landscape v)          = printField "landscape" v
  unqtDot (Layer v)              = printField "layer" v
  unqtDot (LayerListSep v)       = printField "layerlistsep" v
  unqtDot (Layers v)             = printField "layers" v
  unqtDot (LayerSelect v)        = printField "layerselect" v
  unqtDot (LayerSep v)           = printField "layersep" v
  unqtDot (Layout v)             = printField "layout" v
  unqtDot (Len v)                = printField "len" v
  unqtDot (Levels v)             = printField "levels" v
  unqtDot (LevelsGap v)          = printField "levelsgap" v
  unqtDot (LHead v)              = printField "lhead" v
  unqtDot (LHeight v)            = printField "LHeight" v
  unqtDot (LPos v)               = printField "lp" v
  unqtDot (LTail v)              = printField "ltail" v
  unqtDot (LWidth v)             = printField "lwidth" v
  unqtDot (Margin v)             = printField "margin" v
  unqtDot (MaxIter v)            = printField "maxiter" v
  unqtDot (MCLimit v)            = printField "mclimit" v
  unqtDot (MinDist v)            = printField "mindist" v
  unqtDot (MinLen v)             = printField "minlen" v
  unqtDot (Mode v)               = printField "mode" v
  unqtDot (Model v)              = printField "model" v
  unqtDot (Mosek v)              = printField "mosek" v
  unqtDot (NodeSep v)            = printField "nodesep" v
  unqtDot (NoJustify v)          = printField "nojustify" v
  unqtDot (Normalize v)          = printField "normalize" v
  unqtDot (NoTranslate v)        = printField "notranslate" v
  unqtDot (Nslimit v)            = printField "nslimit" v
  unqtDot (Nslimit1 v)           = printField "nslimit1" v
  unqtDot (Ordering v)           = printField "ordering" v
  unqtDot (Orientation v)        = printField "orientation" v
  unqtDot (OutputOrder v)        = printField "outputorder" v
  unqtDot (Overlap v)            = printField "overlap" v
  unqtDot (OverlapScaling v)     = printField "overlap_scaling" v
  unqtDot (OverlapShrink v)      = printField "overlap_shrink" v
  unqtDot (Pack v)               = printField "pack" v
  unqtDot (PackMode v)           = printField "packmode" v
  unqtDot (Pad v)                = printField "pad" v
  unqtDot (Page v)               = printField "page" v
  unqtDot (PageDir v)            = printField "pagedir" v
  unqtDot (PenColor v)           = printField "pencolor" v
  unqtDot (PenWidth v)           = printField "penwidth" v
  unqtDot (Peripheries v)        = printField "peripheries" v
  unqtDot (Pin v)                = printField "pin" v
  unqtDot (Pos v)                = printField "pos" v
  unqtDot (QuadTree v)           = printField "quadtree" v
  unqtDot (Quantum v)            = printField "quantum" v
  unqtDot (Rank v)               = printField "rank" v
  unqtDot (RankDir v)            = printField "rankdir" v
  unqtDot (RankSep v)            = printField "ranksep" v
  unqtDot (Ratio v)              = printField "ratio" v
  unqtDot (Rects v)              = printField "rects" v
  unqtDot (Regular v)            = printField "regular" v
  unqtDot (ReMinCross v)         = printField "remincross" v
  unqtDot (RepulsiveForce v)     = printField "repulsiveforce" v
  unqtDot (Root v)               = printField "root" v
  unqtDot (Rotate v)             = printField "rotate" v
  unqtDot (Rotation v)           = printField "rotation" v
  unqtDot (SameHead v)           = printField "samehead" v
  unqtDot (SameTail v)           = printField "sametail" v
  unqtDot (SamplePoints v)       = printField "samplepoints" v
  unqtDot (Scale v)              = printField "scale" v
  unqtDot (SearchSize v)         = printField "searchsize" v
  unqtDot (Sep v)                = printField "sep" v
  unqtDot (Shape v)              = printField "shape" v
  unqtDot (ShowBoxes v)          = printField "showboxes" v
  unqtDot (Sides v)              = printField "sides" v
  unqtDot (Size v)               = printField "size" v
  unqtDot (Skew v)               = printField "skew" v
  unqtDot (Smoothing v)          = printField "smoothing" v
  unqtDot (SortV v)              = printField "sortv" v
  unqtDot (Splines v)            = printField "splines" v
  unqtDot (Start v)              = printField "start" v
  unqtDot (Style v)              = printField "style" v
  unqtDot (StyleSheet v)         = printField "stylesheet" v
  unqtDot (TailURL v)            = printField "tailURL" v
  unqtDot (Tail_LP v)            = printField "tail_lp" v
  unqtDot (TailClip v)           = printField "tailclip" v
  unqtDot (TailLabel v)          = printField "taillabel" v
  unqtDot (TailPort v)           = printField "tailport" v
  unqtDot (TailTarget v)         = printField "tailtarget" v
  unqtDot (TailTooltip v)        = printField "tailtooltip" v
  unqtDot (Target v)             = printField "target" v
  unqtDot (Tooltip v)            = printField "tooltip" v
  unqtDot (TrueColor v)          = printField "truecolor" v
  unqtDot (Vertices v)           = printField "vertices" v
  unqtDot (ViewPort v)           = printField "viewport" v
  unqtDot (VoroMargin v)         = printField "voro_margin" v
  unqtDot (Weight v)             = printField "weight" v
  unqtDot (Width v)              = printField "width" v
  unqtDot (XDotVersion v)        = printField "xdotversion" v
  unqtDot (XLabel v)             = printField "xlabel" v
  unqtDot (XLP v)                = printField "xlp" v
  unqtDot (UnknownAttribute a v) = toDot a <> equals <> toDot v

  listToDot = unqtListToDot

instance ParseDot Attribute where
  parseUnqt = stringParse (concat [ parseField Damping "Damping"
                                  , parseField K "K"
                                  , parseFields URL ["URL", "href"]
                                  , parseField Area "area"
                                  , parseField ArrowHead "arrowhead"
                                  , parseField ArrowSize "arrowsize"
                                  , parseField ArrowTail "arrowtail"
                                  , parseField Background "_background"
                                  , parseField BoundingBox "bb"
                                  , parseField BgColor "bgcolor"
                                  , parseFieldBool Center "center"
                                  , parseField ClusterRank "clusterrank"
                                  , parseField Color "color"
                                  , parseField ColorScheme "colorscheme"
                                  , parseField Comment "comment"
                                  , parseFieldBool Compound "compound"
                                  , parseFieldBool Concentrate "concentrate"
                                  , parseFieldBool Constraint "constraint"
                                  , parseFieldBool Decorate "decorate"
                                  , parseField DefaultDist "defaultdist"
                                  , parseField Dim "dim"
                                  , parseField Dimen "dimen"
                                  , parseField Dir "dir"
                                  , parseFieldDef DirEdgeConstraints EdgeConstraints "diredgeconstraints"
                                  , parseField Distortion "distortion"
                                  , parseFields DPI ["dpi", "resolution"]
                                  , parseFields EdgeURL ["edgeURL", "edgehref"]
                                  , parseField EdgeTarget "edgetarget"
                                  , parseField EdgeTooltip "edgetooltip"
                                  , parseField Epsilon "epsilon"
                                  , parseField ESep "esep"
                                  , parseField FillColor "fillcolor"
                                  , parseFieldDef FixedSize SetNodeSize "fixedsize"
                                  , parseField FontColor "fontcolor"
                                  , parseField FontName "fontname"
                                  , parseField FontNames "fontnames"
                                  , parseField FontPath "fontpath"
                                  , parseField FontSize "fontsize"
                                  , parseFieldBool ForceLabels "forcelabels"
                                  , parseField GradientAngle "gradientangle"
                                  , parseField Group "group"
                                  , parseFields HeadURL ["headURL", "headhref"]
                                  , parseField Head_LP "head_lp"
                                  , parseFieldBool HeadClip "headclip"
                                  , parseField HeadLabel "headlabel"
                                  , parseField HeadPort "headport"
                                  , parseField HeadTarget "headtarget"
                                  , parseField HeadTooltip "headtooltip"
                                  , parseField Height "height"
                                  , parseField ID "id"
                                  , parseField Image "image"
                                  , parseField ImagePath "imagepath"
                                  , parseFieldDef ImageScale UniformScale "imagescale"
                                  , parseField InputScale "inputscale"
                                  , parseField Label "label"
                                  , parseFields LabelURL ["labelURL", "labelhref"]
                                  , parseField LabelScheme "label_scheme"
                                  , parseField LabelAngle "labelangle"
                                  , parseField LabelDistance "labeldistance"
                                  , parseFieldBool LabelFloat "labelfloat"
                                  , parseField LabelFontColor "labelfontcolor"
                                  , parseField LabelFontName "labelfontname"
                                  , parseField LabelFontSize "labelfontsize"
                                  , parseField LabelJust "labeljust"
                                  , parseField LabelLoc "labelloc"
                                  , parseField LabelTarget "labeltarget"
                                  , parseField LabelTooltip "labeltooltip"
                                  , parseFieldBool Landscape "landscape"
                                  , parseField Layer "layer"
                                  , parseField LayerListSep "layerlistsep"
                                  , parseField Layers "layers"
                                  , parseField LayerSelect "layerselect"
                                  , parseField LayerSep "layersep"
                                  , parseField Layout "layout"
                                  , parseField Len "len"
                                  , parseField Levels "levels"
                                  , parseField LevelsGap "levelsgap"
                                  , parseField LHead "lhead"
                                  , parseField LHeight "LHeight"
                                  , parseField LPos "lp"
                                  , parseField LTail "ltail"
                                  , parseField LWidth "lwidth"
                                  , parseField Margin "margin"
                                  , parseField MaxIter "maxiter"
                                  , parseField MCLimit "mclimit"
                                  , parseField MinDist "mindist"
                                  , parseField MinLen "minlen"
                                  , parseField Mode "mode"
                                  , parseField Model "model"
                                  , parseFieldBool Mosek "mosek"
                                  , parseField NodeSep "nodesep"
                                  , parseFieldBool NoJustify "nojustify"
                                  , parseFieldDef Normalize IsNormalized "normalize"
                                  , parseFieldBool NoTranslate "notranslate"
                                  , parseField Nslimit "nslimit"
                                  , parseField Nslimit1 "nslimit1"
                                  , parseField Ordering "ordering"
                                  , parseField Orientation "orientation"
                                  , parseField OutputOrder "outputorder"
                                  , parseFieldDef Overlap KeepOverlaps "overlap"
                                  , parseField OverlapScaling "overlap_scaling"
                                  , parseFieldBool OverlapShrink "overlap_shrink"
                                  , parseFieldDef Pack DoPack "pack"
                                  , parseField PackMode "packmode"
                                  , parseField Pad "pad"
                                  , parseField Page "page"
                                  , parseField PageDir "pagedir"
                                  , parseField PenColor "pencolor"
                                  , parseField PenWidth "penwidth"
                                  , parseField Peripheries "peripheries"
                                  , parseFieldBool Pin "pin"
                                  , parseField Pos "pos"
                                  , parseFieldDef QuadTree NormalQT "quadtree"
                                  , parseField Quantum "quantum"
                                  , parseField Rank "rank"
                                  , parseField RankDir "rankdir"
                                  , parseField RankSep "ranksep"
                                  , parseField Ratio "ratio"
                                  , parseField Rects "rects"
                                  , parseFieldBool Regular "regular"
                                  , parseFieldBool ReMinCross "remincross"
                                  , parseField RepulsiveForce "repulsiveforce"
                                  , parseFieldDef Root IsCentral "root"
                                  , parseField Rotate "rotate"
                                  , parseField Rotation "rotation"
                                  , parseField SameHead "samehead"
                                  , parseField SameTail "sametail"
                                  , parseField SamplePoints "samplepoints"
                                  , parseField Scale "scale"
                                  , parseField SearchSize "searchsize"
                                  , parseField Sep "sep"
                                  , parseField Shape "shape"
                                  , parseField ShowBoxes "showboxes"
                                  , parseField Sides "sides"
                                  , parseField Size "size"
                                  , parseField Skew "skew"
                                  , parseField Smoothing "smoothing"
                                  , parseField SortV "sortv"
                                  , parseFieldDef Splines SplineEdges "splines"
                                  , parseField Start "start"
                                  , parseField Style "style"
                                  , parseField StyleSheet "stylesheet"
                                  , parseFields TailURL ["tailURL", "tailhref"]
                                  , parseField Tail_LP "tail_lp"
                                  , parseFieldBool TailClip "tailclip"
                                  , parseField TailLabel "taillabel"
                                  , parseField TailPort "tailport"
                                  , parseField TailTarget "tailtarget"
                                  , parseField TailTooltip "tailtooltip"
                                  , parseField Target "target"
                                  , parseField Tooltip "tooltip"
                                  , parseFieldBool TrueColor "truecolor"
                                  , parseField Vertices "vertices"
                                  , parseField ViewPort "viewport"
                                  , parseField VoroMargin "voro_margin"
                                  , parseField Weight "weight"
                                  , parseField Width "width"
                                  , parseField XDotVersion "xdotversion"
                                  , parseField XLabel "xlabel"
                                  , parseField XLP "xlp"
                                  ])
              `onFail`
              do attrName <- stringBlock
                 liftEqParse ("UnknownAttribute (" ++ T.unpack attrName ++ ")")
                             (UnknownAttribute attrName)

  parse = parseUnqt

  parseList = parseUnqtList

-- | Determine if this 'Attribute' is valid for use with Graphs.
usedByGraphs                      :: Attribute -> Bool
usedByGraphs Damping{}            = True
usedByGraphs K{}                  = True
usedByGraphs URL{}                = True
usedByGraphs Background{}         = True
usedByGraphs BoundingBox{}        = True
usedByGraphs BgColor{}            = True
usedByGraphs Center{}             = True
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
usedByGraphs ForceLabels{}        = True
usedByGraphs GradientAngle{}      = True
usedByGraphs ID{}                 = True
usedByGraphs ImagePath{}          = True
usedByGraphs Label{}              = True
usedByGraphs LabelScheme{}        = True
usedByGraphs LabelJust{}          = True
usedByGraphs LabelLoc{}           = True
usedByGraphs Landscape{}          = True
usedByGraphs LayerListSep{}       = True
usedByGraphs Layers{}             = True
usedByGraphs LayerSelect{}        = True
usedByGraphs LayerSep{}           = True
usedByGraphs Layout{}             = True
usedByGraphs Levels{}             = True
usedByGraphs LevelsGap{}          = True
usedByGraphs LHeight{}            = True
usedByGraphs LPos{}               = True
usedByGraphs LWidth{}             = True
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
usedByGraphs NoTranslate{}        = True
usedByGraphs Nslimit{}            = True
usedByGraphs Nslimit1{}           = True
usedByGraphs Ordering{}           = True
usedByGraphs OutputOrder{}        = True
usedByGraphs Overlap{}            = True
usedByGraphs OverlapScaling{}     = True
usedByGraphs OverlapShrink{}      = True
usedByGraphs Pack{}               = True
usedByGraphs PackMode{}           = True
usedByGraphs Pad{}                = True
usedByGraphs Page{}               = True
usedByGraphs PageDir{}            = True
usedByGraphs QuadTree{}           = True
usedByGraphs Quantum{}            = True
usedByGraphs RankDir{}            = True
usedByGraphs RankSep{}            = True
usedByGraphs Ratio{}              = True
usedByGraphs ReMinCross{}         = True
usedByGraphs RepulsiveForce{}     = True
usedByGraphs Root{}               = True
usedByGraphs Rotate{}             = True
usedByGraphs Rotation{}           = True
usedByGraphs Scale{}              = True
usedByGraphs SearchSize{}         = True
usedByGraphs Sep{}                = True
usedByGraphs ShowBoxes{}          = True
usedByGraphs Size{}               = True
usedByGraphs Smoothing{}          = True
usedByGraphs SortV{}              = True
usedByGraphs Splines{}            = True
usedByGraphs Start{}              = True
usedByGraphs Style{}              = True
usedByGraphs StyleSheet{}         = True
usedByGraphs Target{}             = True
usedByGraphs TrueColor{}          = True
usedByGraphs ViewPort{}           = True
usedByGraphs VoroMargin{}         = True
usedByGraphs XDotVersion{}        = True
usedByGraphs UnknownAttribute{}   = True
usedByGraphs _                    = False

-- | Determine if this 'Attribute' is valid for use with Clusters.
usedByClusters                    :: Attribute -> Bool
usedByClusters K{}                = True
usedByClusters URL{}              = True
usedByClusters Area{}             = True
usedByClusters BgColor{}          = True
usedByClusters Color{}            = True
usedByClusters ColorScheme{}      = True
usedByClusters FillColor{}        = True
usedByClusters FontColor{}        = True
usedByClusters FontName{}         = True
usedByClusters FontSize{}         = True
usedByClusters GradientAngle{}    = True
usedByClusters Label{}            = True
usedByClusters LabelJust{}        = True
usedByClusters LabelLoc{}         = True
usedByClusters Layer{}            = True
usedByClusters LHeight{}          = True
usedByClusters LPos{}             = True
usedByClusters LWidth{}           = True
usedByClusters Margin{}           = True
usedByClusters NoJustify{}        = True
usedByClusters PenColor{}         = True
usedByClusters PenWidth{}         = True
usedByClusters Peripheries{}      = True
usedByClusters Rank{}             = True
usedByClusters SortV{}            = True
usedByClusters Style{}            = True
usedByClusters Target{}           = True
usedByClusters Tooltip{}          = True
usedByClusters UnknownAttribute{} = True
usedByClusters _                  = False

-- | Determine if this 'Attribute' is valid for use with SubGraphs.
usedBySubGraphs                    :: Attribute -> Bool
usedBySubGraphs Rank{}             = True
usedBySubGraphs UnknownAttribute{} = True
usedBySubGraphs _                  = False

-- | Determine if this 'Attribute' is valid for use with Nodes.
usedByNodes                    :: Attribute -> Bool
usedByNodes URL{}              = True
usedByNodes Area{}             = True
usedByNodes Color{}            = True
usedByNodes ColorScheme{}      = True
usedByNodes Comment{}          = True
usedByNodes Distortion{}       = True
usedByNodes FillColor{}        = True
usedByNodes FixedSize{}        = True
usedByNodes FontColor{}        = True
usedByNodes FontName{}         = True
usedByNodes FontSize{}         = True
usedByNodes GradientAngle{}    = True
usedByNodes Group{}            = True
usedByNodes Height{}           = True
usedByNodes ID{}               = True
usedByNodes Image{}            = True
usedByNodes ImageScale{}       = True
usedByNodes InputScale{}       = True
usedByNodes Label{}            = True
usedByNodes LabelLoc{}         = True
usedByNodes Layer{}            = True
usedByNodes Margin{}           = True
usedByNodes NoJustify{}        = True
usedByNodes Ordering{}         = True
usedByNodes Orientation{}      = True
usedByNodes PenWidth{}         = True
usedByNodes Peripheries{}      = True
usedByNodes Pin{}              = True
usedByNodes Pos{}              = True
usedByNodes Rects{}            = True
usedByNodes Regular{}          = True
usedByNodes Root{}             = True
usedByNodes SamplePoints{}     = True
usedByNodes Shape{}            = True
usedByNodes ShowBoxes{}        = True
usedByNodes Sides{}            = True
usedByNodes Skew{}             = True
usedByNodes SortV{}            = True
usedByNodes Style{}            = True
usedByNodes Target{}           = True
usedByNodes Tooltip{}          = True
usedByNodes Vertices{}         = True
usedByNodes Width{}            = True
usedByNodes XLabel{}           = True
usedByNodes XLP{}              = True
usedByNodes UnknownAttribute{} = True
usedByNodes _                  = False

-- | Determine if this 'Attribute' is valid for use with Edges.
usedByEdges                    :: Attribute -> Bool
usedByEdges URL{}              = True
usedByEdges ArrowHead{}        = True
usedByEdges ArrowSize{}        = True
usedByEdges ArrowTail{}        = True
usedByEdges Color{}            = True
usedByEdges ColorScheme{}      = True
usedByEdges Comment{}          = True
usedByEdges Constraint{}       = True
usedByEdges Decorate{}         = True
usedByEdges Dir{}              = True
usedByEdges EdgeURL{}          = True
usedByEdges EdgeTarget{}       = True
usedByEdges EdgeTooltip{}      = True
usedByEdges FillColor{}        = True
usedByEdges FontColor{}        = True
usedByEdges FontName{}         = True
usedByEdges FontSize{}         = True
usedByEdges HeadURL{}          = True
usedByEdges Head_LP{}          = True
usedByEdges HeadClip{}         = True
usedByEdges HeadLabel{}        = True
usedByEdges HeadPort{}         = True
usedByEdges HeadTarget{}       = True
usedByEdges HeadTooltip{}      = True
usedByEdges ID{}               = True
usedByEdges Label{}            = True
usedByEdges LabelURL{}         = True
usedByEdges LabelAngle{}       = True
usedByEdges LabelDistance{}    = True
usedByEdges LabelFloat{}       = True
usedByEdges LabelFontColor{}   = True
usedByEdges LabelFontName{}    = True
usedByEdges LabelFontSize{}    = True
usedByEdges LabelTarget{}      = True
usedByEdges LabelTooltip{}     = True
usedByEdges Layer{}            = True
usedByEdges Len{}              = True
usedByEdges LHead{}            = True
usedByEdges LPos{}             = True
usedByEdges LTail{}            = True
usedByEdges MinLen{}           = True
usedByEdges NoJustify{}        = True
usedByEdges PenWidth{}         = True
usedByEdges Pos{}              = True
usedByEdges SameHead{}         = True
usedByEdges SameTail{}         = True
usedByEdges ShowBoxes{}        = True
usedByEdges Style{}            = True
usedByEdges TailURL{}          = True
usedByEdges Tail_LP{}          = True
usedByEdges TailClip{}         = True
usedByEdges TailLabel{}        = True
usedByEdges TailPort{}         = True
usedByEdges TailTarget{}       = True
usedByEdges TailTooltip{}      = True
usedByEdges Target{}           = True
usedByEdges Tooltip{}          = True
usedByEdges Weight{}           = True
usedByEdges XLabel{}           = True
usedByEdges XLP{}              = True
usedByEdges UnknownAttribute{} = True
usedByEdges _                  = False

-- | Determine if two 'Attributes' are the same type of 'Attribute'.
sameAttribute                                                 :: Attribute -> Attribute -> Bool
sameAttribute Damping{}               Damping{}               = True
sameAttribute K{}                     K{}                     = True
sameAttribute URL{}                   URL{}                   = True
sameAttribute Area{}                  Area{}                  = True
sameAttribute ArrowHead{}             ArrowHead{}             = True
sameAttribute ArrowSize{}             ArrowSize{}             = True
sameAttribute ArrowTail{}             ArrowTail{}             = True
sameAttribute Background{}            Background{}            = True
sameAttribute BoundingBox{}           BoundingBox{}           = True
sameAttribute BgColor{}               BgColor{}               = True
sameAttribute Center{}                Center{}                = True
sameAttribute ClusterRank{}           ClusterRank{}           = True
sameAttribute Color{}                 Color{}                 = True
sameAttribute ColorScheme{}           ColorScheme{}           = True
sameAttribute Comment{}               Comment{}               = True
sameAttribute Compound{}              Compound{}              = True
sameAttribute Concentrate{}           Concentrate{}           = True
sameAttribute Constraint{}            Constraint{}            = True
sameAttribute Decorate{}              Decorate{}              = True
sameAttribute DefaultDist{}           DefaultDist{}           = True
sameAttribute Dim{}                   Dim{}                   = True
sameAttribute Dimen{}                 Dimen{}                 = True
sameAttribute Dir{}                   Dir{}                   = True
sameAttribute DirEdgeConstraints{}    DirEdgeConstraints{}    = True
sameAttribute Distortion{}            Distortion{}            = True
sameAttribute DPI{}                   DPI{}                   = True
sameAttribute EdgeURL{}               EdgeURL{}               = True
sameAttribute EdgeTarget{}            EdgeTarget{}            = True
sameAttribute EdgeTooltip{}           EdgeTooltip{}           = True
sameAttribute Epsilon{}               Epsilon{}               = True
sameAttribute ESep{}                  ESep{}                  = True
sameAttribute FillColor{}             FillColor{}             = True
sameAttribute FixedSize{}             FixedSize{}             = True
sameAttribute FontColor{}             FontColor{}             = True
sameAttribute FontName{}              FontName{}              = True
sameAttribute FontNames{}             FontNames{}             = True
sameAttribute FontPath{}              FontPath{}              = True
sameAttribute FontSize{}              FontSize{}              = True
sameAttribute ForceLabels{}           ForceLabels{}           = True
sameAttribute GradientAngle{}         GradientAngle{}         = True
sameAttribute Group{}                 Group{}                 = True
sameAttribute HeadURL{}               HeadURL{}               = True
sameAttribute Head_LP{}               Head_LP{}               = True
sameAttribute HeadClip{}              HeadClip{}              = True
sameAttribute HeadLabel{}             HeadLabel{}             = True
sameAttribute HeadPort{}              HeadPort{}              = True
sameAttribute HeadTarget{}            HeadTarget{}            = True
sameAttribute HeadTooltip{}           HeadTooltip{}           = True
sameAttribute Height{}                Height{}                = True
sameAttribute ID{}                    ID{}                    = True
sameAttribute Image{}                 Image{}                 = True
sameAttribute ImagePath{}             ImagePath{}             = True
sameAttribute ImageScale{}            ImageScale{}            = True
sameAttribute InputScale{}            InputScale{}            = True
sameAttribute Label{}                 Label{}                 = True
sameAttribute LabelURL{}              LabelURL{}              = True
sameAttribute LabelScheme{}           LabelScheme{}           = True
sameAttribute LabelAngle{}            LabelAngle{}            = True
sameAttribute LabelDistance{}         LabelDistance{}         = True
sameAttribute LabelFloat{}            LabelFloat{}            = True
sameAttribute LabelFontColor{}        LabelFontColor{}        = True
sameAttribute LabelFontName{}         LabelFontName{}         = True
sameAttribute LabelFontSize{}         LabelFontSize{}         = True
sameAttribute LabelJust{}             LabelJust{}             = True
sameAttribute LabelLoc{}              LabelLoc{}              = True
sameAttribute LabelTarget{}           LabelTarget{}           = True
sameAttribute LabelTooltip{}          LabelTooltip{}          = True
sameAttribute Landscape{}             Landscape{}             = True
sameAttribute Layer{}                 Layer{}                 = True
sameAttribute LayerListSep{}          LayerListSep{}          = True
sameAttribute Layers{}                Layers{}                = True
sameAttribute LayerSelect{}           LayerSelect{}           = True
sameAttribute LayerSep{}              LayerSep{}              = True
sameAttribute Layout{}                Layout{}                = True
sameAttribute Len{}                   Len{}                   = True
sameAttribute Levels{}                Levels{}                = True
sameAttribute LevelsGap{}             LevelsGap{}             = True
sameAttribute LHead{}                 LHead{}                 = True
sameAttribute LHeight{}               LHeight{}               = True
sameAttribute LPos{}                  LPos{}                  = True
sameAttribute LTail{}                 LTail{}                 = True
sameAttribute LWidth{}                LWidth{}                = True
sameAttribute Margin{}                Margin{}                = True
sameAttribute MaxIter{}               MaxIter{}               = True
sameAttribute MCLimit{}               MCLimit{}               = True
sameAttribute MinDist{}               MinDist{}               = True
sameAttribute MinLen{}                MinLen{}                = True
sameAttribute Mode{}                  Mode{}                  = True
sameAttribute Model{}                 Model{}                 = True
sameAttribute Mosek{}                 Mosek{}                 = True
sameAttribute NodeSep{}               NodeSep{}               = True
sameAttribute NoJustify{}             NoJustify{}             = True
sameAttribute Normalize{}             Normalize{}             = True
sameAttribute NoTranslate{}           NoTranslate{}           = True
sameAttribute Nslimit{}               Nslimit{}               = True
sameAttribute Nslimit1{}              Nslimit1{}              = True
sameAttribute Ordering{}              Ordering{}              = True
sameAttribute Orientation{}           Orientation{}           = True
sameAttribute OutputOrder{}           OutputOrder{}           = True
sameAttribute Overlap{}               Overlap{}               = True
sameAttribute OverlapScaling{}        OverlapScaling{}        = True
sameAttribute OverlapShrink{}         OverlapShrink{}         = True
sameAttribute Pack{}                  Pack{}                  = True
sameAttribute PackMode{}              PackMode{}              = True
sameAttribute Pad{}                   Pad{}                   = True
sameAttribute Page{}                  Page{}                  = True
sameAttribute PageDir{}               PageDir{}               = True
sameAttribute PenColor{}              PenColor{}              = True
sameAttribute PenWidth{}              PenWidth{}              = True
sameAttribute Peripheries{}           Peripheries{}           = True
sameAttribute Pin{}                   Pin{}                   = True
sameAttribute Pos{}                   Pos{}                   = True
sameAttribute QuadTree{}              QuadTree{}              = True
sameAttribute Quantum{}               Quantum{}               = True
sameAttribute Rank{}                  Rank{}                  = True
sameAttribute RankDir{}               RankDir{}               = True
sameAttribute RankSep{}               RankSep{}               = True
sameAttribute Ratio{}                 Ratio{}                 = True
sameAttribute Rects{}                 Rects{}                 = True
sameAttribute Regular{}               Regular{}               = True
sameAttribute ReMinCross{}            ReMinCross{}            = True
sameAttribute RepulsiveForce{}        RepulsiveForce{}        = True
sameAttribute Root{}                  Root{}                  = True
sameAttribute Rotate{}                Rotate{}                = True
sameAttribute Rotation{}              Rotation{}              = True
sameAttribute SameHead{}              SameHead{}              = True
sameAttribute SameTail{}              SameTail{}              = True
sameAttribute SamplePoints{}          SamplePoints{}          = True
sameAttribute Scale{}                 Scale{}                 = True
sameAttribute SearchSize{}            SearchSize{}            = True
sameAttribute Sep{}                   Sep{}                   = True
sameAttribute Shape{}                 Shape{}                 = True
sameAttribute ShowBoxes{}             ShowBoxes{}             = True
sameAttribute Sides{}                 Sides{}                 = True
sameAttribute Size{}                  Size{}                  = True
sameAttribute Skew{}                  Skew{}                  = True
sameAttribute Smoothing{}             Smoothing{}             = True
sameAttribute SortV{}                 SortV{}                 = True
sameAttribute Splines{}               Splines{}               = True
sameAttribute Start{}                 Start{}                 = True
sameAttribute Style{}                 Style{}                 = True
sameAttribute StyleSheet{}            StyleSheet{}            = True
sameAttribute TailURL{}               TailURL{}               = True
sameAttribute Tail_LP{}               Tail_LP{}               = True
sameAttribute TailClip{}              TailClip{}              = True
sameAttribute TailLabel{}             TailLabel{}             = True
sameAttribute TailPort{}              TailPort{}              = True
sameAttribute TailTarget{}            TailTarget{}            = True
sameAttribute TailTooltip{}           TailTooltip{}           = True
sameAttribute Target{}                Target{}                = True
sameAttribute Tooltip{}               Tooltip{}               = True
sameAttribute TrueColor{}             TrueColor{}             = True
sameAttribute Vertices{}              Vertices{}              = True
sameAttribute ViewPort{}              ViewPort{}              = True
sameAttribute VoroMargin{}            VoroMargin{}            = True
sameAttribute Weight{}                Weight{}                = True
sameAttribute Width{}                 Width{}                 = True
sameAttribute XDotVersion{}           XDotVersion{}           = True
sameAttribute XLabel{}                XLabel{}                = True
sameAttribute XLP{}                   XLP{}                   = True
sameAttribute (UnknownAttribute a1 _) (UnknownAttribute a2 _) = a1 == a2
sameAttribute _                       _                       = False

-- | Return the default value for a specific 'Attribute' if possible; graph/cluster values are preferred over node/edge values.
defaultAttributeValue                      :: Attribute -> Maybe Attribute
defaultAttributeValue Damping{}            = Just $ Damping 0.99
defaultAttributeValue K{}                  = Just $ K 0.3
defaultAttributeValue URL{}                = Just $ URL ""
defaultAttributeValue Area{}               = Just $ Area 1.0
defaultAttributeValue ArrowHead{}          = Just $ ArrowHead normal
defaultAttributeValue ArrowSize{}          = Just $ ArrowSize 1.0
defaultAttributeValue ArrowTail{}          = Just $ ArrowTail normal
defaultAttributeValue Background{}         = Just $ Background ""
defaultAttributeValue BgColor{}            = Just $ BgColor []
defaultAttributeValue Center{}             = Just $ Center False
defaultAttributeValue ClusterRank{}        = Just $ ClusterRank Local
defaultAttributeValue Color{}              = Just $ Color [toWColor Black]
defaultAttributeValue ColorScheme{}        = Just $ ColorScheme X11
defaultAttributeValue Comment{}            = Just $ Comment ""
defaultAttributeValue Compound{}           = Just $ Compound False
defaultAttributeValue Concentrate{}        = Just $ Concentrate False
defaultAttributeValue Constraint{}         = Just $ Constraint True
defaultAttributeValue Decorate{}           = Just $ Decorate False
defaultAttributeValue Dim{}                = Just $ Dim 2
defaultAttributeValue Dimen{}              = Just $ Dimen 2
defaultAttributeValue DirEdgeConstraints{} = Just $ DirEdgeConstraints NoConstraints
defaultAttributeValue Distortion{}         = Just $ Distortion 0.0
defaultAttributeValue DPI{}                = Just $ DPI 96.0
defaultAttributeValue EdgeURL{}            = Just $ EdgeURL ""
defaultAttributeValue EdgeTooltip{}        = Just $ EdgeTooltip ""
defaultAttributeValue ESep{}               = Just $ ESep (DVal 3)
defaultAttributeValue FillColor{}          = Just $ FillColor [toWColor Black]
defaultAttributeValue FixedSize{}          = Just $ FixedSize GrowAsNeeded
defaultAttributeValue FontColor{}          = Just $ FontColor (X11Color Black)
defaultAttributeValue FontName{}           = Just $ FontName "Times-Roman"
defaultAttributeValue FontNames{}          = Just $ FontNames SvgNames
defaultAttributeValue FontSize{}           = Just $ FontSize 14.0
defaultAttributeValue ForceLabels{}        = Just $ ForceLabels True
defaultAttributeValue GradientAngle{}      = Just $ GradientAngle 0
defaultAttributeValue Group{}              = Just $ Group ""
defaultAttributeValue HeadURL{}            = Just $ HeadURL ""
defaultAttributeValue HeadClip{}           = Just $ HeadClip True
defaultAttributeValue HeadLabel{}          = Just $ HeadLabel (StrLabel "")
defaultAttributeValue HeadPort{}           = Just $ HeadPort (CompassPoint CenterPoint)
defaultAttributeValue HeadTarget{}         = Just $ HeadTarget ""
defaultAttributeValue HeadTooltip{}        = Just $ HeadTooltip ""
defaultAttributeValue Height{}             = Just $ Height 0.5
defaultAttributeValue ID{}                 = Just $ ID ""
defaultAttributeValue Image{}              = Just $ Image ""
defaultAttributeValue ImagePath{}          = Just $ ImagePath (Paths [])
defaultAttributeValue ImageScale{}         = Just $ ImageScale NoScale
defaultAttributeValue Label{}              = Just $ Label (StrLabel "")
defaultAttributeValue LabelURL{}           = Just $ LabelURL ""
defaultAttributeValue LabelScheme{}        = Just $ LabelScheme NotEdgeLabel
defaultAttributeValue LabelAngle{}         = Just $ LabelAngle (-25.0)
defaultAttributeValue LabelDistance{}      = Just $ LabelDistance 1.0
defaultAttributeValue LabelFloat{}         = Just $ LabelFloat False
defaultAttributeValue LabelFontColor{}     = Just $ LabelFontColor (X11Color Black)
defaultAttributeValue LabelFontName{}      = Just $ LabelFontName "Times-Roman"
defaultAttributeValue LabelFontSize{}      = Just $ LabelFontSize 14.0
defaultAttributeValue LabelJust{}          = Just $ LabelJust JCenter
defaultAttributeValue LabelLoc{}           = Just $ LabelLoc VTop
defaultAttributeValue LabelTarget{}        = Just $ LabelTarget ""
defaultAttributeValue LabelTooltip{}       = Just $ LabelTooltip ""
defaultAttributeValue Landscape{}          = Just $ Landscape False
defaultAttributeValue Layer{}              = Just $ Layer []
defaultAttributeValue LayerListSep{}       = Just $ LayerListSep (LLSep ",")
defaultAttributeValue Layers{}             = Just $ Layers (LL [])
defaultAttributeValue LayerSelect{}        = Just $ LayerSelect []
defaultAttributeValue LayerSep{}           = Just $ LayerSep (LSep " :\t")
defaultAttributeValue Levels{}             = Just $ Levels maxBound
defaultAttributeValue LevelsGap{}          = Just $ LevelsGap 0.0
defaultAttributeValue LHead{}              = Just $ LHead ""
defaultAttributeValue LTail{}              = Just $ LTail ""
defaultAttributeValue MCLimit{}            = Just $ MCLimit 1.0
defaultAttributeValue MinDist{}            = Just $ MinDist 1.0
defaultAttributeValue MinLen{}             = Just $ MinLen 1
defaultAttributeValue Mode{}               = Just $ Mode Major
defaultAttributeValue Model{}              = Just $ Model ShortPath
defaultAttributeValue Mosek{}              = Just $ Mosek False
defaultAttributeValue NodeSep{}            = Just $ NodeSep 0.25
defaultAttributeValue NoJustify{}          = Just $ NoJustify False
defaultAttributeValue Normalize{}          = Just $ Normalize NotNormalized
defaultAttributeValue NoTranslate{}        = Just $ NoTranslate False
defaultAttributeValue Orientation{}        = Just $ Orientation 0.0
defaultAttributeValue OutputOrder{}        = Just $ OutputOrder BreadthFirst
defaultAttributeValue Overlap{}            = Just $ Overlap KeepOverlaps
defaultAttributeValue OverlapScaling{}     = Just $ OverlapScaling (-4)
defaultAttributeValue OverlapShrink{}      = Just $ OverlapShrink True
defaultAttributeValue Pack{}               = Just $ Pack DontPack
defaultAttributeValue PackMode{}           = Just $ PackMode PackNode
defaultAttributeValue Pad{}                = Just $ Pad (DVal 0.0555)
defaultAttributeValue PageDir{}            = Just $ PageDir Bl
defaultAttributeValue PenColor{}           = Just $ PenColor (X11Color Black)
defaultAttributeValue PenWidth{}           = Just $ PenWidth 1.0
defaultAttributeValue Peripheries{}        = Just $ Peripheries 1
defaultAttributeValue Pin{}                = Just $ Pin False
defaultAttributeValue QuadTree{}           = Just $ QuadTree NormalQT
defaultAttributeValue Quantum{}            = Just $ Quantum 0
defaultAttributeValue RankDir{}            = Just $ RankDir FromTop
defaultAttributeValue Regular{}            = Just $ Regular False
defaultAttributeValue ReMinCross{}         = Just $ ReMinCross False
defaultAttributeValue RepulsiveForce{}     = Just $ RepulsiveForce 1.0
defaultAttributeValue Root{}               = Just $ Root (NodeName "")
defaultAttributeValue Rotate{}             = Just $ Rotate 0
defaultAttributeValue Rotation{}           = Just $ Rotation 0
defaultAttributeValue SameHead{}           = Just $ SameHead ""
defaultAttributeValue SameTail{}           = Just $ SameTail ""
defaultAttributeValue SearchSize{}         = Just $ SearchSize 30
defaultAttributeValue Sep{}                = Just $ Sep (DVal 4)
defaultAttributeValue Shape{}              = Just $ Shape Ellipse
defaultAttributeValue ShowBoxes{}          = Just $ ShowBoxes 0
defaultAttributeValue Sides{}              = Just $ Sides 4
defaultAttributeValue Skew{}               = Just $ Skew 0.0
defaultAttributeValue Smoothing{}          = Just $ Smoothing NoSmooth
defaultAttributeValue SortV{}              = Just $ SortV 0
defaultAttributeValue StyleSheet{}         = Just $ StyleSheet ""
defaultAttributeValue TailURL{}            = Just $ TailURL ""
defaultAttributeValue TailClip{}           = Just $ TailClip True
defaultAttributeValue TailLabel{}          = Just $ TailLabel (StrLabel "")
defaultAttributeValue TailPort{}           = Just $ TailPort (CompassPoint CenterPoint)
defaultAttributeValue TailTarget{}         = Just $ TailTarget ""
defaultAttributeValue TailTooltip{}        = Just $ TailTooltip ""
defaultAttributeValue Target{}             = Just $ Target ""
defaultAttributeValue Tooltip{}            = Just $ Tooltip ""
defaultAttributeValue VoroMargin{}         = Just $ VoroMargin 0.05
defaultAttributeValue Weight{}             = Just $ Weight (Int 1)
defaultAttributeValue Width{}              = Just $ Width 0.75
defaultAttributeValue XLabel{}             = Just $ XLabel (StrLabel "")
defaultAttributeValue _                    = Nothing

-- | Determine if the provided 'Text' value is a valid name for an 'UnknownAttribute'.
validUnknown     :: AttributeName -> Bool
validUnknown txt = T.toLower txt `S.notMember` names
                   && isIDString txt
  where
    names = (S.fromList . map T.toLower
             $ [ "Damping"
               , "K"
               , "URL"
               , "href"
               , "area"
               , "arrowhead"
               , "arrowsize"
               , "arrowtail"
               , "_background"
               , "bb"
               , "bgcolor"
               , "center"
               , "clusterrank"
               , "color"
               , "colorscheme"
               , "comment"
               , "compound"
               , "concentrate"
               , "constraint"
               , "decorate"
               , "defaultdist"
               , "dim"
               , "dimen"
               , "dir"
               , "diredgeconstraints"
               , "distortion"
               , "dpi"
               , "resolution"
               , "edgeURL"
               , "edgehref"
               , "edgetarget"
               , "edgetooltip"
               , "epsilon"
               , "esep"
               , "fillcolor"
               , "fixedsize"
               , "fontcolor"
               , "fontname"
               , "fontnames"
               , "fontpath"
               , "fontsize"
               , "forcelabels"
               , "gradientangle"
               , "group"
               , "headURL"
               , "headhref"
               , "head_lp"
               , "headclip"
               , "headlabel"
               , "headport"
               , "headtarget"
               , "headtooltip"
               , "height"
               , "id"
               , "image"
               , "imagepath"
               , "imagescale"
               , "inputscale"
               , "label"
               , "labelURL"
               , "labelhref"
               , "label_scheme"
               , "labelangle"
               , "labeldistance"
               , "labelfloat"
               , "labelfontcolor"
               , "labelfontname"
               , "labelfontsize"
               , "labeljust"
               , "labelloc"
               , "labeltarget"
               , "labeltooltip"
               , "landscape"
               , "layer"
               , "layerlistsep"
               , "layers"
               , "layerselect"
               , "layersep"
               , "layout"
               , "len"
               , "levels"
               , "levelsgap"
               , "lhead"
               , "LHeight"
               , "lp"
               , "ltail"
               , "lwidth"
               , "margin"
               , "maxiter"
               , "mclimit"
               , "mindist"
               , "minlen"
               , "mode"
               , "model"
               , "mosek"
               , "nodesep"
               , "nojustify"
               , "normalize"
               , "notranslate"
               , "nslimit"
               , "nslimit1"
               , "ordering"
               , "orientation"
               , "outputorder"
               , "overlap"
               , "overlap_scaling"
               , "overlap_shrink"
               , "pack"
               , "packmode"
               , "pad"
               , "page"
               , "pagedir"
               , "pencolor"
               , "penwidth"
               , "peripheries"
               , "pin"
               , "pos"
               , "quadtree"
               , "quantum"
               , "rank"
               , "rankdir"
               , "ranksep"
               , "ratio"
               , "rects"
               , "regular"
               , "remincross"
               , "repulsiveforce"
               , "root"
               , "rotate"
               , "rotation"
               , "samehead"
               , "sametail"
               , "samplepoints"
               , "scale"
               , "searchsize"
               , "sep"
               , "shape"
               , "showboxes"
               , "sides"
               , "size"
               , "skew"
               , "smoothing"
               , "sortv"
               , "splines"
               , "start"
               , "style"
               , "stylesheet"
               , "tailURL"
               , "tailhref"
               , "tail_lp"
               , "tailclip"
               , "taillabel"
               , "tailport"
               , "tailtarget"
               , "tailtooltip"
               , "target"
               , "tooltip"
               , "truecolor"
               , "vertices"
               , "viewport"
               , "voro_margin"
               , "weight"
               , "width"
               , "xdotversion"
               , "xlabel"
               , "xlp"
               , "charset" -- Defined upstream, just not used here.
               ])
            `S.union`
            keywords
{- Delete to here -}

-- | Remove attributes that we don't want to consider:
--
--   * Those that are defaults
--   * colorscheme (as the colors embed it anyway)
rmUnwantedAttributes :: Attributes -> Attributes
rmUnwantedAttributes = filter (not . (`any` tests) . flip ($))
  where
    tests = [isDefault, isColorScheme]

    isDefault a = maybe False (a==) $ defaultAttributeValue a

    isColorScheme ColorScheme{} = True
    isColorScheme _             = False

-- -----------------------------------------------------------------------------
-- These parsing combinators are defined here for customisation purposes.

parseField       :: (ParseDot a) => (a -> Attribute) -> String
                    -> [(String, Parse Attribute)]
parseField c fld = [(fld, liftEqParse fld c)]

parseFields   :: (ParseDot a) => (a -> Attribute) -> [String]
                 -> [(String, Parse Attribute)]
parseFields c = concatMap (parseField c)

parseFieldBool :: (Bool -> Attribute) -> String -> [(String, Parse Attribute)]
parseFieldBool = (`parseFieldDef` True)

-- | For 'Bool'-like data structures where the presence of the field
--   name without a value implies a default value.
parseFieldDef         :: (ParseDot a) => (a -> Attribute) -> a -> String
                         -> [(String, Parse Attribute)]
parseFieldDef c d fld = [(fld, p)]
  where
    p = liftEqParse fld c
        `onFail`
        do nxt <- optional $ satisfy restIDString
           bool (fail "Not actually the field you were after")
                (return $ c d)
                (isNothing nxt)

-- | Attempt to parse the @\"=value\"@ part of a @key=value@ pair.  If
--   there is an equal sign but the @value@ part doesn't parse, throw
--   an un-recoverable error.
liftEqParse :: (ParseDot a) => String -> (a -> Attribute) -> Parse Attribute
liftEqParse k c = do pStrict <- getsGS parseStrictly
                     let adjErr = bool adjustErr adjustErrBad pStrict
                     parseEq
                       *> ( hasDef (fmap c parse)
                            `adjErr`
                            (("Unable to parse key=value with key of " ++ k
                              ++ "\n\t") ++)
                          )
  where
    hasDef p = maybe p (onFail p . (`stringRep` "\"\""))
               . defaultAttributeValue $ c undefined

-- -----------------------------------------------------------------------------

{- | If performing any custom pre-/post-processing on Dot code, you
     may wish to utilise some custom 'Attributes'.  These are wrappers
     around the 'UnknownAttribute' constructor (and thus 'CustomAttribute'
     is just an alias for 'Attribute').

     You should ensure that 'validUnknown' is 'True' for any potential
     custom attribute name.

-}
type CustomAttribute = Attribute

-- | Create a custom attribute.
customAttribute :: AttributeName -> Text -> CustomAttribute
customAttribute = UnknownAttribute

-- | Determines whether or not this is a custom attribute.
isCustom                    :: Attribute -> Bool
isCustom UnknownAttribute{} = True
isCustom _                  = False

isSpecifiedCustom :: AttributeName -> Attribute -> Bool
isSpecifiedCustom nm (UnknownAttribute nm' _) = nm == nm'
isSpecifiedCustom _  _                        = False

-- | The value of a custom attribute.  Will throw a
--   'GraphvizException' if the provided 'Attribute' isn't a custom
--   one.
customValue :: CustomAttribute -> Text
customValue (UnknownAttribute _ v) = v
customValue attr                   = throw . NotCustomAttr . T.unpack
                                     $ printIt attr

-- | The name of a custom attribute.  Will throw a
--   'GraphvizException' if the provided 'Attribute' isn't a custom
--   one.
customName :: CustomAttribute -> AttributeName
customName (UnknownAttribute nm _) = nm
customName attr                    = throw . NotCustomAttr . T.unpack
                                      $ printIt attr

-- | Returns all custom attributes and the list of non-custom Attributes.
findCustoms :: Attributes -> ([CustomAttribute], Attributes)
findCustoms = partition isCustom

-- | Find the (first instance of the) specified custom attribute and
--   returns it along with all other Attributes.
findSpecifiedCustom :: AttributeName -> Attributes
                       -> Maybe (CustomAttribute, Attributes)
findSpecifiedCustom nm attrs
  = case break (isSpecifiedCustom nm) attrs of
      (bf,cust:aft) -> Just (cust, bf ++ aft)
      _             -> Nothing

-- | Delete all custom attributes (actually, this will delete all
--   'UnknownAttribute' values; as such it can also be used to remove
--   legacy attributes).
deleteCustomAttributes :: Attributes -> Attributes
deleteCustomAttributes = filter (not . isCustom)

-- | Removes all instances of the specified custom attribute.
deleteSpecifiedCustom :: AttributeName -> Attributes -> Attributes
deleteSpecifiedCustom nm = filter (not . isSpecifiedCustom nm)
