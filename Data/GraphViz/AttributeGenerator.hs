{- |
   Module      : Data.GraphViz.AttributeGenerator
   Description : Definition of the GraphViz attributes.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module is a stand-alone that generates the correct code for
   Data.GraphViz.Attributes.
  -}
module Data.GraphViz.AttributeGenerator where

import Text.PrettyPrint
import Data.List(transpose)
import Data.Maybe(catMaybes, isJust, fromJust)
import Control.Monad(liftM)

type Code = Doc

main :: IO ()
main = print $ genCode att
    where
      att = AS { tpNm = text "Attribute"
               , atts = attributes
               }

genCode     :: Atts -> Doc
genCode att = vsep $ map ($att) cds
    where
      cds = [ createDefn
            , createAlias
            , showInstance
            , parseInstance
            , usedByFunc "Graphs" forGraphs
            , usedByFunc "Clusters" forClusters
            , usedByFunc "SubGraphs" forSubGraphs
            , usedByFunc "Nodes" forNodes
            , usedByFunc "Edges" forEdges
            ]

-- -----------------------------------------------------------------------------

-- Defining data structures

data Atts = AS { tpNm :: Code
               , atts :: [Attribute]
               }

data Attribute = A { cnst         :: Code
                   , name         :: Code
                   , parseNames   :: [Code]
                   , valtype      :: VType
                   , parseDef     :: Maybe Code
                   , forGraphs    :: Bool
                   , forClusters  :: Bool
                   , forSubGraphs :: Bool
                   , forNodes     :: Bool
                   , forEdges     :: Bool
                   , comment      :: Doc
                   }

makeAttr                   :: Constructor -> [Name] -> UsedBy -> VType
                              -> Maybe Default -- Used when parsing the field
                              -> Maybe Default -> Maybe Minimum -> Maybe Comment
                              -> Attribute
makeAttr c ns u v df d m cm = A { cnst         = text c
                                , name         = head ns'
                                , parseNames   = ns'
                                , valtype      = v -- just in case need to do fancy
                                                   -- stuff
                                , parseDef     = liftM text df
                                , forGraphs    = isFor 'G'
                                , forClusters  = isFor 'C' || forSG
                                , forSubGraphs = forSG
                                , forNodes     = isFor 'N'
                                , forEdges     = isFor 'E'
                                , comment      = cm'
                                }
    where
      ns' = map text ns
      isFor f = f `elem` u
      forSG = isFor 'S'
      df' = if v == Bl then Just "'True'" else fmap ( \ t -> '\'' : t ++ "'") df
      mDoc (f,fc) = f <> colon <+> text fc
      addF f = fmap (\ dc -> (wrap (char '/') (text f), dc))
      cm' = hsep
            . punctuate semi
            . map mDoc
            $ catMaybes [ addF "Valid for" (Just u)
                        , addF "Default" d
                        , addF "Parsing Default" df'
                        , addF "Minimum" m
                        , addF "Notes" cm
                        ]

type Constructor = String
type Name = String
type UsedBy = String -- should only contain subset of "ENGCS"
type Default = String
type Minimum = String
type Comment = String

data VType = Dbl
           | Integ
           | Strng
           | QStrng
           | URL
           | Bl
           | Cust String
             deriving (Eq, Show, Read)

vtype           :: VType -> Doc
vtype Dbl       = text "Double"
vtype Integ     = text "Int"
vtype Strng     = text "String"
vtype QStrng    = text "QuotedString"
vtype URL       = text "URL"
vtype Bl        = text "Bool"
vtype (Cust t)  = text t

vtypeCode :: Attribute -> Code
vtypeCode = vtype . valtype

-- -----------------------------------------------------------------------------

createDefn     :: Atts -> Code
createDefn att = hdr $+$ constructors $+$ derivs
    where
      hdr = text "data" <+> tpNm att
      constructors = nest tab
                     . asRows
                     . firstOthers equals (char '|')
                     . map createDefn
                     $ atts att
      derivs = nest (tab + 2) $ text "deriving (Eq, Show, Read)"
      createDefn a = [cnst a <+> vtypeCode a
                     , if isEmpty cm
                       then empty
                       else text "-- ^" <+> cm
                     ]
          where
            cm = comment a

createAlias     :: Atts -> Code
createAlias att = text "type"
                  <+> tp <> char 's'
                  <+> equals
                  <+> brackets tp
    where
      tp = tpNm att

showInstance     :: Atts -> Code
showInstance att = hdr $+$ insts'
    where
      hdr = text "instance" <+> text "PrintDot" <+> tpNm att <+> text "where"
      var = char 'v'
      sFunc = text "unqtDot"
      cnct = text "<>"
      insts = asRows
              . map mkInstance
              $ atts att
      mkInstance a = [ sFunc <+> parens (cnst a <+> var)
                     , equals <+> text "printField" <+> doubleQuotes (name a)
                                  <+>  var
                     ]
      insts' = nest tab
              $ vsep [ insts
                     , text "listToDot" <+> equals <+> text "unqtListToDot"
                     ]

parseInstance     :: Atts -> Code
parseInstance att = hdr $+$ nest tab fns
    where
      hdr = text "instance" <+> text "ParseDot" <+> tpNm att <+> text "where"
      fn = pFunc <+> equals <+> text "oneOf" <+> ops
      fns = vsep [ fn
                 , text "parse" <+> equals <+> pFunc
                 , text "parseList" <+> equals <+> text "parseUnqtList"
                 ]
      ops = flip ($$) rbrack
            . asRows
            . firstOthers lbrack comma
            . map parseAttr
            $ atts att
      pFunc = text "parseUnqt"
      parseAttr a = [ text "liftM" <+> cnst a
                    , char '$' <+> pfFunc a
                    ]
      pType b a
          | valtype a == Bl     = pFld <> text "Bool"
          | isJust $ parseDef a = pFld <> text "Def" <+> fromJust (parseDef a)
          | otherwise           = pFld
          where
            pFld = text "parseField" <> if b then char 's' else empty

      pfFunc a = case map doubleQuotes $ parseNames a of
                   [n] -> pType False a <+> n
                   ns  -> pType True  a <+> docList ns

usedByFunc          :: String -> (Attribute -> Bool) -> Atts -> Code
usedByFunc nm p att = cmnt $$ asRows (tpSig : trs ++ [fls])
    where
      nm' = text nm
      dt = tpNm att
      cmnt = text "-- | Determine if this" <+> dt
             <+> text "is valid for use with" <+> nm' <> dot
      tpSig = [ fn
              , colon <> colon
                <+> dt <+> text "->" <+> text "Bool"
              ]
      fn = text "usedBy" <> nm'
      tr = text "True"
      trs = map aTr as'
      fl = text "False"
      fls = [ fn <+> char '_'
            , equals <+> fl
            ]
      as' = filter p $ atts att
      aTr a = [ fn <+> cnst a <> braces empty
              , equals <+> tr
              ]


-- -----------------------------------------------------------------------------

-- Helper functions

-- Size of a tab character
tab :: Int
tab = 4

firstOthers            :: Doc -> Doc -> [[Doc]] -> [[Doc]]
firstOthers _ _ []     = []
firstOthers f o (d:ds) = (f : d) : map ((:) o) ds

wrap     :: Doc -> Doc -> Doc
wrap w d = w <> d <> w

vsep :: [Doc] -> Doc
vsep = vcat . punctuate newline
    where
      newline = char '\n'

asRows    :: [[Doc]] -> Doc
asRows as = vcat $ map padR asL
    where
      asL = map (map (\d -> (d, docLen d))) as
      cWidths = map (maximum . map snd) $ transpose asL
      shiftLen rls = let (rs,ls) = unzip rls
                     in zip rs (0:ls)
      padR = hsep . zipWith append (0 : cWidths) . shiftLen
      append l' (d,l) = hcat (repl (l' - l) space) <> d
      repl n xs | n <= 0    = []
                | otherwise = replicate n xs

-- A really hacky thing to do, but oh well...
-- Don't use this for multi-line Docs!
docLen :: Doc -> Int
docLen = length . render

docList :: [Doc] -> Doc
docList = brackets . hsep . punctuate comma

dot :: Doc
dot = char '.'

-- -----------------------------------------------------------------------------

-- The actual attributes

attributes :: [Attribute]
attributes = [ makeAttr "Damping" ["Damping"] "G" Dbl Nothing (Just "@0.99@") (Just "@0.0@") (Just "neato only")
             , makeAttr "K" ["K"] "GC" Dbl Nothing (Just "@0.3@") (Just "@0@") (Just "sfdp, fdp only")
             , makeAttr "URL" ["URL", "href"] "ENGC" URL Nothing Nothing (Just "none") (Just "svg, postscript, map only")
             , makeAttr "ArrowHead" ["arrowhead"] "E" (Cust "ArrowType") Nothing (Just "@'normal'@") Nothing Nothing
             , makeAttr "ArrowSize" ["arrowsize"] "E" Dbl Nothing (Just "@1.0@") (Just "@0.0@") Nothing
             , makeAttr "ArrowTail" ["arrowtail"] "E" (Cust "ArrowType") Nothing (Just "@'normal'@") Nothing Nothing
             , makeAttr "Aspect" ["aspect"] "G" (Cust "AspectType") Nothing Nothing Nothing (Just "dot only")
             , makeAttr "Bb" ["bb"] "G" (Cust "Rect") Nothing Nothing Nothing (Just "write only")
             , makeAttr "BgColor" ["bgcolor"] "GC" (Cust "Color") Nothing (Just "none") Nothing Nothing
             , makeAttr "Center" ["center"] "G" Bl (Just "True") (Just "@'False'@") Nothing Nothing
             , makeAttr "Charset" ["charset"] "G" Strng Nothing (Just "@\\\"UTF-8\\\"@") Nothing Nothing
             , makeAttr "ClusterRank" ["clusterrank"] "G" (Cust "ClusterMode") Nothing (Just "@'Local'@") Nothing (Just "dot only")
             , makeAttr "Color" ["color"] "ENC" (Cust "[Color]") Nothing (Just "@black@") Nothing Nothing
             , makeAttr "ColorScheme" ["colorscheme"] "ENCG" Strng Nothing (Just "@\\\"\\\"@") Nothing Nothing
             , makeAttr "Comment" ["comment"] "ENG" Strng Nothing (Just "@\\\"\\\"@") Nothing Nothing
             , makeAttr "Compound" ["compound"] "G" Bl (Just "True") (Just "@'False'@") Nothing (Just "dot only")
             , makeAttr "Concentrate" ["concentrate"] "G" Bl (Just "True") (Just "@'False'@") Nothing Nothing
             , makeAttr "Constraint" ["constraint"] "E" Bl (Just "True")  (Just "@'True'@") Nothing (Just "dot only")
             , makeAttr "Decorate" ["decorate"] "E" Bl (Just "True") (Just "@'False'@") Nothing Nothing
             , makeAttr "DefaultDist" ["defaultdist"] "G" Dbl Nothing (Just "@1+(avg. len)*sqrt(|V|)@") (Just "@epsilon@") (Just "neato only")
             , makeAttr "Dim" ["dim"] "G" Integ Nothing (Just "@2@") (Just "@2@") (Just "sfdp, fdp, neato only")
             , makeAttr "Dimen" ["dimen"] "G" Integ Nothing (Just "@2@") (Just "@2@") (Just "sfdp, fdp, neato only")
             , makeAttr "Dir" ["dir"] "E" (Cust "DirType") Nothing (Just "@'Forward'@ (directed), @'NoDir'@ (undirected)") Nothing Nothing
             , makeAttr "DirEdgeConstraints" ["diredgeconstraints"] "G" (Cust "DEConstraints") (Just "EdgeConstraints") (Just "@'NoConstraints'@") Nothing (Just "neato only")
             , makeAttr "Distortion" ["distortion"] "N" Dbl Nothing (Just "@0.0@") (Just "@-100.0@") Nothing
             , makeAttr "DPI" ["dpi", "resolution"] "G" Dbl Nothing (Just "@96.0@, @0.0@") Nothing (Just "svg, bitmap output only; \\\"resolution\\\" is a synonym")
             , makeAttr "EdgeURL" ["edgeURL", "edgehref"] "E" URL Nothing (Just "@\\\"\\\"@") Nothing (Just "svg, map only")
             , makeAttr "EdgeTarget" ["edgetarget"] "E" Strng Nothing (Just "none") Nothing (Just "svg, map only")
             , makeAttr "EdgeTooltip" ["edgetooltip"] "E" Strng Nothing (Just "@\\\"\\\"@") Nothing (Just "svg, cmap only")
             , makeAttr "Epsilon" ["epsilon"] "G" Dbl Nothing (Just "@.0001 * # nodes@ (@mode == 'KK'@), @.0001@ (@mode == 'Major'@)") Nothing (Just "neato only")
             , makeAttr "ESep" ["esep"] "G" (Cust "DPoint") Nothing (Just "@+3@") Nothing (Just "not dot")
             , makeAttr "FillColor" ["fillcolor"] "NC" (Cust "Color") Nothing (Just "@lightgrey@ (nodes), @black@ (clusters)") Nothing Nothing
             , makeAttr "FixedSize" ["fixedsize"] "N" Bl (Just "True") (Just "@'False'@") Nothing Nothing
             , makeAttr "FontColor" ["fontcolor"] "ENGC" (Cust "Color") Nothing (Just "@black@") Nothing Nothing
             , makeAttr "FontName" ["fontname"] "ENGC" Strng Nothing (Just "@\\\"Times-Roman\\\"@") Nothing Nothing
             , makeAttr "FontNames" ["fontnames"] "G" Strng Nothing (Just "@\\\"\\\"@") Nothing (Just "svg only")
             , makeAttr "FontPath" ["fontpath"] "G" Strng Nothing (Just "system-dependent") Nothing Nothing
             , makeAttr "FontSize" ["fontsize"] "ENGC" Dbl Nothing (Just "@14.0@") (Just "@1.0@") Nothing
             , makeAttr "Group" ["group"] "N" Strng Nothing (Just "@\\\"\\\"@") Nothing (Just "dot only")
             , makeAttr "HeadURL" ["headURL", "headhref"] "E" URL Nothing (Just "@\\\"\\\"@") Nothing (Just "svg, map only")
             , makeAttr "HeadClip" ["headclip"] "E" Bl (Just "True") (Just "@'True'@") Nothing Nothing
             , makeAttr "HeadLabel" ["headlabel"] "E" (Cust "Label") Nothing (Just "@\\\"\\\"@") Nothing Nothing
             , makeAttr "HeadPort" ["headport"] "E" (Cust "PortPos") Nothing (Just "@'PP' 'CenterPoint'@") Nothing Nothing
             , makeAttr "HeadTarget" ["headtarget"] "E" Strng Nothing (Just "none") Nothing (Just "svg, map only")
             , makeAttr "HeadTooltip" ["headtooltip"] "E" Strng Nothing (Just "@\\\"\\\"@") Nothing (Just "svg, cmap only")
             , makeAttr "Height" ["height"] "N" Dbl Nothing (Just "@0.5@") (Just "@0.02@") Nothing
             , makeAttr "ID" ["id"] "GNE" (Cust "Label") Nothing (Just "@\\\"\\\"@") Nothing (Just "svg, postscript, map only")
             , makeAttr "Image" ["image"] "N" Strng Nothing (Just "@\\\"\\\"@") Nothing Nothing
             , makeAttr "ImageScale" ["imagescale"] "N" (Cust "ScaleType") (Just "UniformScale") (Just "@'NoScale'@") Nothing Nothing
             , makeAttr "Label" ["label"] "ENGC" (Cust "Label") Nothing (Just "@'StrLabel' \\\"\\N\\\"@ (nodes), @'StrLabel' \\\"\\\"@ (otherwise)") Nothing Nothing
             , makeAttr "LabelURL" ["labelURL", "labelhref"] "E" URL Nothing (Just "@\\\"\\\"@") Nothing (Just "svg, map only")
             , makeAttr "LabelAngle" ["labelangle"] "E" Dbl Nothing (Just "@-25.0@") (Just "@-180.0@") Nothing
             , makeAttr "LabelDistance" ["labeldistance"] "E" Dbl Nothing (Just "@1.0@") (Just "@0.0@") Nothing
             , makeAttr "LabelFloat" ["labelfloat"] "E" Bl (Just "True") (Just "@'False'@") Nothing Nothing
             , makeAttr "LabelFontColor" ["labelfontcolor"] "E" (Cust "Color") Nothing (Just "@black@") Nothing Nothing
             , makeAttr "LabelFontName" ["labelfontname"] "E" Strng Nothing (Just "@\\\"Times-Roman\\\"@") Nothing Nothing
             , makeAttr "LabelFontSize" ["labelfontsize"] "E" Dbl Nothing (Just "@14.0@") (Just "@1.0@") Nothing
             , makeAttr "LabelJust" ["labeljust"] "GC" (Cust "Justification") Nothing (Just "@'JCenter'@") Nothing Nothing
             , makeAttr "LabelLoc" ["labelloc"] "GCN" (Cust "VerticalPlacement") Nothing (Just "@'VTop'@ (clusters), @'VBottom'@ (root graphs), @'VCenter'@ (nodes)") Nothing Nothing
             , makeAttr "LabelTarget" ["labeltarget"] "E" Strng Nothing (Just "none") Nothing (Just "svg, map only")
             , makeAttr "LabelTooltip" ["labeltooltip"] "E" Strng Nothing (Just "@\\\"\\\"@") Nothing (Just "svg, cmap only")
             , makeAttr "Landscape" ["landscape"] "G" Bl (Just "True") (Just "@'False'@") Nothing Nothing
             , makeAttr "Layer" ["layer"] "EN" (Cust "LayerRange") Nothing (Just "@\\\"\\\"@") Nothing Nothing
             , makeAttr "Layers" ["layers"] "G" (Cust "LayerList") Nothing (Just "@\\\"\\\"@") Nothing Nothing
             , makeAttr "LayerSep" ["layersep"] "G" Strng Nothing (Just "@\\\" :\\t\\\"@") Nothing Nothing
             , makeAttr "Layout" ["layout"] "G" Strng Nothing (Just "@\\\"\\\"@") Nothing Nothing
             , makeAttr "Len" ["len"] "E" Dbl Nothing (Just "@1.0@ (neato), @0.3@ (fdp)") Nothing (Just "fdp, neato only")
             , makeAttr "Levels" ["levels"] "G" Integ Nothing (Just "@MAXINT@") (Just "@0@") (Just "sfdp only")
             , makeAttr "LevelsGap" ["levelsgap"] "G" Dbl Nothing (Just "@0.0@") Nothing (Just "neato only")
             , makeAttr "LHead" ["lhead"] "E" Strng Nothing (Just "@\\\"\\\"@") Nothing (Just "dot only")
             , makeAttr "LPos" ["lp"] "EGC" (Cust "Point") Nothing Nothing Nothing (Just "write only")
             , makeAttr "LTail" ["ltail"] "E" Strng Nothing (Just "@\\\"\\\"@") Nothing (Just "dot only")
             , makeAttr "Margin" ["margin"] "NG" (Cust "DPoint") Nothing (Just "device-dependent") Nothing Nothing
             , makeAttr "MaxIter" ["maxiter"] "G" Integ Nothing (Just "@100 * # nodes@ (@mode == 'KK'@), @200@ (@mode == 'Major'@), @600@ (fdp)") Nothing (Just "fdp, neato only")
             , makeAttr "MCLimit" ["mclimit"] "G" Dbl Nothing (Just "@1.0@") Nothing (Just "dot only")
             , makeAttr "MinDist" ["mindist"] "G" Dbl Nothing (Just "@1.0@") (Just "@0.0@") (Just "circo only")
             , makeAttr "MinLen" ["minlen"] "E" Integ Nothing (Just "@1@") (Just "@0@") (Just "dot only")
             , makeAttr "Mode" ["mode"] "G" (Cust "ModeType") Nothing (Just "@'Major'@") Nothing (Just "neato only")
             , makeAttr "Model" ["model"] "G" (Cust "Model") Nothing (Just "@'ShortPath'@") Nothing (Just "neato only")
             , makeAttr "Mosek" ["mosek"] "G" Bl (Just "True") (Just "@'False'@") Nothing (Just "neato only; requires the Mosek software")
             , makeAttr "NodeSep" ["nodesep"] "G" Dbl Nothing (Just "@0.25@") (Just "@0.02@") (Just "dot only")
             , makeAttr "NoJustify" ["nojustify"] "GCNE" Bl (Just "True") (Just "@'False'@") Nothing Nothing
             , makeAttr "Normalize" ["normalize"] "G" Bl (Just "True") (Just "@'False'@") Nothing (Just "not dot")
             , makeAttr "Nslimit" ["nslimit"] "G" Dbl Nothing Nothing Nothing (Just "dot only")
             , makeAttr "Nslimit1" ["nslimit1"] "G" Dbl Nothing Nothing Nothing (Just "dot only")
             , makeAttr "Ordering" ["ordering"] "G" Strng Nothing (Just "@\\\"\\\"@") Nothing (Just "dot only")
             , makeAttr "Orientation" ["orientation"] "N" Dbl Nothing (Just "@0.0@") (Just "@360.0@") Nothing
             , makeAttr "OrientationGraph" ["orientation"] "G" Strng Nothing (Just "@\\\"\\\"@") Nothing (Just "Landscape if \\\"[lL]*\\\" and rotate not defined")
             , makeAttr "OutputOrder" ["outputorder"] "G" (Cust "OutputMode") Nothing (Just "@'BreadthFirst'@") Nothing Nothing
             , makeAttr "Overlap" ["overlap"] "G" (Cust "Overlap") (Just "KeepOverlaps") (Just "@'KeepOverlaps'@") Nothing (Just "not dot")
             , makeAttr "OverlapScaling" ["overlap_scaling"] "G" Dbl Nothing (Just "@-4@") (Just "@-1.0e10@") (Just "prism only")
             , makeAttr "Pack" ["pack"] "G" (Cust "Pack") (Just "DoPack") (Just "@'False'@") Nothing (Just "not dot")
             , makeAttr "PackMode" ["packmode"] "G" (Cust "PackMode") Nothing (Just "@'PackNode'@") Nothing (Just "not dot")
             , makeAttr "Pad" ["pad"] "G" (Cust "DPoint") Nothing (Just "@'DVal' 0.0555@ (4 points)") Nothing Nothing
             , makeAttr "Page" ["page"] "G" (Cust "Point") Nothing Nothing Nothing Nothing
             , makeAttr "PageDir" ["pagedir"] "G" (Cust "PageDir") Nothing (Just "@'BL'@") Nothing Nothing
             , makeAttr "PenColor" ["pencolor"] "C" (Cust "Color") Nothing (Just "@black@") Nothing Nothing
             , makeAttr "PenWidth" ["penwidth"] "CNE" Dbl Nothing (Just "@1.0@") (Just "@0.0@") Nothing
             , makeAttr "Peripheries" ["peripheries"] "NC" Integ Nothing (Just "shape default (nodes), @1@ (clusters)") (Just "0") Nothing
             , makeAttr "Pin" ["pin"] "N" Bl (Just "True") (Just "@'False'@") Nothing (Just "fdp, neato only")
             , makeAttr "Pos" ["pos"] "EN" (Cust "Pos") Nothing Nothing Nothing Nothing
             , makeAttr "QuadTree" ["quadtree"] "G" (Cust "QuadType") (Just "NormalQT") (Just "@'NormalQT'@") Nothing (Just "sfdp only")
             , makeAttr "Quantum" ["quantum"] "G" Dbl Nothing (Just "@0.0@") (Just "@0.0@") Nothing
             , makeAttr "Rank" ["rank"] "S" (Cust "RankType") Nothing Nothing Nothing (Just "dot only")
             , makeAttr "RankDir" ["rankdir"] "G" (Cust "RankDir") Nothing (Just "@'TB'@") Nothing (Just "dot only")
             , makeAttr "Ranksep" ["ranksep"] "G" Dbl Nothing (Just "@0.5@ (dot), @1.0@ (twopi)") (Just "0.02") (Just "twopi, dot only")
             , makeAttr "Ratio" ["ratio"] "G" (Cust "Ratios") Nothing Nothing Nothing Nothing
             , makeAttr "Rects" ["rects"] "N" (Cust "Rect") Nothing Nothing Nothing (Just "write only")
             , makeAttr "Regular" ["regular"] "N" Bl (Just "True") (Just "@'False'@") Nothing Nothing
             , makeAttr "ReMinCross" ["remincross"] "G" Bl (Just "True") (Just "@'False'@") Nothing (Just "dot only")
             , makeAttr "RepulsiveForce" ["repulsiveforce"] "G" Dbl Nothing (Just "@1.0@") (Just "@0.0@") (Just "sfdp only")
             , makeAttr "Root" ["root"] "GN" (Cust "Root") (Just "IsCentral") (Just "@'NodeName' \\\"\\\"@ (graphs), @'NotCentral'@ (nodes)") Nothing (Just "circo, twopi only")
             , makeAttr "Rotate" ["rotate"] "G" Integ Nothing (Just "@0@") Nothing Nothing
             , makeAttr "SameHead" ["samehead"] "E" Strng Nothing (Just "@\\\"\\\"@") Nothing (Just "dot only")
             , makeAttr "SameTail" ["sametail"] "E" Strng Nothing (Just "@\\\"\\\"@") Nothing (Just "dot only")
             , makeAttr "SamplePoints" ["samplepoints"] "N" Integ Nothing (Just "@8@ (output), @20@ (overlap and image maps)") Nothing Nothing
             , makeAttr "SearchSize" ["searchsize"] "G" Integ Nothing (Just "@30@") Nothing (Just "dot only")
             , makeAttr "Sep" ["sep"] "G" (Cust "DPoint") Nothing (Just "@+4@") Nothing (Just "not dot")
             , makeAttr "Shape" ["shape"] "N" (Cust "Shape") Nothing (Just "@'Ellipse'@") Nothing Nothing
             , makeAttr "ShapeFile" ["shapefile"] "N" Strng Nothing (Just "@\\\"\\\"@") Nothing Nothing
             , makeAttr "ShowBoxes" ["showboxes"] "ENG" Integ Nothing (Just "@0@") (Just "@0@") (Just "dot only")
             , makeAttr "Sides" ["sides"] "N" Integ Nothing (Just "@4@") (Just "@0@") Nothing
             , makeAttr "Size" ["size"] "G" (Cust "Point") Nothing Nothing Nothing Nothing
             , makeAttr "Skew" ["skew"] "N" Dbl Nothing (Just "@0.0@") (Just "@-100.0@") Nothing
             , makeAttr "Smoothing" ["smoothing"] "G" (Cust "SmoothType") Nothing (Just "@'NoSmooth'@") Nothing (Just "sfdp only")
             , makeAttr "SortV" ["sortv"] "GCN" Integ Nothing (Just "@0@") (Just "@0@") Nothing
             , makeAttr "Splines" ["splines"] "G" (Cust "EdgeType") (Just "SplineEdges") Nothing Nothing Nothing
             , makeAttr "Start" ["start"] "G" (Cust "StartType") Nothing (Just "@\\\"\\\"@") Nothing (Just "fdp, neato only")
             , makeAttr "Style" ["style"] "ENC" (Cust "[StyleItem]") Nothing Nothing Nothing Nothing
             , makeAttr "StyleSheet" ["stylesheet"] "G" Strng Nothing (Just "@\\\"\\\"@") Nothing (Just "svg only")
             , makeAttr "TailURL" ["tailURL", "tailhref"] "E" URL Nothing (Just "@\\\"\\\"@") Nothing (Just "svg, map only")
             , makeAttr "TailClip" ["tailclip"] "E" Bl (Just "True") (Just "@'True'@") Nothing Nothing
             , makeAttr "TailLabel" ["taillabel"] "E" (Cust "Label") Nothing (Just "@\\\"\\\"@") Nothing Nothing
             , makeAttr "TailPort" ["tailport"] "E" (Cust "PortPos") Nothing (Just "center") Nothing Nothing
             , makeAttr "TailTarget" ["tailtarget"] "E" Strng Nothing (Just "none") Nothing (Just "svg, map only")
             , makeAttr "TailTooltip" ["tailtooltip"] "E" Strng Nothing (Just "@\\\"\\\"@") Nothing (Just "svg, cmap only")
             , makeAttr "Target" ["target"] "ENGC" Strng Nothing (Just "none") Nothing (Just "svg, map only")
             , makeAttr "Tooltip" ["tooltip"] "NEC" Strng Nothing (Just "@\\\"\\\"@") Nothing (Just "svg, cmap only")
             , makeAttr "TrueColor" ["truecolor"] "G" Bl (Just "True") Nothing Nothing (Just "bitmap output only")
             , makeAttr "Vertices" ["vertices"] "N" (Cust "[Point]") Nothing Nothing Nothing (Just "write only")
             , makeAttr "ViewPort" ["viewport"] "G" (Cust "ViewPort") Nothing (Just "none") Nothing Nothing
             , makeAttr "VoroMargin" ["voro_margin"] "G" Dbl Nothing (Just "@0.05@") (Just "@0.0@") (Just "not dot")
             , makeAttr "Weight" ["weight"] "E" Dbl Nothing (Just "@1.0@") (Just "@0@ (dot), @1@ (neato,fdp,sfdp)") Nothing
             , makeAttr "Width" ["width"] "N" Dbl Nothing (Just "@0.75@") (Just "@0.01@") Nothing
             , makeAttr "Z" ["z"] "N" Dbl Nothing (Just "@0.0@") (Just "@-MAXFLOAT@, @-1000@") Nothing
             ]

attrs = take 10 $ drop 5 attributes

attrs' = AS (text "Attribute") attrs
