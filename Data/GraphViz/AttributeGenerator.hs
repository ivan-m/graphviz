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
import Data.Maybe(catMaybes)

type Code = Doc

type Constructor = String
type Name = String
type UsedBy = String -- should only contain subset of "ENGCS"
type Default = String
type Minimum = String
type Comment = String

data VType = Dbl
           | Integ
           | Strng
           | Bl
           | Cust String
           | Opt VType VType

vtype           :: VType -> Doc
vtype Dbl       = text "Double"
vtype Integ     = text "Int"
vtype Strng     = text "String"
vtype Bl        = text "Bool"
vtype (Cust t)  = text t
vtype (Opt a b) = parens $ text "Either" <+> vtype a <+> vtype b

data Attribute = A { cnst         :: Code
                   , name         :: Code
                   , valtype      :: VType
                   , forGraphs    :: Bool
                   , forClusters  :: Bool
                   , forSubGraphs :: Bool
                   , forNodes     :: Bool
                   , forEdges     :: Bool
                   , comment      :: Doc
                   }

makeAttr                :: Constructor -> Name -> UsedBy -> VType
                           -> Maybe Default -> Maybe Minimum -> Maybe Comment
                           -> Attribute
makeAttr c n u v d m cm = A { cnst         = text c
                            , name         = text n
                            , valtype      = v -- just in case need to do fancy
                                               -- stuff
                            , forGraphs    = isFor 'G'
                            , forClusters  = isFor 'C' || forSG
                            , forSubGraphs = forSG
                            , forNodes     = isFor 'N'
                            , forEdges     = isFor 'E'
                            , comment      = cm'
                            }
    where
      isFor f = f `elem` u
      forSG = isFor 'S'
      mDoc (f,fc) = f <> colon <+> text fc
      addF f = fmap (\ dc -> (wrap (char '/') (text f), dc))
      cm' = hsep
            . punctuate semi
            . map mDoc
            $ catMaybes [ addF "Default" d
                        , addF "Minimum" m
                        , addF "Notes" cm
                        ]

vtypeCode :: Attribute -> Code
vtypeCode = vtype . valtype

createDefn       :: String -> [Attribute] -> Code
createDefn dt as = hdr $+$ constructors $+$ derivs
    where
      hdr = text "data" <+> text dt
      constructors = nest tab
                     . asRows
                     . firstOthers' equals (char '|')
                     $ map createDefn as
      derivs = nest (tab + 2) $ text "deriving (Eq, Show, Read)"
      createDefn a = [cnst a <+> vtypeCode a
                     , if isEmpty cm
                       then empty
                       else text "-- ^" <+> cm
                     ]
          where
            cm = comment a

showInstance       :: String -> [Attribute] -> Code
showInstance dt as = hdr $+$ insts
    where
      hdr = text "instance" <+> text "Show" <+> text dt <+> text "where"
      var = char 'v'
      sFunc = text "show"
      cnct = text "++"
      insts = nest tab
              . asRows
              $ map mkInstance as
      mkInstance a = [ sFunc <+> parens (cnst a <+> var)
                     , equals <+> doubleQuotes (name a <> equals) <+> cnct
                                  <+> sFunc <+> var
                     ]

parseInstance       :: String -> [Attribute] -> Code
parseInstance dt as = hdr $+$ nest tab fn
    where
      hdr = text "instance" <+> text "Parseable" <+> text dt <+> text "where"
      fn = pFunc <+> equals <+> text "oneOf" <+> ops
      ops = flip ($$) rbrack
            . asRows
            . firstOthers' lbrack comma
            $ map parseAttr as
      pFunc = text "parse"
      parseAttr a = [ text "liftM" <+> cnst a
                    , char '$' <+> text "parseField"
                               <+> doubleQuotes (name a)
                    ]

usedByFunc            :: String -> (Attribute -> Bool) -> String -> [Attribute] -> Code
usedByFunc nm p dt as = asRows $ tpSig : trs ++ [fls]
    where
      tpSig = [ fn
              , colon <> colon
                <+> text dt <+> text "->" <+> text "Bool"
              ]
      fn = text "usedBy" <> text nm
      tr = text "True"
      trs = map aTr as'
      fl = text "False"
      fls = [ fn <+> char '_'
            , equals <+> fl
            ]
      as' = filter p as
      aTr a = [ fn <+> cnst a <> braces empty
              , equals <+> tr
              ]

attributes :: [Attribute]
attributes = [ makeAttr "Damping" "Damping" "G" Dbl (Just "0.99") (Just "0.0") (Just "neato only")
             , makeAttr "K" "K" "GC" Dbl (Just "0.3") (Just "0") (Just "sfdp, fdp only")
             , makeAttr "URL" "URL" "ENGC" Strng (Just "<none>") Nothing (Just "svg, postscript, map only")
             , makeAttr "ArrowHead" "arrowhead" "E" (Cust "ArrowType") (Just "Normal") Nothing Nothing
             , makeAttr "ArrowSize" "arrowsize" "E" Dbl (Just "1.0") (Just "0.0") Nothing
             , makeAttr "ArrowTail" "arrowtail" "E" (Cust "ArrowType") (Just "Normal") Nothing Nothing
             , makeAttr "Aspect" "aspect" "G" (Cust "AspectType") Nothing Nothing (Just "dot only")
             , makeAttr "Bb" "bb" "G" (Cust "Rect") Nothing Nothing (Just "write only")
             , makeAttr "BgColor" "bgcolor" "GC" (Cust "Color") (Just "<none>") Nothing Nothing
             , makeAttr "Center" "center" "G" Bl (Just "false") Nothing Nothing
             , makeAttr "Charset" "charset" "G" Strng (Just "\"UTF-8\"") Nothing Nothing
             , makeAttr "ClusterRank" "clusterrank" "G" (Cust "clusterMode") (Just "local") Nothing (Just "dot only")
             , makeAttr "Color" "color" "ENC" (Opt (Cust "Color") (Cust "ColorList")) (Just "black") Nothing Nothing
             , makeAttr "ColorScheme" "colorscheme" "ENCG" Strng (Just "\"\"") Nothing Nothing
             , makeAttr "Comment" "comment" "ENG" Strng (Just "\"\"") Nothing Nothing
             , makeAttr "Compound" "compound" "G" Bl (Just "false") Nothing (Just "dot only")
             , makeAttr "Concentrate" "concentrate" "G" Bl (Just "false") Nothing Nothing
             , makeAttr "Constraint" "constraint" "E" Bl  (Just "true") Nothing (Just "dot only")
             , makeAttr "Decorate" "decorate" "E" Bl (Just "false") Nothing Nothing
             , makeAttr "DefaultDist" "defaultdist" "G" Dbl (Just "1+(avg. len)*sqrt(|V|)") (Just "epsilon") (Just "neato only")
             , makeAttr "Dim" "dim" "G" Integ (Just "2") (Just "2") (Just "sfdp, fdp, neato only")
             , makeAttr "Dimen" "dimen" "G" Integ (Just "2") (Just "2") (Just "sfdp, fdp, neato only")
             , makeAttr "Dir" "dir" "E" (Cust "DirType") (Just "forward(directed)/none(undirected)") Nothing Nothing
             , makeAttr "DirEdgeConstraints" "diredgeconstraints" "G" (Opt Strng Bl) (Just "false") Nothing (Just "neato only")
             , makeAttr "Distortion" "distortion" "N" Dbl (Just "0.0") (Just "-100.0") Nothing
             , makeAttr "DPI" "dpi" "G" Dbl (Just "96.0 | 0.0") Nothing (Just "svg, bitmap output only")
             , makeAttr "EdgeURL" "edgeURL" "E" Strng (Just "\"\"") Nothing (Just "svg, map only")
             , makeAttr "EdgeHref" "edgehref" "E" Strng (Just "\"\"") Nothing (Just "svg, map only")
             , makeAttr "EdgeTarget" "edgetarget" "E" Strng (Just "<none>") Nothing (Just "svg, map only")
             , makeAttr "EdgeTooltip" "edgetooltip" "E" Strng (Just "\"\"") Nothing (Just "svg, cmap only")
             , makeAttr "Epsilon" "epsilon" "G" Dbl (Just ".0001 * # nodes(mode == KK) | .0001(mode == major)") Nothing (Just "neato only")
             , makeAttr "ESep" "esep" "G" (Opt Dbl (Cust "Pointf")) (Just "+3") Nothing (Just "not dot")
             , makeAttr "FillColor" "fillcolor" "NC" (Cust "Color") (Just "lightgrey(nodes) | black(clusters)") Nothing Nothing
             , makeAttr "FixedSize" "fixedsize" "N" Bl (Just "false") Nothing Nothing
             , makeAttr "FontColor" "fontcolor" "ENGC" (Cust "Color") (Just "black") Nothing Nothing
             , makeAttr "FontName" "fontname" "ENGC" Strng (Just "\"Times-Roman\"") Nothing Nothing
             , makeAttr "FontNames" "fontnames" "G" Strng (Just "\"\"") Nothing (Just "svg only")
             , makeAttr "FontPath" "fontpath" "G" Strng (Just "system-dependent") Nothing Nothing
             , makeAttr "FontSize" "fontsize" "ENGC" Dbl (Just "14.0") (Just "1.0") Nothing
             , makeAttr "Group" "group" "N" Strng (Just "\"\"") Nothing (Just "dot only")
             , makeAttr "HeadURL" "headURL" "E" Strng (Just "\"\"") Nothing (Just "svg, map only")
             , makeAttr "HeadClip" "headclip" "E" Bl (Just "true") Nothing Nothing
             , makeAttr "HeadHref" "headhref" "E" Strng (Just "\"\"") Nothing (Just "svg, map only")
             , makeAttr "HeadLabel" "headlabel" "E" Strng (Just "\"\"") Nothing Nothing
             , makeAttr "HeadPort" "headport" "E" (Cust "PortPos") (Just "center") Nothing Nothing
             , makeAttr "HeadTarget" "headtarget" "E" Strng (Just "<none>") Nothing (Just "svg, map only")
             , makeAttr "HeadTooltip" "headtooltip" "E" Strng (Just "\"\"") Nothing (Just "svg, cmap only")
             , makeAttr "Height" "height" "N" Dbl (Just "0.5") (Just "0.02") Nothing
             , makeAttr "Href" "href" "E" Strng (Just "\"\"") Nothing (Just "svg, postscript, map only")
             , makeAttr "ID" "id" "GNE" Strng (Just "\"\"") Nothing (Just "svg, postscript, map only")
             , makeAttr "Image" "image" "N" Strng (Just "\"\"") Nothing Nothing
             , makeAttr "ImageScale" "imagescale" "N" (Opt Bl Strng) (Just "false") Nothing Nothing
             , makeAttr "Label" "label" "ENGC" Strng (Just "\"\\N\" (nodes) | \"\" (otherwise)") Nothing Nothing
             , makeAttr "LabelURL" "labelURL" "E" Strng (Just "\"\"") Nothing (Just "svg, map only")
             , makeAttr "LabelAngle" "labelangle" "E" Dbl (Just "-25.0") (Just "-180.0") Nothing
             , makeAttr "LabelDistance" "labeldistance" "E" Dbl (Just "1.0") (Just "0.0") Nothing
             , makeAttr "LabelFloat" "labelfloat" "E" Bl (Just "false") Nothing Nothing
             , makeAttr "LabelFontColor" "labelfontcolor" "E" (Cust "Color") (Just "black") Nothing Nothing
             , makeAttr "LabelFontName" "labelfontname" "E" Strng (Just "\"Times-Roman\"") Nothing Nothing
             , makeAttr "LabelFontSize" "labelfontsize" "E" Dbl (Just "14.0") (Just "1.0") Nothing
             , makeAttr "LabelHref" "labelhref" "E" Strng (Just "\"\"") Nothing (Just "svg, map only")
             , makeAttr "LabelJust" "labeljust" "GC" Strng (Just "\"c\"") Nothing Nothing
             , makeAttr "LabelLoc" "labelloc" "GCN" Strng (Just "\"t\"(clusters) | \"b\"(root graphs) | \"c\"(clusters)") Nothing Nothing
             , makeAttr "LabelTarget" "labeltarget" "E" Strng (Just "<none>") Nothing (Just "svg, map only")
             , makeAttr "LabelTooltip" "labeltooltip" "E" Strng (Just "\"\"") Nothing (Just "svg, cmap only")
             , makeAttr "Landscape" "landscape" "G" Bl (Just "false") Nothing Nothing
             , makeAttr "Layer" "layer" "EN" (Cust "LayerRange") (Just "\"\"") Nothing Nothing
             , makeAttr "Layers" "layers" "G" (Cust "LayerList") (Just "\"\"") Nothing Nothing
             , makeAttr "LayerSep" "layersep" "G" Strng (Just "\" :\t\"") Nothing Nothing
             , makeAttr "Layout" "layout" "G" Strng (Just "\"\"") Nothing Nothing
             , makeAttr "Len" "len" "E" Dbl (Just "1.0(neato)/0.3(fdp)") Nothing (Just "fdp, neato only")
             , makeAttr "Levels" "levels" "G" Integ (Just "MAXINT") (Just "0.0") (Just "sfdp only")
             , makeAttr "LevelsGap" "levelsgap" "G" Dbl (Just "0.0") Nothing (Just "neato only")
             , makeAttr "LHead" "lhead" "E" Strng (Just "\"\"") Nothing (Just "dot only")
             , makeAttr "LP" "lp" "EGC" (Cust "Point") Nothing Nothing (Just "write only")
             , makeAttr "LTail" "ltail" "E" Strng (Just "\"\"") Nothing (Just "dot only")
             , makeAttr "Margin" "margin" "NG" (Opt Dbl (Cust "Pointf")) (Just "<device-dependent>") Nothing Nothing
             , makeAttr "MaxIter" "maxiter" "G" Integ (Just "100 * # nodes(mode == KK) | 200(mode == major) | 600(fdp)") Nothing (Just "fdp, neato only")
             , makeAttr "MCLimit" "mclimit" "G" Dbl (Just "1.0") Nothing (Just "dot only")
             , makeAttr "MinDist" "mindist" "G" Dbl (Just "1.0") (Just "0.0") (Just "circo only")
             , makeAttr "MinLen" "minlen" "E" Integ (Just "1") (Just "0") (Just "dot only")
             , makeAttr "Mode" "mode" "G" Strng (Just "\"major\"") Nothing (Just "neato only")
             , makeAttr "Model" "model" "G" Strng (Just "\"shortpath\"") Nothing (Just "neato only")
             , makeAttr "Mosek" "mosek" "G" Bl (Just "false") Nothing (Just "neato only; requires the Mosek software")
             , makeAttr "NodeSep" "nodesep" "G" Dbl (Just "0.25") (Just "0.02") (Just "dot only")
             , makeAttr "NoJustify" "nojustify" "GCNE" Bl (Just "false") Nothing Nothing
             , makeAttr "Normalize" "normalize" "G" Bl (Just "false") Nothing (Just "not dot")
             , makeAttr "Nslimit" "nslimit" "G" Dbl Nothing Nothing (Just "dot only")
             , makeAttr "Nslimit1" "nslimit1" "G" Dbl Nothing Nothing (Just "dot only")
             , makeAttr "Ordering" "ordering" "G" Strng (Just "\"\"") Nothing (Just "dot only")
             , makeAttr "Orientation" "orientation" "N" Dbl (Just "0.0") (Just "360.0") Nothing
             , makeAttr "OrientationGraph" "orientation" "G" Strng (Just "\"\"") Nothing (Just "Landscape if \"[lL]*\" and rotate not defined")
             , makeAttr "OutputOrder" "outputorder" "G" (Cust "OutputMode") (Just "breadthfirst") Nothing Nothing
             , makeAttr "Overlap" "overlap" "G" (Opt Strng Bl) (Just "true") Nothing (Just "not dot")
             , makeAttr "OverlapScaling" "overlap_scaling" "G" Dbl (Just "-4") (Just "-1.0e10") (Just "prism only")
             , makeAttr "Pack" "pack" "G" (Opt Bl Integ) (Just "false") Nothing (Just "not dot")
             , makeAttr "PackMode" "packmode" "G" (Cust "PackMode") (Just "node") Nothing (Just "not dot")
             , makeAttr "Pad" "pad" "G" (Opt Dbl (Cust "Pointf")) (Just "0.0555 (4 points)") Nothing Nothing
             , makeAttr "Page" "page" "G" (Cust "Pointf") Nothing Nothing Nothing
             , makeAttr "PageDir" "pagedir" "G" (Cust "PageDir") (Just "BL") Nothing Nothing
             , makeAttr "PenColor" "pencolor" "C" (Cust "Color") (Just "black") Nothing Nothing
             , makeAttr "PenWidth" "penwidth" "CNE" Dbl (Just "1.0") (Just "0.0") Nothing
             , makeAttr "Peripheries" "peripheries" "NC" Integ (Just "shape default(nodes) | 1(clusters)") (Just "0") Nothing
             , makeAttr "Pin" "pin" "N" Bl (Just "false") Nothing (Just "fdp, neato only")
             , makeAttr "Pos" "pos" "EN" (Opt (Cust "Point") (Cust "SplineType")) Nothing Nothing Nothing
             , makeAttr "QuadTree" "quadtree" "G" (Opt (Cust "QuadType") Bl) (Just "\"normal\"") Nothing (Just "sfdp only")
             , makeAttr "Quantum" "quantum" "G" Dbl (Just "0.0") (Just "0.0") Nothing
             , makeAttr "Rank" "rank" "S" (Cust "RankType") Nothing Nothing (Just "dot only")
             , makeAttr "RankDir" "rankdir" "G" (Cust "RankDir") (Just "TB") Nothing (Just "dot only")
             , makeAttr "Ranksep" "ranksep" "G" Dbl (Just "0.5(dot) | 1.0(twopi)") (Just "0.02") (Just "twopi, dot only")
             , makeAttr "Ratio" "ratio" "G" (Opt Dbl Strng) Nothing Nothing Nothing
             , makeAttr "Rects" "rects" "N" (Cust "Rect") Nothing Nothing (Just "write only")
             , makeAttr "Regular" "regular" "N" Bl (Just "false") Nothing Nothing
             , makeAttr "ReMinCross" "remincross" "G" Bl (Just "false") Nothing (Just "dot only")
             , makeAttr "RepulsiveForce" "repulsiveforce" "G" Dbl (Just "1.0") (Just "0.0") (Just "sfdp only")
             , makeAttr "Resolution" "resolution" "G" Dbl (Just "96.0 | 0.0") Nothing (Just "svg, bitmap output only")
             , makeAttr "Root" "root" "GN" (Opt Strng Bl) (Just "\"\"(graphs) | false(nodes)") Nothing (Just "circo, twopi only")
             , makeAttr "Rotate" "rotate" "G" Integ (Just "0") Nothing Nothing
             , makeAttr "SameHead" "samehead" "E" Strng (Just "\"\"") Nothing (Just "dot only")
             , makeAttr "SameTail" "sametail" "E" Strng (Just "\"\"") Nothing (Just "dot only")
             , makeAttr "SamplePoints" "samplepoints" "N" Integ (Just "8(output) | 20(overlap and image maps)") Nothing Nothing
             , makeAttr "SearchSize" "searchsize" "G" Integ (Just "30") Nothing (Just "dot only")
             , makeAttr "Sep" "sep" "G" (Opt Dbl (Cust "Pointf")) (Just "+4") Nothing (Just "not dot")
             , makeAttr "Shape" "shape" "N" (Cust "Shape") (Just "ellipse") Nothing Nothing
             , makeAttr "ShapeFile" "shapefile" "N" Strng (Just "\"\"") Nothing Nothing
             , makeAttr "ShowBoxes" "showboxes" "ENG" Integ (Just "0") (Just "0") (Just "dot only")
             , makeAttr "Sides" "sides" "N" Integ (Just "4") (Just "0") Nothing
             , makeAttr "Size" "size" "G" (Cust "Pointf") Nothing Nothing Nothing
             , makeAttr "Skew" "skew" "N" Dbl (Just "0.0") (Just "-100.0") Nothing
             , makeAttr "Smoothing" "smoothing" "G" (Cust "SmoothType") (Just "\"none\"") Nothing (Just "sfdp only")
             , makeAttr "SortV" "sortv" "GCN" Integ (Just "0") (Just "0") Nothing
             , makeAttr "Splines" "splines" "G" (Opt Bl Strng) Nothing Nothing Nothing
             , makeAttr "Start" "start" "G" (Cust "StartType") (Just "\"\"") Nothing (Just "fdp, neato only")
             , makeAttr "Style" "style" "ENC" (Cust "Style") Nothing Nothing Nothing
             , makeAttr "Stylesheet" "stylesheet" "G" Strng (Just "\"\"") Nothing (Just "svg only")
             , makeAttr "TailURL" "tailURL" "E" Strng (Just "\"\"") Nothing (Just "svg, map only")
             , makeAttr "TailClip" "tailclip" "E" Bl (Just "true") Nothing Nothing
             , makeAttr "TailHref" "tailhref" "E" Strng (Just "\"\"") Nothing (Just "svg, map only")
             , makeAttr "TailLabel" "taillabel" "E" Strng (Just "\"\"") Nothing Nothing
             , makeAttr "TailPort" "tailport" "E" (Cust "PortPos") (Just "center") Nothing Nothing
             , makeAttr "TailTarget" "tailtarget" "E" Strng (Just "<none>") Nothing (Just "svg, map only")
             , makeAttr "TailTooltip" "tailtooltip" "E" Strng (Just "\"\"") Nothing (Just "svg, cmap only")
             , makeAttr "Target" "target" "ENGC" Strng (Just "<none>") Nothing (Just "svg, map only")
             , makeAttr "Tooltip" "tooltip" "NEC" Strng (Just "\"\"") Nothing (Just "svg, cmap only")
             , makeAttr "TrueColor" "truecolor" "G" Bl Nothing Nothing (Just "bitmap output only")
             , makeAttr "Vertices" "vertices" "N" (Cust "PointfList") Nothing Nothing (Just "write only")
             , makeAttr "ViewPort" "viewport" "G" (Cust "ViewPort") (Just "\"\"") Nothing Nothing
             , makeAttr "VoroMargin" "voro_margin" "G" Dbl (Just "0.05") (Just "0.0") (Just "not dot")
             , makeAttr "Weight" "weight" "E" Dbl (Just "1.0") (Just "0(dot) | 1(neato,fdp,sfdp)") Nothing
             , makeAttr "Width" "width" "N" Dbl (Just "0.75") (Just "0.01") Nothing
             , makeAttr "Z" "z" "N" Dbl (Just "0.0") (Just "-MAXFLOAT | -1000") Nothing
             ]

attrs = take 5 attributes

-- -----------------------------------------------------------------------------

tab :: Int
tab = 4

punctuate' _ []     = []
punctuate' p (d:ds) = d : map ((<>) p) ds

firstOthers            :: Doc -> Doc -> [Doc] -> [Doc]
firstOthers _ _ []     = []
firstOthers f o (d:ds) = f <> d : map ((<>) o) ds

firstOthers'            :: Doc -> Doc -> [[Doc]] -> [[Doc]]
firstOthers' _ _ []     = []
firstOthers' f o (d:ds) = (f : d) : map ((:) o) ds

wrap     :: Doc -> Doc -> Doc
wrap w d = w <> d <> w

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
