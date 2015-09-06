#!/usr/bin/runhaskell

{- |
   Module      : AttributeGenerator
   Description : Definition of the Graphviz attributes.
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
import System.Environment(getArgs)

type Code = Doc

-- If any args are passed in, then generate the Arbitrary instance
-- instead of the definition.
main :: IO ()
main = do args <- getArgs
          let f = if null args
                  then genCode
                  else genArbitrary
          print $ f att
    where
      att = AS { tpNm = text "Attribute"
               , atts = attributes
               }

genCode     :: Atts -> Doc
genCode att = vsep $ map ($att) cds
    where
      cds = [ createDefn
            , createAlias
            , nameAlias
            , showInstance
            , parseInstance
            , usedByFunc "Graphs" forGraphs
            , usedByFunc "Clusters" forClusters
            , usedByFunc "SubGraphs" forSubGraphs
            , usedByFunc "Nodes" forNodes
            , usedByFunc "Edges" forEdges
            , sameAttributeFunc
            , defValueFunc
            , validUnknownFunc
            ]

genArbitrary :: Atts -> Doc
genArbitrary = arbitraryInstance

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
                   , defValue     :: Maybe Code
                   , forGraphs    :: Bool
                   , forClusters  :: Bool
                   , forSubGraphs :: Bool
                   , forNodes     :: Bool
                   , forEdges     :: Bool
                   , comment      :: Doc
                   }

makeAttr                      :: Constructor -> [Name] -> UsedBy -> VType
                                 -> Maybe Default -- Used when parsing the field
                                 -> Maybe Default -- Used as a default value if necessary
                                 -> Maybe Default -> Maybe Minimum -> Maybe Comment
                                 -> Attribute
makeAttr c ns u v df d fd m cm = A { cnst         = text c
                                   , name         = head ns'
                                   , parseNames   = ns'
                                   , valtype      = v -- just in case need to do fancy
                                                      -- stuff
                                   , parseDef     = liftM text df
                                   , defValue     = liftM text d
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
                        , addF "Default" fd
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
           | EStrng
           | Bl
           | Cust String
             deriving (Eq, Ord, Show, Read)

vtype           :: VType -> Doc
vtype Dbl       = text "Double"
vtype Integ     = text "Int"
vtype Strng     = text "Text"
vtype EStrng    = text "EscString"
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
                     . (++ [defUnknown])
                     . map createDf
                     $ atts att
      derivs = nest tab $ text "deriving (Eq, Ord, Show, Read)"
      createDf a = [cnst a <+> vtypeCode a
                   , if isEmpty cm
                     then empty
                     else text "-- ^" <+> cm
                   ]
          where
            cm = comment a
      defUnknown = [ unknownAttr <+> unknownNameAlias <+> vtype Strng
                   , text "-- ^ /Valid for/: Assumed valid for all; the fields are 'Attribute' name and value respectively."
                   ]

createAlias     :: Atts -> Code
createAlias att = text "type"
                  <+> tp <> char 's'
                  <+> equals
                  <+> brackets tp
    where
      tp = tpNm att

-- The Atts value isn't used; this is just to make it have the same
-- type as the other code-generating functions.
nameAlias   :: Atts -> Code
nameAlias _ = cmnt
              $$ (text "type"
                  <+> unknownNameAlias
                  <+> equals
                  <+> vtype Strng)
  where
    cmnt = text "-- | The name for an" <+> unknownAttr
           <> text "; must satisfy "
           <+> quotes validUnknownName
           <> text "."

unknownNameAlias :: Code
unknownNameAlias = text "AttributeName"

showInstance     :: Atts -> Code
showInstance att = hdr $+$ insts'
    where
      hdr = text "instance" <+> text "PrintDot" <+> tpNm att <+> text "where"
      var = char 'v'
      sFunc = text "unqtDot"
      insts = asRows
              . (++ [unknownInst])
              . map mkInstance
              $ atts att
      mkInstance a = [ sFunc <+> parens (cnst a <+> var)
                     , equals <+> text "printField" <+> doubleQuotes (name a)
                                  <+>  var
                     ]
      unknownInst = [ sFunc <+> parens (unknownAttr <+> char 'a' <+> var)
                    , equals <+> text "toDot" <+> char 'a'
                      <+> text "<> equals <>" <+> text "toDot" <+> var
                    ]
      insts' = nest tab
              $ vsep [ insts
                     , text "listToDot" <+> equals <+> text "unqtListToDot"
                     ]

parseInstance     :: Atts -> Code
parseInstance att = hdr $+$ nest tab fns
    where
      hdr = text "instance" <+> text "ParseDot" <+> tpNm att <+> text "where"
      fn = pFunc <+> equals <+> (text "stringParse" <+> parens (text "concat" <+> ops)
                                 $$ text "`onFail`" $$ pUnknown)
      fns = vsep [ fn
                 , text "parse" <+> equals <+> pFunc
                 , text "parseList" <+> equals <+> text "parseUnqtList"
                 ]
      ops = flip ($$) rbrack
            . asRows
            . firstOthers lbrack comma
            . map return
            . map parseAttr
            $ atts att
      pFunc = text "parseUnqt"
      pType b a
          | valtype a == Bl       = pFld <> text "Bool" <+> cnst a
          | isJust $ parseDef a   = pFld <> text "Def"  <+> cnst a <+> fromJust (parseDef a)
          | otherwise             = pFld <+> cnst a
          where
            pFld = text "parseField" <> if b then char 's' else empty

      parseAttr a = case map doubleQuotes $ parseNames a of
                      [n] -> pType False a <+> n
                      ns  -> pType True  a <+> docList ns
      unknownName = text "attrName"
      pUnknown = text "do"
                 <+> (   (unknownName <+> text "<- stringBlock")
                      $$ (text "liftEqParse"
                          <+> (parens (text "\"" <> unknownAttr <+> text "(\""
                                       <+> text "++ T.unpack" <+> unknownName
                                       <+> text "++ \")\"")
                               $$ parens (unknownAttr <+> unknownName)
                               )
                         )
                     )

arbitraryInstance     :: Atts -> Code
arbitraryInstance att = hdr $+$ fns
    where
      hdr = text "instance" <+> text "Arbitrary" <+> tpNm att <+> text "where"
      fns = nest tab $ vsep [aFn, sFn]
      aFn = aFunc <+> equals <+> text "oneof" <+> ops
      ops = flip ($$) rbrack
            . asRows
            . firstOthers lbrack comma
            . (++ [[aUnknown]])
            . map (return . arbAttr)
            $ atts att
      aFunc = text "arbitrary"
      arbAttr a = text "liftM" <+> cnst a <+> arbitraryFor' a
      sFn = asRows
            . (++ [sUnknown])
            . map shrinkAttr
            $ atts att
      sFunc = text "shrink"
      var = char 'v'
      shrinkAttr a = [ sFunc <+> parens (cnst a <+> var)
                     , equals <+> text "map" <+> cnst a
                     , dollar <+> shrinkFor (valtype a) <+> var
                     ]
      aUnknown = text "liftM2" <+> unknownAttr
                 <+> parens (text "suchThat" <+> text "arbIDString" <+> validUnknownName)
                 <+> arbitraryFor Strng
      sUnknown = [ sFunc <+> parens (unknownAttr <+> char 'a' <+> var)
                 , equals <+> text "liftM2" <+> unknownAttr
                 , parens (text "liftM" <+> parens (text "filter" <+> validUnknownName)
                           <+> shrinkFor Strng <+> char 'a')
                   <+> parens (shrinkFor Strng <+> var)
                 ]

validUnknownName :: Code
validUnknownName = text "validUnknown"

validUnknownFunc     :: Atts -> Code
validUnknownFunc att = cmnt $$ asRows [tpSig, def] $$ whClause
    where
      var = text "txt"
      setVar = text "names"
      cmnt = text "-- | Determine if the provided 'Text' value is a valid name"
             <+> text "for an '" <> unknownAttr <> text "'."
      tpSig = [ validUnknownName
              , colon <> colon <+> text "AttributeName -> Bool"
              ]
      def = [ validUnknownName <+> var
            , equals <+>
              (text "T.toLower" <+> var
               <+> text "`S.notMember`" <+> setVar
               $$ text "&&" <+> text "isIDString" <+> var)
            ]
      whClause = nest tab
                 $ text "where"
                 $$ nest tab setDef
      setDef = setVar <+> equals <+> mkSet
      mkSet = parens (text "S.fromList . map T.toLower"
                      $$ dollar
                      <+> setList)
              $$ text "`S.union`"
              $$ text "keywords"
      setList = flip ($$) rbrack
                . asRows
                . firstOthers lbrack comma
                . flip (++) [[doubleQuotes (text "charset")
                              <+> text "-- Defined upstream, just not used here."]]
                . map ((:[]) . doubleQuotes)
                . concatMap parseNames
                $ atts att

arbitraryFor :: VType -> Doc
arbitraryFor = text . bool "arbitrary" "arbList" . isListType

arbitraryFor' :: Attribute -> Doc
arbitraryFor' = arbitraryFor . valtype

shrinkFor :: VType -> Doc
shrinkFor = text . bool "shrink" "nonEmptyShrinks" . isListType

-- Some types are aliases for lists.
isListType :: VType -> Bool
isListType (Cust ('[':_))     = True
isListType (Cust "ColorList") = True
isListType _                  = False

usedByFunc          :: String -> (Attribute -> Bool) -> Atts -> Code
usedByFunc nm p att = cmnt $$ asRows (tpSig : trs ++ [fls])
    where
      nm' = text nm
      dt = tpNm att
      cmnt = text "-- | Determine if this '" <> dt
             <> text "' is valid for use with" <+> nm' <> dot
      tpSig = [ fn
              , colon <> colon
                <+> dt <+> text "->" <+> text "Bool"
              ]
      fn = text "usedBy" <> nm'
      tr = text "True"
      trs = map aTr as' ++ [unknownATr]
      fl = text "False"
      fls = [ fn <+> char '_'
            , equals <+> fl
            ]
      as' = filter p $ atts att
      aTr a = [ fn <+> cnst a <> braces empty
              , equals <+> tr
              ]
      unknownATr = [ fn <+> unknownAttr <> braces empty
                   , equals <+> tr
                   ]

sameAttributeFunc     :: Atts -> Code
sameAttributeFunc att = cmnt $$ asRows (tpSig : stmts ++ [unknownAtr, rst])
  where
    cmnt = text "-- | Determine if two '" <> dt
           <> text "s' are the same type of '"<> dt <> text"'."
    sFunc = text "sameAttribute"
    dt = tpNm att
    tpSig = [ sFunc
            , char ' ' -- first arg, for some reason won't line up
                       -- properly if its empty
            , empty -- second arg
            , colon <> colon
              <+> dt <+> text "->" <+> dt <+> text "->" <+> text "Bool"
            ]
    stmts = map sf $ atts att
    sf a = [ sFunc
           , cnst a <> braces empty
           , cnst a <> braces empty
           , equals <+> tr
           ]
    tr = text "True"
    catchAll = char '_'
    unknownAtr = [ sFunc
                 , parens $ unknownAttr <+> text "a1" <+> catchAll
                 , parens $ unknownAttr <+> text "a2" <+> catchAll
                 , equals <+> text "a1" <+> equals <> equals <+> text "a2"
                 ]
    rst = [ sFunc
          , catchAll
          , catchAll
          , equals <+> text "False"
          ]

defValueFunc :: Atts -> Code
defValueFunc att = cmnt $$ asRows (tpSig : stmts ++ [unknownAtr])
  where
    cmnt = text "-- | Return the default value for a specific" <+> quotes dt
           <+> text "if possible; graph/cluster values are preferred"
           <+> text "over node/edge values."
    dFunc = text "defaultAttributeValue"
    dt = tpNm att
    tpSig = [ dFunc
            , colon <> colon
              <+> dt <+> text "->" <+> text "Maybe" <+> dt
            ]
    stmts = map sf . filter (isJust . defValue) $ atts att
    sf a = [ dFunc <+> cnst a <> braces empty
           , equals <+> text "Just" <+> text "$" <+> cnst a
             <+> fromJust (defValue a)
           ]
    unknownAtr = [ dFunc <+> char '_'
                 , equals <+> text "Nothing"
                 ]

-- -----------------------------------------------------------------------------

-- Helper functions

-- Size of a tab character
tab :: Int
tab = 2

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

-- Don't edit this value directly; edit the table below instead.

attributes :: [Attribute]
attributes = [
  -- BEGIN RECEIVE ORGTBL Attributes
  makeAttr "Damping" ["Damping"] "G" (Dbl) Nothing (Just "0.99") (Just "@0.99@") (Just "@0.0@") (Just "'Neato' only"),
  makeAttr "K" ["K"] "GC" (Dbl) Nothing (Just "0.3") (Just "@0.3@") (Just "@0@") (Just "'Sfdp', 'Fdp' only"),
  makeAttr "URL" ["URL", "href"] "ENGC" (EStrng) Nothing (Just "\"\"") (Just "none") Nothing (Just "svg, postscript, map only"),
  makeAttr "Area" ["area"] "NC" (Dbl) Nothing (Just "1.0") (Just "@1.0@") (Just "@>0@") (Just "'Patchwork' only, requires Graphviz >= 2.30.0"),
  makeAttr "ArrowHead" ["arrowhead"] "E" (Cust "ArrowType") Nothing (Just "normal") (Just "@'normal'@") Nothing Nothing,
  makeAttr "ArrowSize" ["arrowsize"] "E" (Dbl) Nothing (Just "1.0") (Just "@1.0@") (Just "@0.0@") Nothing,
  makeAttr "ArrowTail" ["arrowtail"] "E" (Cust "ArrowType") Nothing (Just "normal") (Just "@'normal'@") Nothing Nothing,
  makeAttr "Background" ["_background"] "G" (Strng) Nothing (Just "\"\"") (Just "none") Nothing (Just "xdot only"),
  makeAttr "BoundingBox" ["bb"] "G" (Cust "Rect") Nothing Nothing Nothing Nothing (Just "write only"),
  makeAttr "BgColor" ["bgcolor"] "GC" (Cust "ColorList") Nothing (Just "[]") (Just "@[]@") Nothing Nothing,
  makeAttr "Center" ["center"] "G" (Bl) (Just "True") (Just "False") (Just "@'False'@") Nothing Nothing,
  makeAttr "ClusterRank" ["clusterrank"] "G" (Cust "ClusterMode") Nothing (Just "Local") (Just "@'Local'@") Nothing (Just "'Dot' only"),
  makeAttr "Color" ["color"] "ENC" (Cust "ColorList") Nothing (Just "[toWColor Black]") (Just "@['WC' ('X11Color' 'Black') Nothing]@") Nothing Nothing,
  makeAttr "ColorScheme" ["colorscheme"] "ENCG" (Cust "ColorScheme") Nothing (Just "X11") (Just "@'X11'@") Nothing Nothing,
  makeAttr "Comment" ["comment"] "ENG" (Strng) Nothing (Just "\"\"") (Just "@\\\"\\\"@") Nothing Nothing,
  makeAttr "Compound" ["compound"] "G" (Bl) (Just "True") (Just "False") (Just "@'False'@") Nothing (Just "'Dot' only"),
  makeAttr "Concentrate" ["concentrate"] "G" (Bl) (Just "True") (Just "False") (Just "@'False'@") Nothing Nothing,
  makeAttr "Constraint" ["constraint"] "E" (Bl) (Just "True") (Just "True") (Just "@'True'@") Nothing (Just "'Dot' only"),
  makeAttr "Decorate" ["decorate"] "E" (Bl) (Just "True") (Just "False") (Just "@'False'@") Nothing Nothing,
  makeAttr "DefaultDist" ["defaultdist"] "G" (Dbl) Nothing Nothing (Just "@1+(avg. len)*sqrt(abs(V))@ (unable to statically define)") (Just "The value of 'Epsilon'.") (Just "'Neato' only, only if @'Pack' 'DontPack'@"),
  makeAttr "Dim" ["dim"] "G" (Integ) Nothing (Just "2") (Just "@2@") (Just "@2@") (Just "maximum of @10@; 'Sfdp', 'Fdp', 'Neato' only"),
  makeAttr "Dimen" ["dimen"] "G" (Integ) Nothing (Just "2") (Just "@2@") (Just "@2@") (Just "maximum of @10@; 'Sfdp', 'Fdp', 'Neato' only"),
  makeAttr "Dir" ["dir"] "E" (Cust "DirType") Nothing Nothing (Just "@'Forward'@ (directed), @'NoDir'@ (undirected)") Nothing Nothing,
  makeAttr "DirEdgeConstraints" ["diredgeconstraints"] "G" (Cust "DEConstraints") (Just "EdgeConstraints") (Just "NoConstraints") (Just "@'NoConstraints'@") Nothing (Just "'Neato' only"),
  makeAttr "Distortion" ["distortion"] "N" (Dbl) Nothing (Just "0.0") (Just "@0.0@") (Just "@-100.0@") Nothing,
  makeAttr "DPI" ["dpi", "resolution"] "G" (Dbl) Nothing (Just "96.0") (Just "@96.0@, @0.0@") Nothing (Just "svg, bitmap output only; \\\"resolution\\\" is a synonym"),
  makeAttr "EdgeURL" ["edgeURL", "edgehref"] "E" (EStrng) Nothing (Just "\"\"") (Just "@\\\"\\\"@") Nothing (Just "svg, map only"),
  makeAttr "EdgeTarget" ["edgetarget"] "E" (EStrng) Nothing Nothing (Just "none") Nothing (Just "svg, map only"),
  makeAttr "EdgeTooltip" ["edgetooltip"] "E" (EStrng) Nothing (Just "\"\"") (Just "@\\\"\\\"@") Nothing (Just "svg, cmap only"),
  makeAttr "Epsilon" ["epsilon"] "G" (Dbl) Nothing Nothing (Just "@.0001 * # nodes@ (@mode == 'KK'@), @.0001@ (@mode == 'Major'@)") Nothing (Just "'Neato' only"),
  makeAttr "ESep" ["esep"] "G" (Cust "DPoint") Nothing (Just "(DVal 3)") (Just "@'DVal' 3@") Nothing (Just "not 'Dot'"),
  makeAttr "FillColor" ["fillcolor"] "NEC" (Cust "ColorList") Nothing (Just "[toWColor Black]") (Just "@['WC' ('X11Color' 'LightGray') Nothing]@ (nodes), @['WC' ('X11Color' 'Black') Nothing]@ (clusters)") Nothing Nothing,
  makeAttr "FixedSize" ["fixedsize"] "N" (Cust "NodeSize") (Just "SetNodeSize") (Just "GrowAsNeeded") (Just "@'GrowAsNeeded'@") Nothing Nothing,
  makeAttr "FontColor" ["fontcolor"] "ENGC" (Cust "Color") Nothing (Just "(X11Color Black)") (Just "@'X11Color' 'Black'@") Nothing Nothing,
  makeAttr "FontName" ["fontname"] "ENGC" (Strng) Nothing (Just "\"Times-Roman\"") (Just "@\\\"Times-Roman\\\"@") Nothing Nothing,
  makeAttr "FontNames" ["fontnames"] "G" (Cust "SVGFontNames") Nothing (Just "SvgNames") (Just "@'SvgNames'@") Nothing (Just "svg only"),
  makeAttr "FontPath" ["fontpath"] "G" (Cust "Paths") Nothing Nothing (Just "system dependent") Nothing Nothing,
  makeAttr "FontSize" ["fontsize"] "ENGC" (Dbl) Nothing (Just "14.0") (Just "@14.0@") (Just "@1.0@") Nothing,
  makeAttr "ForceLabels" ["forcelabels"] "G" (Bl) (Just "True") (Just "True") (Just "@'True'@") Nothing (Just "only for 'XLabel' attributes, requires Graphviz >= 2.29.0"),
  makeAttr "GradientAngle" ["gradientangle"] "NCG" (Integ) Nothing (Just "0") (Just "0") Nothing (Just "requires Graphviz >= 2.29.0"),
  makeAttr "Group" ["group"] "N" (Strng) Nothing (Just "\"\"") (Just "@\\\"\\\"@") Nothing (Just "'Dot' only"),
  makeAttr "HeadURL" ["headURL", "headhref"] "E" (EStrng) Nothing (Just "\"\"") (Just "@\\\"\\\"@") Nothing (Just "svg, map only"),
  makeAttr "Head_LP" ["head_lp"] "E" (Cust "Point") Nothing Nothing Nothing Nothing (Just "write only, requires Graphviz >= 2.30.0"),
  makeAttr "HeadClip" ["headclip"] "E" (Bl) (Just "True") (Just "True") (Just "@'True'@") Nothing Nothing,
  makeAttr "HeadLabel" ["headlabel"] "E" (Cust "Label") Nothing (Just "(StrLabel \"\")") (Just "@'StrLabel' \\\"\\\"@") Nothing Nothing,
  makeAttr "HeadPort" ["headport"] "E" (Cust "PortPos") Nothing (Just "(CompassPoint CenterPoint)") (Just "@'CompassPoint' 'CenterPoint'@") Nothing Nothing,
  makeAttr "HeadTarget" ["headtarget"] "E" (EStrng) Nothing (Just "\"\"") (Just "none") Nothing (Just "svg, map only"),
  makeAttr "HeadTooltip" ["headtooltip"] "E" (EStrng) Nothing (Just "\"\"") (Just "@\\\"\\\"@") Nothing (Just "svg, cmap only"),
  makeAttr "Height" ["height"] "N" (Dbl) Nothing (Just "0.5") (Just "@0.5@") (Just "@0.02@") Nothing,
  makeAttr "ID" ["id"] "GNE" (EStrng) Nothing (Just "\"\"") (Just "@\\\"\\\"@") Nothing (Just "svg, postscript, map only"),
  makeAttr "Image" ["image"] "N" (Strng) Nothing (Just "\"\"") (Just "@\\\"\\\"@") Nothing Nothing,
  makeAttr "ImagePath" ["imagepath"] "G" (Cust "Paths") Nothing (Just "(Paths [])") (Just "@'Paths' []@") Nothing (Just "Printing and parsing is OS-specific, requires Graphviz >= 2.29.0"),
  makeAttr "ImageScale" ["imagescale"] "N" (Cust "ScaleType") (Just "UniformScale") (Just "NoScale") (Just "@'NoScale'@") Nothing Nothing,
  makeAttr "InputScale" ["inputscale"] "N" (Dbl) Nothing Nothing (Just "none") Nothing (Just "'Fdp', 'Neato' only, a value of @0@ is equivalent to being @72@, requires Graphviz >= 2.36.0"),
  makeAttr "Label" ["label"] "ENGC" (Cust "Label") Nothing (Just "(StrLabel \"\")") (Just "@'StrLabel' \\\"\\\\N\\\"@ (nodes), @'StrLabel' \\\"\\\"@ (otherwise)") Nothing Nothing,
  makeAttr "LabelURL" ["labelURL", "labelhref"] "E" (EStrng) Nothing (Just "\"\"") (Just "@\\\"\\\"@") Nothing (Just "svg, map only"),
  makeAttr "LabelScheme" ["label_scheme"] "G" (Cust "LabelScheme") Nothing (Just "NotEdgeLabel") (Just "@'NotEdgeLabel'@") Nothing (Just "'Sfdp' only, requires Graphviz >= 2.28.0"),
  makeAttr "LabelAngle" ["labelangle"] "E" (Dbl) Nothing (Just "(-25.0)") (Just "@-25.0@") (Just "@-180.0@") Nothing,
  makeAttr "LabelDistance" ["labeldistance"] "E" (Dbl) Nothing (Just "1.0") (Just "@1.0@") (Just "@0.0@") Nothing,
  makeAttr "LabelFloat" ["labelfloat"] "E" (Bl) (Just "True") (Just "False") (Just "@'False'@") Nothing Nothing,
  makeAttr "LabelFontColor" ["labelfontcolor"] "E" (Cust "Color") Nothing (Just "(X11Color Black)") (Just "@'X11Color' 'Black'@") Nothing Nothing,
  makeAttr "LabelFontName" ["labelfontname"] "E" (Strng) Nothing (Just "\"Times-Roman\"") (Just "@\\\"Times-Roman\\\"@") Nothing Nothing,
  makeAttr "LabelFontSize" ["labelfontsize"] "E" (Dbl) Nothing (Just "14.0") (Just "@14.0@") (Just "@1.0@") Nothing,
  makeAttr "LabelJust" ["labeljust"] "GC" (Cust "Justification") Nothing (Just "JCenter") (Just "@'JCenter'@") Nothing Nothing,
  makeAttr "LabelLoc" ["labelloc"] "GCN" (Cust "VerticalPlacement") Nothing (Just "VTop") (Just "@'VTop'@ (clusters), @'VBottom'@ (root graphs), @'VCenter'@ (nodes)") Nothing Nothing,
  makeAttr "LabelTarget" ["labeltarget"] "E" (EStrng) Nothing (Just "\"\"") (Just "none") Nothing (Just "svg, map only"),
  makeAttr "LabelTooltip" ["labeltooltip"] "E" (EStrng) Nothing (Just "\"\"") (Just "@\\\"\\\"@") Nothing (Just "svg, cmap only"),
  makeAttr "Landscape" ["landscape"] "G" (Bl) (Just "True") (Just "False") (Just "@'False'@") Nothing Nothing,
  makeAttr "Layer" ["layer"] "ENC" (Cust "LayerRange") Nothing (Just "[]") (Just "@[]@") Nothing Nothing,
  makeAttr "LayerListSep" ["layerlistsep"] "G" (Cust "LayerListSep") Nothing (Just "(LLSep \",\")") (Just "@'LLSep' \\\",\\\"@") Nothing (Just "requires Graphviz >= 2.30.0"),
  makeAttr "Layers" ["layers"] "G" (Cust "LayerList") Nothing (Just "(LL [])") (Just "@'LL' []@") Nothing Nothing,
  makeAttr "LayerSelect" ["layerselect"] "G" (Cust "LayerRange") Nothing (Just "[]") (Just "@[]@") Nothing Nothing,
  makeAttr "LayerSep" ["layersep"] "G" (Cust "LayerSep") Nothing (Just "(LSep \" :\\t\")") (Just "@'LSep' \\\" :\\t\\\"@") Nothing Nothing,
  makeAttr "Layout" ["layout"] "G" (Cust "GraphvizCommand") Nothing Nothing Nothing Nothing Nothing,
  makeAttr "Len" ["len"] "E" (Dbl) Nothing Nothing (Just "@1.0@ ('Neato'), @0.3@ ('Fdp')") Nothing (Just "'Fdp', 'Neato' only"),
  makeAttr "Levels" ["levels"] "G" (Integ) Nothing (Just "maxBound") (Just "@'maxBound'@") (Just "@0@") (Just "'Sfdp' only"),
  makeAttr "LevelsGap" ["levelsgap"] "G" (Dbl) Nothing (Just "0.0") (Just "@0.0@") Nothing (Just "'Neato' only"),
  makeAttr "LHead" ["lhead"] "E" (Strng) Nothing (Just "\"\"") (Just "@\\\"\\\"@") Nothing (Just "'Dot' only"),
  makeAttr "LHeight" ["LHeight"] "GC" (Dbl) Nothing Nothing Nothing Nothing (Just "write only, requires Graphviz >= 2.28.0"),
  makeAttr "LPos" ["lp"] "EGC" (Cust "Point") Nothing Nothing Nothing Nothing (Just "write only"),
  makeAttr "LTail" ["ltail"] "E" (Strng) Nothing (Just "\"\"") (Just "@\\\"\\\"@") Nothing (Just "'Dot' only"),
  makeAttr "LWidth" ["lwidth"] "GC" (Dbl) Nothing Nothing Nothing Nothing (Just "write only, requires Graphviz >= 2.28.0"),
  makeAttr "Margin" ["margin"] "NGC" (Cust "DPoint") Nothing Nothing (Just "device dependent") Nothing Nothing,
  makeAttr "MaxIter" ["maxiter"] "G" (Integ) Nothing Nothing (Just "@100 * # nodes@ (@mode == 'KK'@), @200@ (@mode == 'Major'@), @600@ ('Fdp')") Nothing (Just "'Fdp', 'Neato' only"),
  makeAttr "MCLimit" ["mclimit"] "G" (Dbl) Nothing (Just "1.0") (Just "@1.0@") Nothing (Just "'Dot' only"),
  makeAttr "MinDist" ["mindist"] "G" (Dbl) Nothing (Just "1.0") (Just "@1.0@") (Just "@0.0@") (Just "'Circo' only"),
  makeAttr "MinLen" ["minlen"] "E" (Integ) Nothing (Just "1") (Just "@1@") (Just "@0@") (Just "'Dot' only"),
  makeAttr "Mode" ["mode"] "G" (Cust "ModeType") Nothing (Just "Major") (Just "@'Major'@ (actually @'Spring'@ for 'Sfdp', but this isn't used as a default in this library)") Nothing (Just "'Neato', 'Sfdp' only"),
  makeAttr "Model" ["model"] "G" (Cust "Model") Nothing (Just "ShortPath") (Just "@'ShortPath'@") Nothing (Just "'Neato' only"),
  makeAttr "Mosek" ["mosek"] "G" (Bl) (Just "True") (Just "False") (Just "@'False'@") Nothing (Just "'Neato' only; requires the Mosek software"),
  makeAttr "NodeSep" ["nodesep"] "G" (Dbl) Nothing (Just "0.25") (Just "@0.25@") (Just "@0.02@") Nothing,
  makeAttr "NoJustify" ["nojustify"] "GCNE" (Bl) (Just "True") (Just "False") (Just "@'False'@") Nothing Nothing,
  makeAttr "Normalize" ["normalize"] "G" (Cust "Normalized") (Just "IsNormalized") (Just "NotNormalized") (Just "@'NotNormalized'@") Nothing (Just "not 'Dot'"),
  makeAttr "NoTranslate" ["notranslate"] "G" (Bl) (Just "True") (Just "False") (Just "@'False'@") Nothing (Just "'Neato' only, requires Graphviz >= 2.38.0"),
  makeAttr "Nslimit" ["nslimit"] "G" (Dbl) Nothing Nothing Nothing Nothing (Just "'Dot' only"),
  makeAttr "Nslimit1" ["nslimit1"] "G" (Dbl) Nothing Nothing Nothing Nothing (Just "'Dot' only"),
  makeAttr "Ordering" ["ordering"] "GN" (Cust "Order") Nothing Nothing (Just "none") Nothing (Just "'Dot' only"),
  makeAttr "Orientation" ["orientation"] "N" (Dbl) Nothing (Just "0.0") (Just "@0.0@") (Just "@360.0@") Nothing,
  makeAttr "OutputOrder" ["outputorder"] "G" (Cust "OutputMode") Nothing (Just "BreadthFirst") (Just "@'BreadthFirst'@") Nothing Nothing,
  makeAttr "Overlap" ["overlap"] "G" (Cust "Overlap") (Just "KeepOverlaps") (Just "KeepOverlaps") (Just "@'KeepOverlaps'@") Nothing (Just "not 'Dot'"),
  makeAttr "OverlapScaling" ["overlap_scaling"] "G" (Dbl) Nothing (Just "(-4)") (Just "@-4@") (Just "@-1.0e10@") (Just "'PrismOverlap' only"),
  makeAttr "OverlapShrink" ["overlap_shrink"] "G" (Bl) (Just "True") (Just "True") (Just "@'True'@") Nothing (Just "'PrismOverlap' only, requires Graphviz >= 2.36.0"),
  makeAttr "Pack" ["pack"] "G" (Cust "Pack") (Just "DoPack") (Just "DontPack") (Just "@'DontPack'@") Nothing Nothing,
  makeAttr "PackMode" ["packmode"] "G" (Cust "PackMode") Nothing (Just "PackNode") (Just "@'PackNode'@") Nothing Nothing,
  makeAttr "Pad" ["pad"] "G" (Cust "DPoint") Nothing (Just "(DVal 0.0555)") (Just "@'DVal' 0.0555@ (4 points)") Nothing Nothing,
  makeAttr "Page" ["page"] "G" (Cust "Point") Nothing Nothing Nothing Nothing Nothing,
  makeAttr "PageDir" ["pagedir"] "G" (Cust "PageDir") Nothing (Just "Bl") (Just "@'Bl'@") Nothing Nothing,
  makeAttr "PenColor" ["pencolor"] "C" (Cust "Color") Nothing (Just "(X11Color Black)") (Just "@'X11Color' 'Black'@") Nothing Nothing,
  makeAttr "PenWidth" ["penwidth"] "CNE" (Dbl) Nothing (Just "1.0") (Just "@1.0@") (Just "@0.0@") Nothing,
  makeAttr "Peripheries" ["peripheries"] "NC" (Integ) Nothing (Just "1") (Just "shape default (nodes), @1@ (clusters)") (Just "0") Nothing,
  makeAttr "Pin" ["pin"] "N" (Bl) (Just "True") (Just "False") (Just "@'False'@") Nothing (Just "'Fdp', 'Neato' only"),
  makeAttr "Pos" ["pos"] "EN" (Cust "Pos") Nothing Nothing Nothing Nothing Nothing,
  makeAttr "QuadTree" ["quadtree"] "G" (Cust "QuadType") (Just "NormalQT") (Just "NormalQT") (Just "@'NormalQT'@") Nothing (Just "'Sfdp' only"),
  makeAttr "Quantum" ["quantum"] "G" (Dbl) Nothing (Just "0") (Just "@0.0@") (Just "@0.0@") Nothing,
  makeAttr "Rank" ["rank"] "S" (Cust "RankType") Nothing Nothing Nothing Nothing (Just "'Dot' only"),
  makeAttr "RankDir" ["rankdir"] "G" (Cust "RankDir") Nothing (Just "FromTop") (Just "@'FromTop'@") Nothing (Just "'Dot' only"),
  makeAttr "RankSep" ["ranksep"] "G" (Cust "[Double]") Nothing Nothing (Just "@[0.5]@ ('Dot'), @[1.0]@ ('Twopi')") (Just "@[0.02]@") (Just "'Twopi', 'Dot' only"),
  makeAttr "Ratio" ["ratio"] "G" (Cust "Ratios") Nothing Nothing Nothing Nothing Nothing,
  makeAttr "Rects" ["rects"] "N" (Cust "[Rect]") Nothing Nothing Nothing Nothing (Just "write only"),
  makeAttr "Regular" ["regular"] "N" (Bl) (Just "True") (Just "False") (Just "@'False'@") Nothing Nothing,
  makeAttr "ReMinCross" ["remincross"] "G" (Bl) (Just "True") (Just "False") (Just "@'False'@") Nothing (Just "'Dot' only"),
  makeAttr "RepulsiveForce" ["repulsiveforce"] "G" (Dbl) Nothing (Just "1.0") (Just "@1.0@") (Just "@0.0@") (Just "'Sfdp' only"),
  makeAttr "Root" ["root"] "GN" (Cust "Root") (Just "IsCentral") (Just "(NodeName \"\")") (Just "@'NodeName' \\\"\\\"@ (graphs), @'NotCentral'@ (nodes)") Nothing (Just "'Circo', 'Twopi' only"),
  makeAttr "Rotate" ["rotate"] "G" (Integ) Nothing (Just "0") (Just "@0@") Nothing Nothing,
  makeAttr "Rotation" ["rotation"] "G" (Dbl) Nothing (Just "0") (Just "@0@") Nothing (Just "'Sfdp' only, requires Graphviz >= 2.28.0"),
  makeAttr "SameHead" ["samehead"] "E" (Strng) Nothing (Just "\"\"") (Just "@\\\"\\\"@") Nothing (Just "'Dot' only"),
  makeAttr "SameTail" ["sametail"] "E" (Strng) Nothing (Just "\"\"") (Just "@\\\"\\\"@") Nothing (Just "'Dot' only"),
  makeAttr "SamplePoints" ["samplepoints"] "N" (Integ) Nothing Nothing (Just "@8@ (output), @20@ (overlap and image maps)") Nothing Nothing,
  makeAttr "Scale" ["scale"] "G" (Cust "DPoint") Nothing Nothing Nothing Nothing (Just "Not 'Dot', requires Graphviz >= 2.28.0 (>= 2.38.0 for anything except 'TwoPi')"),
  makeAttr "SearchSize" ["searchsize"] "G" (Integ) Nothing (Just "30") (Just "@30@") Nothing (Just "'Dot' only"),
  makeAttr "Sep" ["sep"] "G" (Cust "DPoint") Nothing (Just "(DVal 4)") (Just "@'DVal' 4@") Nothing (Just "not 'Dot'"),
  makeAttr "Shape" ["shape"] "N" (Cust "Shape") Nothing (Just "Ellipse") (Just "@'Ellipse'@") Nothing Nothing,
  makeAttr "ShowBoxes" ["showboxes"] "ENG" (Integ) Nothing (Just "0") (Just "@0@") (Just "@0@") (Just "'Dot' only; used for debugging by printing PostScript guide boxes"),
  makeAttr "Sides" ["sides"] "N" (Integ) Nothing (Just "4") (Just "@4@") (Just "@0@") Nothing,
  makeAttr "Size" ["size"] "G" (Cust "GraphSize") Nothing Nothing Nothing Nothing Nothing,
  makeAttr "Skew" ["skew"] "N" (Dbl) Nothing (Just "0.0") (Just "@0.0@") (Just "@-100.0@") Nothing,
  makeAttr "Smoothing" ["smoothing"] "G" (Cust "SmoothType") Nothing (Just "NoSmooth") (Just "@'NoSmooth'@") Nothing (Just "'Sfdp' only"),
  makeAttr "SortV" ["sortv"] "GCN" (Cust "Word16") Nothing (Just "0") (Just "@0@") (Just "@0@") Nothing,
  makeAttr "Splines" ["splines"] "G" (Cust "EdgeType") (Just "SplineEdges") Nothing (Just "@'SplineEdges'@ ('Dot'), @'LineEdges'@ (other)") Nothing Nothing,
  makeAttr "Start" ["start"] "G" (Cust "StartType") Nothing Nothing (Just "@'StartStyleSeed' 'RandomStyle' seed@ for some unknown fixed seed.") Nothing (Just "'Fdp', 'Neato' only"),
  makeAttr "Style" ["style"] "ENCG" (Cust "[StyleItem]") Nothing Nothing Nothing Nothing Nothing,
  makeAttr "StyleSheet" ["stylesheet"] "G" (Strng) Nothing (Just "\"\"") (Just "@\\\"\\\"@") Nothing (Just "svg only"),
  makeAttr "TailURL" ["tailURL", "tailhref"] "E" (EStrng) Nothing (Just "\"\"") (Just "@\\\"\\\"@") Nothing (Just "svg, map only"),
  makeAttr "Tail_LP" ["tail_lp"] "E" (Cust "Point") Nothing Nothing Nothing Nothing (Just "write only, requires Graphviz >= 2.30.0"),
  makeAttr "TailClip" ["tailclip"] "E" (Bl) (Just "True") (Just "True") (Just "@'True'@") Nothing Nothing,
  makeAttr "TailLabel" ["taillabel"] "E" (Cust "Label") Nothing (Just "(StrLabel \"\")") (Just "@'StrLabel' \\\"\\\"@") Nothing Nothing,
  makeAttr "TailPort" ["tailport"] "E" (Cust "PortPos") Nothing (Just "(CompassPoint CenterPoint)") (Just "@'CompassPoint' 'CenterPoint'@") Nothing Nothing,
  makeAttr "TailTarget" ["tailtarget"] "E" (EStrng) Nothing (Just "\"\"") (Just "none") Nothing (Just "svg, map only"),
  makeAttr "TailTooltip" ["tailtooltip"] "E" (EStrng) Nothing (Just "\"\"") (Just "@\\\"\\\"@") Nothing (Just "svg, cmap only"),
  makeAttr "Target" ["target"] "ENGC" (EStrng) Nothing (Just "\"\"") (Just "none") Nothing (Just "svg, map only"),
  makeAttr "Tooltip" ["tooltip"] "NEC" (EStrng) Nothing (Just "\"\"") (Just "@\\\"\\\"@") Nothing (Just "svg, cmap only"),
  makeAttr "TrueColor" ["truecolor"] "G" (Bl) (Just "True") Nothing Nothing Nothing (Just "bitmap output only"),
  makeAttr "Vertices" ["vertices"] "N" (Cust "[Point]") Nothing Nothing Nothing Nothing (Just "write only"),
  makeAttr "ViewPort" ["viewport"] "G" (Cust "ViewPort") Nothing Nothing (Just "none") Nothing Nothing,
  makeAttr "VoroMargin" ["voro_margin"] "G" (Dbl) Nothing (Just "0.05") (Just "@0.05@") (Just "@0.0@") (Just "not 'Dot'"),
  makeAttr "Weight" ["weight"] "E" (Cust "Number") Nothing (Just "(Int 1)") (Just "@'Int' 1@") (Just "@'Int' 0@ ('Dot'), @'Int' 1@ ('Neato','Fdp','Sfdp')") (Just "as of Graphviz 2.30: weights for dot need to be 'Int's"),
  makeAttr "Width" ["width"] "N" (Dbl) Nothing (Just "0.75") (Just "@0.75@") (Just "@0.01@") Nothing,
  makeAttr "XDotVersion" ["xdotversion"] "G" (Cust "Version") Nothing Nothing Nothing Nothing (Just "xdot only, requires Graphviz >= 2.34.0, equivalent to specifying version of xdot to be used"),
  makeAttr "XLabel" ["xlabel"] "EN" (Cust "Label") Nothing (Just "(StrLabel \"\")") (Just "@'StrLabel' \\\"\\\"@") Nothing (Just "requires Graphviz >= 2.29.0"),
  makeAttr "XLP" ["xlp"] "EN" (Cust "Point") Nothing Nothing Nothing Nothing (Just "write only, requires Graphviz >= 2.29.0")
  -- END RECEIVE ORGTBL Attributes
  ]

{-

When using Emacs with org-mode available, hitting ` C-c C-c ' inside
the table below should update the actual Haskell code above.

This way, you can more easily edit/update the appropriate values.

* Values in "Allowed names" should be space-separated (and are
  converted into a list of Strings).

* Used by should consist of a sub-set of `ENGCS'.

* The entries in "Type" should be a valid Haskell value of type
  'VType' (parens are added).

* An empty cell in the last five columns translates to a `Nothing'
  value; anything else is wrapped in quotes and then has Just applied to
  it.  As such, you still need to escape quotes.

* Any entries in 'Parsing default' or "Default value" should be a
  valid Haskell value (add parens if needed) with the exception that
  double-quotes should be escaped.

* @C-c `@ can be used to edit a field that has been narrowed.

#+ORGTBL: SEND Attributes orgtbl-to-generic :skip 2 :splice t :hline nil :no-escape t :lstart "  makeAttr " :lend "," :llend "" :sep " " :fmt (1 cell-quote 2 cell-to-list 3 cell-quote 4 cell-parens 5 cell-to-maybe 6 cell-to-maybe 7 cell-to-maybe 8 cell-to-maybe 9 cell-to-maybe)
| Constructor     | Allowed names   | Used By | Type            | Parse def  | Default value   | Default for Documentation | Minimum    | Comment notes        |
|-----------------+-----------------+---------+-----------------+------------+-----------------+----------------------+------------+----------------------|
| <15>            | <15>            |         | <15>            | <10>       | <15>            | <20>                 | <10>       | <20>                 |
| Damping         | Damping         | G       | Dbl             |            | 0.99            | @0.99@               | @0.0@      | 'Neato' only         |
| K               | K               | GC      | Dbl             |            | 0.3             | @0.3@                | @0@        | 'Sfdp', 'Fdp' only   |
| URL             | URL href        | ENGC    | EStrng          |            | \"\"            | none                 |            | svg, postscript, map only |
| Area            | area            | NC      | Dbl             |            | 1.0             | @1.0@                | @>0@       | 'Patchwork' only, requires Graphviz >= 2.30.0 |
| ArrowHead       | arrowhead       | E       | Cust "ArrowType" |            | normal          | @'normal'@           |            |                      |
| ArrowSize       | arrowsize       | E       | Dbl             |            | 1.0             | @1.0@                | @0.0@      |                      |
| ArrowTail       | arrowtail       | E       | Cust "ArrowType" |            | normal          | @'normal'@           |            |                      |
| Background      | _background     | G       | Strng           |            | \"\"            | none                 |            | xdot only            |
| BoundingBox     | bb              | G       | Cust "Rect"     |            |                 |                      |            | write only           |
| BgColor         | bgcolor         | GC      | Cust "ColorList" |            | []              | @[]@                 |            |                      |
| Center          | center          | G       | Bl              | True       | False           | @'False'@            |            |                      |
| ClusterRank     | clusterrank     | G       | Cust "ClusterMode" |            | Local           | @'Local'@            |            | 'Dot' only           |
| Color           | color           | ENC     | Cust "ColorList" |            | [toWColor Black] | @['WC' ('X11Color' 'Black') Nothing]@ |            |                      |
| ColorScheme     | colorscheme     | ENCG    | Cust "ColorScheme" |            | X11             | @'X11'@              |            |                      |
| Comment         | comment         | ENG     | Strng           |            | \"\"            | @\\\"\\\"@           |            |                      |
| Compound        | compound        | G       | Bl              | True       | False           | @'False'@            |            | 'Dot' only           |
| Concentrate     | concentrate     | G       | Bl              | True       | False           | @'False'@            |            |                      |
| Constraint      | constraint      | E       | Bl              | True       | True            | @'True'@             |            | 'Dot' only           |
| Decorate        | decorate        | E       | Bl              | True       | False           | @'False'@            |            |                      |
| DefaultDist     | defaultdist     | G       | Dbl             |            |                 | @1+(avg. len)*sqrt(abs(V))@ (unable to statically define) | The value of 'Epsilon'. | 'Neato' only, only if @'Pack' 'DontPack'@ |
| Dim             | dim             | G       | Integ           |            | 2               | @2@                  | @2@        | maximum of @10@; 'Sfdp', 'Fdp', 'Neato' only |
| Dimen           | dimen           | G       | Integ           |            | 2               | @2@                  | @2@        | maximum of @10@; 'Sfdp', 'Fdp', 'Neato' only |
| Dir             | dir             | E       | Cust "DirType"  |            |                 | @'Forward'@ (directed), @'NoDir'@ (undirected) |            |                      |
| DirEdgeConstraints | diredgeconstraints | G       | Cust "DEConstraints" | EdgeConstraints | NoConstraints   | @'NoConstraints'@    |            | 'Neato' only         |
| Distortion      | distortion      | N       | Dbl             |            | 0.0             | @0.0@                | @-100.0@   |                      |
| DPI             | dpi resolution  | G       | Dbl             |            | 96.0            | @96.0@, @0.0@        |            | svg, bitmap output only; \\\"resolution\\\" is a synonym |
| EdgeURL         | edgeURL edgehref | E       | EStrng          |            | \"\"            | @\\\"\\\"@           |            | svg, map only        |
| EdgeTarget      | edgetarget      | E       | EStrng          |            |                 | none                 |            | svg, map only        |
| EdgeTooltip     | edgetooltip     | E       | EStrng          |            | \"\"            | @\\\"\\\"@           |            | svg, cmap only       |
| Epsilon         | epsilon         | G       | Dbl             |            |                 | @.0001 * # nodes@ (@mode == 'KK'@), @.0001@ (@mode == 'Major'@) |            | 'Neato' only         |
| ESep            | esep            | G       | Cust "DPoint"   |            | (DVal 3)        | @'DVal' 3@           |            | not 'Dot'            |
| FillColor       | fillcolor       | NEC     | Cust "ColorList" |            | [toWColor Black] | @['WC' ('X11Color' 'LightGray') Nothing]@ (nodes), @['WC' ('X11Color' 'Black') Nothing]@ (clusters) |            |                      |
| FixedSize       | fixedsize       | N       | Cust "NodeSize" | SetNodeSize | GrowAsNeeded    | @'GrowAsNeeded'@     |            |                      |
| FontColor       | fontcolor       | ENGC    | Cust "Color"    |            | (X11Color Black) | @'X11Color' 'Black'@ |            |                      |
| FontName        | fontname        | ENGC    | Strng           |            | \"Times-Roman\" | @\\\"Times-Roman\\\"@ |            |                      |
| FontNames       | fontnames       | G       | Cust "SVGFontNames" |            | SvgNames        | @'SvgNames'@         |            | svg only             |
| FontPath        | fontpath        | G       | Cust "Paths"    |            |                 | system dependent     |            |                      |
| FontSize        | fontsize        | ENGC    | Dbl             |            | 14.0            | @14.0@               | @1.0@      |                      |
| ForceLabels     | forcelabels     | G       | Bl              | True       | True            | @'True'@             |            | only for 'XLabel' attributes, requires Graphviz >= 2.29.0 |
| GradientAngle   | gradientangle   | NCG     | Integ           |            | 0               | 0                    |            | requires Graphviz >= 2.29.0 |
| Group           | group           | N       | Strng           |            | \"\"            | @\\\"\\\"@           |            | 'Dot' only           |
| HeadURL         | headURL headhref | E       | EStrng          |            | \"\"            | @\\\"\\\"@           |            | svg, map only        |
| Head_LP         | head_lp         | E       | Cust "Point"    |            |                 |                      |            | write only, requires Graphviz >= 2.30.0 |
| HeadClip        | headclip        | E       | Bl              | True       | True            | @'True'@             |            |                      |
| HeadLabel       | headlabel       | E       | Cust "Label"    |            | (StrLabel \"\") | @'StrLabel' \\\"\\\"@ |            |                      |
| HeadPort        | headport        | E       | Cust "PortPos"  |            | (CompassPoint CenterPoint) | @'CompassPoint' 'CenterPoint'@ |            |                      |
| HeadTarget      | headtarget      | E       | EStrng          |            | \"\"            | none                 |            | svg, map only        |
| HeadTooltip     | headtooltip     | E       | EStrng          |            | \"\"            | @\\\"\\\"@           |            | svg, cmap only       |
| Height          | height          | N       | Dbl             |            | 0.5             | @0.5@                | @0.02@     |                      |
| ID              | id              | GNE     | EStrng          |            | \"\"            | @\\\"\\\"@           |            | svg, postscript, map only |
| Image           | image           | N       | Strng           |            | \"\"            | @\\\"\\\"@           |            |                      |
| ImagePath       | imagepath       | G       | Cust "Paths"    |            | (Paths [])      | @'Paths' []@         |            | Printing and parsing is OS-specific, requires Graphviz >= 2.29.0 |
| ImageScale      | imagescale      | N       | Cust "ScaleType" | UniformScale | NoScale         | @'NoScale'@          |            |                      |
| InputScale      | inputscale      | N       | Dbl             |            |                 | none                 |            | 'Fdp', 'Neato' only, a value of @0@ is equivalent to being @72@, requires Graphviz >= 2.36.0 |
| Label           | label           | ENGC    | Cust "Label"    |            | (StrLabel \"\") | @'StrLabel' \\\"\\\\N\\\"@ (nodes), @'StrLabel' \\\"\\\"@ (otherwise) |            |                      |
| LabelURL        | labelURL labelhref | E       | EStrng          |            | \"\"            | @\\\"\\\"@           |            | svg, map only        |
| LabelScheme     | label_scheme    | G       | Cust "LabelScheme" |            | NotEdgeLabel    | @'NotEdgeLabel'@     |            | 'Sfdp' only, requires Graphviz >= 2.28.0 |
| LabelAngle      | labelangle      | E       | Dbl             |            | (-25.0)         | @-25.0@              | @-180.0@   |                      |
| LabelDistance   | labeldistance   | E       | Dbl             |            | 1.0             | @1.0@                | @0.0@      |                      |
| LabelFloat      | labelfloat      | E       | Bl              | True       | False           | @'False'@            |            |                      |
| LabelFontColor  | labelfontcolor  | E       | Cust "Color"    |            | (X11Color Black) | @'X11Color' 'Black'@ |            |                      |
| LabelFontName   | labelfontname   | E       | Strng           |            | \"Times-Roman\" | @\\\"Times-Roman\\\"@ |            |                      |
| LabelFontSize   | labelfontsize   | E       | Dbl             |            | 14.0            | @14.0@               | @1.0@      |                      |
| LabelJust       | labeljust       | GC      | Cust "Justification" |            | JCenter         | @'JCenter'@          |            |                      |
| LabelLoc        | labelloc        | GCN     | Cust "VerticalPlacement" |            | VTop            | @'VTop'@ (clusters), @'VBottom'@ (root graphs), @'VCenter'@ (nodes) |            |                      |
| LabelTarget     | labeltarget     | E       | EStrng          |            | \"\"            | none                 |            | svg, map only        |
| LabelTooltip    | labeltooltip    | E       | EStrng          |            | \"\"            | @\\\"\\\"@           |            | svg, cmap only       |
| Landscape       | landscape       | G       | Bl              | True       | False           | @'False'@            |            |                      |
| Layer           | layer           | ENC     | Cust "LayerRange" |            | []              | @[]@                 |            |                      |
| LayerListSep    | layerlistsep    | G       | Cust "LayerListSep" |            | (LLSep \",\")   | @'LLSep' \\\",\\\"@  |            | requires Graphviz >= 2.30.0 |
| Layers          | layers          | G       | Cust "LayerList" |            | (LL [])         | @'LL' []@            |            |                      |
| LayerSelect     | layerselect     | G       | Cust "LayerRange" |            | []              | @[]@                 |            |                      |
| LayerSep        | layersep        | G       | Cust "LayerSep" |            | (LSep \" :\\t\") | @'LSep' \\\" :\\t\\\"@ |            |                      |
| Layout          | layout          | G       | Cust "GraphvizCommand" |            |                 |                      |            |                      |
| Len             | len             | E       | Dbl             |            |                 | @1.0@ ('Neato'), @0.3@ ('Fdp') |            | 'Fdp', 'Neato' only  |
| Levels          | levels          | G       | Integ           |            | maxBound        | @'maxBound'@         | @0@        | 'Sfdp' only          |
| LevelsGap       | levelsgap       | G       | Dbl             |            | 0.0             | @0.0@                |            | 'Neato' only         |
| LHead           | lhead           | E       | Strng           |            | \"\"            | @\\\"\\\"@           |            | 'Dot' only           |
| LHeight         | LHeight         | GC      | Dbl             |            |                 |                      |            | write only, requires Graphviz >= 2.28.0 |
| LPos            | lp              | EGC     | Cust "Point"    |            |                 |                      |            | write only           |
| LTail           | ltail           | E       | Strng           |            | \"\"            | @\\\"\\\"@           |            | 'Dot' only           |
| LWidth          | lwidth          | GC      | Dbl             |            |                 |                      |            | write only, requires Graphviz >= 2.28.0 |
| Margin          | margin          | NGC     | Cust "DPoint"   |            |                 | device dependent     |            |                      |
| MaxIter         | maxiter         | G       | Integ           |            |                 | @100 * # nodes@ (@mode == 'KK'@), @200@ (@mode == 'Major'@), @600@ ('Fdp') |            | 'Fdp', 'Neato' only  |
| MCLimit         | mclimit         | G       | Dbl             |            | 1.0             | @1.0@                |            | 'Dot' only           |
| MinDist         | mindist         | G       | Dbl             |            | 1.0             | @1.0@                | @0.0@      | 'Circo' only         |
| MinLen          | minlen          | E       | Integ           |            | 1               | @1@                  | @0@        | 'Dot' only           |
| Mode            | mode            | G       | Cust "ModeType" |            | Major           | @'Major'@ (actually @'Spring'@ for 'Sfdp', but this isn't used as a default in this library) |            | 'Neato', 'Sfdp' only |
| Model           | model           | G       | Cust "Model"    |            | ShortPath       | @'ShortPath'@        |            | 'Neato' only         |
| Mosek           | mosek           | G       | Bl              | True       | False           | @'False'@            |            | 'Neato' only; requires the Mosek software |
| NodeSep         | nodesep         | G       | Dbl             |            | 0.25            | @0.25@               | @0.02@     |                      |
| NoJustify       | nojustify       | GCNE    | Bl              | True       | False           | @'False'@            |            |                      |
| Normalize       | normalize       | G       | Cust "Normalized" | IsNormalized | NotNormalized   | @'NotNormalized'@    |            | not 'Dot'            |
| NoTranslate     | notranslate     | G       | Bl              | True       | False           | @'False'@            |            | 'Neato' only, requires Graphviz >= 2.38.0 |
| Nslimit         | nslimit         | G       | Dbl             |            |                 |                      |            | 'Dot' only           |
| Nslimit1        | nslimit1        | G       | Dbl             |            |                 |                      |            | 'Dot' only           |
| Ordering        | ordering        | GN      | Cust "Order"    |            |                 | none                 |            | 'Dot' only           |
| Orientation     | orientation     | N       | Dbl             |            | 0.0             | @0.0@                | @360.0@    |                      |
| OutputOrder     | outputorder     | G       | Cust "OutputMode" |            | BreadthFirst    | @'BreadthFirst'@     |            |                      |
| Overlap         | overlap         | G       | Cust "Overlap"  | KeepOverlaps | KeepOverlaps    | @'KeepOverlaps'@     |            | not 'Dot'            |
| OverlapScaling  | overlap_scaling | G       | Dbl             |            | (-4)            | @-4@                 | @-1.0e10@  | 'PrismOverlap' only  |
| OverlapShrink   | overlap_shrink  | G       | Bl              | True       | True            | @'True'@             |            | 'PrismOverlap' only, requires Graphviz >= 2.36.0 |
| Pack            | pack            | G       | Cust "Pack"     | DoPack     | DontPack        | @'DontPack'@         |            |                      |
| PackMode        | packmode        | G       | Cust "PackMode" |            | PackNode        | @'PackNode'@         |            |                      |
| Pad             | pad             | G       | Cust "DPoint"   |            | (DVal 0.0555)   | @'DVal' 0.0555@ (4 points) |            |                      |
| Page            | page            | G       | Cust "Point"    |            |                 |                      |            |                      |
| PageDir         | pagedir         | G       | Cust "PageDir"  |            | Bl              | @'Bl'@               |            |                      |
| PenColor        | pencolor        | C       | Cust "Color"    |            | (X11Color Black) | @'X11Color' 'Black'@ |            |                      |
| PenWidth        | penwidth        | CNE     | Dbl             |            | 1.0             | @1.0@                | @0.0@      |                      |
| Peripheries     | peripheries     | NC      | Integ           |            | 1               | shape default (nodes), @1@ (clusters) | 0          |                      |
| Pin             | pin             | N       | Bl              | True       | False           | @'False'@            |            | 'Fdp', 'Neato' only  |
| Pos             | pos             | EN      | Cust "Pos"      |            |                 |                      |            |                      |
| QuadTree        | quadtree        | G       | Cust "QuadType" | NormalQT   | NormalQT        | @'NormalQT'@         |            | 'Sfdp' only          |
| Quantum         | quantum         | G       | Dbl             |            | 0               | @0.0@                | @0.0@      |                      |
| Rank            | rank            | S       | Cust "RankType" |            |                 |                      |            | 'Dot' only           |
| RankDir         | rankdir         | G       | Cust "RankDir"  |            | FromTop         | @'FromTop'@          |            | 'Dot' only           |
| RankSep         | ranksep         | G       | Cust "[Double]" |            |                 | @[0.5]@ ('Dot'), @[1.0]@ ('Twopi') | @[0.02]@   | 'Twopi', 'Dot' only  |
| Ratio           | ratio           | G       | Cust "Ratios"   |            |                 |                      |            |                      |
| Rects           | rects           | N       | Cust "[Rect]"   |            |                 |                      |            | write only           |
| Regular         | regular         | N       | Bl              | True       | False           | @'False'@            |            |                      |
| ReMinCross      | remincross      | G       | Bl              | True       | False           | @'False'@            |            | 'Dot' only           |
| RepulsiveForce  | repulsiveforce  | G       | Dbl             |            | 1.0             | @1.0@                | @0.0@      | 'Sfdp' only          |
| Root            | root            | GN      | Cust "Root"     | IsCentral  | (NodeName \"\") | @'NodeName' \\\"\\\"@ (graphs), @'NotCentral'@ (nodes) |            | 'Circo', 'Twopi' only |
| Rotate          | rotate          | G       | Integ           |            | 0               | @0@                  |            |                      |
| Rotation        | rotation        | G       | Dbl             |            | 0               | @0@                  |            | 'Sfdp' only, requires Graphviz >= 2.28.0 |
| SameHead        | samehead        | E       | Strng           |            | \"\"            | @\\\"\\\"@           |            | 'Dot' only           |
| SameTail        | sametail        | E       | Strng           |            | \"\"            | @\\\"\\\"@           |            | 'Dot' only           |
| SamplePoints    | samplepoints    | N       | Integ           |            |                 | @8@ (output), @20@ (overlap and image maps) |            |                      |
| Scale           | scale           | G       | Cust "DPoint"   |            |                 |                      |            | Not 'Dot', requires Graphviz >= 2.28.0 (>= 2.38.0 for anything except 'TwoPi') |
| SearchSize      | searchsize      | G       | Integ           |            | 30              | @30@                 |            | 'Dot' only           |
| Sep             | sep             | G       | Cust "DPoint"   |            | (DVal 4)        | @'DVal' 4@           |            | not 'Dot'            |
| Shape           | shape           | N       | Cust "Shape"    |            | Ellipse         | @'Ellipse'@          |            |                      |
| ShowBoxes       | showboxes       | ENG     | Integ           |            | 0               | @0@                  | @0@        | 'Dot' only; used for debugging by printing PostScript guide boxes |
| Sides           | sides           | N       | Integ           |            | 4               | @4@                  | @0@        |                      |
| Size            | size            | G       | Cust "GraphSize" |            |                 |                      |            |                      |
| Skew            | skew            | N       | Dbl             |            | 0.0             | @0.0@                | @-100.0@   |                      |
| Smoothing       | smoothing       | G       | Cust "SmoothType" |            | NoSmooth        | @'NoSmooth'@         |            | 'Sfdp' only          |
| SortV           | sortv           | GCN     | Cust "Word16"   |            | 0               | @0@                  | @0@        |                      |
| Splines         | splines         | G       | Cust "EdgeType" | SplineEdges |                 | @'SplineEdges'@ ('Dot'), @'LineEdges'@ (other) |            |                      |
| Start           | start           | G       | Cust "StartType" |            |                 | @'StartStyleSeed' 'RandomStyle' seed@ for some unknown fixed seed. |            | 'Fdp', 'Neato' only  |
| Style           | style           | ENCG    | Cust "[StyleItem]" |            |                 |                      |            |                      |
| StyleSheet      | stylesheet      | G       | Strng           |            | \"\"            | @\\\"\\\"@           |            | svg only             |
| TailURL         | tailURL tailhref | E       | EStrng          |            | \"\"            | @\\\"\\\"@           |            | svg, map only        |
| Tail_LP         | tail_lp         | E       | Cust "Point"    |            |                 |                      |            | write only, requires Graphviz >= 2.30.0 |
| TailClip        | tailclip        | E       | Bl              | True       | True            | @'True'@             |            |                      |
| TailLabel       | taillabel       | E       | Cust "Label"    |            | (StrLabel \"\") | @'StrLabel' \\\"\\\"@ |            |                      |
| TailPort        | tailport        | E       | Cust "PortPos"  |            | (CompassPoint CenterPoint) | @'CompassPoint' 'CenterPoint'@ |            |                      |
| TailTarget      | tailtarget      | E       | EStrng          |            | \"\"            | none                 |            | svg, map only        |
| TailTooltip     | tailtooltip     | E       | EStrng          |            | \"\"            | @\\\"\\\"@           |            | svg, cmap only       |
| Target          | target          | ENGC    | EStrng          |            | \"\"            | none                 |            | svg, map only        |
| Tooltip         | tooltip         | NEC     | EStrng          |            | \"\"            | @\\\"\\\"@           |            | svg, cmap only       |
| TrueColor       | truecolor       | G       | Bl              | True       |                 |                      |            | bitmap output only   |
| Vertices        | vertices        | N       | Cust "[Point]"  |            |                 |                      |            | write only           |
| ViewPort        | viewport        | G       | Cust "ViewPort" |            |                 | none                 |            |                      |
| VoroMargin      | voro_margin     | G       | Dbl             |            | 0.05            | @0.05@               | @0.0@      | not 'Dot'            |
| Weight          | weight          | E       | Cust "Number"   |            | (Int 1)         | @'Int' 1@            | @'Int' 0@ ('Dot'), @'Int' 1@ ('Neato','Fdp','Sfdp') | as of Graphviz 2.30: weights for dot need to be 'Int's |
| Width           | width           | N       | Dbl             |            | 0.75            | @0.75@               | @0.01@     |                      |
| XDotVersion     | xdotversion     | G       | Cust "Version"  |            |                 |                      |            | xdot only, requires Graphviz >= 2.34.0, equivalent to specifying version of xdot to be used |
| XLabel          | xlabel          | EN      | Cust "Label"    |            | (StrLabel \"\") | @'StrLabel' \\\"\\\"@ |            | requires Graphviz >= 2.29.0 |
| XLP             | xlp             | EN      | Cust "Point"    |            |                 |                      |            | write only, requires Graphviz >= 2.29.0 |

-}

unknownAttr :: Doc
unknownAttr = text "UnknownAttribute"

-- For testing purposes
attrs :: [Attribute]
attrs = take 10 $ drop 5 attributes

-- For testing purposes
attrs' :: Atts
attrs' = AS (text "Attribute") attrs

bool       :: a -> a -> Bool -> a
bool f t b = if b then t else f

dollar :: Doc
dollar = char '$'


-- Local Variables:
-- eval: (turn-on-orgtbl)
-- eval: (defun cell-quote (s) (concat "\"" s "\""))
-- eval: (defun cell-parens (s) (concat "(" s ")"))
-- eval: (defun cell-to-maybe (s) (if (string= "" s) "Nothing" (cell-parens (concat "Just " (cell-quote s)))))
-- eval: (defun cell-to-list (s) (concat "[" (mapconcat 'cell-quote (split-string s) ", ") "]"))
-- END:
