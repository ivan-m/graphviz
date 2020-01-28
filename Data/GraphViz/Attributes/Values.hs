{-# LANGUAGE CPP, OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}
{- |
   Module      : Data.GraphViz.Attributes.Values
   Description : Values for use with the Attribute data type
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   Defined to have smaller modules and thus faster compilation times.

 -}
module Data.GraphViz.Attributes.Values where

import qualified Data.GraphViz.Attributes.HTML     as Html
import           Data.GraphViz.Attributes.Internal
import           Data.GraphViz.Internal.State      (getLayerListSep,
                                                    getLayerSep,
                                                    setLayerListSep,
                                                    setLayerSep)
import           Data.GraphViz.Internal.Util       (bool, stringToInt)
import           Data.GraphViz.Parsing
import           Data.GraphViz.Printing

import           Data.List       (intercalate)
import           Data.Maybe      (isJust)
import           Data.Text.Lazy  (Text)
import qualified Data.Text.Lazy  as T
import           Data.Word       (Word16)
import           System.FilePath (searchPathSeparator, splitSearchPath)

#if !MIN_VERSION_base (4,13,0)
import Data.Monoid ((<>))
#endif

-- -----------------------------------------------------------------------------

{- |

   Some 'Attribute's (mainly label-like ones) take a 'String' argument
   that allows for extra escape codes.  This library doesn't do any
   extra checks or special parsing for these escape codes, but usage
   of 'EscString' rather than 'Text' indicates that the Graphviz
   tools will recognise these extra escape codes for these
   'Attribute's.

   The extra escape codes include (note that these are all Strings):

     [@\\N@] Replace with the name of the node (for Node 'Attribute's).

     [@\\G@] Replace with the name of the graph (for Node 'Attribute's)
             or the name of the graph or cluster, whichever is
             applicable (for Graph, Cluster and Edge 'Attribute's).

     [@\\E@] Replace with the name of the edge, formed by the two
             adjoining nodes and the edge type (for Edge 'Attribute's).

     [@\\T@] Replace with the name of the tail node (for Edge
             'Attribute's).

     [@\\H@] Replace with the name of the head node (for Edge
             'Attribute's).

     [@\\L@] Replace with the object's label (for all 'Attribute's).

   Also, if the 'Attribute' in question is 'Label', 'HeadLabel' or
   'TailLabel', then @\\n@, @\\l@ and @\\r@ split the label into lines
   centered, left-justified and right-justified respectively.

 -}
type EscString = Text

-- -----------------------------------------------------------------------------

-- | Should only have 2D points (i.e. created with 'createPoint').
data Rect = Rect Point Point
            deriving (Eq, Ord, Show, Read)

instance PrintDot Rect where
  unqtDot (Rect p1 p2) = printPoint2DUnqt p1 <> comma <> printPoint2DUnqt p2

  toDot = dquotes . unqtDot

  unqtListToDot = hsep . mapM unqtDot

instance ParseDot Rect where
  parseUnqt = uncurry Rect <$> commaSep' parsePoint2D parsePoint2D

  parse = quotedParse parseUnqt

  parseUnqtList = sepBy1 parseUnqt whitespace1

-- -----------------------------------------------------------------------------

-- | If 'Local', then sub-graphs that are clusters are given special
--   treatment.  'Global' and 'NoCluster' currently appear to be
--   identical and turn off the special cluster processing.
data ClusterMode = Local
                 | Global
                 | NoCluster
                 deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot ClusterMode where
  unqtDot Local     = text "local"
  unqtDot Global    = text "global"
  unqtDot NoCluster = text "none"

instance ParseDot ClusterMode where
  parseUnqt = oneOf [ stringRep Local "local"
                    , stringRep Global "global"
                    , stringRep NoCluster "none"
                    ]

-- -----------------------------------------------------------------------------

-- | Specify where to place arrow heads on an edge.
data DirType = Forward -- ^ Draw a directed edge with an arrow to the
                       --   node it's pointing go.
             | Back    -- ^ Draw a reverse directed edge with an arrow
                       --   to the node it's coming from.
             | Both    -- ^ Draw arrows on both ends of the edge.
             | NoDir   -- ^ Draw an undirected edge.
             deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot DirType where
  unqtDot Forward = text "forward"
  unqtDot Back    = text "back"
  unqtDot Both    = text "both"
  unqtDot NoDir   = text "none"

instance ParseDot DirType where
  parseUnqt = oneOf [ stringRep Forward "forward"
                    , stringRep Back "back"
                    , stringRep Both "both"
                    , stringRep NoDir "none"
                    ]

-- -----------------------------------------------------------------------------

-- | Only when @mode == 'IpSep'@.
data DEConstraints = EdgeConstraints
                   | NoConstraints
                   | HierConstraints
                   deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot DEConstraints where
  unqtDot EdgeConstraints = unqtDot True
  unqtDot NoConstraints   = unqtDot False
  unqtDot HierConstraints = text "hier"

instance ParseDot DEConstraints where
  parseUnqt = fmap (bool NoConstraints EdgeConstraints) parse
              `onFail`
              stringRep HierConstraints "hier"

-- -----------------------------------------------------------------------------

-- | Either a 'Double' or a (2D) 'Point' (i.e. created with
--   'createPoint').
--
--   Whilst it is possible to create a 'Point' value with either a
--   third co-ordinate or a forced position, these are ignored for
--   printing/parsing.
--
--   An optional prefix of @\'+\'@ is allowed when parsing.
data DPoint = DVal Double
            | PVal Point
            deriving (Eq, Ord, Show, Read)

instance PrintDot DPoint where
  unqtDot (DVal d) = unqtDot d
  unqtDot (PVal p) = printPoint2DUnqt p

  toDot (DVal d) = toDot d
  toDot (PVal p) = printPoint2D p

instance ParseDot DPoint where
  parseUnqt = optional (character '+')
              *> oneOf [ PVal <$> parsePoint2D
                       , DVal <$> parseUnqt
                       ]

  parse = quotedParse parseUnqt -- A `+' would need to be quoted.
          `onFail`
          fmap DVal (parseSignedFloat False) -- Don't use parseUnqt!

-- -----------------------------------------------------------------------------

-- | The mapping used for 'FontName' values in SVG output.
--
--   More information can be found at <http://www.graphviz.org/doc/fontfaq.txt>.
data SVGFontNames = SvgNames        -- ^ Use the legal generic SVG font names.
                  | PostScriptNames -- ^ Use PostScript font names.
                  | FontConfigNames -- ^ Use fontconfig font conventions.
                  deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot SVGFontNames where
  unqtDot SvgNames        = text "svg"
  unqtDot PostScriptNames = text "ps"
  unqtDot FontConfigNames = text "gd"

instance ParseDot SVGFontNames where
  parseUnqt = oneOf [ stringRep SvgNames "svg"
                    , stringRep PostScriptNames "ps"
                    , stringRep FontConfigNames "gd"
                    ]

  parse = stringRep SvgNames "\"\""
          `onFail`
          optionalQuoted parseUnqt

-- -----------------------------------------------------------------------------

-- | Maximum width and height of drawing in inches.
data GraphSize = GSize { width       :: Double
                         -- | If @Nothing@, then the height is the
                         --   same as the width.
                       , height      :: Maybe Double
                         -- | If drawing is smaller than specified
                         --   size, this value determines whether it
                         --   is scaled up.
                       , desiredSize :: Bool
                       }
               deriving (Eq, Ord, Show, Read)

instance PrintDot GraphSize where
  unqtDot (GSize w mh ds) = bool id (<> char '!') ds
                            . maybe id (\h -> (<> unqtDot h) . (<> comma)) mh
                            $ unqtDot w

  toDot (GSize w Nothing False) = toDot w
  toDot gs                      = dquotes $ unqtDot gs

instance ParseDot GraphSize where
  parseUnqt = GSize <$> parseUnqt
                    <*> optional (parseComma *> whitespace *> parseUnqt)
                    <*> (isJust <$> optional (character '!'))

  parse = quotedParse parseUnqt
          `onFail`
          fmap (\ w -> GSize w Nothing False) (parseSignedFloat False)

-- -----------------------------------------------------------------------------

-- | For 'Neato' unless indicated otherwise.
data ModeType = Major
              | KK
              | Hier
              | IpSep
              | SpringMode -- ^ For 'Sfdp', requires Graphviz >= 2.32.0.
              | MaxEnt     -- ^ For 'Sfdp', requires Graphviz >= 2.32.0.
              deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot ModeType where
  unqtDot Major      = text "major"
  unqtDot KK         = text "KK"
  unqtDot Hier       = text "hier"
  unqtDot IpSep      = text "ipsep"
  unqtDot SpringMode = text "spring"
  unqtDot MaxEnt     = text "maxent"

instance ParseDot ModeType where
  parseUnqt = oneOf [ stringRep Major "major"
                    , stringRep KK "KK"
                    , stringRep Hier "hier"
                    , stringRep IpSep "ipsep"
                    , stringRep SpringMode "spring"
                    , stringRep MaxEnt "maxent"
                    ]

-- -----------------------------------------------------------------------------

data Model = ShortPath
           | SubSet
           | Circuit
           | MDS
           deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot Model where
  unqtDot ShortPath = text "shortpath"
  unqtDot SubSet    = text "subset"
  unqtDot Circuit   = text "circuit"
  unqtDot MDS       = text "mds"

instance ParseDot Model where
  parseUnqt = oneOf [ stringRep ShortPath "shortpath"
                    , stringRep SubSet "subset"
                    , stringRep Circuit "circuit"
                    , stringRep MDS "mds"
                    ]

-- -----------------------------------------------------------------------------

data Label = StrLabel EscString
           | HtmlLabel Html.Label -- ^ If 'PlainText' is used, the
                                  --   'Html.Label' value is the entire
                                  --   \"shape\"; if anything else
                                  --   except 'PointShape' is used then
                                  --   the 'Html.Label' is embedded
                                  --   within the shape.
           | RecordLabel RecordFields -- ^ For nodes only; requires
                                      --   either 'Record' or
                                      --   'MRecord' as the shape.
           deriving (Eq, Ord, Show, Read)

instance PrintDot Label where
  unqtDot (StrLabel s)     = unqtDot s
  unqtDot (HtmlLabel h)    = angled $ unqtDot h
  unqtDot (RecordLabel fs) = unqtDot fs

  toDot (StrLabel s)     = toDot s
  toDot h@HtmlLabel{}    = unqtDot h
  toDot (RecordLabel fs) = toDot fs

instance ParseDot Label where
  -- Don't have to worry about being able to tell the difference
  -- between an HtmlLabel and a RecordLabel starting with a PortPos,
  -- since the latter will be in quotes and the former won't.

  parseUnqt = oneOf [ HtmlLabel <$> parseAngled parseUnqt
                    , RecordLabel <$> parseUnqt
                    , StrLabel <$> parseUnqt
                    ]

  parse = oneOf [ HtmlLabel <$> parseAngled parse
                , RecordLabel <$> parse
                , StrLabel <$> parse
                ]

-- -----------------------------------------------------------------------------

-- | A RecordFields value should never be empty.
type RecordFields = [RecordField]

-- | Specifies the sub-values of a record-based label.  By default,
--   the cells are laid out horizontally; use 'FlipFields' to change
--   the orientation of the fields (can be applied recursively).  To
--   change the default orientation, use 'RankDir'.
data RecordField = LabelledTarget PortName EscString
                 | PortName PortName -- ^ Will result in no label for
                                     --   that cell.
                 | FieldLabel EscString
                 | FlipFields RecordFields
                 deriving (Eq, Ord, Show, Read)

instance PrintDot RecordField where
  -- Have to use 'printPortName' to add the @\'<\'@ and @\'>\'@.
  unqtDot (LabelledTarget t s) = printPortName t <+> unqtRecordString s
  unqtDot (PortName t)         = printPortName t
  unqtDot (FieldLabel s)       = unqtRecordString s
  unqtDot (FlipFields rs)      = braces $ unqtDot rs

  toDot (FieldLabel s) = printEscaped recordEscChars s
  toDot rf             = dquotes $ unqtDot rf

  unqtListToDot [f] = unqtDot f
  unqtListToDot fs  = hcat . punctuate (char '|') $ mapM unqtDot fs

  listToDot [f] = toDot f
  listToDot fs  = dquotes $ unqtListToDot fs

instance ParseDot RecordField where
  parseUnqt = (liftA2 maybe PortName LabelledTarget
                <$> (PN <$> parseAngled parseRecord)
                <*> optional (whitespace1 *> parseRecord)
              )
              `onFail`
              fmap FieldLabel parseRecord
              `onFail`
              fmap FlipFields (parseBraced parseUnqt)
              `onFail`
              fail "Unable to parse RecordField"

  parse = quotedParse parseUnqt

  parseUnqtList = wrapWhitespace $ sepBy1 parseUnqt (wrapWhitespace $ character '|')

  -- Note: a singleton unquoted 'FieldLabel' is /not/ valid, as it
  -- will cause parsing problems for other 'Label' types.
  parseList = do rfs <- quotedParse parseUnqtList
                 if validRFs rfs
                   then return rfs
                   else fail "This is a StrLabel, not a RecordLabel"
    where
      validRFs [FieldLabel str] = T.any (`elem` recordEscChars) str
      validRFs _                = True

-- | Print a 'PortName' value as expected within a Record data
--   structure.
printPortName :: PortName -> DotCode
printPortName = angled . unqtRecordString . portName

parseRecord :: Parse Text
parseRecord = parseEscaped False recordEscChars []

unqtRecordString :: Text -> DotCode
unqtRecordString = unqtEscaped recordEscChars

recordEscChars :: [Char]
recordEscChars = ['{', '}', '|', ' ', '<', '>']

-- -----------------------------------------------------------------------------

-- | How to treat a node whose name is of the form \"@|edgelabel|*@\"
--   as a special node representing an edge label.
data LabelScheme = NotEdgeLabel        -- ^ No effect
                 | CloseToCenter       -- ^ Make node close to center of neighbor
                 | CloseToOldCenter    -- ^ Make node close to old center of neighbor
                 | RemoveAndStraighten -- ^ Use a two-step process.
                 deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot LabelScheme where
  unqtDot NotEdgeLabel        = int 0
  unqtDot CloseToCenter       = int 1
  unqtDot CloseToOldCenter    = int 2
  unqtDot RemoveAndStraighten = int 3

instance ParseDot LabelScheme where
  -- Use string-based parsing rather than parsing an integer just to make it easier
  parseUnqt = stringValue [ ("0", NotEdgeLabel)
                          , ("1", CloseToCenter)
                          , ("2", CloseToOldCenter)
                          , ("3", RemoveAndStraighten)
                          ]

-- -----------------------------------------------------------------------------

data Point = Point { xCoord   :: Double
                   , yCoord   :: Double
                      -- | Can only be 'Just' for @'Dim' 3@ or greater.
                   , zCoord   :: Maybe Double
                     -- | Input to Graphviz only: specify that the
                     --   node position should not change.
                   , forcePos :: Bool
                   }
           deriving (Eq, Ord, Show, Read)

-- | Create a point with only @x@ and @y@ values.
createPoint     :: Double -> Double -> Point
createPoint x y = Point x y Nothing False

printPoint2DUnqt   :: Point -> DotCode
printPoint2DUnqt p = commaDel (xCoord p) (yCoord p)

printPoint2D :: Point -> DotCode
printPoint2D = dquotes . printPoint2DUnqt

parsePoint2D :: Parse Point
parsePoint2D = uncurry createPoint <$> commaSepUnqt

instance PrintDot Point where
  unqtDot (Point x y mz frs) = bool id (<> char '!') frs
                               . maybe id (\ z -> (<> unqtDot z) . (<> comma)) mz
                               $ commaDel x y

  toDot = dquotes . unqtDot

  unqtListToDot = hsep . mapM unqtDot

  listToDot = dquotes . unqtListToDot

instance ParseDot Point where
  parseUnqt = uncurry Point
                <$> commaSepUnqt
                <*> optional (parseComma *> parseUnqt)
                <*> (isJust <$> optional (character '!'))

  parse = quotedParse parseUnqt

  parseUnqtList = sepBy1 parseUnqt whitespace1

-- -----------------------------------------------------------------------------

-- | How to deal with node overlaps.
--
--   Defaults to 'KeepOverlaps' /except/ for 'Fdp' and 'Sfdp'.
--
--   The ability to specify the number of tries for 'Fdp''s initial
--   force-directed technique is /not/ supported (by default, 'Fdp' uses
--   @9@ passes of its in-built technique, and then @'PrismOverlap'
--   Nothing@).
--
--   For 'Sfdp', the default is @'PrismOverlap' (Just 0)@.
data Overlap = KeepOverlaps
             | ScaleOverlaps -- ^ Remove overlaps by uniformly scaling in x and y.
             | ScaleXYOverlaps -- ^ Remove overlaps by separately scaling x and y.
             | PrismOverlap (Maybe Word16) -- ^ Requires the Prism
                                           --   library to be
                                           --   available (if not,
                                           --   this is equivalent to
                                           --   'VoronoiOverlap'). @'Nothing'@
                                           --   is equivalent to
                                           --   @'Just' 1000@.
                                           --   Influenced by
                                           --   'OverlapScaling'.
             | VoronoiOverlap -- ^ Requires Graphviz >= 2.30.0.
             | CompressOverlap -- ^ Scale layout down as much as
                               --   possible without introducing
                               --   overlaps, assuming none to begin
                               --   with.
             | VpscOverlap -- ^ Uses quadratic optimization to
                           --   minimize node displacement.
             | IpsepOverlap -- ^ Only when @mode == 'IpSep'@
             deriving (Eq, Ord, Show, Read)

instance PrintDot Overlap where
  unqtDot KeepOverlaps     = unqtDot True
  unqtDot ScaleOverlaps    = text "scale"
  unqtDot ScaleXYOverlaps  = text "scalexy"
  unqtDot (PrismOverlap i) = maybe id (flip (<>) . unqtDot) i $ text "prism"
  unqtDot VoronoiOverlap   = text "voronoi"
  unqtDot CompressOverlap  = text "compress"
  unqtDot VpscOverlap      = text "vpsc"
  unqtDot IpsepOverlap     = text "ipsep"

-- | Note that @overlap=false@ defaults to @'PrismOverlap' Nothing@,
--   but if the Prism library isn't available then it is equivalent to
--   'VoronoiOverlap'.
instance ParseDot Overlap where
  parseUnqt = oneOf [ stringRep KeepOverlaps "true"
                    , stringRep ScaleXYOverlaps "scalexy"
                    , stringRep ScaleOverlaps "scale"
                    , string "prism" *> fmap PrismOverlap (optional parse)
                    , stringRep (PrismOverlap Nothing) "false"
                    , stringRep VoronoiOverlap "voronoi"
                    , stringRep CompressOverlap "compress"
                    , stringRep VpscOverlap "vpsc"
                    , stringRep IpsepOverlap "ipsep"
                    ]

-- -----------------------------------------------------------------------------

newtype LayerSep = LSep Text
                 deriving (Eq, Ord, Show, Read)

instance PrintDot LayerSep where
  unqtDot (LSep ls) = setLayerSep (T.unpack ls) *> unqtDot ls

  toDot (LSep ls) = setLayerSep (T.unpack ls) *> toDot ls

instance ParseDot LayerSep where
  parseUnqt = do ls <- parseUnqt
                 setLayerSep $ T.unpack ls
                 return $ LSep ls

  parse = do ls <- parse
             setLayerSep $ T.unpack ls
             return $ LSep ls

newtype LayerListSep = LLSep Text
                     deriving (Eq, Ord, Show, Read)

instance PrintDot LayerListSep where
  unqtDot (LLSep ls) = setLayerListSep (T.unpack ls) *> unqtDot ls

  toDot (LLSep ls) = setLayerListSep (T.unpack ls) *> toDot ls

instance ParseDot LayerListSep where
  parseUnqt = do ls <- parseUnqt
                 setLayerListSep $ T.unpack ls
                 return $ LLSep ls

  parse = do ls <- parse
             setLayerListSep $ T.unpack ls
             return $ LLSep ls

type LayerRange = [LayerRangeElem]

data LayerRangeElem = LRID LayerID
                    | LRS LayerID LayerID
                    deriving (Eq, Ord, Show, Read)

instance PrintDot LayerRangeElem where
  unqtDot (LRID lid)    = unqtDot lid
  unqtDot (LRS id1 id2) = do ls <- getLayerSep
                             let s = unqtDot $ head ls
                             unqtDot id1 <> s <> unqtDot id2

  toDot (LRID lid) = toDot lid
  toDot lrs        = dquotes $ unqtDot lrs

  unqtListToDot lr = do lls <- getLayerListSep
                        let s = unqtDot $ head lls
                        hcat . punctuate s $ mapM unqtDot lr

  listToDot [lre] = toDot lre
  listToDot lrs   = dquotes $ unqtListToDot lrs

instance ParseDot LayerRangeElem where
  parseUnqt = ignoreSep LRS parseUnqt parseLayerSep parseUnqt
              `onFail`
              fmap LRID parseUnqt

  parse = quotedParse (ignoreSep LRS parseUnqt parseLayerSep parseUnqt)
          `onFail`
          fmap LRID parse

  parseUnqtList = sepBy parseUnqt parseLayerListSep

  parseList = quotedParse parseUnqtList
              `onFail`
              fmap ((:[]) . LRID) parse

parseLayerSep :: Parse ()
parseLayerSep = do ls <- getLayerSep
                   many1Satisfy (`elem` ls) *> return ()

parseLayerName :: Parse Text
parseLayerName = parseEscaped False [] =<< liftA2 (++) getLayerSep getLayerListSep

parseLayerName' :: Parse Text
parseLayerName' = stringBlock
                  `onFail`
                  quotedParse parseLayerName

parseLayerListSep :: Parse ()
parseLayerListSep = do lls <- getLayerListSep
                       many1Satisfy (`elem` lls) *> return ()

-- | You should not have any layer separator characters for the
--   'LRName' option, as they won't be parseable.
data LayerID = AllLayers
             | LRInt Int
             | LRName Text -- ^ Should not be a number or @"all"@.
             deriving (Eq, Ord, Show, Read)

instance PrintDot LayerID where
  unqtDot AllLayers   = text "all"
  unqtDot (LRInt n)   = unqtDot n
  unqtDot (LRName nm) = unqtDot nm

  toDot (LRName nm) = toDot nm
  -- Other two don't need quotes
  toDot li          = unqtDot li

  unqtListToDot ll = do ls <- getLayerSep
                        let s = unqtDot $ head ls
                        hcat . punctuate s $ mapM unqtDot ll

  listToDot [l] = toDot l
  -- Might not need quotes, but probably will.  Can't tell either
  -- way since we don't know what the separator character will be.
  listToDot ll  = dquotes $ unqtDot ll

instance ParseDot LayerID where
  parseUnqt = checkLayerName <$> parseLayerName -- tests for Int and All

  parse = oneOf [ checkLayerName <$> parseLayerName'
                , LRInt <$> parse -- Mainly for unquoted case.
                ]

checkLayerName     :: Text -> LayerID
checkLayerName str = maybe checkAll LRInt $ stringToInt str
  where
    checkAll = if T.toLower str == "all"
               then AllLayers
               else LRName str

-- Remember: this /must/ be a newtype as we can't use arbitrary
-- LayerID values!

-- | A list of layer names.  The names should all be unique 'LRName'
--   values, and when printed will use an arbitrary character from
--   'defLayerSep'.  The values in the list are implicitly numbered
--   @1, 2, ...@.
newtype LayerList = LL [LayerID]
                  deriving (Eq, Ord, Show, Read)

instance PrintDot LayerList where
  unqtDot (LL ll) = unqtDot ll

  toDot (LL ll) = toDot ll

instance ParseDot LayerList where
  parseUnqt = LL <$> sepBy1 parseUnqt parseLayerSep

  parse = quotedParse parseUnqt
          `onFail`
          fmap (LL . (:[]) . LRName) stringBlock
          `onFail`
          quotedParse (stringRep (LL []) "")

-- -----------------------------------------------------------------------------

data Order = OutEdges -- ^ Draw outgoing edges in order specified.
           | InEdges  -- ^ Draw incoming edges in order specified.
           deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot Order where
  unqtDot OutEdges = text "out"
  unqtDot InEdges  = text "in"

instance ParseDot Order where
  parseUnqt = oneOf [ stringRep OutEdges "out"
                    , stringRep InEdges  "in"
                    ]

-- -----------------------------------------------------------------------------

data OutputMode = BreadthFirst | NodesFirst | EdgesFirst
                deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot OutputMode where
  unqtDot BreadthFirst = text "breadthfirst"
  unqtDot NodesFirst   = text "nodesfirst"
  unqtDot EdgesFirst   = text "edgesfirst"

instance ParseDot OutputMode where
  parseUnqt = oneOf [ stringRep BreadthFirst "breadthfirst"
                    , stringRep NodesFirst "nodesfirst"
                    , stringRep EdgesFirst "edgesfirst"
                    ]

-- -----------------------------------------------------------------------------

data Pack = DoPack
          | DontPack
          | PackMargin Int -- ^ If non-negative, then packs; otherwise doesn't.
          deriving (Eq, Ord, Show, Read)

instance PrintDot Pack where
  unqtDot DoPack         = unqtDot True
  unqtDot DontPack       = unqtDot False
  unqtDot (PackMargin m) = unqtDot m

instance ParseDot Pack where
  -- What happens if it parses 0?  It's non-negative, but parses as False
  parseUnqt = oneOf [ PackMargin <$> parseUnqt
                    , bool DontPack DoPack <$> onlyBool
                    ]

-- -----------------------------------------------------------------------------

data PackMode = PackNode
              | PackClust
              | PackGraph
              | PackArray Bool Bool (Maybe Int) -- ^ Sort by cols, sort
                                                -- by user, number of
                                                -- rows/cols
              deriving (Eq, Ord, Show, Read)

instance PrintDot PackMode where
  unqtDot PackNode           = text "node"
  unqtDot PackClust          = text "clust"
  unqtDot PackGraph          = text "graph"
  unqtDot (PackArray c u mi) = addNum . isU . isC . isUnder
                               $ text "array"
    where
      addNum = maybe id (flip (<>) . unqtDot) mi
      isUnder = if c || u
                then (<> char '_')
                else id
      isC = if c
            then (<> char 'c')
            else id
      isU = if u
            then (<> char 'u')
            else id

instance ParseDot PackMode where
  parseUnqt = oneOf [ stringRep PackNode "node"
                    , stringRep PackClust "clust"
                    , stringRep PackGraph "graph"
                    , do string "array"
                         mcu <- optional $ character '_' *> many1 (satisfy isCU)
                         let c = hasCharacter mcu 'c'
                             u = hasCharacter mcu 'u'
                         mi <- optional parseUnqt
                         return $ PackArray c u mi
                    ]
    where
      hasCharacter ms c = maybe False (elem c) ms
      -- Also checks and removes quote characters
      isCU = (`elem` ['c', 'u'])

-- -----------------------------------------------------------------------------

data Pos = PointPos Point
         | SplinePos [Spline]
         deriving (Eq, Ord, Show, Read)

instance PrintDot Pos where
  unqtDot (PointPos p)   = unqtDot p
  unqtDot (SplinePos ss) = unqtDot ss

  toDot (PointPos p)   = toDot p
  toDot (SplinePos ss) = toDot ss

instance ParseDot Pos where
  -- Have to be careful with this: if we try to parse points first,
  -- then a spline with no start and end points will erroneously get
  -- parsed as a point and then the parser will crash as it expects a
  -- closing quote character...
  parseUnqt = do splns <- parseUnqt
                 case splns of
                   [Spline Nothing Nothing [p]] -> return $ PointPos p
                   _                            -> return $ SplinePos splns

  parse = quotedParse parseUnqt

-- -----------------------------------------------------------------------------

-- | Controls how (and if) edges are represented.
--
--   For 'Dot', the default is 'SplineEdges'; for all other layouts
--   the default is 'LineEdges'.
data EdgeType = SplineEdges -- ^ Except for 'Dot', requires
                            --   non-overlapping nodes (see
                            --   'Overlap').
              | LineEdges
              | NoEdges
              | PolyLine
              | Ortho -- ^ Does not handle ports or edge labels in 'Dot'.
              | Curved -- ^ Requires Graphviz >= 2.30.0.
              | CompoundEdge -- ^ 'Fdp' only
              deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot EdgeType where
  unqtDot SplineEdges  = text "spline"
  unqtDot LineEdges    = text "line"
  unqtDot NoEdges      = empty
  unqtDot PolyLine     = text "polyline"
  unqtDot Ortho        = text "ortho"
  unqtDot Curved       = text "curved"
  unqtDot CompoundEdge = text "compound"

  toDot NoEdges = dquotes empty
  toDot et      = unqtDot et

instance ParseDot EdgeType where
  -- Can't parse NoEdges without quotes.
  parseUnqt = oneOf [ bool LineEdges SplineEdges <$> parse
                    , stringRep SplineEdges "spline"
                    , stringRep LineEdges "line"
                    , stringRep NoEdges "none"
                    , stringRep PolyLine "polyline"
                    , stringRep Ortho "ortho"
                    , stringRep Curved "curved"
                    , stringRep CompoundEdge "compound"
                    ]

  parse = stringRep NoEdges "\"\""
          `onFail`
          optionalQuoted parseUnqt

-- -----------------------------------------------------------------------------

-- | Upper-case first character is major order;
--   lower-case second character is minor order.
data PageDir = Bl | Br | Tl | Tr | Rb | Rt | Lb | Lt
             deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot PageDir where
  unqtDot Bl = text "BL"
  unqtDot Br = text "BR"
  unqtDot Tl = text "TL"
  unqtDot Tr = text "TR"
  unqtDot Rb = text "RB"
  unqtDot Rt = text "RT"
  unqtDot Lb = text "LB"
  unqtDot Lt = text "LT"

instance ParseDot PageDir where
  parseUnqt = stringValue [ ("BL", Bl)
                          , ("BR", Br)
                          , ("TL", Tl)
                          , ("TR", Tr)
                          , ("RB", Rb)
                          , ("RT", Rt)
                          , ("LB", Lb)
                          , ("LT", Lt)
                          ]

-- -----------------------------------------------------------------------------

-- | The number of points in the list must be equivalent to 1 mod 3;
--   note that this is not checked.
data Spline = Spline { endPoint     :: Maybe Point
                     , startPoint   :: Maybe Point
                     , splinePoints :: [Point]
                     }
            deriving (Eq, Ord, Show, Read)

instance PrintDot Spline where
  unqtDot (Spline me ms ps) = addE . addS
                             . hsep
                             $ mapM unqtDot ps
    where
      addP t = maybe id ((<+>) . commaDel t)
      addS = addP 's' ms
      addE = addP 'e' me

  toDot = dquotes . unqtDot

  unqtListToDot = hcat . punctuate semi . mapM unqtDot

  listToDot = dquotes . unqtListToDot

instance ParseDot Spline where
  parseUnqt = Spline <$> parseP 'e' <*> parseP 's'
                     <*> sepBy1 parseUnqt whitespace1
      where
        parseP t = optional (character t *> parseComma *> parseUnqt <* whitespace1)

  parse = quotedParse parseUnqt

  parseUnqtList = sepBy1 parseUnqt (character ';')

-- -----------------------------------------------------------------------------

data QuadType = NormalQT
              | FastQT
              | NoQT
              deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot QuadType where
  unqtDot NormalQT = text "normal"
  unqtDot FastQT   = text "fast"
  unqtDot NoQT     = text "none"

instance ParseDot QuadType where
  -- Have to take into account the slightly different interpretation
  -- of Bool used as an option for parsing QuadType
  parseUnqt = oneOf [ stringRep NormalQT "normal"
                    , stringRep FastQT "fast"
                    , stringRep NoQT "none"
                    , character '2' *> return FastQT -- weird bool
                    , bool NoQT NormalQT <$> parse
                    ]

-- -----------------------------------------------------------------------------

-- | Specify the root node either as a Node attribute or a Graph attribute.
data Root = IsCentral     -- ^ For Nodes only
          | NotCentral    -- ^ For Nodes only
          | NodeName Text -- ^ For Graphs only
          deriving (Eq, Ord, Show, Read)

instance PrintDot Root where
  unqtDot IsCentral    = unqtDot True
  unqtDot NotCentral   = unqtDot False
  unqtDot (NodeName n) = unqtDot n

  toDot (NodeName n) = toDot n
  toDot r            = unqtDot r

instance ParseDot Root where
  parseUnqt = fmap (bool NotCentral IsCentral) onlyBool
              `onFail`
              fmap NodeName parseUnqt

  parse = optionalQuoted (bool NotCentral IsCentral <$> onlyBool)
          `onFail`
          fmap NodeName parse

-- -----------------------------------------------------------------------------

data RankType = SameRank
              | MinRank
              | SourceRank
              | MaxRank
              | SinkRank
              deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot RankType where
  unqtDot SameRank   = text "same"
  unqtDot MinRank    = text "min"
  unqtDot SourceRank = text "source"
  unqtDot MaxRank    = text "max"
  unqtDot SinkRank   = text "sink"

instance ParseDot RankType where
  parseUnqt = stringValue [ ("same", SameRank)
                          , ("min", MinRank)
                          , ("source", SourceRank)
                          , ("max", MaxRank)
                          , ("sink", SinkRank)
                          ]

-- -----------------------------------------------------------------------------

data RankDir = FromTop
             | FromLeft
             | FromBottom
             | FromRight
             deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot RankDir where
  unqtDot FromTop    = text "TB"
  unqtDot FromLeft   = text "LR"
  unqtDot FromBottom = text "BT"
  unqtDot FromRight  = text "RL"

instance ParseDot RankDir where
  parseUnqt = oneOf [ stringRep FromTop "TB"
                    , stringRep FromLeft "LR"
                    , stringRep FromBottom "BT"
                    , stringRep FromRight "RL"
                    ]

-- -----------------------------------------------------------------------------

-- | Geometries of shapes are affected by the attributes 'Regular',
--   'Peripheries' and 'Orientation'.
data Shape
    = BoxShape -- ^ Has synonyms of /rect/ and /rectangle/.
    | Polygon  -- ^ Also affected by 'Sides', 'Skew' and 'Distortion'.
    | Ellipse  -- ^ Has synonym of /oval/.
    | Circle
    | PointShape -- ^ Only affected by 'Peripheries', 'Width' and
                 --   'Height'.
    | Egg
    | Triangle
    | PlainText -- ^ Has synonym of /none/.  Recommended for
                --   'HtmlLabel's.
    | DiamondShape
    | Trapezium
    | Parallelogram
    | House
    | Pentagon
    | Hexagon
    | Septagon
    | Octagon
    | DoubleCircle
    | DoubleOctagon
    | TripleOctagon
    | InvTriangle
    | InvTrapezium
    | InvHouse
    | MDiamond
    | MSquare
    | MCircle
    | Square
    | Star      -- ^ Requires Graphviz >= 2.32.0.
    | Underline -- ^ Requires Graphviz >= 2.36.0.
    | Note
    | Tab
    | Folder
    | Box3D
    | Component
    | Promoter         -- ^ Requires Graphviz >= 2.30.0.
    | CDS              -- ^ Requires Graphviz >= 2.30.0.
    | Terminator       -- ^ Requires Graphviz >= 2.30.0.
    | UTR              -- ^ Requires Graphviz >= 2.30.0.
    | PrimerSite       -- ^ Requires Graphviz >= 2.30.0.
    | RestrictionSite  -- ^ Requires Graphviz >= 2.30.0.
    | FivePovOverhang  -- ^ Requires Graphviz >= 2.30.0.
    | ThreePovOverhang -- ^ Requires Graphviz >= 2.30.0.
    | NoOverhang       -- ^ Requires Graphviz >= 2.30.0.
    | Assembly         -- ^ Requires Graphviz >= 2.30.0.
    | Signature        -- ^ Requires Graphviz >= 2.30.0.
    | Insulator        -- ^ Requires Graphviz >= 2.30.0.
    | Ribosite         -- ^ Requires Graphviz >= 2.30.0.
    | RNAStab          -- ^ Requires Graphviz >= 2.30.0.
    | ProteaseSite     -- ^ Requires Graphviz >= 2.30.0.
    | ProteinStab      -- ^ Requires Graphviz >= 2.30.0.
    | RPromoter        -- ^ Requires Graphviz >= 2.30.0.
    | RArrow           -- ^ Requires Graphviz >= 2.30.0.
    | LArrow           -- ^ Requires Graphviz >= 2.30.0.
    | LPromoter        -- ^ Requires Graphviz >= 2.30.0.
    | Record  -- ^ Must specify the record shape with a 'Label'.
    | MRecord -- ^ Must specify the record shape with a 'Label'.
    deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot Shape where
  unqtDot BoxShape         = text "box"
  unqtDot Polygon          = text "polygon"
  unqtDot Ellipse          = text "ellipse"
  unqtDot Circle           = text "circle"
  unqtDot PointShape       = text "point"
  unqtDot Egg              = text "egg"
  unqtDot Triangle         = text "triangle"
  unqtDot PlainText        = text "plaintext"
  unqtDot DiamondShape     = text "diamond"
  unqtDot Trapezium        = text "trapezium"
  unqtDot Parallelogram    = text "parallelogram"
  unqtDot House            = text "house"
  unqtDot Pentagon         = text "pentagon"
  unqtDot Hexagon          = text "hexagon"
  unqtDot Septagon         = text "septagon"
  unqtDot Octagon          = text "octagon"
  unqtDot DoubleCircle     = text "doublecircle"
  unqtDot DoubleOctagon    = text "doubleoctagon"
  unqtDot TripleOctagon    = text "tripleoctagon"
  unqtDot InvTriangle      = text "invtriangle"
  unqtDot InvTrapezium     = text "invtrapezium"
  unqtDot InvHouse         = text "invhouse"
  unqtDot MDiamond         = text "Mdiamond"
  unqtDot MSquare          = text "Msquare"
  unqtDot MCircle          = text "Mcircle"
  unqtDot Square           = text "square"
  unqtDot Star             = text "star"
  unqtDot Underline        = text "underline"
  unqtDot Note             = text "note"
  unqtDot Tab              = text "tab"
  unqtDot Folder           = text "folder"
  unqtDot Box3D            = text "box3d"
  unqtDot Component        = text "component"
  unqtDot Promoter         = text "promoter"
  unqtDot CDS              = text "cds"
  unqtDot Terminator       = text "terminator"
  unqtDot UTR              = text "utr"
  unqtDot PrimerSite       = text "primersite"
  unqtDot RestrictionSite  = text "restrictionsite"
  unqtDot FivePovOverhang  = text "fivepovoverhang"
  unqtDot ThreePovOverhang = text "threepovoverhang"
  unqtDot NoOverhang       = text "nooverhang"
  unqtDot Assembly         = text "assembly"
  unqtDot Signature        = text "signature"
  unqtDot Insulator        = text "insulator"
  unqtDot Ribosite         = text "ribosite"
  unqtDot RNAStab          = text "rnastab"
  unqtDot ProteaseSite     = text "proteasesite"
  unqtDot ProteinStab      = text "proteinstab"
  unqtDot RPromoter        = text "rpromoter"
  unqtDot RArrow           = text "rarrow"
  unqtDot LArrow           = text "larrow"
  unqtDot LPromoter        = text "lpromoter"
  unqtDot Record           = text "record"
  unqtDot MRecord          = text "Mrecord"

instance ParseDot Shape where
  parseUnqt = stringValue [ ("box3d", Box3D)
                          , ("box", BoxShape)
                          , ("rectangle", BoxShape)
                          , ("rect", BoxShape)
                          , ("polygon", Polygon)
                          , ("ellipse", Ellipse)
                          , ("oval", Ellipse)
                          , ("circle", Circle)
                          , ("point", PointShape)
                          , ("egg", Egg)
                          , ("triangle", Triangle)
                          , ("plaintext", PlainText)
                          , ("none", PlainText)
                          , ("diamond", DiamondShape)
                          , ("trapezium", Trapezium)
                          , ("parallelogram", Parallelogram)
                          , ("house", House)
                          , ("pentagon", Pentagon)
                          , ("hexagon", Hexagon)
                          , ("septagon", Septagon)
                          , ("octagon", Octagon)
                          , ("doublecircle", DoubleCircle)
                          , ("doubleoctagon", DoubleOctagon)
                          , ("tripleoctagon", TripleOctagon)
                          , ("invtriangle", InvTriangle)
                          , ("invtrapezium", InvTrapezium)
                          , ("invhouse", InvHouse)
                          , ("Mdiamond", MDiamond)
                          , ("Msquare", MSquare)
                          , ("Mcircle", MCircle)
                          , ("square", Square)
                          , ("star", Star)
                          , ("underline", Underline)
                          , ("note", Note)
                          , ("tab", Tab)
                          , ("folder", Folder)
                          , ("component", Component)
                          , ("promoter", Promoter)
                          , ("cds", CDS)
                          , ("terminator", Terminator)
                          , ("utr", UTR)
                          , ("primersite", PrimerSite)
                          , ("restrictionsite", RestrictionSite)
                          , ("fivepovoverhang", FivePovOverhang)
                          , ("threepovoverhang", ThreePovOverhang)
                          , ("nooverhang", NoOverhang)
                          , ("assembly", Assembly)
                          , ("signature", Signature)
                          , ("insulator", Insulator)
                          , ("ribosite", Ribosite)
                          , ("rnastab", RNAStab)
                          , ("proteasesite", ProteaseSite)
                          , ("proteinstab", ProteinStab)
                          , ("rpromoter", RPromoter)
                          , ("rarrow", RArrow)
                          , ("larrow", LArrow)
                          , ("lpromoter", LPromoter)
                          , ("record", Record)
                          , ("Mrecord", MRecord)
                          ]

-- -----------------------------------------------------------------------------

data SmoothType = NoSmooth
                | AvgDist
                | GraphDist
                | PowerDist
                | RNG
                | Spring
                | TriangleSmooth
                deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot SmoothType where
  unqtDot NoSmooth       = text "none"
  unqtDot AvgDist        = text "avg_dist"
  unqtDot GraphDist      = text "graph_dist"
  unqtDot PowerDist      = text "power_dist"
  unqtDot RNG            = text "rng"
  unqtDot Spring         = text "spring"
  unqtDot TriangleSmooth = text "triangle"

instance ParseDot SmoothType where
  parseUnqt = oneOf [ stringRep NoSmooth "none"
                    , stringRep AvgDist "avg_dist"
                    , stringRep GraphDist "graph_dist"
                    , stringRep PowerDist "power_dist"
                    , stringRep RNG "rng"
                    , stringRep Spring "spring"
                    , stringRep TriangleSmooth "triangle"
                    ]

-- -----------------------------------------------------------------------------

data StartType = StartStyle STStyle
               | StartSeed Int
               | StartStyleSeed STStyle Int
               deriving (Eq, Ord, Show, Read)

instance PrintDot StartType where
  unqtDot (StartStyle ss)       = unqtDot ss
  unqtDot (StartSeed s)         = unqtDot s
  unqtDot (StartStyleSeed ss s) = unqtDot ss <> unqtDot s

instance ParseDot StartType where
  parseUnqt = oneOf [ liftA2 StartStyleSeed parseUnqt parseUnqt
                    , StartStyle <$> parseUnqt
                    , StartSeed <$> parseUnqt
                    ]

data STStyle = RegularStyle
             | SelfStyle
             | RandomStyle
             deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot STStyle where
  unqtDot RegularStyle = text "regular"
  unqtDot SelfStyle    = text "self"
  unqtDot RandomStyle  = text "random"

instance ParseDot STStyle where
  parseUnqt = oneOf [ stringRep RegularStyle "regular"
                    , stringRep SelfStyle "self"
                    , stringRep RandomStyle "random"
                    ]

-- -----------------------------------------------------------------------------

-- | An individual style item.  Except for 'DD', the @['String']@
--   should be empty.
data StyleItem = SItem StyleName [Text]
               deriving (Eq, Ord, Show, Read)

instance PrintDot StyleItem where
  unqtDot (SItem nm args)
    | null args = dnm
    | otherwise = dnm <> parens args'
    where
      dnm = unqtDot nm
      args' = hcat . punctuate comma $ mapM unqtDot args

  toDot si@(SItem nm args)
    | null args = toDot nm
    | otherwise = dquotes $ unqtDot si

  unqtListToDot = hcat . punctuate comma . mapM unqtDot

  listToDot [SItem nm []] = toDot nm
  listToDot sis           = dquotes $ unqtListToDot sis

instance ParseDot StyleItem where
  parseUnqt = liftA2 SItem parseUnqt (tryParseList' parseArgs)

  parse = quotedParse (liftA2 SItem parseUnqt parseArgs)
          `onFail`
          fmap (`SItem` []) parse

  parseUnqtList = sepBy1 parseUnqt (wrapWhitespace parseComma)

  parseList = quotedParse parseUnqtList
              `onFail`
              -- Might not necessarily need to be quoted if a singleton...
              fmap return parse

parseArgs :: Parse [Text]
parseArgs = bracketSep (character '(')
                       parseComma
                       (character ')')
                       parseStyleName

data StyleName = Dashed    -- ^ Nodes and Edges
               | Dotted    -- ^ Nodes and Edges
               | Solid     -- ^ Nodes and Edges
               | Bold      -- ^ Nodes and Edges
               | Invisible -- ^ Nodes and Edges
               | Filled    -- ^ Nodes and Clusters
               | Striped   -- ^ Rectangularly-shaped Nodes and
                           --   Clusters; requires Graphviz >= 2.30.0
               | Wedged    -- ^ Elliptically-shaped Nodes only;
                           --   requires Graphviz >= 2.30.0
               | Diagonals -- ^ Nodes only
               | Rounded   -- ^ Nodes and Clusters
               | Tapered   -- ^ Edges only; requires Graphviz >=
                           --   2.29.0
               | Radial    -- ^ Nodes, Clusters and Graphs, for use
                           --   with 'GradientAngle'; requires
                           --   Graphviz >= 2.29.0
               | DD Text   -- ^ Device Dependent
               deriving (Eq, Ord, Show, Read)

instance PrintDot StyleName where
  unqtDot Dashed    = text "dashed"
  unqtDot Dotted    = text "dotted"
  unqtDot Solid     = text "solid"
  unqtDot Bold      = text "bold"
  unqtDot Invisible = text "invis"
  unqtDot Filled    = text "filled"
  unqtDot Striped   = text "striped"
  unqtDot Wedged    = text "wedged"
  unqtDot Diagonals = text "diagonals"
  unqtDot Rounded   = text "rounded"
  unqtDot Tapered   = text "tapered"
  unqtDot Radial    = text "radial"
  unqtDot (DD nm)   = unqtDot nm

  toDot (DD nm) = toDot nm
  toDot sn      = unqtDot sn

instance ParseDot StyleName where
  parseUnqt = checkDD <$> parseStyleName

  parse = quotedParse parseUnqt
          `onFail`
          fmap checkDD quotelessString

checkDD     :: Text -> StyleName
checkDD str = case T.toLower str of
                "dashed"    -> Dashed
                "dotted"    -> Dotted
                "solid"     -> Solid
                "bold"      -> Bold
                "invis"     -> Invisible
                "filled"    -> Filled
                "striped"   -> Striped
                "wedged"    -> Wedged
                "diagonals" -> Diagonals
                "rounded"   -> Rounded
                "tapered"   -> Tapered
                "radial"    -> Radial
                _           -> DD str

parseStyleName :: Parse Text
parseStyleName = liftA2 T.cons (orEscaped . noneOf $ ' ' : disallowedChars)
                               (parseEscaped True [] disallowedChars)
  where
    disallowedChars = [quoteChar, '(', ')', ',']
    -- Used because the first character has slightly stricter requirements than the rest.
    orSlash p = stringRep '\\' "\\\\" `onFail` p
    orEscaped = orQuote . orSlash

-- -----------------------------------------------------------------------------

data ViewPort = VP { wVal  :: Double
                   , hVal  :: Double
                   , zVal  :: Double
                   , focus :: Maybe FocusType
                   }
              deriving (Eq, Ord, Show, Read)

instance PrintDot ViewPort where
  unqtDot vp = maybe vs ((<>) (vs <> comma) . unqtDot)
               $ focus vp
    where
      vs = hcat . punctuate comma
           $ mapM (unqtDot . ($vp)) [wVal, hVal, zVal]

  toDot = dquotes . unqtDot

instance ParseDot ViewPort where
  parseUnqt = VP <$> parseUnqt
                 <*  parseComma
                 <*> parseUnqt
                 <*  parseComma
                 <*> parseUnqt
                 <*> optional (parseComma *> parseUnqt)

  parse = quotedParse parseUnqt

-- | For use with 'ViewPort'.
data FocusType = XY Point
               | NodeFocus Text
               deriving (Eq, Ord, Show, Read)

instance PrintDot FocusType where
  unqtDot (XY p)         = unqtDot p
  unqtDot (NodeFocus nm) = unqtDot nm

  toDot (XY p)         = toDot p
  toDot (NodeFocus nm) = toDot nm

instance ParseDot FocusType where
  parseUnqt = fmap XY parseUnqt
              `onFail`
              fmap NodeFocus parseUnqt

  parse = fmap XY parse
          `onFail`
          fmap NodeFocus parse

-- -----------------------------------------------------------------------------

data VerticalPlacement = VTop
                       | VCenter -- ^ Only valid for Nodes.
                       | VBottom
                       deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot VerticalPlacement where
  unqtDot VTop    = char 't'
  unqtDot VCenter = char 'c'
  unqtDot VBottom = char 'b'

instance ParseDot VerticalPlacement where
  parseUnqt = oneOf [ stringReps VTop    ["top", "t"]
                    , stringReps VCenter ["centre", "center", "c"]
                    , stringReps VBottom ["bottom", "b"]
                    ]

-- -----------------------------------------------------------------------------

-- | A list of search paths.
newtype Paths = Paths { paths :: [FilePath] }
    deriving (Eq, Ord, Show, Read)

instance PrintDot Paths where
    unqtDot = unqtDot . intercalate [searchPathSeparator] . paths

    toDot (Paths [p]) = toDot p
    toDot ps          = dquotes $ unqtDot ps

instance ParseDot Paths where
    parseUnqt = Paths . splitSearchPath <$> parseUnqt

    parse = quotedParse parseUnqt
            `onFail`
            fmap (Paths . (:[]) . T.unpack) quotelessString

-- -----------------------------------------------------------------------------

data ScaleType = UniformScale
               | NoScale
               | FillWidth
               | FillHeight
               | FillBoth
               deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot ScaleType where
  unqtDot UniformScale = unqtDot True
  unqtDot NoScale      = unqtDot False
  unqtDot FillWidth    = text "width"
  unqtDot FillHeight   = text "height"
  unqtDot FillBoth     = text "both"

instance ParseDot ScaleType where
  parseUnqt = oneOf [ stringRep UniformScale "true"
                    , stringRep NoScale "false"
                    , stringRep FillWidth "width"
                    , stringRep FillHeight "height"
                    , stringRep FillBoth "both"
                    ]

-- -----------------------------------------------------------------------------

data Justification = JLeft
                   | JRight
                   | JCenter
                   deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot Justification where
  unqtDot JLeft   = char 'l'
  unqtDot JRight  = char 'r'
  unqtDot JCenter = char 'c'

instance ParseDot Justification where
  parseUnqt = oneOf [ stringReps JLeft ["left", "l"]
                    , stringReps JRight ["right", "r"]
                    , stringReps JCenter ["center", "centre", "c"]
                    ]

-- -----------------------------------------------------------------------------

data Ratios = AspectRatio Double
            | FillRatio
            | CompressRatio
            | ExpandRatio
            | AutoRatio
            deriving (Eq, Ord, Show, Read)

instance PrintDot Ratios where
  unqtDot (AspectRatio r) = unqtDot r
  unqtDot FillRatio       = text "fill"
  unqtDot CompressRatio   = text "compress"
  unqtDot ExpandRatio     = text "expand"
  unqtDot AutoRatio       = text "auto"

  toDot (AspectRatio r) = toDot r
  toDot r               = unqtDot r

instance ParseDot Ratios where
  parseUnqt = parseRatio True

  parse = quotedParse parseUnqt <|> parseRatio False

parseRatio   :: Bool -> Parse Ratios
parseRatio q = oneOf [ AspectRatio <$> parseSignedFloat q
                     , stringRep FillRatio "fill"
                     , stringRep CompressRatio "compress"
                     , stringRep ExpandRatio "expand"
                     , stringRep AutoRatio "auto"
                     ]

-- -----------------------------------------------------------------------------

-- | A numeric type with an explicit separation between integers and
--   floating-point values.
data Number = Int Int
            | Dbl Double
            deriving (Eq, Ord, Show, Read)

instance PrintDot Number where
  unqtDot (Int i) = unqtDot i
  unqtDot (Dbl d) = unqtDot d

  toDot (Int i) = toDot i
  toDot (Dbl d) = toDot d

instance ParseDot Number where
  parseUnqt = parseNumber True

  parse = quotedParse parseUnqt
          <|>
          parseNumber False

parseNumber   :: Bool -> Parse Number
parseNumber q = Dbl <$> parseStrictFloat q
                <|>
                Int <$> parseUnqt

-- -----------------------------------------------------------------------------

-- | If set, normalizes coordinates such that the first point is at
--   the origin and the first edge is at the angle if specified.
data Normalized = IsNormalized -- ^ Equivalent to @'NormalizedAngle' 0@.
                | NotNormalized
                | NormalizedAngle Double -- ^ Angle of first edge when
                                         --   normalized.  Requires
                                         --   Graphviz >= 2.32.0.
                deriving (Eq, Ord, Show, Read)

instance PrintDot Normalized where
  unqtDot IsNormalized        = unqtDot True
  unqtDot NotNormalized       = unqtDot False
  unqtDot (NormalizedAngle a) = unqtDot a

  toDot (NormalizedAngle a) = toDot a
  toDot norm                = unqtDot norm

instance ParseDot Normalized where
  parseUnqt = parseNormalized True

  parse = quotedParse parseUnqt <|> parseNormalized False

parseNormalized :: Bool -> Parse Normalized
parseNormalized q = NormalizedAngle <$> parseSignedFloat q
                    <|>
                    bool NotNormalized IsNormalized <$> onlyBool

-- -----------------------------------------------------------------------------

-- | Determine how the 'Width' and 'Height' attributes specify the
--   size of nodes.
data NodeSize = GrowAsNeeded
                -- ^ Nodes will be the smallest width and height
                --   needed to contain the label and any possible
                --   image.  'Width' and 'Height' are the minimum
                --   allowed sizes.
              | SetNodeSize
                -- ^ 'Width' and 'Height' dictate the size of the node
                --   with a warning if the label cannot fit in this.
              | SetShapeSize
                -- ^ 'Width' and 'Height' dictate the size of the
                --   shape only and the label can expand out of the
                --   shape (with a warning).  Requires Graphviz >=
                --   2.38.0.
              deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot NodeSize where
  unqtDot GrowAsNeeded = unqtDot False
  unqtDot SetNodeSize  = unqtDot True
  unqtDot SetShapeSize = text "shape"

instance ParseDot NodeSize where
  parseUnqt = bool GrowAsNeeded SetNodeSize <$> parseUnqt
              <|>
              stringRep SetShapeSize "shape"

-- -----------------------------------------------------------------------------

{-

As of Graphviz 2.36.0 this was commented out; as such it might come
back, so leave this here in case we need it again.

data AspectType = RatioOnly Double
                | RatioPassCount Double Int
                deriving (Eq, Ord, Show, Read)

instance PrintDot AspectType where
  unqtDot (RatioOnly r)        = unqtDot r
  unqtDot (RatioPassCount r p) = commaDel r p

  toDot at@RatioOnly{}      = unqtDot at
  toDot at@RatioPassCount{} = dquotes $ unqtDot at

instance ParseDot AspectType where
  parseUnqt = fmap (uncurry RatioPassCount) commaSepUnqt
              `onFail`
              fmap RatioOnly parseUnqt


  parse = quotedParse (uncurry RatioPassCount <$> commaSepUnqt)
          `onFail`
          fmap RatioOnly parse

-}
