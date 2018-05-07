{-# LANGUAGE MonadComprehensions, MultiParamTypeClasses #-}

{- |
   Module      : Data.GraphViz.Algorithms
   Description : Various algorithms on Graphviz graphs.
   Copyright   : (c) Matthew Sackman, Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   Defines various algorithms for use on 'DotRepr' graphs.  These are
   typically re-implementations of behaviour found in existing Graphviz
   tools but without the I/O requirement.

   Note that one way that these algorithms differ from those found in
   Graphviz is that the order of clusters is /not/ maintained, which may
   affect layout in some cases.
 -}
module Data.GraphViz.Algorithms
       ( -- * Canonicalisation Options
         -- $options
         CanonicaliseOptions(..)
       , defaultCanonOptions
       , dotLikeOptions
         -- * Canonicalisation
         -- $canonicalisation
       , canonicalise
       , canonicaliseOptions
         -- * Dealing with transitive edges
         -- $transitive
       , transitiveReduction
       , transitiveReductionOptions
       ) where

import Data.GraphViz.Attributes.Complete   (Attributes, defaultAttributeValue)
import Data.GraphViz.Attributes.Same
import Data.GraphViz.Internal.Util         (bool)
import Data.GraphViz.Types
import Data.GraphViz.Types.Canonical
import Data.GraphViz.Types.Internal.Common

import           Control.Arrow       (first, second, (***))
import           Control.Monad       (unless)
import           Control.Monad.State (State, execState, gets, modify)
import qualified Data.DList          as DList
import qualified Data.Foldable       as F
import           Data.Function       (on)
import           Data.List           (deleteBy, groupBy, partition, sortBy,
                                      (\\))
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Maybe          (fromMaybe, listToMaybe, mapMaybe)
import           Data.Set            (Set)
import qualified Data.Set            as Set

-- -----------------------------------------------------------------------------

{- $options
   For simplicity, many algorithms end up using the canonicalisation
   functions to create the new 'DotGraph'.  'CanonicaliseOptions' allows
   you to configure how the output is generated.
 -}

data CanonicaliseOptions = COpts { -- | Place edges in the clusters
                                   --   where their nodes are rather
                                   --   than in the top-level graph.
                                   edgesInClusters :: Bool
                                   -- | Put common 'Attributes' as
                                   --   top-level 'GlobalAttributes'.
                                 , groupAttributes :: Bool
                                 }
                         deriving (Eq, Ord, Show, Read)

defaultCanonOptions :: CanonicaliseOptions
defaultCanonOptions = COpts { edgesInClusters = True
                            , groupAttributes = True
                            }

-- | Options that are more like how @dot -Tcanon@ works.
dotLikeOptions :: CanonicaliseOptions
dotLikeOptions = COpts { edgesInClusters = True
                       , groupAttributes = False
                       }

-- -----------------------------------------------------------------------------

{- $canonicalisation

These functions implement similar functionality to @dot -Tcanon@
(i.e. creates a canonical form of any 'DotRepr' graph).  without
requiring IO.

Note that due to implementation specifics the behaviour is not
identical; in particular:

* Any specified 'Attributes' that equal the defaults are stripped out
  (unless required to override a previous attribute that doesn't apply
  here).

* Grouping of attributes (when @'groupAttributes = True'@) is much
  more conservative; only those node/edge attributes that are common to
  /all/ nodes and edges within that cluster (and within sub-clusters)
  are made global.

* Sub-graphs aren't kept, only clusters.

* 'ColorScheme' Attributes are removed (as all @Color@ values embed
  any needed color scheme anyway) as the output order of attributes may
  change (and this matters for the Haskell side of things).

In particular, note that this function will create a single explicit
definition for every node in the original graph and place it in the
appropriate position in the cluster hierarchy.  All edges are found in
the deepest cluster that contains both nodes.

-}

-- | Canonicalise with some sensible defaults.
canonicalise :: (DotRepr dg n) => dg n -> DotGraph n
canonicalise = canonicaliseOptions defaultCanonOptions

-- | As with 'canonicalise', but allow custom 'CanonicaliseOptions'.
canonicaliseOptions :: (DotRepr dg n) => CanonicaliseOptions
                       -> dg n -> DotGraph n
canonicaliseOptions opts dg = cdg { strictGraph   = graphIsStrict dg
                                  , directedGraph = graphIsDirected dg
                                  }
  where
    cdg = createCanonical opts (getID dg) gas cl nl es

    (gas, cl) = graphStructureInformationClean dg
    nl = nodeInformationClean True dg
    es = edgeInformationClean True dg

type NodePath n = ([Maybe GraphID], DotNode n)
type NodePaths n = [NodePath n]
type EdgeClusters n = Map (Maybe GraphID) [DotEdge n]
type EdgeLocations n = (EdgeClusters n, [DotEdge n])

data CanonControl n = CC { cOpts    :: !CanonicaliseOptions
                         , isGraph  :: !Bool
                         , clusters :: !ClusterLookup
                         , clustEs  :: !(EdgeLocations n)
                         , topID    :: !(Maybe GraphID)
                         , topAttrs :: !Attributes
                         }

createCanonical :: (Ord n) => CanonicaliseOptions -> Maybe GraphID -> GlobalAttributes
                   -> ClusterLookup -> NodeLookup n -> [DotEdge n] -> DotGraph n
createCanonical opts gid gas cl nl es = promoteDSG $ makeGrouping cc ns
  where
    nUnlook (n,(p,as)) = (F.toList p, DotNode n as)
    -- DotNodes paired and sorted by their paths
    ns = sortBy (compLists `on` fst) . map nUnlook $ Map.toList nl

    es' = if edgesInClusters opts
          then edgeClusters nl es
          else (Map.empty, es)

    cc = CC { cOpts    = opts
            , isGraph  = True
            , clusters = cl
            , clustEs  = es'
            , topID    = gid
            , topAttrs = attrs gas
            }

thisLevel :: NodePaths n -> (NodePaths n, [DotNode n])
thisLevel = second (map snd) . span (not . null . fst)

makeGrouping :: CanonControl n -> NodePaths n -> DotSubGraph n
makeGrouping cc cns = DotSG { isCluster = True
                            , subGraphID = cID
                            , subGraphStmts = stmts
                            }
  where
    cID | isGraph cc = topID cc
        | otherwise  = head . fst . head $ cns

    (nestedNs, ns) = thisLevel
                     . bool (map $ first tail) id (isGraph cc)
                     $ cns

    es = bool (fromMaybe [] . Map.lookup cID . fst) snd (isGraph cc)
         $ clustEs cc

    gas | isGraph cc = topAttrs cc
        | otherwise  = attrs . snd $ clusters cc Map.! cID

    subGs = map (makeGrouping $ cc { isGraph = False })
            . groupBy ((==) `on` (listToMaybe . fst))
            $ nestedNs

    stmts = setGlobal (cOpts cc) gas
            $ DotStmts { attrStmts = []
                       , subGraphs = subGs
                       , nodeStmts = ns
                       , edgeStmts = es
                       }

setGlobal :: CanonicaliseOptions
             -> Attributes -- Specified cluster attributes
             -> DotStatements n
             -> DotStatements n
setGlobal opts as stmts = stmts { attrStmts = globs'
                                , subGraphs = sgs'
                                , nodeStmts = ns'
                                , edgeStmts = es'
                                }
  where
    sgs = subGraphs stmts
    sStmts = map subGraphStmts sgs
    ns = nodeStmts stmts
    es = edgeStmts stmts

    sGlobs = map (partitionGlobal . attrStmts) sStmts

    (sgas,snas,seas) = unzip3 sGlobs

    gas' = as -- Can't change graph attrs! Need these!
    nas' = getCommonGlobs opts nodeStmts snas sStmts $ map nodeAttributes ns
    eas' = getCommonGlobs opts edgeStmts seas sStmts $ map edgeAttributes es

    globs' = nonEmptyGAs [ GraphAttrs gas'
                         , NodeAttrs  nas'
                         , EdgeAttrs  eas'
                         ]
    ns' = map (\dn -> dn { nodeAttributes = nodeAttributes dn \\ nas' }) ns
    es' = map (\de -> de { edgeAttributes = edgeAttributes de \\ eas' }) es

    sgas' = updateGraphGlobs gas' sgas
    snas' = map (\\ nas') snas
    seas' = map (\\ eas') seas

    sGlobs' = zip3 sgas' snas' seas'
    sStmts' = zipWith (\ sSt sGl -> sSt { attrStmts = nonEmptyGAs $ unPartitionGlobal sGl })
                      sStmts
                      sGlobs'

    sgs' = zipWith (\ sg sSt -> sg { subGraphStmts = sSt }) sgs sStmts'

updateGraphGlobs :: Attributes -> [Attributes] -> [Attributes]
updateGraphGlobs gas = map go
  where
    gasS = Set.fromList gas

    override = toSAttr $ nonSameDefaults gas

    -- * Remove any identical values
    -- * Override any different values
    go = Set.toList
         . (`Set.difference` gasS) -- Remove identical values
         . unSameSet
         . (`Set.union` override) -- Keeps existing values of constructors
         . toSAttr

nonSameDefaults :: Attributes -> Attributes
nonSameDefaults = mapMaybe (\ a -> [ a' | a' <- defaultAttributeValue a, a' /= a] )

getCommonGlobs :: CanonicaliseOptions
                  -> (DotStatements n -> [a])
                  -> [Attributes] -- ^ From sub-graphs
                  -> [DotStatements n] -- ^ Statements from the sub-graphs for testing.
                  -> [Attributes] -- ^ From nodes/edges
                  -> Attributes
getCommonGlobs opts f sas stmts as
  | not $ groupAttributes opts = []
  | otherwise = case sas' ++ as of
                  []  -> []
                  [_] -> []
                  as' -> Set.toList . foldr1 Set.intersection
                         $ map Set.fromList as'
  where
    sas' = keepIfAny f sas stmts

-- Used to distinguish between having empty list of global attributes
-- for nodes or edges because there aren't any nodes/edges, or because
-- there aren't any common attributes
keepIfAny :: (DotStatements n -> [a]) -> [Attributes] -> [DotStatements n]
             -> [Attributes]
keepIfAny f sas = map fst . filter snd . zip sas . map (hasAny f)

hasAny      :: (DotStatements n -> [a]) -> DotStatements n -> Bool
hasAny f ds = not (null $ f ds) || any (hasAny f . subGraphStmts) (subGraphs ds)

promoteDSG     :: DotSubGraph n -> DotGraph n
promoteDSG dsg = DotGraph { strictGraph     = undefined
                          , directedGraph   = undefined
                          , graphID         = subGraphID dsg
                          , graphStatements = subGraphStmts dsg
                          }

-- Same as compare for lists, except shorter lists are GT
compLists :: (Ord a) => [a] -> [a] -> Ordering
compLists []     []     = EQ
compLists []     _      = GT
compLists _      []     = LT
compLists (x:xs) (y:ys) = case compare x y of
                            EQ  -> compLists xs ys
                            oth -> oth

nonEmptyGAs :: [GlobalAttributes] -> [GlobalAttributes]
nonEmptyGAs = filter (not . null . attrs)

-- Assign each edge into the cluster it belongs in.
edgeClusters    :: (Ord n) => NodeLookup n -> [DotEdge n]
                   -> EdgeLocations n
edgeClusters nl = (toM *** map snd) . partition (not . null . fst)
                  . map inClust
  where
    nl' = Map.map (F.toList . fst) nl
    -- DotEdge n -> (Path, DotEdge n)
    inClust de@(DotEdge n1 n2 _) = (flip (,) de)
                                   . map fst . takeWhile (uncurry (==))
                                   $ zip (nl' Map.! n1) (nl' Map.! n2)
    toM = Map.map DList.toList
          . Map.fromListWith (flip DList.append)
          . map (last *** DList.singleton)

-- -----------------------------------------------------------------------------

{- $transitive

   In large, cluttered graphs, it can often be difficult to see what
   is happening due to the number of edges being drawn.  As such, it is
   often useful to remove transitive edges from the graph before
   visualising it.

   For example, consider the following Dot graph:

   > digraph {
   >     a -> b;
   >     a -> c;
   >     b -> c;
   > }

   This graph has the transitive edge @a -> c@ (as we can reach @c@ from @a@ via @b@).

   Graphviz comes with the @tred@ program to perform these transitive
   reductions.  'transitiveReduction' and 'transitiveReductionOptions'
   are pure Haskell re-implementations of @tred@ with the following differences:

   * @tred@ prints a message to stderr if a cycle is detected; these
     functions do not.

   * @tred@ preserves the original structure of the graph; these
     functions use the canonicalisation functions above to create the new
     graph (rather than re-implement creation functions for each one).

   When a graph contains cycles, an arbitrary edge from that cycle is
   ignored whilst calculating the transitive reduction.  Multiple edges
   are also reduced (such that only the first edge between two nodes is
   kept).

   Note that transitive reduction only makes sense for directed graphs;
   for undirected graphs these functions are identical to the
   canonicalisation functions above.

   The caveats for the canonicalisation functions also apply.
 -}

transitiveReduction :: (DotRepr dg n) => dg n -> DotGraph n
transitiveReduction = transitiveReductionOptions defaultCanonOptions

transitiveReductionOptions         :: (DotRepr dg n) => CanonicaliseOptions
                                      -> dg n -> DotGraph n
transitiveReductionOptions opts dg = cdg { strictGraph = graphIsStrict dg
                                         , directedGraph = graphIsDirected dg
                                         }
  where
    cdg = createCanonical opts (getID dg) gas cl nl es'
    (gas, cl) = graphStructureInformationClean dg
    nl = nodeInformationClean True dg
    es = edgeInformationClean True dg
    es' | graphIsDirected dg = rmTransEdges es
        | otherwise          = es

rmTransEdges    :: (Ord n) => [DotEdge n] -> [DotEdge n]
rmTransEdges [] = []
rmTransEdges es = concatMap (map snd . outgoing) $ Map.elems esM
  where
    tes = tagEdges es

    esMS = do edgeGraph tes
              ns <- getsMap Map.keys
              mapM_ (traverseTag zeroTag) ns

    esM = fst $ execState esMS (Map.empty, Set.empty)

type Tag = Int
type TagSet = Set Int
type TaggedEdge n = (Tag, DotEdge n)

-- A "nonsense" tag to use as an initial value
zeroTag :: Tag
zeroTag = 0

tagEdges :: [DotEdge n] -> [TaggedEdge n]
tagEdges = zip [(succ zeroTag)..]

data TaggedValues n = TV { marked   :: Bool
                         , incoming :: [TaggedEdge n]
                         , outgoing :: [TaggedEdge n]
                         }
                    deriving (Eq, Ord, Show, Read)

defTV :: TaggedValues n
defTV = TV False [] []

type TagMap n = Map n (TaggedValues n)

type TagState n a = State (TagMap n, TagSet) a

getMap :: TagState n (TagMap n)
getMap = gets fst

getsMap   :: (TagMap n -> a) -> TagState n a
getsMap f = gets (f . fst)

modifyMap   :: (TagMap n -> TagMap n) -> TagState n ()
modifyMap f = modify (first f)

getSet :: TagState n TagSet
getSet = gets snd

modifySet   :: (TagSet -> TagSet) -> TagState n ()
modifySet f = modify (second f)

-- Create the Map representing the graph from the edges.
edgeGraph :: (Ord n) => [TaggedEdge n] -> TagState n ()
edgeGraph = mapM_ addEdge . reverse
  where
    addEdge te = addVal f tvOut >> addVal t tvIn
      where
        e = snd te
        f = fromNode e
        t = toNode e
        addVal n tv = modifyMap (Map.insertWith mergeTV n tv)
        tvIn  = defTV { incoming = [te] }
        tvOut = defTV { outgoing = [te] }
        mergeTV tvNew tv  = tv { incoming = incoming tvNew ++ incoming tv
                               , outgoing = outgoing tvNew ++ outgoing tv
                               }

-- Perform a DFS to determine whether or not to keep each edge.
traverseTag     :: (Ord n) => Tag -> n -> TagState n ()
traverseTag t n = do setMark True
                     checkIncoming
                     outEs <- getsMap (maybe [] outgoing . Map.lookup n)
                     mapM_ maybeRecurse outEs
                     setMark False
  where
    setMark mrk = modifyMap (Map.adjust (\tv -> tv { marked = mrk }) n)

    isMarked m n' = maybe False marked $ n' `Map.lookup` m

    checkIncoming = do m <- gets fst
                       let es = incoming $ m Map.! n
                           (keepEs, delEs) = partition (keepEdge m) es
                       modifyMap (Map.adjust (\tv -> tv {incoming = keepEs}) n)
                       modifySet (Set.union $ Set.fromList (map fst delEs))
                       mapM_ delOtherEdge delEs
      where
        keepEdge m (t',e) = t == t' || not (isMarked m $ fromNode e)

        delOtherEdge te = modifyMap (Map.adjust delE . fromNode $ snd te)
          where
            delE tv = tv {outgoing = deleteBy ((==) `on` fst) te $ outgoing tv}

    maybeRecurse (t',e) = do m <- getMap
                             delSet <- getSet
                             let n' = toNode e
                             unless (isMarked m n' || t' `Set.member` delSet)
                               $ traverseTag t' n'
