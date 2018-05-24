{-# LANGUAGE CPP, FlexibleInstances, MultiParamTypeClasses #-}

{- |
   Module      : Data.GraphViz.Types.Graph
   Description : A graph-like representation of Dot graphs.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   It is sometimes useful to be able to manipulate a Dot graph /as/ an
   actual graph.  This representation lets you do so, using an
   inductive approach based upon that from FGL (note that 'DotGraph'
   is /not/ an instance of the FGL classes due to having the wrong
   kind).  Note, however, that the API is not as complete as proper
   graph implementations.

   For purposes of manipulation, all edges are found in the root graph
   and not in a cluster; as such, having 'EdgeAttrs' in a cluster's
   'GlobalAttributes' is redundant.

   Printing is achieved via "Data.GraphViz.Types.Canonical" (using
   'toCanonical') and parsing via "Data.GraphViz.Types.Generalised"
   (so /any/ piece of Dot code can be parsed in).

   This representation doesn't allow non-cluster sub-graphs.  Also, all
   clusters /must/ have a unique identifier.  For those functions (with
   the exception of 'DotRepr' methods) that take or return a \"@Maybe
   GraphID@\", a value of \"@Nothing@\" refers to the root graph; \"@Just
   clust@\" refers to the cluster with the identifier \"@clust@\".

   You would not typically explicitly create these values, instead
   converting existing Dot graphs (via 'fromDotRepr').  However, one
   way of constructing the sample graph would be:

   > setID (Str "G")
   > . setStrictness False
   > . setIsDirected True
   > . setClusterAttributes (Int 0) [GraphAttrs [style filled, color LightGray, textLabel "process #1"], NodeAttrs [style filled, color White]]
   > . setClusterAttributes (Int 1) [GraphAttrs [textLabel "process #2", color Blue], NodeAttrs [style filled]]
   > $ composeList [ Cntxt "a0"    (Just $ Int 0)   []               [("a3",[]),("start",[])] [("a1",[])]
   >               , Cntxt "a1"    (Just $ Int 0)   []               []                       [("a2",[]),("b3",[])]
   >               , Cntxt "a2"    (Just $ Int 0)   []               []                       [("a3",[])]
   >               , Cntxt "a3"    (Just $ Int 0)   []               [("b2",[])]              [("end",[])]
   >               , Cntxt "b0"    (Just $ Int 1)   []               [("start",[])]           [("b1",[])]
   >               , Cntxt "b1"    (Just $ Int 1)   []               []                       [("b2",[])]
   >               , Cntxt "b2"    (Just $ Int 1)   []               []                       [("b3",[])]
   >               , Cntxt "b3"    (Just $ Int 1)   []               []                       [("end",[])]
   >               , Cntxt "end"   Nothing          [shape MSquare]  []                       []
   >               , Cntxt "start" Nothing          [shape MDiamond] []                       []]

 -}
module Data.GraphViz.Types.Graph
       ( DotGraph
       , GraphID(..)
       , Context(..)
         -- * Conversions
       , toCanonical
       , unsafeFromCanonical
       , fromDotRepr
         -- * Graph information
       , isEmpty
       , hasClusters
       , isEmptyGraph
       , graphAttributes
       , parentOf
       , clusterAttributes
       , foundInCluster
       , attributesOf
       , predecessorsOf
       , successorsOf
       , adjacentTo
       , adjacent
         -- * Graph construction
       , mkGraph
       , emptyGraph
       , (&)
       , composeList
       , addNode
       , DotNode(..)
       , addDotNode
       , addEdge
       , DotEdge(..)
       , addDotEdge
       , addCluster
       , setClusterParent
       , setClusterAttributes
         -- * Graph deconstruction
       , decompose
       , decomposeAny
       , decomposeList
       , deleteNode
       , deleteAllEdges
       , deleteEdge
       , deleteDotEdge
       , deleteCluster
       , removeEmptyClusters
       ) where

import           Data.GraphViz.Algorithms            (CanonicaliseOptions(..),
                                                      canonicaliseOptions)
import           Data.GraphViz.Algorithms.Clustering
import           Data.GraphViz.Attributes.Complete   (Attributes)
import           Data.GraphViz.Attributes.Same
import           Data.GraphViz.Internal.Util         (groupSortBy,
                                                      groupSortCollectBy)
import           Data.GraphViz.Types
import qualified Data.GraphViz.Types.Canonical       as C
import qualified Data.GraphViz.Types.Generalised     as G
import           Data.GraphViz.Types.Internal.Common (partitionGlobal)
import qualified Data.GraphViz.Types.State           as St

import           Control.Applicative             (liftA2, (<|>))
import           Control.Arrow                   ((***))
import qualified Data.Foldable                   as F
import           Data.List                       (delete, foldl', unfoldr)
import           Data.Map                        (Map)
import qualified Data.Map                        as M
import           Data.Maybe                      (fromMaybe, mapMaybe,
                                                  maybeToList)
import qualified Data.Sequence                   as Seq
import qualified Data.Set                        as S
import           Text.ParserCombinators.ReadPrec (prec)
import           Text.Read                       (Lexeme(Ident), lexP, parens,
                                                  readPrec)

#if !(MIN_VERSION_base (4,8,0))
import Control.Applicative ((<$>), (<*>))
#endif

-- -----------------------------------------------------------------------------

-- | A Dot graph that allows graph operations on it.
data DotGraph n = DG { strictGraph   :: !Bool
                     , directedGraph :: !Bool
                     , graphAttrs    :: !GlobAttrs
                     , graphID       :: !(Maybe GraphID)
                     , clusters      :: !(Map GraphID ClusterInfo)
                     , values        :: !(NodeMap n)
                     }
                deriving (Eq, Ord)

-- | It should be safe to substitute 'unsafeFromCanonical' for
--   'fromCanonical' in the output of this.
instance (Show n) => Show (DotGraph n) where
  showsPrec d dg = showParen (d > 10) $
                   showString "fromCanonical " . shows (toCanonical dg)

-- | If the graph is the output from 'show', then it should be safe to
--   substitute 'unsafeFromCanonical' for 'fromCanonical'.
instance (Ord n, Read n) => Read (DotGraph n) where
  readPrec = parens . prec 10
             $ do Ident "fromCanonical" <- lexP
                  cdg <- readPrec
                  return $ fromCanonical cdg

data GlobAttrs = GA { graphAs :: !SAttrs
                    , nodeAs  :: !SAttrs
                    , edgeAs  :: !SAttrs
                    }
               deriving (Eq, Ord, Show, Read)

data NodeInfo n = NI { _inCluster    :: !(Maybe GraphID)
                     , _attributes   :: !Attributes
                     , _predecessors :: !(EdgeMap n)
                     , _successors   :: !(EdgeMap n)
                     }
                deriving (Eq, Ord, Show, Read)

data ClusterInfo = CI { parentCluster :: !(Maybe GraphID)
                      , clusterAttrs  :: !GlobAttrs
                      }
                 deriving (Eq, Ord, Show, Read)

type NodeMap n = Map n (NodeInfo n)

type EdgeMap n = Map n [Attributes]

-- | The decomposition of a node from a dot graph.  Any loops should
--   be found in 'successors' rather than 'predecessors'.  Note also
--   that these are created\/consumed as if for /directed/ graphs.
data Context n = Cntxt { node         :: !n
                         -- | The cluster this node can be found in;
                         --   @Nothing@ indicates the node can be
                         --   found in the root graph.
                       , inCluster    :: !(Maybe GraphID)
                       , attributes   :: !Attributes
                       , predecessors :: ![(n, Attributes)]
                       , successors   :: ![(n, Attributes)]
                       }
               deriving (Eq, Ord, Show, Read)

adjacent :: Context n -> [DotEdge n]
adjacent c = mapU (`DotEdge` n) (predecessors c)
             ++ mapU (DotEdge n) (successors c)
  where
    n = node c
    mapU = map . uncurry

emptyGraph :: DotGraph n
emptyGraph = DG { strictGraph   = False
                , directedGraph = True
                , graphID       = Nothing
                , graphAttrs    = emptyGA
                , clusters      = M.empty
                , values        = M.empty
                }

emptyGA :: GlobAttrs
emptyGA = GA S.empty S.empty S.empty

-- -----------------------------------------------------------------------------
-- Construction

-- | Merge the 'Context' into the graph.  Assumes that the specified
--   node is not in the graph but that all endpoints in the
--   'successors' and 'predecessors' (with the exception of loops)
--   are.  If the cluster is not present in the graph, then it will be
--   added with no attributes with a parent of the root graph.
--
--   Note that @&@ and @'decompose'@ are /not/ quite inverses, as this
--   function will add in the cluster if it does not yet exist in the
--   graph, but 'decompose' will not delete it.
(&) :: (Ord n) => Context n -> DotGraph n -> DotGraph n
(Cntxt n mc as ps ss) & dg = withValues merge dg'
  where
    ps' = toMap ps
    ps'' = fromMap (M.delete n ps')
    ss' = toMap ss
    ss'' = fromMap (M.delete n ss')

    dg' = addNode n mc as dg

    merge = addSuccRev n ps'' . addPredRev n ss''
            -- Add reverse edges
            . M.adjust (\ni -> ni { _predecessors = ps', _successors = ss' }) n
            -- Add actual edges

infixr 5 &

-- | Recursively merge the list of contexts.
--
--   > composeList = foldr (&) emptyGraph
composeList :: (Ord n) => [Context n] -> DotGraph n
composeList = foldr (&) emptyGraph

addSuccRev :: (Ord n) => n -> [(n, Attributes)] -> NodeMap n -> NodeMap n
addSuccRev = addEdgeLinks niSkip niSucc

addPredRev :: (Ord n) => n -> [(n, Attributes)] -> NodeMap n -> NodeMap n
addPredRev = addEdgeLinks niSkip niPred

addEdgeLinks :: (Ord n) => UpdateEdgeMap n -> UpdateEdgeMap n
                -> n -> [(n, Attributes)] -> NodeMap n -> NodeMap n
addEdgeLinks fwd rev f tas = updRev . updFwd
  where
    updFwd = M.adjust addFwd f

    addFwd ni = foldl' (\ni' (t,as) -> fwd (M.insertWith (++) t [as]) ni') ni tas

    updRev nm = foldl' (\nm' (t,as) -> M.adjust (addRev as) t nm') nm tas

    addRev as = rev (M.insertWith (++) f [as])

-- | Add a node to the current graph. Merges attributes and edges if
--   the node already exists in the graph.
--
--   If the specified cluster does not yet exist in the graph, then it
--   will be added (as a sub-graph of the overall graph and no
--   attributes).
addNode :: (Ord n)
           => n
           -> Maybe GraphID -- ^ The cluster the node can be found in
                            --   (@Nothing@ refers to the root graph).
           -> Attributes
           -> DotGraph n
           -> DotGraph n
addNode n mc as dg = addEmptyCluster mc $ dg { values = ns' }
  where
    ns = values dg
    ns' = M.insertWith mergeLogic n (NI mc as M.empty M.empty) ns
    mergeLogic (NI newClust newAttrs newPreds newSuccs) (NI oldClust oldAttrs oldPreds oldSuccs) =
        NI resClust resAttrs resPreds resSuccs
      where
        resClust = newClust <|> oldClust
        resAttrs = unSame $ S.union (toSAttr newAttrs) (toSAttr oldAttrs)
        resPreds = M.unionWith (++) newPreds oldPreds
        resSuccs = M.unionWith (++) newSuccs oldSuccs

-- | A variant of 'addNode' that takes in a DotNode (not in a
--   cluster).
addDotNode                :: (Ord n) => DotNode n -> DotGraph n -> DotGraph n
addDotNode (DotNode n as) = addNode n Nothing as

-- | Add the specified edge to the graph; assumes both node values are
--   already present in the graph.  If the graph is undirected then
--   the order of nodes doesn't matter.
addEdge :: (Ord n) => n -> n -> Attributes -> DotGraph n -> DotGraph n
addEdge f t as = withValues merge
  where
    merge = addEdgeLinks niSucc niPred f [(t,as)]

-- | A variant of 'addEdge' that takes a 'DotEdge' value.
addDotEdge                  :: (Ord n) => DotEdge n -> DotGraph n -> DotGraph n
addDotEdge (DotEdge f t as) = addEdge f t as

-- | Add a new cluster to the graph; throws an error if the cluster
--   already exists.  Assumes that it doesn't match the identifier of
--   the overall graph.  If the parent cluster doesn't already exist
--   in the graph then it will be added.
addCluster :: GraphID          -- ^ The identifier for this cluster.
              -> Maybe GraphID -- ^ The parent of this cluster
                               --   (@Nothing@ refers to the root
                               --   graph)
              -> [GlobalAttributes]
              -> DotGraph n
              -> DotGraph n
addCluster c mp gas dg
  | c `M.member` cs = error "Cluster already exists in the graph"
  | otherwise       = addEmptyCluster mp
                      $ dg { clusters = M.insert c ci cs }
  where
    cs = clusters dg
    ci = CI mp $ toGlobAttrs gas

-- Used to make sure that the parent cluster exists
addEmptyCluster :: Maybe GraphID -> DotGraph n -> DotGraph n
addEmptyCluster = maybe id (withClusters . (`dontReplace` defCI))
  where
    dontReplace = M.insertWith (const id)
    defCI = CI Nothing emptyGA

-- | Specify the parent of the cluster; adds both in if not already present.
setClusterParent     :: GraphID -> Maybe GraphID -> DotGraph n -> DotGraph n
setClusterParent c p = withClusters (M.adjust setP c) . addCs
  where
    addCs = addEmptyCluster p . addEmptyCluster (Just c)
    setP ci = ci { parentCluster = p }

-- | Specify the attributes of the cluster; adds it if not already
--   present.
setClusterAttributes       :: GraphID -> [GlobalAttributes]
                              -> DotGraph n -> DotGraph n
setClusterAttributes c gas = withClusters (M.adjust setAs c)
                             . addEmptyCluster (Just c)
  where
    setAs ci = ci { clusterAttrs = toGlobAttrs gas }

-- | Create a graph with no clusters.
mkGraph :: (Ord n) => [DotNode n] -> [DotEdge n] -> DotGraph n
mkGraph ns es = flip (foldl' $ flip addDotEdge) es
                $ foldl' (flip addDotNode) emptyGraph ns

-- | Convert this DotGraph into canonical form.  All edges are found
--   in the outer graph rather than in clusters.
toCanonical :: DotGraph n -> C.DotGraph n
toCanonical dg = C.DotGraph { C.strictGraph     = strictGraph dg
                            , C.directedGraph   = directedGraph dg
                            , C.graphID         = graphID dg
                            , C.graphStatements = stmts
                            }
  where
    stmts = C.DotStmts { C.attrStmts = fromGlobAttrs $ graphAttrs dg
                       , C.subGraphs = cs
                       , C.nodeStmts = ns
                       , C.edgeStmts = getEdgeInfo False dg
                       }

    cls = clusters dg
    pM = clusterPath' dg

    clustAs = maybe [] (fromGlobAttrs . clusterAttrs) . (`M.lookup`cls)

    lns = map (\ (n,ni) -> (n,(_inCluster ni, _attributes ni)))
          . M.assocs $ values dg

    (cs,ns) = clustersToNodes pathOf (const True) id clustAs snd lns

    pathOf (n,(c,as)) = pathFrom c (n,as)
    pathFrom c ln = F.foldr C (N ln) . fromMaybe Seq.empty $ (`M.lookup`pM) =<< c

-- -----------------------------------------------------------------------------
-- Deconstruction

-- | A partial inverse of @'&'@, in that if a node exists in a graph
--   then it will be decomposed, but will not remove the cluster that
--   it was in even if it was the only node in that cluster.
decompose :: (Ord n) => n -> DotGraph n -> Maybe (Context n, DotGraph n)
decompose n dg
  | n `M.notMember` ns = Nothing
  | otherwise          = Just (c, dg')
  where
    ns = values dg
    (Just (NI mc as ps ss), ns') = M.updateLookupWithKey (const . const Nothing) n ns

    c = Cntxt n mc as (fromMap $ n `M.delete` ps) (fromMap ss)
    dg' = dg { values = delSucc n ps . delPred n ss $ ns' }

-- | As with 'decompose', but do not specify /which/ node to
--   decompose.
decomposeAny :: (Ord n) => DotGraph n -> Maybe (Context n, DotGraph n)
decomposeAny dg
  | isEmpty dg = Nothing
  | otherwise  = decompose (fst . M.findMin $ values dg) dg

-- | Recursively decompose the Dot graph into a list of contexts such
--   that if @(c:cs) = decomposeList dg@, then @dg = c & 'composeList' cs@.
--
--   Note that all global attributes are lost, so this is /not/
--   suitable for representing a Dot graph on its own.
decomposeList :: (Ord n) => DotGraph n -> [Context n]
decomposeList = unfoldr decomposeAny

delSucc :: (Ord n) => n -> EdgeMap n -> NodeMap n -> NodeMap n
delSucc = delPS niSucc

delPred :: (Ord n) => n -> EdgeMap n -> NodeMap n -> NodeMap n
delPred = delPS niPred

-- Only takes in EdgeMap rather than [n] to make it easier to call
-- from decompose
delPS :: (Ord n) => ((EdgeMap n -> EdgeMap n) -> NodeInfo n -> NodeInfo n)
         -> n -> EdgeMap n -> NodeMap n -> NodeMap n
delPS fni t fm nm = foldl' delE nm $ M.keys fm
  where
    delE nm' f = M.adjust (fni $ M.delete t) f nm'

-- | Delete the specified node from the graph; returns the original
--   graph if that node isn't present.
deleteNode      :: (Ord n) => n -> DotGraph n -> DotGraph n
deleteNode n dg = maybe dg snd $ decompose n dg

-- | Delete all edges between the two nodes; returns the original
--   graph if there are no edges.
deleteAllEdges          :: (Ord n) => n -> n -> DotGraph n -> DotGraph n
deleteAllEdges n1 n2 = withValues (delAE n1 n2 . delAE n2 n1)
  where
    delAE f t = delSucc f t' . delPred f t'
      where
        t' = M.singleton t []

-- | Deletes the specified edge from the DotGraph (note: for unordered
--   graphs both orientations are considered).
deleteEdge :: (Ord n) => n -> n -> Attributes -> DotGraph n -> DotGraph n
deleteEdge n1 n2 as dg = withValues delEs dg
  where
    delE f t = M.adjust (niSucc $ M.adjust (delete as) t) f
               . M.adjust (niPred $ M.adjust (delete as) f) t

    delEs | directedGraph dg = delE n1 n2
          | otherwise        = delE n1 n2 . delE n2 n1

-- | As with 'deleteEdge' but takes a 'DotEdge' rather than individual
--   values.
deleteDotEdge :: (Ord n) => DotEdge n -> DotGraph n -> DotGraph n
deleteDotEdge (DotEdge n1 n2 as) = deleteEdge n1 n2 as

-- | Delete the specified cluster, and makes any clusters or nodes
--   within it be in its root cluster (or the overall graph if
--   required).
deleteCluster      :: GraphID -> DotGraph n -> DotGraph n
deleteCluster c dg = withValues (M.map adjNode)
                     . withClusters (M.map adjCluster . M.delete c)
                     $ dg
  where
    p = parentCluster =<< c `M.lookup` clusters dg

    adjParent p'
      | p' == Just c = p
      | otherwise    = p'

    adjNode ni = ni { _inCluster = adjParent $ _inCluster ni }

    adjCluster ci = ci { parentCluster = adjParent $ parentCluster ci }

-- | Remove clusters with no sub-clusters and no nodes within them.
removeEmptyClusters :: DotGraph n -> DotGraph n
removeEmptyClusters dg = dg { clusters = cM' }
  where
    cM = clusters dg
    cM' = (cM `M.difference` invCs) `M.difference` invNs

    invCs = usedClustsIn $ M.map parentCluster cM
    invNs = usedClustsIn . M.map _inCluster $ values dg

    usedClustsIn = M.fromAscList
                   . map ((,) <$> fst . head <*> map snd)
                   . groupSortBy fst
                   . mapMaybe (uncurry (fmap . flip (,)))
                   . M.assocs

-- -----------------------------------------------------------------------------
-- Information

-- | Does this graph have any nodes?
isEmpty :: DotGraph n -> Bool
isEmpty = M.null . values

-- | Does this graph have any clusters?
hasClusters :: DotGraph n -> Bool
hasClusters = M.null . clusters

-- | Determine if this graph has nodes or clusters.
isEmptyGraph :: DotGraph n -> Bool
isEmptyGraph = liftA2 (&&) isEmpty (not . hasClusters)

graphAttributes :: DotGraph n -> [GlobalAttributes]
graphAttributes = fromGlobAttrs . graphAttrs

-- | Return the ID for the cluster the node is in.
foundInCluster :: (Ord n) => DotGraph n -> n -> Maybe GraphID
foundInCluster dg n = _inCluster $ values dg M.! n

-- | Return the attributes for the node.
attributesOf :: (Ord n) => DotGraph n -> n -> Attributes
attributesOf dg n = _attributes $ values dg M.! n

-- | Predecessor edges for the specified node.  For undirected graphs
--   equivalent to 'adjacentTo'.
predecessorsOf :: (Ord n) => DotGraph n -> n -> [DotEdge n]
predecessorsOf dg t
  | directedGraph dg = emToDE (`DotEdge` t)
                       . _predecessors $ values dg M.! t
  | otherwise        = adjacentTo dg t

-- | Successor edges for the specified node.  For undirected graphs
--   equivalent to 'adjacentTo'.
successorsOf :: (Ord n) => DotGraph n -> n -> [DotEdge n]
successorsOf dg f
  | directedGraph dg = emToDE (DotEdge f)
                       . _successors $ values dg M.! f
  | otherwise        = adjacentTo dg f

-- | All edges involving this node.
adjacentTo :: (Ord n) => DotGraph n -> n -> [DotEdge n]
adjacentTo dg n = sucs ++ preds
  where
    ni = values dg M.! n
    sucs = emToDE (DotEdge n) $ _successors ni
    preds = emToDE (`DotEdge` n) $ n `M.delete` _predecessors ni

emToDE :: (n -> Attributes -> DotEdge n) -> EdgeMap n -> [DotEdge n]
emToDE f = map (uncurry f) . fromMap

-- | Which cluster (or the root graph) is this cluster in?
parentOf :: DotGraph n -> GraphID -> Maybe GraphID
parentOf dg c = parentCluster $ clusters dg M.! c

clusterAttributes :: DotGraph n -> GraphID -> [GlobalAttributes]
clusterAttributes dg c = fromGlobAttrs . clusterAttrs $ clusters dg M.! c

-- -----------------------------------------------------------------------------
-- For DotRepr instance

instance (Ord n) => DotRepr DotGraph n where
  fromCanonical = fromDotRepr

  getID = graphID

  setID i g = g { graphID = Just i }

  graphIsDirected = directedGraph

  setIsDirected d g = g { directedGraph = d }

  graphIsStrict = strictGraph

  setStrictness s g = g { strictGraph = s }

  mapDotGraph = mapNs

  graphStructureInformation = getGraphInfo

  nodeInformation = getNodeInfo

  edgeInformation = getEdgeInfo

  unAnonymise = id -- No anonymous clusters!

instance (Ord n) => G.FromGeneralisedDot DotGraph n where
  fromGeneralised = fromDotRepr

instance (Ord n, PrintDot n) => PrintDotRepr DotGraph n
instance (Ord n, ParseDot n) => ParseDotRepr DotGraph n
instance (Ord n, PrintDot n, ParseDot n) => PPDotRepr DotGraph n

-- | Uses the PrintDot instance for canonical 'C.DotGraph's.
instance (PrintDot n) => PrintDot (DotGraph n) where
  unqtDot = unqtDot . toCanonical

-- | Uses the ParseDot instance for generalised 'G.DotGraph's.
instance (Ord n, ParseDot n) => ParseDot (DotGraph n) where
  parseUnqt = fromGDot <$> parseUnqt
    where
      -- fromGDot :: G.DotGraph n -> DotGraph n
      fromGDot = fromDotRepr . (`asTypeOf` (undefined :: G.DotGraph n))

  parse = parseUnqt -- Don't want the option of quoting

cOptions :: CanonicaliseOptions
cOptions = COpts { edgesInClusters = False
                 , groupAttributes = True
                 }

-- | Convert any existing DotRepr instance to a 'DotGraph'.
fromDotRepr :: (DotRepr dg n) => dg n -> DotGraph n
fromDotRepr = unsafeFromCanonical . canonicaliseOptions cOptions . unAnonymise

-- | Convert a canonical Dot graph to a graph-based one.  This assumes
--   that the canonical graph is the same format as returned by
--   'toCanonical'.  The \"unsafeness\" is that:
--
--   * All clusters must have a unique identifier ('unAnonymise' can
--     be used to make sure all clusters /have/ an identifier, but it
--     doesn't ensure uniqueness).
--
--   * All nodes are assumed to be explicitly listed precisely once.
--
--   * Only edges found in the root graph are considered.
--
--   If this isn't the case, use 'fromCanonical' instead.
--
--   The 'graphToDot' function from "Data.GraphViz" produces output
--   suitable for this function (assuming all clusters are provided
--   with a unique identifier); 'graphElemsToDot' is suitable if all
--   nodes are specified in the input list (rather than just the
--   edges).
unsafeFromCanonical :: (Ord n) => C.DotGraph n -> DotGraph n
unsafeFromCanonical dg = DG { strictGraph   = C.strictGraph dg
                            , directedGraph = dirGraph
                            , graphAttrs    = as
                            , graphID       = mgid
                            , clusters      = cs
                            , values        = ns
                            }
  where
    stmts = C.graphStatements dg
    mgid = C.graphID dg
    dirGraph = C.directedGraph dg

    (as, cs, ns) = fCStmt Nothing stmts

    fCStmt p stmts' = (sgAs, cs', ns')
      where
        sgAs = toGlobAttrs $ C.attrStmts stmts'
        (cs', sgNs) = (M.unions *** M.unions) . unzip
                      . map (fCSG p) $ C.subGraphs stmts'
        nNs = M.fromList . map (fDN p) $ C.nodeStmts stmts'
        ns' = sgNs `M.union` nNs

    fCSG p sg = (M.insert sgid ci cs', ns')
      where
        msgid@(Just sgid) = C.subGraphID sg
        (as', cs', ns') = fCStmt msgid $ C.subGraphStmts sg
        ci = CI p as'

    fDN p (DotNode n as') = ( n
                            , NI { _inCluster    = p
                                 , _attributes   = as'
                                 , _predecessors = eSel n tEs
                                 , _successors   = eSel n fEs
                                 }
                            )

    es = C.edgeStmts stmts
    fEs = toEdgeMap fromNode toNode es
    tEs = delLoops $ toEdgeMap toNode fromNode es
    eSel n es' = fromMaybe M.empty $ n `M.lookup` es'
    delLoops = M.mapWithKey M.delete

toEdgeMap     :: (Ord n) => (DotEdge n -> n) -> (DotEdge n -> n) -> [DotEdge n]
                 -> Map n (EdgeMap n)
toEdgeMap f t = M.map eM . M.fromList . groupSortCollectBy f t'
  where
    t' = liftA2 (,) t edgeAttributes
    eM = M.fromList . groupSortCollectBy fst snd

mapNs :: (Ord n') => (n -> n') -> DotGraph n -> DotGraph n'
mapNs f (DG st d as mid cs vs) = DG st d as mid cs
                                 $ mapNM vs
  where
    mapNM = M.map mapNI . mpM
    mapNI (NI mc as' ps ss) = NI mc as' (mpM ps) (mpM ss)
    mpM = M.mapKeys f

getGraphInfo    :: DotGraph n -> (GlobalAttributes, ClusterLookup)
getGraphInfo dg = (gas, cl)
  where
    toGA = GraphAttrs . unSame
    (gas, cgs) = (toGA *** M.map toGA) $ globAttrMap graphAs dg
    pM = M.map pInit $ clusterPath dg

    cl = M.mapWithKey addPath $ M.mapKeysMonotonic Just cgs

    addPath c as = ( maybeToList $ c `M.lookup` pM
                   , as
                   )

    pInit p = case Seq.viewr p of
                (p' Seq.:> _) -> p'
                _             -> Seq.empty

getNodeInfo             :: Bool -> DotGraph n -> NodeLookup n
getNodeInfo withGlob dg = M.map toLookup ns
  where
    (gGlob, aM) = globAttrMap nodeAs dg
    pM = clusterPath dg

    ns = values dg

    toLookup ni = (pth, as')
      where
        as = _attributes ni
        mp = _inCluster ni
        pth = fromMaybe Seq.empty $ mp `M.lookup` pM
        pAs = fromMaybe gGlob $ (`M.lookup` aM) =<< mp
        as' | withGlob  = unSame $ toSAttr as `S.union` pAs
            | otherwise = as

getEdgeInfo             :: Bool -> DotGraph n -> [DotEdge n]
getEdgeInfo withGlob dg = concatMap (uncurry mkDotEdges) es
  where
    gGlob = edgeAs $ graphAttrs dg

    es = concatMap (uncurry (map . (,)))
         . M.assocs . M.map (M.assocs . _successors)
         $ values dg

    addGlob as
      | withGlob  = unSame $ toSAttr as `S.union` gGlob
      | otherwise = as

    mkDotEdges f (t, ass) = map (DotEdge f t . addGlob) ass

globAttrMap       :: (GlobAttrs -> SAttrs) -> DotGraph n
                     -> (SAttrs, Map GraphID SAttrs)
globAttrMap af dg = (gGlob, aM)
  where
    gGlob = af $ graphAttrs dg

    cs = clusters dg

    aM = M.map attrsFor cs

    attrsFor ci = as `S.union` pAs
      where
        as = af $ clusterAttrs ci
        p = parentCluster ci
        pAs = fromMaybe gGlob $ (`M.lookup` aM) =<< p

clusterPath :: DotGraph n -> Map (Maybe GraphID) St.Path
clusterPath = M.mapKeysMonotonic Just . M.map (fmap Just) . clusterPath'

clusterPath' :: DotGraph n -> Map GraphID (Seq.Seq GraphID)
clusterPath' dg = pM
  where
    cs = clusters dg

    pM = M.mapWithKey pathOf cs

    pathOf c ci = pPth Seq.|> c
      where
        mp = parentCluster ci
        pPth = fromMaybe Seq.empty $ (`M.lookup` pM) =<< mp

-- -----------------------------------------------------------------------------

withValues      :: (NodeMap n -> NodeMap n) -> DotGraph n -> DotGraph n
withValues f dg = dg { values = f $ values dg }

withClusters      :: (Map GraphID ClusterInfo -> Map GraphID ClusterInfo)
                     -> DotGraph n -> DotGraph n
withClusters f dg = dg { clusters = f $ clusters dg }

toGlobAttrs :: [GlobalAttributes] -> GlobAttrs
toGlobAttrs = mkGA . partitionGlobal
  where
    mkGA (ga,na,ea) = GA (toSAttr ga) (toSAttr na) (toSAttr ea)

fromGlobAttrs :: GlobAttrs -> [GlobalAttributes]
fromGlobAttrs (GA ga na ea) = filter (not . null . attrs)
                              [ GraphAttrs $ unSame ga
                              , NodeAttrs  $ unSame na
                              , EdgeAttrs  $ unSame ea
                              ]

type UpdateEdgeMap n = (EdgeMap n -> EdgeMap n) -> NodeInfo n -> NodeInfo n

niSucc      :: UpdateEdgeMap n
niSucc f ni = ni { _successors = f $ _successors ni }

niPred      :: UpdateEdgeMap n
niPred f ni = ni { _predecessors = f $ _predecessors ni }

niSkip      :: UpdateEdgeMap n
niSkip _ ni = ni

toMap :: (Ord n) => [(n, Attributes)] -> EdgeMap n
toMap = M.fromAscList . groupSortCollectBy fst snd

fromMap :: EdgeMap n -> [(n, Attributes)]
fromMap = concatMap (uncurry (map . (,))) . M.toList
