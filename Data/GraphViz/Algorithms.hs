{- |
   Module      : Data.GraphViz.Algorithms
   Description : Various algorithms on Graphviz graphs.
   Copyright   : (c) Matthew Sackman, Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   Defines various algorithms for use on 'DotRepr' graphs.  These are
   typically re-implementations of behaviour found in existing Graphviz
   tools but without the I/O requirement.
 -}
module Data.GraphViz.Algorithms
       ( -- * Canonicalisation Options
         -- $options
         CanonicaliseOptions(..)
       , defaultCanonOptions
       , dotLikeOptions
         -- * Canonicalisation
       , canonicalise
       , canonicaliseOptions
         -- * Dealing with transitive edges
       , transitiveReduction
       , transitiveReductionOptions
       ) where

import Data.GraphViz.Attributes.Complete( Attributes, usedByClusters
                                        , defaultAttributeValue)
import Data.GraphViz.Attributes.Same
import Data.GraphViz.Types
import Data.GraphViz.Types.Canonical

import Data.Function(on)
import Data.List(groupBy, sortBy, partition, (\\), sort, deleteBy)
import Data.Maybe(listToMaybe, mapMaybe, fromMaybe)
import qualified Data.DList as DList
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Foldable as F
import Control.Arrow(first, second, (***))
import Control.Monad(unless)
import Control.Monad.Trans.State

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

-- | Implements similar functionality to @dot -Tcanon@.  However, this
--   method requires no IO and doesn't care about image locations, etc.
--
--   This function will create a single explicit definition for every
--   node in the original graph and place it in the appropriate
--   position in the cluster hierarchy.  All edges are found in the
--   deepest cluster that contains both nodes.  Currently node and
--   edge attributes are not grouped into global ones.
canonicalise :: (DotRepr dg n) => dg n -> DotGraph n
canonicalise = canonicaliseOptions defaultCanonOptions

-- | As with 'canonicalise', but allow custom 'CanonicaliseOptions'.
canonicaliseOptions :: (DotRepr dg n) => CanonicaliseOptions
                       -> dg n -> DotGraph n
canonicaliseOptions opts dg = cdg { strictGraph   = graphIsStrict dg
                                  , directedGraph = graphIsDirected dg
                                  , graphID       = getID dg
                                  }
  where
    cdg = createCanonical opts gas cl nl es

    (gas, cl) = graphStructureInformation dg
    nl = nodeInformation True dg
    es = edgeInformation True dg

createCanonical :: (Ord n) => CanonicaliseOptions -> GlobalAttributes
                   -> ClusterLookup -> NodeLookup n -> [DotEdge n] -> DotGraph n
createCanonical opts gas cl nl es
  = DotGraph { strictGraph     = undefined
             , directedGraph   = undefined
             , graphID         = undefined
             , graphStatements = gStmts
             }
  where
    gStmts = DotStmts { attrStmts = gas'
                      , subGraphs = sgs
                      , nodeStmts = topNs'
                      , edgeStmts = topEs'
                      }

    gas' = nonEmptyGAs [ gas
                       , NodeAttrs topNAs
                       , EdgeAttrs topEAs
                       ]
    nUnlook (n,(p,as)) = (F.toList p, DotNode n as)
    -- DotNodes paired and sorted by their paths
    ns = sortBy (compLists `on` fst) . map nUnlook $ Map.toList nl
    -- nodes in clusters vs top-level
    (clustNs, topNs) = thisLevel ns
    -- edges in clusters vs top-level
    (clustEL, topEs) = if edgesInClusters opts
                       then edgeClusters nl es
                       else (Map.empty, es)

    -- The global attributes that are also applicable to clusters.
    topClustAs = filter usedByClusters $ attrs gas
    topClustAs' = toSAttr topClustAs

    topNAs = mCommon nodeAttributes topNs
    topNAs' = toSAttr topNAs
    topNs' = map (\dn -> dn {nodeAttributes = nodeAttributes dn \\ topNAs}) topNs

    topEAs = mCommon edgeAttributes topEs
    topEAs' = toSAttr topEAs
    topEs' = map (\de -> de {edgeAttributes = edgeAttributes de \\ topEAs}) topEs

    sgs = clusts topClustAs topClustAs' topNAs topNAs' topEAs topEAs' clustNs

    clusts oAs oAsS nAs nAsS eAs eAsS = map (toClust oAs oAsS nAs nAsS eAs eAsS)
                                        . groupBy ((==) `on` (listToMaybe . fst))

    -- Create a new cluster.
    -- oAs - outer cluster attributes
    -- nAs - outer node attributes
    -- eAs - outer edge attributes
    -- (*S variants are same thing but as sets; premature optimisation?)
    -- cns - nodes in this cluster
    toClust oAs oAsS nAs nAsS eAs eAsS cns
      = DotSG { isCluster     = True
              , subGraphID    = cID
              , subGraphStmts = stmts
              }
      where
        -- Find the ID for this cluster.
        cID = head . fst $ head cns

        -- Get the nodes that are deeper and the ones that belong
        -- here.
        (nested, here) = thisLevel $ map (first tail) cns


        stmts = DotStmts { attrStmts = sgAs
                         , subGraphs = subSGs
                         , nodeStmts = here'
                         , edgeStmts = edges'
                         }

        -- Starting global attributes
        sgAs = nonEmptyGAs [ GraphAttrs as'
                           , NodeAttrs nas'
                           , EdgeAttrs eas'
                           ]

        -- Sub-clusters
        subSGs = clusts as asS nas nasS eas easS nested

        -- The attributes attached to this cluster ID in the original.
        as = attrs . snd $ cl Map.! cID
        asS = toSAttr as

        -- Get the global attributes that apply to this cluster,
        -- ignoring ones set globally.
        as' = fst $ innerAttributes oAs oAsS as

        -- The node attributes that can be stated globally.
        nas = mCommon nodeAttributes here
        nasS = toSAttr nas
        (nas',nOv) = innerAttributes nAs nAsS nas

        -- The nodes that belong here, with updated attributes.
        here' = map (\dn -> dn {nodeAttributes = nodeAttributes dn \\ (nas ++ nOv)}) here

        eas = mCommon edgeAttributes edges
        easS = toSAttr eas
        (eas',eOv) = innerAttributes eAs eAsS eas
        edges' = map (\de -> de {edgeAttributes = edgeAttributes de \\ (eas ++ eOv)}) edges

        -- Find edges that belong here
        edges = fromMaybe [] $ cID `Map.lookup` clustEL

    thisLevel = second (map snd) . span (not . null . fst)

    mCommon f = if groupAttributes opts
                then commonAttrs f
                else const []

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

-- Return all attributes found in every value.
commonAttrs         :: (a -> Attributes) -> [a] -> Attributes
commonAttrs _ []  = []
commonAttrs f [a] = f a
commonAttrs f xs  = Set.toList . foldr1 Set.intersection
                    $ map (Set.fromList . f) xs

-- Assign each edge into the cluster it belongs in.
edgeClusters    :: (Ord n) => NodeLookup n -> [DotEdge n]
                   -> (Map (Maybe GraphID) [DotEdge n], [DotEdge n])
edgeClusters nl = (toM *** map snd) . partition (not . null . fst)
                  . map inClust
  where
    nl' = Map.map (F.toList . fst) nl
    inClust de@(DotEdge n1 n2 _) = (flip (,) de)
                                   . map fst . takeWhile (uncurry (==))
                                   $ zip (nl' Map.! n1) (nl' Map.! n2)
    toM = Map.map DList.toList
          . Map.fromListWith (flip DList.append)
          . map (last *** DList.singleton)

-- Return only those attributes that are required within the inner
-- sub-graph.  Also returns the overrides.
innerAttributes                    :: Attributes -> SAttrs
                                      -> Attributes -> (Attributes, Attributes)
innerAttributes outer outerS inner = (sort $ inner' ++ override, override)
  where
    -- Remove all Attributes that are also defined in the outer cluster
    inner' = inner \\ outer

    -- Need to consider those Attributes that were defined /after/ this value
    override = mapMaybe defAttr . unSame
               $ outerS `Set.difference` toSAttr inner

    -- A version of defaultAttributeValue that returns Nothing if the
    -- value it is replacing /is/ the default.
    defAttr a = case defaultAttributeValue a of
                  Just a' | a == a' -> Nothing
                  ma'               -> ma'

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
 -}

transitiveReduction :: (DotRepr dg n) => dg n -> DotGraph n
transitiveReduction = transitiveReductionOptions defaultCanonOptions

transitiveReductionOptions         :: (DotRepr dg n) => CanonicaliseOptions
                                      -> dg n -> DotGraph n
transitiveReductionOptions opts dg = cdg { strictGraph = graphIsStrict dg
                                         , directedGraph = graphIsDirected dg
                                         , graphID = getID dg
                                         }
  where
    cdg = createCanonical opts gas cl nl es'
    (gas, cl) = graphStructureInformation dg
    nl = nodeInformation True dg
    es = edgeInformation True dg
    es' | graphIsDirected dg = rmTransEdges es
        | otherwise          = es

rmTransEdges    :: (Ord n) => [DotEdge n] -> [DotEdge n]
rmTransEdges [] = []
rmTransEdges es = concatMap (map snd . outgoing) $ Map.elems esM
  where
    tes = tagEdges es

    esMS = do edgeGraph tes
              ns <- getsMap Map.keys
              mapM_ (traverse zeroTag) ns

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
traverse     :: (Ord n) => Tag -> n -> TagState n ()
traverse t n = do setMark True
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
                               $ traverse t' n'
