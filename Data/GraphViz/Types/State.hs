{-# OPTIONS_HADDOCK hide #-}

{- |
   Module      : Data.GraphViz.Types.State
   Description : Create lookups for 'Attribute's.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module provides functions to assist with building 'Attribute'
   lookups.
-}
module Data.GraphViz.Types.State
       ( Path
       , recursiveCall
         --
       , GraphState
       , ClusterLookup
       , getGraphInfo
       , addSubGraph
       , addGraphGlobals
         --
       , NodeState
       , NodeLookup
       , getNodeLookup
       , toDotNodes
       , addNodeGlobals
       , addNode
       , addEdgeNodes
         --
       , EdgeState
       , getDotEdges
       , addEdgeGlobals
       , addEdge
       ) where

import Data.GraphViz.Types.Common
import Data.GraphViz.Attributes(Attributes, Attribute, sameAttribute)

import Data.Function(on)
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Sequence as Seq
import Data.Sequence(Seq, (|>), ViewL(..))
import Control.Arrow((&&&))
import Control.Monad(when)
import Control.Monad.Trans.State

-- -----------------------------------------------------------------------------

type GVState s a = State (StateValue s) a

data StateValue a = SV { globalAttrs :: SAttrs
                       , useGlobals  :: Bool
                       , globalPath  :: Path
                       , value       :: a
                       }
                  deriving (Eq, Ord, Show, Read)

type Path = Seq (Maybe GraphID)

modifyGlobal   :: (SAttrs -> SAttrs) -> GVState s ()
modifyGlobal f = modify f'
  where
    f' sv@(SV{globalAttrs = gas}) = sv{globalAttrs = f gas}

modifyValue   :: (s -> s) -> GVState s ()
modifyValue f = modify f'
  where
    f' sv@(SV{value = s}) = sv{value = f s}

addGlobals    :: Attributes -> GVState s ()
addGlobals as = do addG <- gets useGlobals
                   when addG $ modifyGlobal (`unionWith` as)

getGlobals :: GVState s SAttrs
getGlobals = gets globalAttrs

getPath :: GVState s Path
getPath = gets globalPath

modifyPath   :: (Path -> Path) -> GVState s ()
modifyPath f = modify f'
  where
    f' sv@(SV{globalPath = p}) = sv{globalPath = f p}

-- When calling recursively, back-up and restore the global attrs
-- since they shouldn't change.
--
-- Outer Maybe: Nothing for subgraphs, Just for clusters
recursiveCall      :: Maybe (Maybe GraphID) -> GVState s () -> GVState s ()
recursiveCall mc s = do gas <- getGlobals
                        p   <- getPath
                        maybe (return ()) (modifyPath . flip (|>)) mc
                        s
                        modifyGlobal (const gas)
                        modifyPath (const p)

-- -----------------------------------------------------------------------------

-- | Defined as a wrapper around 'Attribute' where equality is based
--   solely upon the constructor, not the contents.
newtype SameAttr = SA { getAttr :: Attribute }
                 deriving (Show, Read)

instance Eq SameAttr where
  (==) = sameAttribute `on` getAttr

instance Ord SameAttr where
  compare sa1 sa2
    | sa1 == sa2 = EQ
    | otherwise  = (compare `on` getAttr) sa1 sa2

type SAttrs = Set SameAttr

toSAttr :: Attributes -> SAttrs
toSAttr = Set.fromList . map SA

unionWith        :: SAttrs -> Attributes -> SAttrs
unionWith sas as = Set.fromList (map SA as) `Set.union` sas

unSame :: SAttrs -> Attributes
unSame = map getAttr . Set.toList

-- -----------------------------------------------------------------------------
-- Dealing with sub-graphs

type GraphState a = GVState ClusterLookup' a

-- | The available information for each cluster; the @['Path']@
--   denotes all locations where that particular cluster is located
--   (more than one location can indicate possible problems).
type ClusterLookup = Map (Maybe GraphID) (GlobalAttributes, [Path])

type ClusterLookup' = Map (Maybe GraphID) ClusterInfo

data ClusterInfo = CI { parents :: DList Path
                      , cAttrs  :: SAttrs
                      }

getGraphInfo :: GraphState a -> (GlobalAttributes, ClusterLookup)
getGraphInfo = (toGlobal . globalAttrs &&& convert . value)
               . flip execState initState
  where
    convert = Map.map (\(CI ps as) -> (toGlobal as, uniq $ toList ps))
    toGlobal = GraphAttrs . unSame
    initState = SV Set.empty True Seq.empty Map.empty
    uniq = Set.toList . Set.fromList

mergeCInfos                         :: ClusterInfo -> ClusterInfo -> ClusterInfo
mergeCInfos (CI p1 as1) (CI p2 as2) = CI (p1 `append` p2) (as1 `Set.union` as2)

addCluster                 :: Maybe (Maybe GraphID) -> Path -> SAttrs
                              -> GraphState ()
addCluster Nothing    _ _  = return ()
addCluster (Just gid) p as = modifyValue $ Map.insertWith mergeCInfos gid ci
  where
    ci = CI (singleton p) as

-- Use this instead of recursiveCall
addSubGraph           :: Maybe (Maybe GraphID) -> GraphState a -> GraphState ()
addSubGraph mid cntns = do pth <- getPath -- Want path before we add it...
                           recursiveCall mid $ do cntns
                                                  -- But want attrs after we
                                                  -- finish it.
                                                  gas <- getGlobals
                                                  addCluster mid pth gas

addGraphGlobals                :: GlobalAttributes -> GraphState ()
addGraphGlobals (GraphAttrs as) = addGlobals as
addGraphGlobals _              = return ()

-- -----------------------------------------------------------------------------
-- Dealing with DotNodes

-- | The available information on each 'DotNode' (both explicit and implicit).
type NodeLookup n = Map n (Path, SAttrs)

type NodeLookup' n = Map n NodeInfo

data NodeInfo = NI { atts :: SAttrs
                   , gAtts :: SAttrs -- from globals
                   , location :: Path
                   }
              deriving (Eq, Ord, Show, Read)

type NodeState n a = GVState (NodeLookup' n) a

toDotNodes :: (Ord n) => NodeLookup n -> [DotNode n]
toDotNodes = map (\(n,(_,as)) -> DotNode n $ unSame as) . Map.assocs

getNodeLookup       :: (Ord n) => Bool -> NodeState n a -> NodeLookup n
getNodeLookup addGs = Map.map combine . value . flip execState initState
  where
    initState = SV Set.empty addGs Seq.empty Map.empty
    combine ni = (location ni, atts ni `Set.union` gAtts ni)

-- New -> Old -> Inserted
--
-- For specific attributes, newer one takes precedence; for global
-- attributes and path, older one takes precedence.
mergeNInfos :: NodeInfo -> NodeInfo -> NodeInfo
mergeNInfos (NI a1 ga1 p1) (NI a2 ga2 p2) = NI (a1 `Set.union` a2)
                                                -- old one takes precendence
                                               (ga2 `Set.union` ga1)
                                                -- old one takes precendence
                                               (mergePs p2 p1)

-- | If one 'Path' is a prefix of another, then take the longer one;
--   otherwise, take the first 'Path'.
mergePs :: Path -> Path -> Path
mergePs p1 p2 = mrg' p1 p2
  where
    mrg' = mrg `on` Seq.viewl
    mrg EmptyL      _           = p2
    mrg _           EmptyL      = p1
    mrg (c1 :< p1') (c2 :< p2')
      | c1 == c2                = mrg' p1' p2'
      | otherwise               = p1

addNodeGlobals                :: GlobalAttributes -> NodeState n ()
addNodeGlobals (NodeAttrs as) = addGlobals as
addNodeGlobals _              = return ()

mergeNode            :: (Ord n) => n -> Attributes -> SAttrs -> Path
                        -> NodeState n ()
mergeNode n as gas p = modifyValue $ Map.insertWith mergeNInfos n ni
  where
    ni = NI (toSAttr as) gas p

addNode                :: (Ord n) => DotNode n -> NodeState n ()
addNode (DotNode n as) = do gas <- getGlobals
                            p <- getPath
                            -- insertWith takes func (new -> old -> inserted)
                            mergeNode n as gas p

addEdgeNodes :: (Ord n) => DotEdge n -> NodeState n ()
addEdgeNodes (DotEdge f t _ _) = do gas <- getGlobals
                                    p <- getPath
                                    addEN f gas p
                                    addEN t gas p
  where
    addEN n = mergeNode n []

-- -----------------------------------------------------------------------------
-- Dealing with DotEdges

type EdgeState n a = GVState (DList (DotEdge n)) a

getDotEdges       :: Bool -> EdgeState n a -> [DotEdge n]
getDotEdges addGs = toList . value . flip execState initState
  where
    initState = SV Set.empty addGs Seq.empty empty

addEdgeGlobals                :: GlobalAttributes -> EdgeState n ()
addEdgeGlobals (EdgeAttrs as) = addGlobals as
addEdgeGlobals _              = return ()

addEdge :: DotEdge n -> EdgeState n ()
addEdge de@DotEdge{edgeAttributes = as}
  = do gas <- getGlobals
       let de' = de { edgeAttributes = unSame $ unionWith gas as }
       modifyValue $ snoc de'

-- -----------------------------------------------------------------------------

type DList a = [a] -> [a]

snoc :: a -> DList a -> DList a
snoc a da = da . (a:)

empty :: DList a
empty = id

toList :: DList a -> [a]
toList = ($[])

append       :: DList a -> DList a -> DList a
append xs ys = xs . ys

singleton :: a -> DList a
singleton = (:)
