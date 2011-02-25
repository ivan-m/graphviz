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
       ( canonicalise
       ) where

import Data.GraphViz.Attributes( Attribute, Attributes
                               , usedByClusters, defaultAttributeValue)
import Data.GraphViz.Attributes.Same
import Data.GraphViz.Types

import Data.Function(on)
import Data.List(groupBy, sortBy, partition, (\\), sort)
import Data.Maybe(listToMaybe, mapMaybe, fromMaybe)
import qualified Data.DList as DList
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Foldable as F
import Control.Arrow(first, second, (***))

-- -----------------------------------------------------------------------------

-- | Implements similar functionality to @dot -Tcanon@.  However, this
--   method requires no IO and doesn't care about image locations, etc.
--
--   This function will create a single explicit definition for every
--   node in the original graph and place it in the appropriate
--   position in the cluster hierarchy.  All edges are found in the
--   deepest cluster that contains both nodes.  Currently node and
--   edge attributes are not grouped into global ones.
canonicalise :: (Ord n, DotRepr dg n) => dg n -> DotGraph n
canonicalise dg = cdg { strictGraph   = graphIsStrict dg
                      , directedGraph = graphIsDirected dg
                      , graphID       = getID dg
                      }
  where
    cdg = createCanonical gas cl nl es

    (gas, cl) = graphStructureInformation dg
    nl = nodeInformation True dg
    es = edgeInformation True dg

createCanonical :: (Ord n) => GlobalAttributes -> ClusterLookup -> NodeLookup n -> [DotEdge n] -> DotGraph n
createCanonical gas cl nl es
  = DotGraph { strictGraph     = undefined
             , directedGraph   = undefined
             , graphID         = undefined
             , graphStatements = gStmts
             }
  where
    gStmts = DotStmts { attrStmts = [gas]
                      , subGraphs = clusts topClustAs topClustAs' clustNs
                      , nodeStmts = topNs
                      , edgeStmts = topEs
                      }

    nUnlook (n,(p,as)) = (F.toList p, DotNode n as)
    ns = sortBy (flip compare `on` fst) . map nUnlook $ Map.toList nl
    (clustNs, topNs) = thisLevel ns
    (clustEL, topEs) = edgeClusters nl es
    topClustAs = filter usedByClusters $ attrs gas
    topClustAs' = toSAttr topClustAs

    clusts oAs oAsS = map (toClust oAs oAsS)
                      . groupBy ((==) `on` (listToMaybe . fst))

    toClust oAs oAsS cns = DotSG { isCluster     = True
                                 , subGraphID    = cID
                                 , subGraphStmts = stmts
                                 }
      where
        cID = head . fst $ head cns
        (nested, here) = thisLevel $ map (first tail) cns
        stmts = DotStmts { attrStmts = [GraphAttrs as']
                         , subGraphs = clusts as asS nested
                         , nodeStmts = here
                         , edgeStmts = edges
                         }
        as = attrs . snd $ cl Map.! cID
        asS = toSAttr as

        as' = subClusterAttributes oAs oAsS as

        edges = fromMaybe [] $ cID `Map.lookup` clustEL

    thisLevel = second (map snd) . span (not . null . fst)

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
          . map (head *** DList.singleton)
    setE e as = e { edgeAttributes = as }

commonAttrs        :: (a -> Attributes) -> [a] -> Set Attribute
commonAttrs _ [] = Set.empty
commonAttrs f xs = foldr1 Set.intersection
                   $ map (Set.fromList . f) xs

subClusterAttributes                    :: Attributes -> SAttrs
                                           -> Attributes -> Attributes
subClusterAttributes outer outerS inner = sort $ inner' ++ override
  where
    -- Remove all Attributes that are also defined in the outer cluster
    inner' = inner \\ outer

    -- Need to consider those Attributes that were defined /after/ this sub-cluster
    override = mapMaybe defAttr . unSame
               $ outerS `Set.difference` toSAttr inner

    -- A version of defaultAttributeValue that returns Nothing if the
    -- value it is replacing /is/ the default.
    defAttr a = case defaultAttributeValue a of
                  Just a' | a == a' -> Nothing
                  ma'               -> ma'
