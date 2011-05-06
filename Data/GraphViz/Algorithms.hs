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
canonicalise :: (Ord n, DotRepr dg n) => dg n -> DotGraph n
canonicalise = canonicaliseOptions defaultCanonOptions

-- | As with 'canonicalise', but allow custom 'CanonicaliseOptions'.
canonicaliseOptions :: (Ord n, DotRepr dg n) => CanonicaliseOptions
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
    ns = sortBy (flip compare `on` fst) . map nUnlook $ Map.toList nl
    (clustNs, topNs) = thisLevel ns
    (clustEL, topEs) = if edgesInClusters opts
                       then edgeClusters nl es
                       else (Map.empty, es)
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

    toClust oAs oAsS nAs nAsS eAs eAsS cns
      = DotSG { isCluster     = True
              , subGraphID    = cID
              , subGraphStmts = stmts
              }
      where
        cID = head . fst $ head cns
        (nested, here) = thisLevel $ map (first tail) cns
        stmts = DotStmts { attrStmts = sgAs
                         , subGraphs = subSGs
                         , nodeStmts = here'
                         , edgeStmts = edges'
                         }

        sgAs = nonEmptyGAs [ GraphAttrs as'
                           , NodeAttrs nas'
                           , EdgeAttrs eas'
                           ]

        subSGs = clusts as asS nas nasS eas easS nested

        as = attrs . snd $ cl Map.! cID
        asS = toSAttr as
        as' = innerAttributes oAs oAsS as

        nas = mCommon nodeAttributes here
        nasS = toSAttr nas
        nas' = innerAttributes nAs nAsS nas
        here' = map (\dn -> dn {nodeAttributes = nodeAttributes dn \\ nas}) here

        eas = mCommon edgeAttributes edges
        easS = toSAttr eas
        eas' = innerAttributes eAs eAsS eas
        edges' = map (\de -> de {edgeAttributes = edgeAttributes de \\ eas}) edges

        edges = fromMaybe [] $ cID `Map.lookup` clustEL

    thisLevel = second (map snd) . span (not . null . fst)

    mCommon f = if groupAttributes opts
                then commonAttrs f
                else const []

nonEmptyGAs :: [GlobalAttributes] -> [GlobalAttributes]
nonEmptyGAs = filter (not . null . attrs)

commonAttrs         :: (a -> Attributes) -> [a] -> Attributes
commonAttrs _ []  = []
commonAttrs _ [_] = []
commonAttrs f xs  = Set.toList . foldr1 Set.intersection
                    $ map (Set.fromList . f) xs

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
    setE e as = e { edgeAttributes = as }

innerAttributes                    :: Attributes -> SAttrs
                                      -> Attributes -> Attributes
innerAttributes outer outerS inner = sort $ inner' ++ override
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
