{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
   Module      : Data.GraphViz.Testing.Instances.Graph
   Description : Graph dot graph instances for Arbitrary.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com
 -}
module Data.GraphViz.Testing.Instances.Graph where

-- import Data.GraphViz.Testing.Instances.FGL()
-- import Data.GraphViz.Testing.Instances.Attributes()
import Data.GraphViz.Testing.Instances.Canonical()
-- import Data.GraphViz.Testing.Instances.Common

import Data.GraphViz.Types.Graph
import Data.GraphViz.Types(fromCanonical) {-
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete(sameAttribute) -}
import Data.GraphViz.Algorithms(canonicalise)
import Data.GraphViz.Printing(PrintDot)
import Data.GraphViz.Parsing(ParseDot)

import Test.QuickCheck

-- import Data.Graph.Inductive.Tree(Gr)
import Control.Monad(liftM)

-- -----------------------------------------------------------------------------

instance (Arbitrary n, Ord n, PrintDot n, ParseDot n)
         => Arbitrary (DotGraph n) where
  arbitrary = liftM fromCanonical arbitrary

  shrink = map fromCanonical . shrink . canonicalise

{-
  arbitrary = do g <- arbitrary :: Gen (Gr Char Int)
                 gAs <- listOf gaGraph
                 nAs <- arbNonLabel
                 eAs <- arbNonLabel

                 let params = Params { isDirected       = True
                                     , globalAttributes = gAs
                                     , clusterBy        = clustBy
                                     , clusterID        = Int
                                     , fmtCluster       = const []
                                     , fmtNode          = addLabel nAs . snd
                                     , fmtEdge          = \ (_,_,l) -> addLabel eAs l
                                     }
                 return $ graphToDot params g
    where
      clustBy (n,l) = C (n`mod`2) . C (n`mod`4 + 2) $ N (n,l)


arbNonLabel :: Gen Attributes
arbNonLabel = liftM deleteLabel arbitrary

addLabel      :: (Labellable l) => Attributes -> l -> Attributes
addLabel as l = toLabel l : as

deleteLabel :: Attributes -> Attributes
deleteLabel = filter (not . sameAttribute (toLabel 'a'))
-}
