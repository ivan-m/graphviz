{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
   Module      : Data.GraphViz.Testing.Instances.Canonical
   Description : Canonical dot graph instances for Arbitrary.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com
 -}
module Data.GraphViz.Testing.Instances.Canonical where

import Data.GraphViz.Testing.Instances.Common
import Data.GraphViz.Testing.Instances.Helpers

import Data.GraphViz.Internal.Util   (bool)
import Data.GraphViz.Types.Canonical

import Test.QuickCheck

import Control.Monad (liftM2, liftM4)

-- -----------------------------------------------------------------------------
-- Defining Arbitrary instances for the overall types

instance (Eq n, Arbitrary n) => Arbitrary (DotGraph n) where
  arbitrary = liftM4 DotGraph arbitrary arbitrary arbitrary arbitrary

  shrink (DotGraph str dir gid stmts) = map (DotGraph str dir gid)
                                        $ shrink stmts

instance (Eq n, Arbitrary n) => Arbitrary (DotStatements n) where
  arbitrary = sized (arbDS gaGraph True)

  shrink ds@(DotStmts gas sgs ns es) = do gas' <- shrink gas
                                          sgs' <- shrink sgs
                                          ns' <- shrink ns
                                          es' <- shrink es
                                          returnCheck ds
                                            $ DotStmts gas' sgs' ns' es'

-- | If 'True', generate 'DotSubGraph's; otherwise don't.
arbDS              :: (Arbitrary n, Eq n) => Gen GlobalAttributes -> Bool
                      -> Int -> Gen (DotStatements n)
arbDS ga haveSGs s = liftM4 DotStmts (listOf ga) genSGs arbitrary arbitrary
  where
    s' = min s 2
    genSGs = if haveSGs
             then resize s' arbitrary
             else return []

instance (Eq n, Arbitrary n) => Arbitrary (DotSubGraph n) where
  arbitrary = do isClust <- arbitrary
                 let ga = bool gaSubGraph gaClusters isClust
                 liftM2 (DotSG isClust) arbitrary (sized $ arbDS ga False)

  shrink (DotSG isCl mid stmts) = map (DotSG isCl mid) $ shrink stmts
