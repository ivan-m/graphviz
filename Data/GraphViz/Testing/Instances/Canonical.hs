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

import Data.GraphViz.Testing.Instances.Attributes()
import Data.GraphViz.Testing.Instances.Common()
import Data.GraphViz.Testing.Instances.Helpers

import Data.GraphViz.Types.Canonical

import Test.QuickCheck

import Control.Monad(liftM3, liftM4)

-- -----------------------------------------------------------------------------
-- Defining Arbitrary instances for the overall types

instance (Eq n, Arbitrary n) => Arbitrary (DotGraph n) where
  arbitrary = liftM4 DotGraph arbitrary arbitrary arbitrary arbitrary

  shrink (DotGraph str dir gid stmts) = map (DotGraph str dir gid)
                                        $ shrink stmts

instance (Eq n, Arbitrary n) => Arbitrary (DotStatements n) where
  arbitrary = sized (arbDS True)

  shrink ds@(DotStmts gas sgs ns es) = do gas' <- shrinkL gas
                                          sgs' <- shrinkL sgs
                                          ns' <- shrinkL ns
                                          es' <- shrinkL es
                                          returnCheck ds
                                            $ DotStmts gas' sgs' ns' es'

-- | If 'True', generate 'DotSubGraph's; otherwise don't.
arbDS           :: (Arbitrary n, Eq n) => Bool -> Int -> Gen (DotStatements n)
arbDS haveSGs s = liftM4 DotStmts arbitrary genSGs arbitrary arbitrary
  where
    s' = min s 2
    genSGs = if haveSGs
             then resize s' arbitrary
             else return []

instance (Eq n, Arbitrary n) => Arbitrary (DotSubGraph n) where
  arbitrary = liftM3 DotSG arbitrary arbitrary (sized $ arbDS False)

  shrink (DotSG isCl mid stmts) = map (DotSG isCl mid) $ shrink stmts
