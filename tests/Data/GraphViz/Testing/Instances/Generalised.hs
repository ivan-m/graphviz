{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
   Module      : Data.GraphViz.Testing.Instances.Generalised
   Description : Generalised dot graph instances for Arbitrary.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com
 -}
module Data.GraphViz.Testing.Instances.Generalised where

import Data.GraphViz.Testing.Instances.Attributes ()
import Data.GraphViz.Testing.Instances.Common
import Data.GraphViz.Testing.Instances.Helpers    ()

import Data.GraphViz.Internal.Util     (bool)
import Data.GraphViz.Types.Generalised

import Test.QuickCheck

import           Control.Monad (liftM, liftM2, liftM4)
import qualified Data.Sequence as Seq

-- -----------------------------------------------------------------------------
-- Defining Arbitrary instances for the generalised types

instance (Arbitrary n) => Arbitrary (DotGraph n) where
  arbitrary = liftM4 DotGraph arbitrary arbitrary arbitrary genDStmts

  shrink (DotGraph str dir gid stmts) = map (DotGraph str dir gid)
                                          $ shrinkDStmts stmts

genDStmts :: (Arbitrary n) => Gen (DotStatements n)
genDStmts = sized (arbDS gaGraph True)

genDStmt :: (Arbitrary n) => Gen GlobalAttributes -> Bool
            -> Gen (DotStatement n)
genDStmt ga haveSGs = frequency
                      . bool id ((1,genSG):) haveSGs
                      $ [ (3, liftM GA ga)
                        , (5, liftM DN arbitrary)
                        , (7, liftM DE arbitrary)
                        ]
  where
    genSG = liftM SG arbitrary

shrinkDStmts :: (Arbitrary n) => DotStatements n -> [DotStatements n]
shrinkDStmts gds
  | len == 1  = map Seq.singleton . shrink $ Seq.index gds 0
  | otherwise = [gds1, gds2]
    where
      len = Seq.length gds
      -- Halve the sequence
      (gds1, gds2) = (len `div` 2) `Seq.splitAt` gds

instance (Arbitrary n) => Arbitrary (DotStatement n) where
  arbitrary = genDStmt gaGraph True

  shrink (GA ga) = map GA $ shrink ga
  shrink (SG sg) = map SG $ shrink sg
  shrink (DN dn) = map DN $ shrink dn
  shrink (DE de) = map DE $ shrink de

-- | If 'True', generate 'GDotSubGraph's; otherwise don't.
arbDS              :: (Arbitrary n) => Gen GlobalAttributes -> Bool -> Int -> Gen (DotStatements n)
arbDS ga haveSGs s = liftM Seq.fromList . resize s' . listOf $ genDStmt ga haveSGs
  where
    s' = min s 10


instance (Arbitrary n) => Arbitrary (DotSubGraph n) where
  arbitrary = do isClust <- arbitrary
                 let ga = bool gaSubGraph gaClusters isClust
                 liftM2 (DotSG isClust) arbitrary (sized $ arbDS ga False)

  shrink (DotSG isCl mid stmts) = map (DotSG isCl mid) $ shrinkDStmts stmts
