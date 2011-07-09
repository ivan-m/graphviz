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

import Data.GraphViz.Testing.Instances.Attributes()
import Data.GraphViz.Testing.Instances.Common()
import Data.GraphViz.Testing.Instances.Helpers

import Data.GraphViz.Types.Generalised

import Test.QuickCheck

import qualified Data.Sequence as Seq
import Control.Monad(liftM, liftM3, liftM4)

-- -----------------------------------------------------------------------------
-- Defining Arbitrary instances for the generalised types

instance (Eq n, Arbitrary n) => Arbitrary (DotGraph n) where
  arbitrary = liftM4 DotGraph arbitrary arbitrary arbitrary genGDStmts

  shrink (DotGraph str dir gid stmts) = map (DotGraph str dir gid)
                                          $ shrinkGDStmts stmts

genGDStmts :: (Eq n, Arbitrary n) => Gen (DotStatements n)
genGDStmts = sized (arbGDS True)

shrinkGDStmts :: (Eq n, Arbitrary n) => DotStatements n -> [DotStatements n]
shrinkGDStmts gds
  | len == 1  = map Seq.singleton . shrink $ Seq.index gds 0
  | otherwise = [gds1, gds2]
    where
      len = Seq.length gds
      -- Halve the sequence
      (gds1, gds2) = (len `div` 2) `Seq.splitAt` gds

instance (Eq n, Arbitrary n) => Arbitrary (DotStatement n) where
  -- Don't want as many sub-graphs as nodes, etc.
  arbitrary = frequency [ (3, liftM GA arbitrary)
                        , (1, liftM SG arbitrary)
                        , (5, liftM DN arbitrary)
                        , (7, liftM DE arbitrary)
                        ]

  shrink (GA ga) = map GA $ shrink ga
  shrink (SG sg) = map SG $ shrink sg
  shrink (DN dn) = map DN $ shrink dn
  shrink (DE de) = map DE $ shrink de

-- | If 'True', generate 'GDotSubGraph's; otherwise don't.
arbGDS           :: (Arbitrary n, Eq n) => Bool -> Int -> Gen (DotStatements n)
arbGDS haveSGs s = liftM (Seq.fromList . checkSGs) (resize s' arbList)
  where
    checkSGs = if haveSGs
               then id
               else filter notSG
    notSG SG{} = False
    notSG _      = True

    s' = min s 10


instance (Eq n, Arbitrary n) => Arbitrary (DotSubGraph n) where
  arbitrary = liftM3 DotSG arbitrary arbitrary (sized $ arbGDS False)

  shrink (DotSG isCl mid stmts) = map (DotSG isCl mid) $ shrinkGDStmts stmts
