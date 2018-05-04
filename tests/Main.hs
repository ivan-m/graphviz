{- |
   Module      : Main
   Description : Top-level HSpec runner
   Copyright   : Matthew Sackman, Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : Ivan.Miljenovic@gmail.com

   Used as we want to wrap default QuickCheck configurations.

 -}
module Main where

import qualified Spec
import           Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess)
import           Test.Hspec.Runner     (hspec)

--------------------------------------------------------------------------------

main :: IO ()
main = hspec
       . modifyMaxSuccess (const 200)
       . modifyMaxSize (const 50)
       $ Spec.spec
