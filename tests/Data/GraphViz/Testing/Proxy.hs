{-# LANGUAGE KindSignatures #-}

{- |
   Module      : Data.GraphViz.Testing.Proxy
   Description : Proxy implementation
   Copyright   : Matthew Sackman, Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : Ivan.Miljenovic@gmail.com

   Data.Proxy was added to base with GHC 7.8.1, and we want to test
   for older versions than that.

 -}
module Data.GraphViz.Testing.Proxy where

--------------------------------------------------------------------------------

data DGProxy (dg :: * -> *) = DGProxy
  deriving (Eq, Ord, Show, Read)
