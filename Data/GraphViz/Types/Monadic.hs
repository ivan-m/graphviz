{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{- |
   Module      : Data.GraphViz.Types.Monadic
   Description : A monadic interface for making Dot graphs.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

 -}
module Data.GraphViz.Types.Monadic where

import Data.GraphViz.Attributes(Attribute,Attributes)
import Data.GraphViz.Types.Common
import qualified Data.GraphViz.Types.Generalised as G
import Data.GraphViz.Types.Generalised -- (DotGraph)

import qualified Data.DList as DL
import Data.DList(DList)
import Data.Function (on)
import qualified Data.Sequence as Seq
import Data.Sequence(Seq, (><))

-- -----------------------------------------------------------------------------
-- The Dot monad.

-- | The monadic representation of a Dot graph.
type Dot n = DotM n ()

newtype DotM n a = DotM { runDot :: (a, DotStmts n) }

execDot :: DotM n a -> DotStmts n
execDot = snd . runDot

instance Monad (DotM n) where
  return a = DotM (a, DL.empty)

  dt >>= f = DotM
             $ let ~(a,stmts)  = runDot dt
                   ~(b,stmts') = runDot $ f a
               in (b, stmts `DL.append` stmts')

tell :: DotStmts n -> Dot n
tell = DotM . (,) ()

tellStmt :: DotStmt n -> Dot n
tellStmt = tell . DL.singleton

-- -----------------------------------------------------------------------------
-- Creating the DotGraph

-- | Create a directed dot graph with the specified graph ID.
digraph :: GraphID -> DotM n a -> DotGraph n
digraph = mkGraph True . Just

-- | Create a directed dot graph with no graph ID.
digraph' :: DotM n a -> DotGraph n
digraph' = mkGraph True Nothing

-- | Create a undirected dot graph with the specified graph ID.
graph :: GraphID -> DotM n a -> DotGraph n
graph = mkGraph False . Just

-- | Create a undirected dot graph with no graph ID.
graph' :: DotM n a -> DotGraph n
graph' = mkGraph False Nothing

mkGraph :: Bool -> Maybe GraphID -> DotM n a -> DotGraph n
mkGraph isDir mid dot = DotGraph { strictGraph     = False
                                 , directedGraph   = isDir
                                 , graphID         = mid
                                 , graphStatements = stmts
                                 }
  where
    stmts = convertStatements $ execDot dot

-- -----------------------------------------------------------------------------
-- Statements

type DotStmts n = DList (DotStmt n)

convertStatements :: DotStmts n -> DotStatements n
convertStatements = Seq.fromList . map convertStatement . DL.toList

data DotStmt n = MA GlobalAttributes
               -- | MS (DotSubGraph n)
               | MN (DotNode n)
               | ME (DotEdge n)
               deriving (Eq, Ord, Show, Read)

convertStatement          :: DotStmt n -> DotStatement n
convertStatement (MA gas) = GA gas
convertStatement (MN dn)  = DN dn
convertStatement (ME de)  = DE de

-- -----------------------------------------------------------------------------
-- Global Attributes

-- | Add graph/sub-graph/cluster attributes.
graphAttrs :: Attributes -> Dot n
graphAttrs = tellStmt . MA . GraphAttrs

-- | Add global node attributes.
nodeAttrs :: Attributes -> Dot n
nodeAttrs = tellStmt . MA . NodeAttrs

-- | Add global edge attributes
edgeAttrs :: Attributes -> Dot n
edgeAttrs = tellStmt . MA . EdgeAttrs

-- | An optional \"attribute creation\" operator, equivalent to
--   @('$')@.
(=:=) :: (a -> Attribute) -> a -> Attribute
(=:=) = ($)

-- -----------------------------------------------------------------------------
-- Nodes

-- | Add a node to the graph.
node   :: n -> Attributes -> Dot n
node n = tellStmt . MN . DotNode n

-- | Add a node with no attributes to the graph.
node' :: n -> Dot n
node' = flip node []

-- | A dummy instance; do not use.
instance Show (DotM n ()) where
  show = const "Node creation"

-- | A dummy instance; do not use.
instance (Num n) => Eq (DotM n ()) where
  _ == _ = False

-- | Defined solely for the 'fromInteger' method; all other methods
--   throw an error.
instance (Num n) => Num (DotM n ()) where
  fromInteger = flip node [] . fromInteger

  (+) = error "Not valid for Dot monad"
  (*) = error "Not valid for Dot monad"
  abs = error "Not valid for Dot monad"
  signum = error "Not valid for Dot monad"

-- | A dummy instance; do not use.
instance Show (Attributes -> DotM n ()) where
  show = const "Node creation"

-- | A dummy instance; do not use.
instance (Num n) => Eq (Attributes -> DotM n ()) where
  _ == _ = False

-- | Defined solely for the 'fromInteger' method; all other methods
--   throw an error.
instance (Num n) => Num (Attributes -> DotM n ()) where
  fromInteger = node . fromInteger

  (+) = error "Not valid for Dot monad"
  (*) = error "Not valid for Dot monad"
  abs = error "Not valid for Dot monad"
  signum = error "Not valid for Dot monad"

-- -----------------------------------------------------------------------------
-- Edges

newtype EdgePairs n = EP { fromEP :: (n, DList (n,n)) }

class FromRight end n where
  fromEnd :: end -> EdgePairs n

instance FromRight n n where
  fromEnd n = EP (n, DL.empty)

instance FromRight (EdgePairs n) n where
  fromEnd = id

class EdgeResult er n where
  toER :: EdgePairs n -> er

-- | Add one or more edges to the Dot graph.
(-->) :: (FromRight n' n) => n -> n' -> EdgePairs n
f --> t = ep'
  where
    EP (t',ns) = fromEnd t
    ep' = EP (f, (f,t') `DL.cons` ns)

infixr 9 -->

-- | An alias for '-->' to make edges look more undirected.
(<->) :: (FromRight n' n) => n -> n' -> EdgePairs n
(<->) = (-->)

infixr 9 <->

($$) :: EdgePairs n -> Attributes -> Dot n
(EP (_,ns)) $$ as = tell $ DL.map (ME . mkEdge) ns
    where
      mkEdge (n1,n2) = DotEdge n1 n2 as

infix 0 $$

-- -----------------------------------------------------------------------------

