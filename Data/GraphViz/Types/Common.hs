{-# OPTIONS_HADDOCK hide #-}

{- |
   Module      : Data.GraphViz.Types.Common
   Description : Common internal functions for dealing with overall types.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module provides common functions used by both
   "Data.GraphViz.Types" as well as "Data.GraphViz.Types.Generalised".
-}
module Data.GraphViz.Types.Common where

import Data.GraphViz.Parsing
import Data.GraphViz.Printing
import Data.GraphViz.Util
import Data.GraphViz.Attributes(URL)

import Data.Maybe(isJust)
import Control.Monad(liftM, when)

-- -----------------------------------------------------------------------------
-- This is re-exported by Data.GraphViz.Types

-- | A polymorphic type that covers all possible ID values allowed by
--   Dot syntax.  Note that whilst the 'ParseDot' and 'PrintDot'
--   instances for 'String' will properly take care of the special
--   cases for numbers, they are treated differently here.
data GraphID = Str String
             | Int Int
             | Dbl Double
             | HTML URL
               deriving (Eq, Ord, Show, Read)

instance PrintDot GraphID where
    unqtDot (Str str) = unqtDot str
    unqtDot (Int i)   = unqtDot i
    unqtDot (Dbl d)   = unqtDot d
    unqtDot (HTML u)  = unqtDot u

    toDot (Str str) = toDot str
    toDot gID       = unqtDot gID

instance ParseDot GraphID where
    parseUnqt = liftM HTML parseUnqt
                `onFail`
                liftM stringNum parseUnqt

    parse = liftM HTML parse
            `onFail`
            liftM stringNum parse
            `adjustErr`
            (++ "\nNot a valid GraphID")

stringNum     :: String -> GraphID
stringNum str = maybe checkDbl Int $ stringToInt str
  where
    checkDbl = if isNumString str
               then Dbl $ toDouble str
               else Str str

-- -----------------------------------------------------------------------------
-- Labels

dirGraph :: String
dirGraph = "digraph"

dirGraph' :: DotCode
dirGraph' = text dirGraph

undirGraph :: String
undirGraph = "graph"

undirGraph' :: DotCode
undirGraph' = text undirGraph

strGraph :: String
strGraph = "strict"

strGraph' :: DotCode
strGraph' = text strGraph

sGraph :: String
sGraph = "subgraph"

sGraph' :: DotCode
sGraph' = text sGraph

clust :: String
clust = "cluster"

clust' :: DotCode
clust' = text clust

-- -----------------------------------------------------------------------------

printGraphID                 :: (a -> Bool) -> (a -> Bool)
                                -> (a -> Maybe GraphID)
                                -> a -> DotCode
printGraphID str isDir mID g = bool empty strGraph' (str g)
                               <+> bool undirGraph' dirGraph' (isDir g)
                               <+> maybe empty toDot (mID g)

parseGraphID   :: (Bool -> Bool -> Maybe GraphID -> a) -> Parse a
parseGraphID f = do str <- liftM isJust
                           $ optional (parseAndSpace $ string strGraph)
                    dir <- parseAndSpace ( stringRep True dirGraph
                                           `onFail`
                                           stringRep False undirGraph
                                         )
                    gID <- optional $ parseAndSpace parse
                    return $ f str dir gID

printStmtBased          :: (a -> DotCode) -> (a -> b) -> (b -> DotCode)
                           -> a -> DotCode
printStmtBased f r dr a = printBracesBased (f a) (dr $ r a)

printStmtBasedList        :: (a -> DotCode) -> (a -> b) -> (b -> DotCode)
                             -> [a] -> DotCode
printStmtBasedList f r dr = vcat . map (printStmtBased f r dr)

parseStmtBased :: Parse stmt -> Parse (stmt -> a) -> Parse a
parseStmtBased = flip apply . parseBracesBased

parseStmtBasedList       :: Parse stmt -> Parse (stmt -> a) -> Parse [a]
parseStmtBasedList ps pr = sepBy (whitespace' >> parseStmtBased ps pr) newline'

printBracesBased     :: DotCode -> DotCode -> DotCode
printBracesBased h i = vcat [ h <+> lbrace
                            , ind i
                            , rbrace
                            ]
  where
    ind = nest 4

parseBracesBased   :: Parse a -> Parse a
parseBracesBased p = do whitespace'
                        character '{'
                        newline'
                        a <- p
                        newline'
                        whitespace'
                        character '}'
                        return a
                     `adjustErr`
                     (++ "\nNot a valid value wrapped in braces.")


printSubGraphID     :: (a -> (Bool, Maybe GraphID)) -> a -> DotCode
printSubGraphID f a = sGraph'
                      <+> maybe cl dtID mID
  where
    (isCl, mID) = f a
    cl = bool empty clust' isCl
    dtID = printSGID isCl

-- | Print the actual ID for a 'DotSubGraph'.
printSGID          :: Bool -> GraphID -> DotCode
printSGID isCl sID = bool noClust addClust isCl
  where
    noClust = toDot sID
    -- Have to manually render it as we need the un-quoted form.
    addClust = toDot . (++) clust . (:) '_'
               . renderDot $ mkDot sID
    mkDot (Str str) = text str -- Quotes will be escaped later
    mkDot gid       = unqtDot gid

parseSubGraphID   :: (Bool -> Maybe GraphID -> c) -> Parse c
parseSubGraphID f = do string sGraph
                       whitespace
                       liftM (uncurry f) parseSGID

parseSGID :: Parse (Bool, Maybe GraphID)
parseSGID = oneOf [ liftM getClustFrom $ parseAndSpace parse
                  , return (False, Nothing)
                  ]
  where
    -- If it's a String value, check to see if it's actually a
    -- cluster_Blah value; thus need to manually re-parse it.
    getClustFrom (Str str) = runParser' pStr str
    getClustFrom gid       = (False, Just gid)

    checkCl = stringRep True clust
    pStr = do isCl <- checkCl
                      `onFail`
                      return False
              when isCl $ optional (character '_') >> return ()
              sID <- optional pID
              let sID' = if sID == emptyID
                         then Nothing
                         else sID
              return (isCl, sID')

    emptyID = Just $ Str ""

    -- For Strings, there are no more quotes to unescape, so consume
    -- what you can.
    pID = liftM HTML parseUnqt
                `onFail`
                liftM stringNum (many next)

{- This is a much nicer definition, but unfortunately it doesn't work.
   The problem is that Graphviz decides that a subgraph is a cluster
   if the ID starts with "cluster" (no quotes); thus, we _have_ to do
   the double layer of parsing to get it to work :@

            do isCl <- stringRep True clust
                       `onFail`
                       return False
               sID <- optional $ do when isCl
                                      $ optional (character '_') >> return ()
                                    parseUnqt
               when (isCl || isJust sID) $ whitespace >> return ()
               return (isCl, sID)
-}

-- | Parse the separator (and any other whitespace present) between statements.
statementEnd :: Parse ()
statementEnd = do whitespace'
                  optional parseSplit
                  newline'
  where
    parseSplit = oneOf [ liftM return $ character ';'
                       , newline
                       , whitespace
                       ]
