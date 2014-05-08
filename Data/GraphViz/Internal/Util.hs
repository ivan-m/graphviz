{-# LANGUAGE OverloadedStrings, PatternGuards #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
   Module      : Data.GraphViz.Internal.Util
   Description : Internal utility functions
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines internal utility functions.
-}
module Data.GraphViz.Internal.Util where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit, ord)

import           Control.Monad       (liftM2)
import           Data.Function       (on)
import           Data.List           (groupBy, sortBy)
import           Data.Maybe          (isJust)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Text.Lazy      (Text)
import qualified Data.Text.Lazy      as T
import qualified Data.Text.Lazy.Read as T
import           Data.Version        (Version (..))

-- -----------------------------------------------------------------------------

isIDString :: Text -> Bool
isIDString = maybe False (\(f,os) -> frstIDString f && T.all restIDString os)
             . T.uncons

-- | First character of a non-quoted 'String' must match this.
frstIDString   :: Char -> Bool
frstIDString c = any ($c) [ isAsciiUpper
                          , isAsciiLower
                          , (==) '_'
                          , (\ x -> ord x >= 128)
                          ]

-- | The rest of a non-quoted 'String' must match this.
restIDString   :: Char -> Bool
restIDString c = frstIDString c || isDigit c

-- | Determine if this String represents a number.
isNumString     :: Text -> Bool
isNumString ""  = False
isNumString "-" = False
isNumString str = case T.uncons $ T.toLower str of
                    Just ('-',str') -> go str'
                    _               -> go str
  where
    -- Can't use Data.Text.Lazy.Read.double as it doesn't cover all
    -- possible cases
    go s = uncurry go' $ T.span isDigit s
    go' ds nds
      | T.null nds = True
      | T.null ds && nds == "." = False
      | T.null ds
      , Just ('.',nds') <- T.uncons nds
      , Just (d,nds'') <- T.uncons nds' = isDigit d && checkEs' nds''
      | Just ('.',nds') <- T.uncons nds = checkEs $ T.dropWhile isDigit nds'
      | T.null ds = False
      | otherwise = checkEs nds
    checkEs' s = case T.break ('e' ==) s of
                   ("", _) -> False
                   (ds,es) -> T.all isDigit ds && checkEs es
    checkEs str' = case T.uncons str' of
                     Nothing       -> True
                     Just ('e',ds) -> isIntString ds
                     _             -> False

{-
-- | This assumes that 'isNumString' is 'True'.
toDouble     :: Text -> Double
toDouble str = case T.uncons $ T.toLower str of
                 Just ('-', str') -> toD $ '-' `T.cons` adj str'
                 _                -> toD $ adj str
  where
    adj s = T.cons '0'
            $ case T.span ('.' ==) s of
                (ds, ".") | not $ T.null ds -> s `T.snoc` '0'
                (ds, ds') | Just ('.',es) <- T.uncons ds'
                          , Just ('e',es') <- T.uncons es
                            -> ds `T.snoc` '.' `T.snoc` '0'
                                   `T.snoc` 'e' `T.snoc` '0' `T.append` es'
                _         -> s
    toD = either (const $ error "Not a Double") fst . T.signed T.double
-}
-- | This assumes that 'isNumString' is 'True'.
toDouble     :: Text -> Double
toDouble str = case T.uncons $ T.toLower str of
                 Just ('-', str') -> toD $ '-' `T.cons` adj str'
                 _                -> toD $ adj str
  where
    adj s = T.cons '0'
            $ case T.span ('.' ==) s of
                (ds, ".") | not $ T.null ds -> s `T.snoc` '0'
                (ds, ds') | Just ('.',es) <- T.uncons ds'
                          , Just ('e',_) <- T.uncons es
                            -> ds `T.snoc` '.' `T.snoc` '0' `T.append` es
                _              -> s
    toD = read . T.unpack

isIntString :: Text -> Bool
isIntString = isJust . stringToInt

-- | Determine if this String represents an integer.
stringToInt     :: Text -> Maybe Int
stringToInt str = case T.signed T.decimal str of
                       Right (n, "") -> Just n
                       _             -> Nothing

-- | Graphviz requires double quotes to be explicitly escaped.
escapeQuotes           :: String -> String
escapeQuotes []        = []
escapeQuotes ('"':str) = '\\':'"': escapeQuotes str
escapeQuotes (c:str)   = c : escapeQuotes str

-- | Remove explicit escaping of double quotes.
descapeQuotes                :: String -> String
descapeQuotes []             = []
descapeQuotes ('\\':'"':str) = '"' : descapeQuotes str
descapeQuotes (c:str)        = c : descapeQuotes str

isKeyword :: Text -> Bool
isKeyword = (`Set.member` keywords) . T.toLower

-- | The following are Dot keywords and are not valid as labels, etc. unquoted.
keywords :: Set Text
keywords = Set.fromList [ "node"
                        , "edge"
                        , "graph"
                        , "digraph"
                        , "subgraph"
                        , "strict"
                        ]

createVersion    :: [Int] -> Version
createVersion bs = Version { versionBranch = bs, versionTags = []}

-- -----------------------------------------------------------------------------

uniq :: (Ord a) => [a] -> [a]
uniq = uniqBy id

uniqBy   :: (Ord b) => (a -> b) -> [a] -> [a]
uniqBy f = map head . groupSortBy f

groupSortBy   :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupSortBy f = groupBy ((==) `on` f) . sortBy (compare `on` f)

groupSortCollectBy     :: (Ord b) => (a -> b) -> (a -> c) -> [a] -> [(b,[c])]
groupSortCollectBy f g = map (liftM2 (,) (f . head) (map g)) . groupSortBy f

-- | Fold over 'Bool's; first param is for 'False', second for 'True'.
bool       :: a -> a -> Bool -> a
bool f t b = if b
             then t
             else f

isSingle     :: [a] -> Bool
isSingle [_] = True
isSingle _   = False
