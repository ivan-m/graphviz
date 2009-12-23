{- |
   Module      : Data.GraphViz.Types.Internal
   Description : Internal functions
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines internal functions.
-}
module Data.GraphViz.Types.Internal where

import Data.Char( isAsciiUpper
                , isAsciiLower
                , isDigit
                , toLower
                )

import Data.List(groupBy, sortBy)
import Data.Maybe(isJust)
import Data.Function(on)
import qualified Data.Set as Set
import Data.Set(Set)
import Control.Monad(liftM2)

-- -----------------------------------------------------------------------------

isIDString        :: String -> Bool
isIDString []     = True
isIDString (f:os) = frstIDString f
                    && all restIDString os

-- | First character of a non-quoted 'String' must match this.
frstIDString   :: Char -> Bool
frstIDString c = any ($c) [ isAsciiUpper
                          , isAsciiLower
                          , (==) '_'
                          , liftM2 (&&) (>= '\200') (<= '\377')
                          ]

-- | The rest of a non-quoted 'String' must match this.
restIDString   :: Char -> Bool
restIDString c = frstIDString c || isDigit c

-- | Determine if this String represents a number.
isNumString     :: String -> Bool
isNumString ""  = False
isNumString "-" = False
isNumString str = case str of
                    ('-':str') -> go str'
                    _          -> go str
    where
      go s = case span isDigit (map toLower s) of
               (ds@(_:_),[]) -> all isDigit ds
               ([],'.':[])   -> False
               ([],'.':d:ds) -> isDigit d && checkEs' ds
               (_,'.':ds)    -> checkEs $ dropWhile isDigit ds
               ([],_)        -> False
               (_,ds)        -> checkEs ds
      checkEs' s = case break ((==) 'e') s of
                     ([], _) -> False
                     (ds,es) -> all isDigit ds && checkEs es
      checkEs []       = True
      checkEs ('e':ds) = isIntString ds
      checkEs _        = False

-- | This assumes that 'isNumString' is 'True'.
toDouble     :: String -> Double
toDouble str = case str of
                 ('-':str') -> read $ '-' : adj str'
                 _          -> read $ adj str
  where
    adj s = (:) '0'
            $ case span ((==) '.') (map toLower s) of
                (ds@(_:_), '.':[])   -> ds ++ '.' : '0' : []
                (ds, '.':es@('e':_)) -> ds ++ '.' : '0' : es
                _                    -> s

isIntString :: String -> Bool
isIntString = isJust . stringToInt

-- | Determine if this String represents an integer.
stringToInt     :: String -> Maybe Int
stringToInt str = if isNum
                  then Just (read str)
                  else Nothing
  where
    isNum = case str of
              ""        -> False
              ['-']     -> False
              ('-':num) -> isNum' num
              _         -> isNum' str
    isNum' = all isDigit

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

isKeyword :: String -> Bool
isKeyword = flip Set.member keywords . map toLower

-- | The following are Dot keywords and are not valid as labels, etc. unquoted.
keywords :: Set String
keywords = Set.fromList [ "node"
                        , "edge"
                        , "graph"
                        , "digraph"
                        , "subgraph"
                        , "strict"
                        ]

-- -----------------------------------------------------------------------------

uniq :: (Ord a) => [a] -> [a]
uniq = uniqBy id

uniqBy   :: (Ord b) => (a -> b) -> [a] -> [a]
uniqBy f = map head . groupBy ((==) `on` f) . sortBy (compare `on` f)

-- | Fold over 'Bool's; first param is for 'False', second for 'True'.
bool       :: a -> a -> Bool -> a
bool f t b = if b
             then t
             else f
