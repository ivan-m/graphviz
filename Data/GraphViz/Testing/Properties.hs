module Data.GraphViz.Testing.Properties where

import Data.GraphViz.Types.Printing(PrintDot(..), printIt)
import Data.GraphViz.Types.Parsing( ParseDot(..), parseIt
                                  , quoteChar, isIntString, isNumString)

import Data.GraphViz.Attributes
import Data.GraphViz.Types

import Test.QuickCheck

import Data.Maybe(isJust)
import Data.List(nub)
import Control.Monad(liftM, liftM2, liftM3, liftM4, guard)
import Data.Word(Word8)

-- -----------------------------------------------------------------------------
-- The properties to test for

-- | Checking that @parse . print == id@; that is, graphviz can parse
--   its own output.
printParse_prop = id

printParse   :: (ParseDot a, PrintDot a, Eq a) => a -> Bool
printParse a = fst (tryParse a) == a

printParseList    :: (ParseDot a, PrintDot a, Eq a) => [a] -> Property
printParseList as =  not (null as) ==> printParse as

tryParse :: (ParseDot a, PrintDot a) => a -> (a, String)
tryParse = parseIt . printIt
