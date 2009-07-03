{-# LANGUAGE ScopedTypeVariables #-}

{- |
   Module      : Data.GraphViz.ParserCombinators
   Description : Helper functions for Parsing.
   Copyright   : (c) Matthew Sackman, Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines simple helper functions for use with
   @Text.ParserCombinators.Poly.Lazy@.
-}

module Data.GraphViz.ParserCombinators where

import Text.ParserCombinators.Poly.Lazy
import Data.Char(toLower)
import Data.Function(on)
import Data.Maybe(isJust)
import Control.Monad

string :: String -> Parser Char String
string = mapM char

strings :: [String] -> Parser Char String
strings = oneOf . map string

hasString :: String -> Parser Char Bool
hasString = liftM isJust . optional . string

char :: Char -> Parser Char Char
char = satisfy . ((==) `on` toLower)

noneOf :: (Eq a) => [a] -> Parser a a
noneOf t = satisfy (\x -> and . map ((/= x) $) $ t)

digit :: Parser Char Char
digit = oneOf . map char $ ['0'..'9']

number :: (Num a, Read a) => Parser Char a
number = liftM read $ many1 digit

floatingNumber :: (Floating a, Read a) => Parser Char a
floatingNumber = do { a::Integer <- number
                    ; char '.'
                    ; b::Integer <- number
                    ; return . read $ (show a) ++ ('.':(show b))
                    }

whitespace :: Parser Char String
whitespace = many1 (oneOf . map char $ [' ', '\t'])

optionalQuotedString :: String -> Parser Char String
optionalQuotedString s = oneOf [string s, char '"' >> string s >>= \s' -> char '"' >> return s']

optionalQuoted :: Parser Char a -> Parser Char a
optionalQuoted p = oneOf [p, char '"' >> p >>= \r -> char '"' >> return r]

newline :: Parser Char String
newline = oneOf . map string $ ["\r\n", "\n", "\r"]

skipToNewline :: Parser Char ()
skipToNewline = many (noneOf ['\n','\r']) >> newline >> return ()
