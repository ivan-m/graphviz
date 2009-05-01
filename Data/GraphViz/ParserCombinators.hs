{-# LANGUAGE PatternSignatures #-}

 {- GraphViz ------------------------------------------------------\
 |                                                                 |
 | Copyright (c) 2008, Matthew Sackman (matthew@wellquite.org)     |
 |                                                                 |
 | DisTract is freely distributable under the terms of a 3-Clause  |
 | BSD-style license.                                              |
 |                                                                 |
 \-----------------------------------------------------------------}

module Data.GraphViz.ParserCombinators where

import Text.ParserCombinators.PolyLazy
import Control.Monad

string :: String -> Parser Char String
string = mapM char

char :: Char -> Parser Char Char
char = satisfy . (==)

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
