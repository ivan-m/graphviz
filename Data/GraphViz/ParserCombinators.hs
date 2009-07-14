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
import Data.Char( chr
                , digitToInt
                , isDigit
                , isHexDigit
                , isOctDigit
                , isSpace
                , isUpper
                , ord
                , toLower
                )
import Data.Function(on)
import Data.Maybe(isJust)
import Data.Ratio((%))
import Control.Monad

-- -----------------------------------------------------------------------------
-- Based off code from Text.Parse in the polyparse library

type Parse a = Parser Char a

class Parseable a where
    parse :: Parse a

    parseList :: Parse [a]
    parseList = oneOf [ char '[' >> whitespace' >> char ']' >> return []
                      , bracketSep (parseAndSpace $ char '[')
                                   (parseAndSpace $ char ',')
                                   (parseAndSpace $ char ']')
                                   (parseAndSpace parse)
                      ]

instance Parseable Int where
    parse = parseInt

instance Parseable Double where
    parse = parseSigned parseFloat

instance Parseable Bool where
    parse = oneOf [ string "true" >> return True
                  , string "false" >> return False
                  , liftM (0 /=) parseInt
                  ]

instance Parseable Char where
    parse = next

    parseList = do w <- word
                   if head w == '"'
                      then return (init (tail w))
                      else fail "not a String"

instance (Parseable a) => Parseable [a] where
    parse = parseList

instance (Parseable a, Parseable b) => Parseable (Either a b) where
    parse = oneOf [ liftM Left parse
                  , liftM Right parse
                  ]

word :: Parse String
word = P (\s-> case lex s of
                   []         -> Failure s  ("no input? (impossible)")
                   [("","")]  -> Failure "" ("no input?")
                   [("",s')]  -> Failure s  ("lexing failed?")
                   ((x,s'):_) -> Success s' x
         )

parseSigned :: Real a => Parse a -> Parse a
parseSigned p = do '-' <- next; commit (fmap negate p)
                `onFail`
                p

parseInt :: (Integral a) => Parse a
parseInt = do cs <- many1 (satisfy isDigit)
              return (foldl1 (\n d-> n*radix+d)
                                   (map (fromIntegral . digitToInt) cs))
           `adjustErr` (++("\nexpected one or more digits"))
    where
      radix = 10

parseFloat :: (RealFrac a) => Parse a
parseFloat = do ds   <- many1 (satisfy isDigit)
                frac <- (do '.' <- next
                            many (satisfy isDigit)
                              `adjustErrBad` (++"expected digit after .")
                         `onFail` return [] )
                exp  <- exponent `onFail` return 0
                ( return . fromRational . (* (10^^(exp - length frac)))
                  . (%1) . fst
                  . runParser parseInt) (ds++frac)
             `onFail`
             do w <- many (satisfy (not.isSpace))
                case map toLower w of
                  "nan"      -> return (0/0)
                  "infinity" -> return (1/0)
                  _          -> fail "expected a floating point number"
  where exponent = do 'e' <- fmap toLower next
                      commit (do '+' <- next; parseInt
                              `onFail`
                              parseSigned parseInt)

-- -----------------------------------------------------------------------------

parseAndSpace   :: Parse a -> Parse a
parseAndSpace p = p `discard` whitespace'

string :: String -> Parse String
string = mapM char

strings :: [String] -> Parse String
strings = oneOf . map string

hasString :: String -> Parse Bool
hasString = liftM isJust . optional . string

char :: Char -> Parse Char
char = satisfy . ((==) `on` toLower)

noneOf :: (Eq a) => [a] -> Parser a a
noneOf t = satisfy (\x -> all (/= x) t)

digit :: Parse Char
digit = satisfy isDigit

number :: (Num a, Read a) => Parse a
number = liftM read $ many1 digit

floatingNumber :: (Floating a, Read a) => Parse a
floatingNumber = do a::Integer <- number
                    char '.'
                    b::Integer <- number
                    return . read $ (show a) ++ ('.':(show b))


whitespace :: Parse String
whitespace = many1 (satisfy isSpace)

whitespace' :: Parse String
whitespace' = many (satisfy isSpace)

optionalQuotedString   :: String -> Parse String
optionalQuotedString s = optionalQuoted (string s)

optionalQuoted   :: Parse a -> Parse a
optionalQuoted p = oneOf [ p
                         , char '"' >> p `discard` char '"'
                         ]

newline :: Parse String
newline = oneOf . map string $ ["\r\n", "\n", "\r"]

skipToNewline :: Parse ()
skipToNewline = many (noneOf ['\n','\r']) >> newline >> return ()

parseField     :: (Parseable a) => String -> Parse a
parseField fld = do string fld
                    whitespace'
                    char '='
                    whitespace'
                    parse

commaSep :: (Parseable a, Parseable b) => Parse (a, b)
commaSep = do a <- parse
              whitespace
              char ','
              whitespace
              b <- parse
              return (a,b)
