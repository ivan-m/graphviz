{- |
   Module      : Data.GraphViz.Types.Parsing
   Description : Helper functions for Parsing.
   Copyright   : (c) Matthew Sackman, Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines simple helper functions for use with
   "Text.ParserCombinators.Poly.Lazy".

   Note that the 'ParseDot' instances for 'Bool', etc. match those
   specified for use with GraphViz (e.g. non-zero integers are
   equivalent to 'True').

   You should not be using this module; rather, it is here for
   informative/documentative reasons.  If you want to parse a
   @'Data.GraphViz.Types.DotGraph'@, you should use
   @'Data.GraphViz.Types.parseDotGraph'@ rather than its 'ParseDot'
   instance.
-}

module Data.GraphViz.Types.Parsing
    ( module Text.ParserCombinators.Poly.Lazy
    , Parse
    , ParseDot(..)
    , stringBlock
    , quotedString
    , parseAndSpace
    , string
    , strings
    , hasString
    , character
    , noneOf
    , whitespace
    , whitespace'
    , optionalQuotedString
    , optionalQuoted
    , quotedParse
    , newline
    , skipToNewline
    , parseField
    , parseBoolField
    , parseFieldDef
    , commaSep
    , commaSep'
    ) where

import Data.GraphViz.Types.Internal

import Text.ParserCombinators.Poly.Lazy
import Data.Char( digitToInt
                , isAsciiLower
                , isAsciiUpper
                , isDigit
                , isSpace
                , toLower
                )
import Data.Function(on)
import Data.Maybe(isJust)
import Data.Ratio((%))
import Control.Monad

-- -----------------------------------------------------------------------------
-- Based off code from Text.Parse in the polyparse library

-- | A @ReadS@-like type alias.
type Parse a = Parser Char a

class ParseDot a where
    parse :: Parse a

    parseList :: Parse [a]
    parseList = oneOf [ character '[' >> whitespace' >> character ']' >> return []
                      , bracketSep (parseAndSpace $ character '[')
                                   (parseAndSpace $ character ',')
                                   (parseAndSpace $ character ']')
                                   (parseAndSpace parse)
                      ]

instance ParseDot Int where
    parse = parseInt

instance ParseDot Double where
    parse = parseSigned parseFloat

instance ParseDot Bool where
    parse = oneOf [ string "true" >> return True
                  , string "false" >> return False
                  , liftM (zero /=) parseInt
                  ]
        where
          zero :: Int
          zero = 0

instance ParseDot Char where
    parse = next

    parseList = oneOf [ stringBlock
                      , quotedString
                      ]

instance (ParseDot a) => ParseDot [a] where
    parse = parseList

stringBlock :: Parse String
stringBlock = do frst <- satisfy frstIDString
                 rest <- many (satisfy restIDString)
                 return $ frst : rest

-- | Used when quotes are explicitly required;
quotedString :: Parse String
quotedString = do character '"'
                  str <- many $ oneOf [ string "\\\"" >> return '"'
                                      , satisfy ((/=) '"')
                                      ]
                  character '"'
                  return str

parseSigned :: Real a => Parse a -> Parse a
parseSigned p = do '-' <- next; commit (fmap negate p)
                `onFail`
                p

parseInt :: (Integral a) => Parse a
parseInt = do cs <- many1 (satisfy isDigit)
              return (foldl1 (\n d-> n*radix+d)
                                   (map (fromIntegral . digitToInt) cs))
           `adjustErr` (++ "\nexpected one or more digits")
    where
      radix = 10

parseFloat :: (RealFrac a) => Parse a
parseFloat = do ds   <- many1 (satisfy isDigit)
                frac <- (do '.' <- next
                            many (satisfy isDigit)
                              `adjustErrBad` (++"expected digit after .")
                         `onFail` return [] )
                expn  <- parseExp `onFail` return 0
                ( return . fromRational . (* (10^^(expn - length frac)))
                  . (%1) . fst
                  . runParser parseInt) (ds++frac)
             `onFail`
             do w <- many (satisfy (not.isSpace))
                case map toLower w of
                  "nan"      -> return (0/0)
                  "infinity" -> return (1/0)
                  _          -> fail "expected a floating point number"
  where parseExp = do 'e' <- fmap toLower next
                      commit (do '+' <- next; parseInt
                              `onFail`
                              parseSigned parseInt)

-- -----------------------------------------------------------------------------

parseAndSpace   :: Parse a -> Parse a
parseAndSpace p = p `discard` whitespace'

string :: String -> Parse String
string = mapM character

strings :: [String] -> Parse String
strings = oneOf . map string

hasString :: String -> Parse Bool
hasString = liftM isJust . optional . string

character   :: Char -> Parse Char
character c = satisfy (((==) `on` toLower) c)
              `adjustErr`
              (++ "\nnot the expected char: " ++ [c])

noneOf :: (Eq a) => [a] -> Parser a a
noneOf t = satisfy (\x -> all (/= x) t)

whitespace :: Parse String
whitespace = many1 (satisfy isSpace)

whitespace' :: Parse String
whitespace' = many (satisfy isSpace)

optionalQuotedString :: String -> Parse String
optionalQuotedString = optionalQuoted . string

optionalQuoted   :: Parse a -> Parse a
optionalQuoted p = oneOf [ p
                         , quotedParse p
                         ]

quotedParse   :: Parse a -> Parse a
quotedParse p = character '"' >> p `discard` character '"'

newline :: Parse String
newline = oneOf . map string $ ["\r\n", "\n", "\r"]

skipToNewline :: Parse ()
skipToNewline = many (noneOf ['\n','\r']) >> newline >> return ()

parseField     :: (ParseDot a) => String -> Parse a
parseField fld = do string fld
                    whitespace'
                    character '='
                    whitespace'
                    parse

parseBoolField :: String -> Parse Bool
parseBoolField = parseFieldDef True

-- | For 'Bool'-like data structures where the presence of the field
--   name without a value implies a default value.
parseFieldDef       :: (ParseDot a) => a -> String -> Parse a
parseFieldDef d fld = oneOf [ parseField fld
                            , string fld >> return d
                            ]

commaSep :: (ParseDot a, ParseDot b) => Parse (a, b)
commaSep = commaSep' parse parse

commaSep'       :: Parse a -> Parse b -> Parse (a,b)
commaSep' pa pb = do a <- pa
                     whitespace'
                     character ','
                     whitespace'
                     b <- pb
                     return (a,b)
