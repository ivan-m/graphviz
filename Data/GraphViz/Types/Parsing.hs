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
    , numString
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
    , parseComma
    , skipToNewline
    , parseField
    , parseFields
    , parseFieldBool
    , parseFieldsBool
    , parseFieldDef
    , parseFieldsDef
    , commaSep
    , commaSep'
    , stringRep
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
    parseUnqt :: Parse a

    parse :: Parse a
    parse = optionalQuoted parseUnqt

    parseUnqtList :: Parse [a]
    parseUnqtList = bracketSep (parseAndSpace $ character '[')
                               (parseAndSpace $ parseComma)
                               (parseAndSpace $ character ']')
                               (parseAndSpace parse)

    parseList :: Parse [a]
    parseList = quotedParse parseUnqtList

instance ParseDot Int where
    parseUnqt = parseInt'

instance ParseDot Double where
    parseUnqt = parseFloat'

instance ParseDot Bool where
    parseUnqt = oneOf [ stringRep True "true"
                      , stringRep False "false"
                      , liftM (zero /=) parseInt'
                      ]
        where
          zero :: Int
          zero = 0

instance ParseDot Char where
    -- Can't be a quote character.
    parseUnqt = satisfy ((/=) '"')

    parse = satisfy restIDString
            `onFail`
            quotedParse parseUnqt

    parseUnqtList = oneOf [ numString
                          , stringBlock
                          , quotedString
                          ]

    parseList = optionalQuoted (oneOf [numString, stringBlock])
                `onFail`
                quotedParse quotedString

instance (ParseDot a) => ParseDot [a] where
    parseUnqt = parseUnqtList

    parse = parseList

numString :: Parse String
numString = liftM show parseInt'
            `onFail`
            liftM show parseFloat'

stringBlock :: Parse String
stringBlock = do frst <- satisfy frstIDString
                 rest <- many (satisfy restIDString)
                 return $ frst : rest

-- | Used when quotes are explicitly required;
quotedString :: Parse String
quotedString = many $ oneOf [ stringRep '"' "\\\""
                            , satisfy ((/=) '"')
                            ]

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

parseInt' :: (Integral a) => Parse a
parseInt' = parseSigned parseInt

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

parseFloat' :: (RealFrac a) => Parse a
parseFloat' = parseSigned parseFloat

-- -----------------------------------------------------------------------------

parseAndSpace   :: Parse a -> Parse a
parseAndSpace p = p `discard` whitespace'

string :: String -> Parse String
string = mapM character

stringRep     :: a -> String -> Parse a
stringRep v s = string s >> return v

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
optionalQuoted p = p
                   `onFail`
                   quotedParse p

quotedParse   :: Parse a -> Parse a
quotedParse p = bracket quote quote p
    where
      quote = character '"'

newline :: Parse String
newline = oneOf $ map string ["\r\n", "\n", "\r"]

skipToNewline :: Parse ()
skipToNewline = many (noneOf ['\n','\r']) >> newline >> return ()

parseField     :: (ParseDot a) => String -> Parse a
parseField fld = do string fld
                    whitespace'
                    character '='
                    whitespace'
                    parse

parseFields :: (ParseDot a) => [String] -> Parse a
parseFields = oneOf . map parseField

parseFieldBool :: String -> Parse Bool
parseFieldBool = parseFieldDef True

parseFieldsBool :: [String] -> Parse Bool
parseFieldsBool = oneOf . map parseFieldBool

-- | For 'Bool'-like data structures where the presence of the field
--   name without a value implies a default value.
parseFieldDef       :: (ParseDot a) => a -> String -> Parse a
parseFieldDef d fld = parseField fld
                      `onFail`
                      stringRep d fld

parseFieldsDef   :: (ParseDot a) => a -> [String] -> Parse a
parseFieldsDef d = oneOf . map (parseFieldDef d)

commaSep :: (ParseDot a, ParseDot b) => Parse (a, b)
commaSep = commaSep' parse parse

commaSep'       :: Parse a -> Parse b -> Parse (a,b)
commaSep' pa pb = do a <- pa
                     whitespace'
                     parseComma
                     whitespace'
                     b <- pb
                     return (a,b)

parseComma :: Parse Char
parseComma = parseComma
