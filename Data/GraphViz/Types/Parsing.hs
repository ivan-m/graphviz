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
    ( -- * Re-exporting pertinent parts of Polyparse.
      module Text.ParserCombinators.Poly.Lazy
      -- * The ParseDot class.
    , Parse
    , ParseDot(..)
      -- * Convenience parsing combinators.
    , stringBlock
    , numString
    , quotedString
    , parseAndSpace
    , string
    , strings
    , hasString
    , character
    , parseStrictFloat
    , noneOf
    , whitespace
    , whitespace'
    , optionalQuotedString
    , optionalQuoted
    , quotedParse
    , newline
    , newline'
    , parseComma
    , tryParseList
    , tryParseList'
    , skipToNewline
    , parseField
    , parseFields
    , parseFieldBool
    , parseFieldsBool
    , parseFieldDef
    , parseFieldsDef
    , commaSep
    , commaSepUnqt
    , commaSep'
    , stringRep
    -- * Pre-processing
    , preprocess
    ) where

import Data.GraphViz.Types.Internal

import Text.ParserCombinators.Poly.Lazy
import Data.Char( digitToInt
                , isDigit
                , isSpace
                , toLower
                )
import Data.Function(on)
import Data.Maybe(isJust, fromMaybe)
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
    parseUnqt = satisfy ((/=) quoteChar)

    parse = satisfy restIDString
            `onFail`
            quotedParse parseUnqt

    parseUnqtList = oneOf [ numString
                          , stringBlock
                          , quotedString
                          ]

    parseList = oneOf [numString, stringBlock]
                `onFail`
                -- This will also take care of quoted versions of
                -- above.
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
quotedString = many stringInterior

stringInterior :: Parse Char
stringInterior = stringRep quoteChar "\\\""
                 `onFail`
                 satisfy ((/=) quoteChar)

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

parseInt' :: Parse Int
parseInt' = parseSigned parseInt

-- | Parse a floating point number that actually contains decimals.
parseStrictFloat :: (RealFrac a) => Parse a
parseStrictFloat = parseSigned parseFloat

parseFloat :: (RealFrac a) => Parse a
parseFloat = do ds   <- many (satisfy isDigit)
                frac <- do character '.'
                           many (satisfy isDigit)
                                    `adjustErrBad` (++ "\nexpected digit after .")
                       `adjustErr` (++ "expected decimal component")
                let nDs = length frac
                ( return . fromRational . (* (10^^(-nDs)))
                  . (%1) . fst
                  . runParser parseInt) (ds++frac)
             `onFail`
             fail "Expected a floating point number"

parseFloat' :: Parse Double
parseFloat' = parseSigned ( parseFloat
                            `onFail`
                            liftM fI parseInt
                          )
    where
      fI :: Integer -> Double
      fI = fromIntegral

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
quotedParse p = bracket parseQuote parseQuote p

parseQuote :: Parse Char
parseQuote = character quoteChar

quoteChar :: Char
quoteChar = '"'

newline :: Parse String
newline = oneOf $ map string ["\r\n", "\n", "\r"]

-- | Consume all whitespace and newlines until a line with
--   non-whitespace is reached.  The whitespace on that line is
--   not consumed.
newline' :: Parse ()
newline' = many (whitespace' >> newline) >> return ()

-- | Parses and returns all characters up till the end of the line,
--   then skips to the beginning of the next line.
skipToNewline :: Parse String
skipToNewline = many (noneOf ['\n','\r']) `discard` newline

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

commaSepUnqt :: (ParseDot a, ParseDot b) => Parse (a, b)
commaSepUnqt = commaSep' parseUnqt parseUnqt

commaSep'       :: Parse a -> Parse b -> Parse (a,b)
commaSep' pa pb = do a <- pa
                     whitespace'
                     parseComma
                     whitespace'
                     b <- pb
                     return (a,b)

parseComma :: Parse Char
parseComma = character ','

tryParseList :: (ParseDot a) => Parse [a]
tryParseList = tryParseList' parse

tryParseList' :: Parse [a] -> Parse [a]
tryParseList' = liftM (fromMaybe []) . optional

-- -----------------------------------------------------------------------------
-- Filtering out unwanted Dot items such as comments

-- | Remove unparseable features of Dot, such as comments and
--   multi-line strings (which are converted to single-line strings).
preprocess :: String -> String
preprocess = fst . runParser parseOutUnwanted

-- | Parse out comments and make quoted strings spread over multiple
--   lines only over a single line.  Should parse the /entire/ input
--   'String'.
parseOutUnwanted :: Parse String
parseOutUnwanted = liftM concat (many getNext)
    where
      getNext :: Parse String
      getNext = (oneOf [ parseLineComment, parseMultiLineComment ] >> return [])
                `onFail`
                parseSplitStrings
                `onFail`
                liftM return next

-- | Parse @//@-style comments.
parseLineComment :: Parse String
parseLineComment = string "//" >> newline

-- | Parse @/* ... */@-style comments.
parseMultiLineComment :: Parse String
parseMultiLineComment = bracket start end (liftM concat $ many inner)
    where
      start = string "/*"
      end = string "*/"
      inner = many1 (satisfy ((/=) '*'))
              `onFail`
              do ast <- character '*'
                 n <- satisfy ((/=) '/')
                 liftM ((:) ast . (:) n) inner
-- | Parse out @\<newline>@ from a quoted string.
parseSplitStrings :: Parse String
parseSplitStrings = do oq <- parseQuote
                       inner <- liftM concat $ many parseInner
                       cq <- parseQuote
                       return $ oq : inner ++ [cq]
    where
      parseInner = string "\\\""
                   `onFail`
                   (character '\\' >> newline >> return [])
                   `onFail`
                   liftM return (satisfy ((/=) quoteChar))

