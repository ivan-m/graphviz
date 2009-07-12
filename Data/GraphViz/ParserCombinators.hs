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
    parse = parseLitChar

    parseList = do w <- word
                   if head w == '"'
                      then return (init (tail w))
                      else fail "not a String"

instance (Parseable a) => Parseable [a] where
    parse = parseList

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

parseIntegral :: (Integral a) => String ->
                 a -> (Char -> Bool) -> (Char -> Int) ->
                 a -> Parse a
parseIntegral base radix isDigit digitToInt n = go n
  where go acc = do cs <- many1 (satisfy isDigit)
                    return (foldl1 (\n d-> n*radix+d)
                                   (map (fromIntegral.digitToInt) cs))
                 `adjustErr` (++("\nexpected one or more "++base++" digits"))

parseDec, parseOct, parseHex :: (Integral a) => a -> Parse a
parseDec = parseIntegral "decimal" 10 isDigit    digitToInt
parseOct = parseIntegral "octal"    8 isOctDigit digitToInt
parseHex = parseIntegral "hex"     16 isHexDigit digitToInt

parseInt :: Parse Int
parseInt = parseSigned (parseDec 0)

parseFloat :: (RealFrac a) => Parse a
parseFloat = do ds   <- many1 (satisfy isDigit)
                frac <- (do '.' <- next
                            many (satisfy isDigit)
                              `adjustErrBad` (++"expected digit after .")
                         `onFail` return [] )
                exp  <- if null frac then exponent
                                     else exponent `onFail` return 0
                ( return . fromRational . (* (10^^(exp - length frac)))
                  . (%1) . fst
                  . runParser (parseDec 0) ) (ds++frac)
             `onFail`
             do w <- many (satisfy (not.isSpace))
                case map toLower w of
                  "nan"      -> return (0/0)
                  "infinity" -> return (1/0)
                  _          -> fail "expected a floating point number"
  where exponent = do 'e' <- fmap toLower next
                      commit (do '+' <- next; parseDec 0
                              `onFail`
                              parseSigned (parseDec 0) )


parseLitChar :: Parse Char
parseLitChar = do '\'' <- next `adjustErr` (++"expected a literal char")
                  c <- next
                  char <- case c of
                            '\\' -> next >>= escape
                            '\'' -> fail "expected a literal char, got ''"
                            _    -> return c
                  '\'' <- next `adjustErrBad` (++"literal char has no final '")
                  return char
  where
    escape 'a'  = return '\a'
    escape 'b'  = return '\b'
    escape 'f'  = return '\f'
    escape 'n'  = return '\n'
    escape 'r'  = return '\r'
    escape 't'  = return '\t'
    escape 'v'  = return '\v'
    escape '\\' = return '\\'
    escape '"'  = return '"'
    escape '\'' = return '\''
    escape '^'  = do ctrl <- next
                     if ctrl >= '@' && ctrl <= '_'
                       then return (chr (ord ctrl - ord '@'))
                       else fail ("literal char ctrl-escape malformed: \\^"
                                   ++[ctrl])
    escape d | isDigit d
                = fmap chr $  parseDec (digitToInt d)
    escape 'o'  = fmap chr $  parseOct 0
    escape 'x'  = fmap chr $  parseHex 0
    escape c | isUpper c
                = mnemonic c
    escape c    = fail ("unrecognised escape sequence in literal char: \\"++[c])

    mnemonic 'A' = do 'C' <- next; 'K' <- next; return '\ACK'
                   `wrap` "'\\ACK'"
    mnemonic 'B' = do 'E' <- next; 'L' <- next; return '\BEL'
                   `onFail`
                   do 'S' <- next; return '\BS'
                   `wrap` "'\\BEL' or '\\BS'"
    mnemonic 'C' = do 'R' <- next; return '\CR'
                   `onFail`
                   do 'A' <- next; 'N' <- next; return '\CAN'
                   `wrap` "'\\CR' or '\\CAN'"
    mnemonic 'D' = do 'E' <- next; 'L' <- next; return '\DEL'
                   `onFail`
                   do 'L' <- next; 'E' <- next; return '\DLE'
                   `onFail`
                   do 'C' <- next; ( do '1' <- next; return '\DC1'
                                     `onFail`
                                     do '2' <- next; return '\DC2'
                                     `onFail`
                                     do '3' <- next; return '\DC3'
                                     `onFail`
                                     do '4' <- next; return '\DC4' )
                   `wrap` "'\\DEL' or '\\DLE' or '\\DC[1..4]'"
    mnemonic 'E' = do 'T' <- next; 'X' <- next; return '\ETX'
                   `onFail`
                   do 'O' <- next; 'T' <- next; return '\EOT'
                   `onFail`
                   do 'N' <- next; 'Q' <- next; return '\ENQ'
                   `onFail`
                   do 'T' <- next; 'B' <- next; return '\ETB'
                   `onFail`
                   do 'M' <- next; return '\EM'
                   `onFail`
                   do 'S' <- next; 'C' <- next; return '\ESC'
                   `wrap` "one of '\\ETX' '\\EOT' '\\ENQ' '\\ETB' '\\EM' or '\\ESC'"
    mnemonic 'F' = do 'F' <- next; return '\FF'
                   `onFail`
                   do 'S' <- next; return '\FS'
                   `wrap` "'\\FF' or '\\FS'"
    mnemonic 'G' = do 'S' <- next; return '\GS'
                   `wrap` "'\\GS'"
    mnemonic 'H' = do 'T' <- next; return '\HT'
                   `wrap` "'\\HT'"
    mnemonic 'L' = do 'F' <- next; return '\LF'
                   `wrap` "'\\LF'"
    mnemonic 'N' = do 'U' <- next; 'L' <- next; return '\NUL'
                   `onFail`
                   do 'A' <- next; 'K' <- next; return '\NAK'
                   `wrap` "'\\NUL' or '\\NAK'"
    mnemonic 'R' = do 'S' <- next; return '\RS'
                   `wrap` "'\\RS'"
    mnemonic 'S' = do 'O' <- next; 'H' <- next; return '\SOH'
                   `onFail`
                   do 'O' <- next; return '\SO'
                   `onFail`
                   do 'T' <- next; 'X' <- next; return '\STX'
                   `onFail`
                   do 'I' <- next; return '\SI'
                   `onFail`
                   do 'Y' <- next; 'N' <- next; return '\SYN'
                   `onFail`
                   do 'U' <- next; 'B' <- next; return '\SUB'
                   `onFail`
                   do 'P' <- next; return '\SP'
                   `wrap` "'\\SOH' '\\SO' '\\STX' '\\SI' '\\SYN' '\\SUB' or '\\SP'"
    mnemonic 'U' = do 'S' <- next; return '\US'
                   `wrap` "'\\US'"
    mnemonic 'V' = do 'T' <- next; return '\VT'
                   `wrap` "'\\VT'"
    wrap p s = p `onFail` fail ("expected literal char "++s)


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

optionalQuotedString :: String -> Parse String
optionalQuotedString s = oneOf [string s, char '"' >> string s >>= \s' -> char '"' >> return s']

optionalQuoted :: Parse a -> Parse a
optionalQuoted p = oneOf [p, char '"' >> p >>= \r -> char '"' >> return r]

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
