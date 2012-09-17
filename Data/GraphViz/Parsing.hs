{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

{- |
   Module      : Data.GraphViz.Parsing
   Description : Helper functions for Parsing.
   Copyright   : (c) Matthew Sackman, Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines simple helper functions for use with
   "Text.ParserCombinators.Poly.Lazy".

   Note that the 'ParseDot' instances for 'Bool', etc. match those
   specified for use with Graphviz (e.g. non-zero integers are
   equivalent to 'True').

   You should not be using this module; rather, it is here for
   informative/documentative reasons.  If you want to parse a
   @'Data.GraphViz.Types.DotRepr'@, you should use
   @'Data.GraphViz.Types.parseDotGraph'@ rather than its 'ParseDot'
   instance.
-}
module Data.GraphViz.Parsing
    ( -- * Re-exporting pertinent parts of Polyparse.
      module Text.ParserCombinators.Poly.StateText
      -- * The ParseDot class.
    , Parse
    , ParseDot(..)
    , parseIt
    , parseIt'
    , runParser
    , runParser'
    , checkValidParse
      -- * Convenience parsing combinators.
    , bracket
    , ignoreSep
    , onlyBool
    , quotelessString
    , stringBlock
    , numString
    , isNumString
    , isIntString
    , quotedString
    , parseEscaped
    , parseAndSpace
    , string
    , strings
    , character
    , parseStrictFloat
    , noneOf
    , whitespace1
    , whitespace
    , wrapWhitespace
    , optionalQuotedString
    , optionalQuoted
    , quotedParse
    , orQuote
    , quoteChar
    , newline
    , newline'
    , parseComma
    , parseEq
    , tryParseList
    , tryParseList'
    , consumeLine
    , parseField
    , parseFields
    , parseFieldBool
    , parseFieldsBool
    , parseFieldDef
    , parseFieldsDef
    , liftEqParse
    , liftEqParse'
    , commaSep
    , commaSepUnqt
    , commaSep'
    , stringRep
    , stringReps
    , stringParse
    , stringValue
    , parseAngled
    , parseBraced
    , parseColorScheme
    ) where

import Data.GraphViz.Util
import Data.GraphViz.State
-- To avoid orphan instances and cyclic imports
import Data.GraphViz.Attributes.ColorScheme
import Data.GraphViz.Exception(GraphvizException(NotDotCode), throw)

import Text.ParserCombinators.Poly.StateText hiding (bracket, empty, indent, runParser)
import qualified Text.ParserCombinators.Poly.StateText as P
import Data.Char( isDigit
                , isSpace
                , isLower
                , toLower
                , toUpper
                )
import Data.List(groupBy, sortBy)
import Data.Function(on)
import Data.Maybe(fromMaybe, isNothing, listToMaybe)
import Data.Ratio((%))
import qualified Data.Set as Set
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import Data.Text.Lazy(Text)
import Data.Word(Word8, Word16)
import Control.Arrow(first, second)
import Control.Monad(when)

-- -----------------------------------------------------------------------------
-- Based off code from Text.Parse in the polyparse library

-- | A @ReadS@-like type alias.
type Parse a = Parser GraphvizState a

runParser     :: Parse a -> Text -> (Either String a, Text)
runParser p t = let (r,_,t') = P.runParser p initialState t
                in (r,t')

-- | A variant of 'runParser' where it is assumed that the provided
--   parsing function consumes all of the 'Text' input (with the
--   exception of whitespace at the end).
runParser'   :: Parse a -> Text -> a
runParser' p = checkValidParse . fst . runParser p'
  where
    p' = p `discard` (whitespace *> eof)

class ParseDot a where
  parseUnqt :: Parse a

  parse :: Parse a
  parse = optionalQuoted parseUnqt

  parseUnqtList :: Parse [a]
  parseUnqtList = bracketSep (parseAndSpace $ character '[')
                             ( wrapWhitespace parseComma
                               `onFail`
                               whitespace1
                             )
                             (whitespace *> character ']')
                             parseUnqt

  parseList :: Parse [a]
  parseList = quotedParse parseUnqtList

-- | Parse the required value, returning also the rest of the input
--   'String' that hasn't been parsed (for debugging purposes).
parseIt :: (ParseDot a) => Text -> (a, Text)
parseIt = first checkValidParse . runParser parse

-- | If unable to parse /Dot/ code properly, 'throw' a
--   'GraphvizException'.
checkValidParse :: Either String a -> a
checkValidParse (Left err) = throw (NotDotCode err)
checkValidParse (Right a)  = a

-- | Parse the required value with the assumption that it will parse
--   all of the input 'String'.
parseIt' :: (ParseDot a) => Text -> a
parseIt' = runParser' parse

instance ParseDot Int where
  parseUnqt = parseInt'

instance ParseDot Integer where
  parseUnqt = parseSigned parseInt

instance ParseDot Word8 where
  parseUnqt = parseInt

instance ParseDot Word16 where
  parseUnqt = parseInt

instance ParseDot Double where
  parseUnqt = parseFloat'

  parseUnqtList = sepBy1 parseUnqt (character ':')

  parseList = quotedParse parseUnqtList
              `onFail`
              fmap (:[]) parse

instance ParseDot Bool where
  parseUnqt = onlyBool
              `onFail`
              fmap (zero /=) parseInt'
    where
      zero :: Int
      zero = 0

-- | Use this when you do not want numbers to be treated as 'Bool' values.
onlyBool :: Parse Bool
onlyBool = oneOf [ stringRep True "true"
                 , stringRep False "false"
                 ]

instance ParseDot Char where
  -- Can't be a quote character.
  parseUnqt = satisfy (quoteChar /=)

  parse = satisfy restIDString
          `onFail`
          quotedParse parseUnqt

  parseUnqtList = T.unpack <$> parseUnqt

  parseList = T.unpack <$> parse

instance ParseDot Text where
  -- Too many problems with using this within other parsers where
  -- using numString or stringBlock will cause a parse failure.  As
  -- such, this will successfully parse all un-quoted Texts.
  parseUnqt = quotedString

  parse = quotelessString
          `onFail`
          -- This will also take care of quoted versions of
          -- above.
          quotedParse quotedString

instance (ParseDot a) => ParseDot [a] where
  parseUnqt = parseUnqtList

  parse = parseList

-- | Parse a 'String' that doesn't need to be quoted.
quotelessString :: Parse Text
quotelessString = numString `onFail` stringBlock

numString :: Parse Text
numString = fmap tShow parseStrictFloat
            `onFail`
            fmap tShow parseInt'
  where
    tShow :: (Show a) => a -> Text
    tShow = T.pack . show

stringBlock :: Parse Text
stringBlock = liftA2 T.cons (satisfy frstIDString) (manySatisfy restIDString)

-- | Used when quotes are explicitly required;
quotedString :: Parse Text
quotedString = parseEscaped True [] []

parseSigned :: (Num a) => Parse a -> Parse a
parseSigned p = (character '-' *> fmap negate p)
                `onFail`
                p

parseInt :: (Integral a) => Parse a
parseInt = do cs <- many1Satisfy isDigit
              case T.decimal cs of
                Right (n,"")  -> return n
                Right (_,txt) -> fail $ "Trailing digits not parsed as Integral: " ++ T.unpack txt
                Left err      -> fail $ "Could not read Integral: " ++ err
           `adjustErr` ("Expected one or more digits\n\t"++)

parseInt' :: Parse Int
parseInt' = parseSigned parseInt

-- | Parse a floating point number that actually contains decimals.
parseStrictFloat :: Parse Double
parseStrictFloat = parseSigned parseFloat

parseFloat :: (RealFrac a) => Parse a
parseFloat = do ds   <- manySatisfy isDigit
                frac <- optional $ character '.' *> manySatisfy isDigit
                when (T.null ds && noDec frac)
                  (fail "No actual digits in floating point number!")
                expn  <- optional parseExp
                when (isNothing frac && isNothing expn)
                  (fail "This is an integer, not a floating point number!")
                let frac' = fromMaybe "" frac
                    expn' = fromMaybe 0 expn
                ( return . fromRational . (* (10^^(expn' - fromIntegral (T.length frac'))))
                  . (%1) . runParser' parseInt) (ds `T.append` frac')
             `onFail`
             fail "Expected a floating point number"
  where
    parseExp = character 'e'
               *> ((character '+' *> parseInt)
                   `onFail`
                   parseInt')
    noDec = maybe True T.null

parseFloat' :: Parse Double
parseFloat' = parseSigned ( parseFloat
                            `onFail`
                            fmap fI parseInt
                          )
  where
    fI :: Integer -> Double
    fI = fromIntegral

-- -----------------------------------------------------------------------------

-- | Parse a bracketed item, discarding the brackets.
--
--   The definition of @bracket@ defined in Polyparse uses
--   'adjustErrBad' and thus doesn't allow backtracking and trying the
--   next possible parser.  This is a version of @bracket@ that does.
bracket               :: Parse bra -> Parse ket -> Parse a -> Parse a
bracket open close pa = (open `adjustErr` ("Missing opening bracket:\n\t"++))
                        *> pa
                        <* (close `adjustErr` ("Missing closing bracket:\n\t"++))

parseAndSpace   :: Parse a -> Parse a
parseAndSpace p = p `discard` whitespace

string :: String -> Parse ()
string = mapM_ character

stringRep   :: a -> String -> Parse a
stringRep v = stringReps v . return

stringReps      :: a -> [String] -> Parse a
stringReps v ss = oneOf (map string ss) *> return v

stringParse :: [(String, Parse a)] -> Parse a
stringParse = toPM . sortBy (flip compare `on` fst)
  where
    toPM = oneOf . map mkPM . groupBy ((==) `on` (listToMaybe . fst))

    mkPM [("",p)] = p
    mkPM [(str,p)] = string str *> p
    mkPM kv = character (head . fst $ head kv) *> toPM (map (first tail) kv)

stringValue :: [(String, a)] -> Parse a
stringValue = stringParse . map (second return)

strings :: [String] -> Parse ()
strings = oneOf . map string

character   :: Char -> Parse Char
character c = satisfy parseC
              `adjustErr`
              (const $ "Not the expected character: " ++ [c])
  where
    parseC c' = c' == c || c == flipCase c'
    flipCase c' = if isLower c'
                  then toUpper c'
                  else toLower c'

noneOf   :: [Char] -> Parse Char
noneOf t = satisfy (\x -> all (/= x) t)

-- | Parses at least one whitespace character.
whitespace1 :: Parse ()
whitespace1 = many1Satisfy isSpace *> return ()

-- | Parses zero or more whitespace characters.
whitespace :: Parse ()
whitespace = manySatisfy isSpace *> return ()

-- | Parse and discard optional whitespace.
wrapWhitespace :: Parse a -> Parse a
wrapWhitespace = bracket whitespace whitespace

optionalQuotedString :: String -> Parse ()
optionalQuotedString = optionalQuoted . string

optionalQuoted   :: Parse a -> Parse a
optionalQuoted p = quotedParse p
                   `onFail`
                   p

quotedParse :: Parse a -> Parse a
quotedParse = bracket parseQuote parseQuote

parseQuote :: Parse Char
parseQuote = character quoteChar

orQuote   :: Parse Char -> Parse Char
orQuote p = stringRep quoteChar "\\\""
            `onFail`
            p

quoteChar :: Char
quoteChar = '"'

-- | Parse a 'String' where the provided 'Char's (as well as @\"@ and
--   @\\@) are escaped and the second list of 'Char's are those that
--   are not permitted.  Note: does not parse surrounding quotes.  The
--   'Bool' value indicates whether empty 'String's are allowed or
--   not.
parseEscaped             :: Bool -> [Char] -> [Char] -> Parse Text
parseEscaped empt cs bnd = fmap T.pack . lots $ qPrs `onFail` oth
  where
    lots = if empt then many else many1
    cs' = quoteChar : slash : cs
    csSet = Set.fromList cs'
    bndSet = Set.fromList bnd `Set.union` csSet
    slash = '\\'
    -- Have to allow standard slashes
    qPrs = fromMaybe slash
           <$> (character slash
                *> optional (oneOf $ map character cs')
               )
    oth = satisfy (`Set.notMember` bndSet)

-- | Parses a newline.
newline :: Parse ()
newline = strings ["\r\n", "\n", "\r"]

-- | Consume all whitespace and newlines until a line with
--   non-whitespace is reached.  The whitespace on that line is
--   not consumed.
newline' :: Parse ()
newline' = many (whitespace *> newline) *> return ()

-- | Parses and returns all characters up till the end of the line,
--   but does not touch the newline characters.
consumeLine :: Parse Text
consumeLine = manySatisfy (`notElem` ['\n','\r'])

parseEq :: Parse ()
parseEq = wrapWhitespace (character '=') *> return ()

parseField       :: (ParseDot a) => (a -> b) -> String -> [(String, Parse b)]
parseField c fld = [(fld, liftEqParse' fld c)]

parseFields   :: (ParseDot a) => (a -> b) -> [String] -> [(String, Parse b)]
parseFields c = concatMap (parseField c)

parseFieldBool :: (Bool -> b) -> String -> [(String, Parse b)]
parseFieldBool = flip parseFieldDef True

parseFieldsBool   :: (Bool -> b) -> [String] -> [(String, Parse b)]
parseFieldsBool c = concatMap (parseFieldBool c)

-- | For 'Bool'-like data structures where the presence of the field
--   name without a value implies a default value.
parseFieldDef         :: (ParseDot a) => (a -> b) -> a -> String -> [(String, Parse b)]
parseFieldDef c d fld = [(fld, p)]
  where
    p = liftEqParse' fld c
        `onFail`
        do nxt <- optional $ satisfy restIDString
           bool (fail "Not actually the field you were after")
                (return $ c d)
                (isNothing nxt)

parseFieldsDef     :: (ParseDot a) => (a -> b) -> a -> [String] -> [(String, Parse b)]
parseFieldsDef c d = concatMap (parseFieldDef c d)

-- | 'liftEqParse'' using 'parse' as the parser.
liftEqParse' :: (ParseDot a) => String -> (a -> b) -> Parse b
liftEqParse' = liftEqParse parse

-- | Attempt to parse the @\"=value\"@ part of a @key=value@ pair.  If
--   there is an equal sign but the @value@ part doesn't parse, throw
--   an un-recoverable error.
liftEqParse :: Parse a -> String -> (a -> b) -> Parse b
liftEqParse p k c = parseEq
                    *> ( fmap c p
                         `adjustErrBad`
                         (("Unable to parse key=value with key of " ++ k
                           ++ "\n\t") ++)
                       )

ignoreSep :: (a -> b -> c) -> Parse a -> Parse sep -> Parse b -> Parse c
ignoreSep f pa sep pb = f <$> pa <* sep <*> pb

commaSep :: (ParseDot a, ParseDot b) => Parse (a, b)
commaSep = commaSep' parse parse

commaSepUnqt :: (ParseDot a, ParseDot b) => Parse (a, b)
commaSepUnqt = commaSep' parseUnqt parseUnqt

commaSep'       :: Parse a -> Parse b -> Parse (a,b)
commaSep' pa pb = ignoreSep (,) pa (wrapWhitespace parseComma) pb

parseComma :: Parse ()
parseComma = character ',' *> return ()

tryParseList :: (ParseDot a) => Parse [a]
tryParseList = tryParseList' parse

tryParseList' :: Parse [a] -> Parse [a]
tryParseList' = fmap (fromMaybe []) . optional

parseAngled :: Parse a -> Parse a
parseAngled = bracket (character '<') (character '>')

parseBraced :: Parse a -> Parse a
parseBraced = bracket (character '{') (character '}')

-- -----------------------------------------------------------------------------
-- These instances are defined here to avoid cyclic imports and orphan instances

instance ParseDot ColorScheme where
  parseUnqt = parseColorScheme True

parseColorScheme     :: Bool -> Parse ColorScheme
parseColorScheme scs = do cs <- oneOf [ stringRep X11 "X11"
                                      , stringRep SVG "svg"
                                      , Brewer <$> parseUnqt
                                      ]
                          when scs $ setColorScheme cs
                          return cs

instance ParseDot BrewerScheme where
  parseUnqt = liftA2 BScheme parseUnqt parseUnqt

instance ParseDot BrewerName where
  -- The order is different from above to make sure longer names are
  -- parsed first.
  parseUnqt = stringValue [ ("accent", Accent)
                          , ("blues", Blues)
                          , ("brbg", Brbg)
                          , ("bugn", Bugn)
                          , ("bupu", Bupu)
                          , ("dark2", Dark2)
                          , ("gnbu", Gnbu)
                          , ("greens", Greens)
                          , ("greys", Greys)
                          , ("oranges", Oranges)
                          , ("orrd", Orrd)
                          , ("paired", Paired)
                          , ("pastel1", Pastel1)
                          , ("pastel2", Pastel2)
                          , ("piyg", Piyg)
                          , ("prgn", Prgn)
                          , ("pubugn", Pubugn)
                          , ("pubu", Pubu)
                          , ("puor", Puor)
                          , ("purd", Purd)
                          , ("purples", Purples)
                          , ("rdbu", Rdbu)
                          , ("rdgy", Rdgy)
                          , ("rdpu", Rdpu)
                          , ("rdylbu", Rdylbu)
                          , ("rdylgn", Rdylgn)
                          , ("reds", Reds)
                          , ("set1", Set1)
                          , ("set2", Set2)
                          , ("set3", Set3)
                          , ("spectral", Spectral)
                          , ("ylgnbu", Ylgnbu)
                          , ("ylgn", Ylgn)
                          , ("ylorbr", Ylorbr)
                          , ("ylorrd", Ylorrd)
                          ]
