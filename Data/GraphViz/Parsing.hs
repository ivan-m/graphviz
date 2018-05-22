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
    , runParserWith
    , parseLiberally
    , checkValidParse
    , checkValidParseWithRest
      -- * Convenience parsing combinators.
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
    , parseSignedFloat
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

import Data.GraphViz.Exception      (GraphvizException(NotDotCode), throw)
import Data.GraphViz.Internal.State
import Data.GraphViz.Internal.Util

-- To avoid orphan instances and cyclic imports
import Data.GraphViz.Attributes.ColorScheme

import           Text.ParserCombinators.Poly.StateText hiding (empty, indent,
                                                        runParser)
import qualified Text.ParserCombinators.Poly.StateText as P

import           Control.Arrow       (first, second)
import           Control.Monad       (when)
import           Data.Char           (isDigit, isLower, isSpace, toLower,
                                      toUpper)
import           Data.Function       (on)
import           Data.List           (groupBy, sortBy)
import           Data.Maybe          (fromMaybe, isJust, isNothing, listToMaybe,
                                      maybeToList)
import           Data.Ratio          ((%))
import qualified Data.Set            as Set
import qualified Data.Text           as ST
import           Data.Text.Lazy      (Text)
import qualified Data.Text.Lazy      as T
import qualified Data.Text.Lazy.Read as T
import           Data.Version        (Version(..))
import           Data.Word           (Word16, Word8)

-- -----------------------------------------------------------------------------
-- Based off code from Text.Parse in the polyparse library

-- | A @ReadS@-like type alias.
type Parse a = Parser GraphvizState a

runParser :: Parse a -> Text -> (Either String a, Text)
runParser = runParserWith id

parseLiberally    :: GraphvizState -> GraphvizState
parseLiberally gs = gs { parseStrictly = False }

runParserWith     :: (GraphvizState -> GraphvizState) -> Parse a -> Text
                     -> (Either String a, Text)
runParserWith f p t = let (r,_,t') = P.runParser p (f initialState) t
                      in (r,t')

-- | A variant of 'runParser' where it is assumed that the provided
--   parsing function consumes all of the 'Text' input (with the
--   exception of whitespace at the end).
runParser'   :: Parse a -> Text -> a
runParser' p = checkValidParseWithRest . runParser p'
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
--   'Text' that hasn't been parsed (for debugging purposes).
parseIt :: (ParseDot a) => Text -> (a, Text)
parseIt = first checkValidParse . runParser parse

-- | If unable to parse /Dot/ code properly, 'throw' a
--   'GraphvizException'.
checkValidParse :: Either String a -> a
checkValidParse (Left err) = throw (NotDotCode err)
checkValidParse (Right a)  = a

-- | If unable to parse /Dot/ code properly, 'throw' a
--   'GraphvizException', with the error containing the remaining
--   unparsed code..
checkValidParseWithRest :: (Either String a, Text) -> a
checkValidParseWithRest (Left err, rst) = throw (NotDotCode err')
  where
    err' = err ++ "\n\nRemaining input:\n\t" ++ show rst
checkValidParseWithRest (Right a,_)     = a

-- | Parse the required value with the assumption that it will parse
--   all of the input 'Text'.
parseIt' :: (ParseDot a) => Text -> a
parseIt' = runParser' parse

instance ParseDot Int where
  parseUnqt = parseSignedInt

instance ParseDot Integer where
  parseUnqt = parseSigned parseInt

instance ParseDot Word8 where
  parseUnqt = parseInt

instance ParseDot Word16 where
  parseUnqt = parseInt

instance ParseDot Double where
  parseUnqt = parseSignedFloat True

  parse = quotedParse parseUnqt
          <|> parseSignedFloat False

  parseUnqtList = sepBy1 parseUnqt (character ':')

  parseList = quotedParse parseUnqtList
              `onFail`
              fmap (:[]) parse

instance ParseDot Bool where
  parseUnqt = onlyBool
              `onFail`
              fmap (zero /=) parseSignedInt
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

-- | Ignores 'versionTags' and assumes 'not . null . versionBranch'
--   (usually you want 'length . versionBranch == 2') and that all
--   such values are non-negative.
instance ParseDot Version where
  parseUnqt = createVersion <$> sepBy1 (parseIntCheck False) (character '.')

  parse = quotedParse parseUnqt
          <|>
          (createVersion .) . (. maybeToList) . (:)
             <$> (parseIntCheck False) <*> optional (character '.' *> parseInt)
             -- Leave the last one to check for possible decimals
             -- afterwards as there should be at most two version
             -- numbers here.

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

instance ParseDot ST.Text where
  parseUnqt = T.toStrict <$> parseUnqt

  parse = T.toStrict <$> parse

instance (ParseDot a) => ParseDot [a] where
  parseUnqt = parseUnqtList

  parse = parseList

-- | Parse a 'Text' that doesn't need to be quoted.
quotelessString :: Parse Text
quotelessString = numString False `onFail` stringBlock

numString :: Bool -> Parse Text
numString q = fmap tShow (parseStrictFloat q)
              `onFail`
              fmap tShow parseSignedInt
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
parseInt = parseIntCheck True

-- | Flag indicates whether to check whether the number is actually a
--   floating-point value.
parseIntCheck    :: (Integral a) => Bool -> Parse a
parseIntCheck ch = do cs <- many1Satisfy isDigit
                            `adjustErr` ("Expected one or more digits\n\t"++)
                      case T.decimal cs of
                        Right (n,"")  -> bool return checkInt ch n
                        -- This case should never actually happen...
                        Right (_,txt) -> fail $ "Trailing digits not parsed as Integral: " ++ T.unpack txt
                        Left err      -> fail $ "Could not read Integral: " ++ err
  where
    checkInt n = do c <- optional $ oneOf [ character '.', character 'e' ]
                    if isJust c
                      then fail "This number is actually Floating, not Integral!"
                      else return n

parseSignedInt :: Parse Int
parseSignedInt = parseSigned parseInt

-- | Parse a floating point number that actually contains decimals.
--   Bool flag indicates whether values that need to be quoted are
--   parsed.
parseStrictFloat :: Bool -> Parse Double
parseStrictFloat = parseSigned . parseFloat

-- | Bool flag indicates whether to allow parsing exponentiated term,
-- as this is only allowed when quoted.
parseFloat :: (RealFrac a) => Bool -> Parse a
parseFloat q = do ds   <- manySatisfy isDigit
                  frac <- optional $ character '.' *> manySatisfy isDigit
                  when (T.null ds && noDec frac)
                    (fail "No actual digits in floating point number!")
                  expn  <- bool (pure Nothing) (optional parseExp) q
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
                   parseSignedInt)
    noDec = maybe True T.null

-- Bool indicates whether we can parse values that need quotes.
parseSignedFloat :: Bool -> Parse Double
parseSignedFloat q = parseSigned ( parseFloat q <|> fmap fI parseInt )
  where
    fI :: Integer -> Double
    fI = fromIntegral

-- -----------------------------------------------------------------------------

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

-- | Assumes that any letter is ASCII for case-insensitive
--   comparisons.
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

-- | Parse and discard optional surrounding whitespace.
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

parseQuote :: Parse ()
parseQuote = character quoteChar *> return ()

orQuote   :: Parse Char -> Parse Char
orQuote p = stringRep quoteChar "\\\""
            `onFail`
            p

quoteChar :: Char
quoteChar = '"'

-- | Parse a 'Text' where the provided 'Char's (as well as @\"@ and
--   @\\@) are escaped and the second list of 'Char's are those that
--   are not permitted.  Note: does not parse surrounding quotes.  The
--   'Bool' value indicates whether empty 'Text's are allowed or not.
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

-- | The opposite of 'bracket'.
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

-- | Try to parse a list of the specified type; returns an empty list
--   if parsing fails.
tryParseList :: (ParseDot a) => Parse [a]
tryParseList = tryParseList' parse

-- | Return an empty list if parsing a list fails.
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
