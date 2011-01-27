{- |
   Module      : Data.GraphViz.PreProcessing
   Description : Pre-process imported Dot code.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   "Real life" Dot code contains various items that are not directly
   parseable by this library.  This module defines the 'preProcess'
   function to remove these components, which include:

     * Comments (both @\/* ... *\/@ style and @\/\/ ... @ style);

     * Pre-processor lines (lines starting with a @#@);

     * Split lines (by inserting a @\\@ the rest of that \"line\" is
       continued on the next line).

     * 'String's concatenated together using @\"...\" + \"...\"@;
       these are concatenated into one big 'String'.
-}
module Data.GraphViz.PreProcessing(preProcess) where

import Data.GraphViz.Parsing

import qualified Data.Text.Lazy as T
import Data.Text.Lazy(Text)
import Control.Monad(liftM)

-- -----------------------------------------------------------------------------
-- Filtering out unwanted Dot items such as comments

-- | Remove unparseable features of Dot, such as comments and
--   multi-line strings (which are converted to single-line strings).
preProcess :: Text -> Text
preProcess = runParser' parseOutUnwanted
             -- snd should be null

-- | Parse out comments and make quoted strings spread over multiple
--   lines only over a single line.  Should parse the /entire/ input
--   'Text'.
parseOutUnwanted :: Parse Text
parseOutUnwanted = liftM T.concat (many getNext)
    where
      getNext = parseConcatStrings
                `onFail`
                parseHTML
                `onFail`
                (parseUnwanted >> return T.empty)
                `onFail`
                liftM T.singleton next

-- | Parses an unwanted part of the Dot code (comments and
--   pre-processor lines; also un-splits lines).
parseUnwanted :: Parse ()
parseUnwanted = oneOf [ parseLineComment
                      , parseMultiLineComment
                      , parsePreProcessor
                      , parseSplitLine
                      ]
                >> return ()

-- | Remove pre-processor lines (that is, those that start with a
--   @#@).  Will consume the newline from the beginning of the
--   previous line, but will leave the one from the pre-processor line
--   there (so in the end it just removes the line).
parsePreProcessor :: Parse Text
parsePreProcessor = do newline
                       character '#'
                       consumeLine

-- | Parse @//@-style comments.
parseLineComment :: Parse Text
parseLineComment = string "//"
                   -- Note: do /not/ consume the newlines, as they're
                   -- needed in case the next line is a pre-processor
                   -- line.
                   >> consumeLine

-- | Parse @/* ... */@-style comments.
parseMultiLineComment :: Parse Text
parseMultiLineComment = bracket start end (liftM T.concat $ many inner)
    where
      start = string "/*"
      end = string "*/"
      inner = many1Satisfy ('*' /=)
              `onFail`
              do ast <- character '*'
                 n <- satisfy ('/' /=)
                 liftM (T.cons ast . T.cons n) inner

parseConcatStrings :: Parse Text
parseConcatStrings = liftM (wrapQuotes . T.concat)
                     $ sepBy1 parseString parseConcat
  where
    parseString = quotedParse (liftM T.concat $ many parseInner)
    parseInner = (string "\\\"" >> return (T.pack "\\\""))
                 `onFail`
                 parseSplitLine -- in case there's a split mid-quote
                 `onFail`
                 liftM T.singleton (satisfy (quoteChar /=))
    parseConcat = parseSep >> character '+' >> parseSep
    parseSep = many $ allWhitespace `onFail` parseUnwanted
    wrapQuotes str = quoteChar `T.cons` str `T.snoc` quoteChar


-- | Lines can be split with a @\\@ at the end of the line.
parseSplitLine :: Parse Text
parseSplitLine = character '\\' >> newline >> return T.empty

parseHTML :: Parse Text
parseHTML = liftM (addAngled . T.concat)
            . parseAngled $ many inner
  where
    inner = parseHTML
            `onFail`
            many1Satisfy (\c -> c /= open && c /= close)
    addAngled str = open `T.cons` str `T.snoc` close
    open = '<'
    close = '>'
