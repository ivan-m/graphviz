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

import Control.Monad(liftM)

-- -----------------------------------------------------------------------------
-- Filtering out unwanted Dot items such as comments

-- | Remove unparseable features of Dot, such as comments and
--   multi-line strings (which are converted to single-line strings).
preProcess :: String -> String
preProcess = runParser' parseOutUnwanted
             -- snd should be null

-- | Parse out comments and make quoted strings spread over multiple
--   lines only over a single line.  Should parse the /entire/ input
--   'String'.
parseOutUnwanted :: Parse String
parseOutUnwanted = liftM concat (many getNext)
    where
      getNext = parseConcatStrings
                `onFail`
                (parseUnwanted >> return [])
                `onFail`
                liftM return next

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
parsePreProcessor :: Parse String
parsePreProcessor = do newline
                       character '#'
                       consumeLine

-- | Parse @//@-style comments.
parseLineComment :: Parse String
parseLineComment = string "//"
                   -- Note: do /not/ consume the newlines, as they're
                   -- needed in case the next line is a pre-processor
                   -- line.
                   >> consumeLine

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

parseConcatStrings :: Parse String
parseConcatStrings = liftM (wrapQuotes . concat)
                     $ sepBy1 parseString parseConcat
  where
    parseString = quotedParse (liftM concat $ many parseInner)
    parseInner = string "\\\""
                 `onFail`
                 parseSplitLine -- in case there's a split mid-quote
                 `onFail`
                 liftM return (satisfy ((/=) quoteChar))
    parseConcat = parseSep >> character '+' >> parseSep
    parseSep = many $ allWhitespace `onFail` parseUnwanted
    wrapQuotes str = quoteChar : str ++ [quoteChar]


-- | Lines can be split with a @\\@ at the end of the line.
parseSplitLine :: Parse String
parseSplitLine = character '\\' >> newline >> return ""
