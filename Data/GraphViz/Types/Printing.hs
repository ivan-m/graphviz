{- |
   Module      : Data.GraphViz.Types.Printing
   Description : Helper functions for converting to Dot format.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines simple helper functions for use with
   "Text.PrettyPrint".  It also re-exports all the pretty-printing
   combinators from that module.

   Note that the 'PrintDot' instances for 'Bool', etc. match those
   specified for use with Graphviz.

   You should only be using this module if you are writing custom node
   types for use with "Data.GraphViz.Types".  For actual printing of
   code, use @'Data.GraphViz.Types.printDotGraph'@ (which produces a
   'String' value).

   The Dot language specification specifies that any identifier is in
   one of four forms:

       * Any string of alphabetic ([a-zA-Z\200-\377]) characters, underscores ('_') or digits ([0-9]), not beginning with a digit;

       * a number [-]?(.[0-9]+ | [0-9]+(.[0-9]*)? );

       * any double-quoted string (\"...\") possibly containing escaped quotes (\");

       * an HTML string (<...>).

   Due to these restrictions, you should only use 'text' when you are
   sure that the 'String' in question is static and quotes are
   definitely needed/unneeded; it is better to use the 'String'
   instance for 'PrintDot'.  For more information, see the
   specification page:
      <http://graphviz.org/doc/info/lang.html>
-}
module Data.GraphViz.Types.Printing
    ( module Text.PrettyPrint
    , DotCode
    , renderDot -- Exported for Data.GraphViz.Types.printSGID
    , PrintDot(..)
    , printIt
    , addQuotes
    , wrap
    , commaDel
    , printField
    ) where

import Data.GraphViz.Types.Internal

-- Only implicitly import and re-export combinators.
import Text.PrettyPrint hiding ( Style(..)
                               , Mode(..)
                               , TextDetails(..)
                               , render
                               , style
                               , renderStyle
                               , fullRender
                               )

import qualified Text.PrettyPrint as PP

import Data.Word(Word8)

-- -----------------------------------------------------------------------------

-- | A type alias to indicate what is being produced.
type DotCode = Doc

-- | Correctly render Graphviz output.
renderDot :: DotCode -> String
renderDot = PP.renderStyle style'
    where
      style' = PP.style { PP.mode = PP.LeftMode }

-- | A class used to correctly print parts of the Graphviz Dot language.
--   Minimal implementation is 'unqtDot'.
class PrintDot a where
    -- | The unquoted representation, for use when composing values to
    --   produce a larger printing value.
    unqtDot :: a -> DotCode

    -- | The actual quoted representation; this should be quoted if it
    --   contains characters not permitted a plain ID String, a number
    --   or it is not an HTML string.
    --   Defaults to 'unqtDot'.
    toDot :: a -> DotCode
    toDot = unqtDot

    -- | The correct way of representing a list of this value when
    --   printed; not all Dot values require this to be implemented.
    --   Defaults to Haskell-like list representation.
    unqtListToDot :: [a] -> DotCode
    unqtListToDot = brackets . hsep . punctuate comma
                    . map unqtDot

    -- | The quoted form of 'unqtListToDot'; defaults to wrapping
    --   double quotes around the result of 'unqtListToDot' (since the
    --   default implementation has characters that must be quoted).
    listToDot :: [a] -> DotCode
    listToDot = doubleQuotes . unqtListToDot

-- | Convert to DotCode; note that this has no indentation, as we can
--   only have one of indentation and (possibly) infinite line lengths.
printIt :: (PrintDot a) => a -> String
printIt = renderDot . toDot

instance PrintDot Int where
    unqtDot = int

instance PrintDot Word8 where
    unqtDot = int . fromIntegral

instance PrintDot Double where
    -- If it's an "integral" double, then print as an integer.
    -- This seems to match how Graphviz apps use Dot.
    unqtDot d = if d == fromIntegral di
                then int di
                else double d
        where
          di = round d

instance PrintDot Bool where
    unqtDot True  = text "true"
    unqtDot False = text "false"

instance PrintDot Char where
    unqtDot = char

    toDot = qtChar

    unqtListToDot = unqtString

    listToDot = qtString

-- | Check to see if this 'Char' needs to be quoted or not.
qtChar :: Char -> DotCode
qtChar c
    | restIDString c = char c -- Could be a number as well.
    | otherwise      = doubleQuotes $ char c

needsQuotes :: String -> Bool
needsQuotes str
  | null str        = True
  | isKeyword str   = True
  | isIDString str  = False
  | isNumString str = False
  | otherwise       = True

addQuotes :: String -> DotCode -> DotCode
addQuotes = bool id doubleQuotes . needsQuotes

-- | Escape quotes in Strings that need them.
unqtString     :: String -> DotCode
unqtString ""  = empty
unqtString str = text $ escapeQuotes str -- no quotes? no worries!

-- | Escape quotes and quote Strings that need them (including keywords).
qtString     :: String -> DotCode
qtString str = addQuotes str $ unqtString str

instance (PrintDot a) => PrintDot [a] where
    unqtDot = unqtListToDot

    toDot = listToDot

wrap       :: DotCode -> DotCode -> DotCode -> DotCode
wrap b a d = b <> d <> a

commaDel     :: (PrintDot a, PrintDot b) => a -> b -> DotCode
commaDel a b = unqtDot a <> comma <> unqtDot b

printField     :: (PrintDot a) => String -> a -> DotCode
printField f v = text f <> equals <> toDot v
