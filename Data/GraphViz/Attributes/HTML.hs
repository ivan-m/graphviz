{- |
   Module      : Data.GraphViz.Attributes.HTML
   Description : Specification of HTML-like types for Graphviz.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines the syntax for HTML-like values for use in
   Graphviz.  Please note that these values are /not/ really HTML, but
   the term \"HTML\" is used throughout as it is less cumbersome than
   \"HTML-like\".  To be able to use this, the version of Graphviz must
   be at least 1.10.  For more information, please see:
       <http://graphviz.org/doc/info/shapes.html#html>

   The actual definition of the syntax specifies that these types must
   be valid XML syntax.  As such, this assumed when printing and parsing,
   though the correct escape/descaping for @\"@, @&@, @<@ and @>@ are
   automatically done when printing and parsing.

   Differences from how Graphviz treats HTML-like values:

   * Graphviz only specifies the above-listed characters must be
     escaped; however, internally it also escapes @-@, @\'@ and multiple
     sequences of spaces.  This library attempts to match this behaviour.
     Please let me know if this behaviour (especially about escaping
     multiple spaces) is unwanted.

   * When parsing escaped HTML characters, numeric escapes are
     converted to the corresponding character as are the various characters
     listed above; all other escaped characters (apart from those listed
     above) are silently ignored and removed from the input (since
     technically these must be valid /XML/, which doesn't recognise as many
     named escape characters as does HTML).

   * All whitespace read in is kept (even if Graphviz would ignore
     multiple whitespace characters); when printing them, however, they are
     replaced with non-breaking spaces.  As such, if multiple literal
     whitespace characters are used in a sequence, then the result of
     parsing and then printing some Dot code will /not/ be the same as the
     initial Dot code.  Furthermore, all whitespace characters are printed
     as spaces.

   * It is assumed that all parsed @&@ values are the beginning of an
     XML escape sequence (which /must/ finish with a @;@ character).

   * There should be no pre-escaped characters in values; when
     printing, the @&@ will get escaped without considering if that is an
     escaped character.

-}
module Data.GraphViz.Attributes.HTML
       ( HtmlLabel(..)
       , HtmlText
       , HtmlTextItem(..)
       , HtmlTable(..)
       , HtmlRow(..)
       , HtmlCell(..)
       , HtmlImg(..)
       , HtmlAttributes
       , HtmlAttribute(..)
       , HtmlAlign(..)
       , HtmlVAlign(..)
       , HtmlScale(..)
       ) where

import Data.GraphViz.Parsing
import Data.GraphViz.Printing
import Data.GraphViz.Attributes.Colors
import Data.GraphViz.Attributes.Internal
import Data.GraphViz.Util(bool)

import Numeric(readHex)
import Data.Char(chr, ord, isSpace)
import Data.Function(on)
import Data.List(groupBy, delete)
import Data.Maybe(maybeToList, listToMaybe)
import Data.Word(Word8, Word16)
import qualified Data.Map as Map
import Control.Monad(liftM)

-- -----------------------------------------------------------------------------

-- | The overall type for HTML-like labels.  Fundamentally, HTML-like
--   values in Graphviz are either textual (i.e. a single element with
--   formatting) or a table.  Note that 'HtmlLabel' values can be
--   nested via 'HtmlLabelCell'.
data HtmlLabel = HtmlText HtmlText
               | HtmlTable HtmlTable
               deriving (Eq, Ord, Show, Read)

instance PrintDot HtmlLabel where
  unqtDot (HtmlText txt)  = unqtDot txt
  unqtDot (HtmlTable tbl) = unqtDot tbl

instance ParseDot HtmlLabel where
  -- Try parsing HtmlTable first in case of a FONT tag being used.
  parseUnqt = liftM HtmlTable parseUnqt
              `onFail`
              liftM HtmlText parseUnqt
              `adjustErr`
              (++ "\nCan't parse HtmlLabel")

  parse = parseUnqt

-- | Represents a textual component of an HTML-like label.  It is
--   assumed that an 'HtmlText' list is non-empty.  It is preferable
--   to \"group\" 'HtmlStr' values together rather than have
--   individual ones.  Note that when printing, the individual values
--   are concatenated together without spaces, and when parsing
--   anything that isn't a tag is assumed to be an 'HtmlStr': that is,
--   something like @<BR/> <BR/>@ is parsed as:
--
--  > ['HtmlNewline' [], 'HtmlStr' \" \", 'HtmlNewline' []]
type HtmlText = [HtmlTextItem]

-- | Textual items in HTML-like labels.
data HtmlTextItem = HtmlStr String
                    -- | Only accepts an optional 'HtmlAlign'
                    --   'HtmlAttribute'; defined this way for ease of
                    --   printing/parsing.
                  | HtmlNewline HtmlAttributes
                  | HtmlFont HtmlAttributes HtmlText
                  deriving (Eq, Ord, Show, Read)

instance PrintDot HtmlTextItem where
  unqtDot (HtmlStr str)     = escapeValue str
  unqtDot (HtmlNewline as)  = printHtmlEmptyTag (text "BR") as
  unqtDot (HtmlFont as txt) = printHtmlFontTag as $ unqtDot txt

  unqtListToDot = hcat . map unqtDot

  listToDot = unqtListToDot

instance ParseDot HtmlTextItem where
  parseUnqt = oneOf [ liftM HtmlStr unescapeValue
                    , parseHtmlEmptyTag HtmlNewline "BR"
                    , parseHtmlFontTag HtmlFont parseUnqt
                    ]
              `adjustErr`
              (++ "\nCan't parse HtmlTextItem")

  parse = parseUnqt

  parseUnqtList = many1 parseUnqt -- sepBy1 parseUnqt allWhitespace'

  parseList = parseUnqtList

-- | A table in HTML-like labels.  Tables are optionally wrapped in
--   overall @FONT@ tags.
data HtmlTable = HTable { -- | Optional @FONT@ attributes.  @'Just'
                          --   []@ denotes empty @FONT@ tags;
                          --   @'Nothing'@ denotes no such tags.
                          tableFontAttrs :: Maybe HtmlAttributes
                        , tableAttrs     :: HtmlAttributes
                          -- | This list is assumed to be non-empty.
                        , tableRows      :: [HtmlRow]
                        }
               deriving (Eq, Ord, Show, Read)

instance PrintDot HtmlTable where
  unqtDot tbl = case tableFontAttrs tbl of
                  (Just as) -> printHtmlFontTag as tbl'
                  Nothing   -> tbl'
    where
      tbl' = printHtmlTag (text "TABLE")
                          (tableAttrs tbl)
                          (toDot $ tableRows tbl)

instance ParseDot HtmlTable where
  parseUnqt = wrapWhitespace (parseHtmlFontTag addFontAttrs pTbl)
              `onFail`
              pTbl
              `adjustErr`
              (++ "\nCan't parse HtmlTable")
    where
      pTbl = wrapWhitespace $ parseHtmlTag (HTable Nothing)
                                           "TABLE"
                                           (wrapWhitespace parseUnqt)
      addFontAttrs fas tbl = tbl { tableFontAttrs = Just fas }

  parse = parseUnqt

-- | A row in an 'HtmlTable'.  The list of 'HtmlCell' values is
--   assumed to be non-empty.
newtype HtmlRow = HtmlRow [HtmlCell]
                deriving (Eq, Ord, Show, Read)

instance PrintDot HtmlRow where
  unqtDot (HtmlRow cs) = printHtmlTag tr [] $ unqtDot cs
    where
      tr = text "TR"

  unqtListToDot = hcat . map unqtDot

  listToDot = unqtListToDot

instance ParseDot HtmlRow where
  -- To save doing it manually, use 'parseHtmlTag' and ignore any
  -- 'HtmlAttributes' that it might accidentally parse.
  parseUnqt = wrapWhitespace $ parseHtmlTag (const HtmlRow) "TR" parseUnqt
              `adjustErr`
              (++ "\nCan't parse HtmlRow")

  parse = parseUnqt

  parseUnqtList = wrapWhitespace $ sepBy1 parseUnqt allWhitespace'

  parseList = parseUnqtList

-- | Cells either recursively contain another 'HtmlLabel' or else a
--   path to an image file.
data HtmlCell = HtmlLabelCell HtmlAttributes HtmlLabel
              | HtmlImgCell HtmlAttributes HtmlImg
              deriving (Eq, Ord, Show, Read)

instance PrintDot HtmlCell where
  unqtDot (HtmlLabelCell as l) = printCell as $ unqtDot l
  unqtDot (HtmlImgCell as img) = printCell as $ unqtDot img

  unqtListToDot = hsep . map unqtDot

  listToDot = unqtListToDot

printCell :: HtmlAttributes -> DotCode -> DotCode
printCell = printHtmlTag (text "TD")

instance ParseDot HtmlCell where
  parseUnqt = oneOf [ parseCell HtmlLabelCell parse
                    , parseCell HtmlImgCell $ wrapWhitespace parseUnqt
                    ]
              `adjustErr`
              (++ "\nCan't parse HtmlCell")
    where
      parseCell = flip parseHtmlTag "TD"

  parse = parseUnqt

  parseUnqtList = wrapWhitespace $ sepBy1 parseUnqt allWhitespace'

  parseList = parseUnqtList

-- | The path to an image; accepted 'HtmlAttributes' are 'HtmlScale' and 'HtmlSrc'.
newtype HtmlImg = HtmlImg HtmlAttributes
             deriving (Eq, Ord, Show, Read)

instance PrintDot HtmlImg where
  unqtDot (HtmlImg as) = printHtmlEmptyTag (text "IMG") as

instance ParseDot HtmlImg where
  parseUnqt = wrapWhitespace (parseHtmlEmptyTag HtmlImg "IMG")
              `adjustErr`
              (++ "\nCan't parse HtmlImg")

  parse = parseUnqt

-- -----------------------------------------------------------------------------

-- | The various HTML-like label-specific attributes being used.
type HtmlAttributes = [HtmlAttribute]

-- | Note that not all 'HtmlAttribute' values are valid everywhere:
--   see the comments for each one on where it is valid.
data HtmlAttribute = HtmlAlign HtmlAlign   -- ^ Valid for:  'HtmlTable', 'HtmlCell', 'HtmlNewline'.
                   | HtmlBAlign HtmlAlign  -- ^ Valid for: 'HtmlCell'.
                   | HtmlBGColor Color     -- ^ Valid for: 'HtmlTable' (including 'tableFontAttrs'), 'HtmlCell', 'HtmlFont'.
                   | HtmlBorder Word8      -- ^ Valid for: 'HtmlTable', 'HtmlCell'.  Default is @1@; @0@ represents no border.
                   | HtmlCellBorder Word8  -- ^ Valid for: 'HtmlTable'.  Default is @1@; @0@ represents no border.
                   | HtmlCellPadding Word8 -- ^ Valid for: 'HtmlTable', 'HtmlCell'.  Default is @2@.
                   | HtmlCellSpacing Word8 -- ^ Valid for: 'HtmlTable', 'HtmlCell'.  Default is @2@; maximum is @127@.
                   | HtmlColor Color       -- ^ Valid for: 'HtmlTable', 'HtmlCell'.
                   | HtmlColSpan Word16    -- ^ Valid for: 'HtmlCell'.  Default is @1@.
                   | HtmlFace String       -- ^ Valid for: 'tableFontAttrs', 'HtmlFont'.
                   | HtmlFixedSize Bool    -- ^ Valid for: 'HtmlTable', 'HtmlCell'.  Default is @'False'@.
                   | HtmlHeight Word16     -- ^ Valid for: 'HtmlTable', 'HtmlCell'.
                   | HtmlHRef String       -- ^ Valid for: 'HtmlTable', 'HtmlCell'.
                   | HtmlPointSize Double  -- ^ Valid for: 'tableFontAttrs', 'HtmlFont'.
                   | HtmlPort PortName     -- ^ Valid for: 'HtmlTable', 'HtmlCell'.
                   | HtmlRowSpan Word16    -- ^ Valid for: 'HtmlCell'.
                   | HtmlScale HtmlScale   -- ^ Valid for: 'HtmlImg'.
                   | HtmlSrc FilePath      -- ^ Valid for: 'HtmlImg'.
                   | HtmlTarget String     -- ^ Valid for: 'HtmlTable', 'HtmlCell'.
                   | HtmlTitle String      -- ^ Valid for: 'HtmlTable', 'HtmlCell'.  Has an alias of @TOOLTIP@.
                   | HtmlVAlign HtmlVAlign -- ^ Valid for: 'HtmlTable', 'HtmlCell'.
                   | HtmlWidth Word16      -- ^ Valid for: 'HtmlTable', 'HtmlCell'.
                   deriving (Eq, Ord, Show, Read)

instance PrintDot HtmlAttribute where
  unqtDot (HtmlAlign v)       = printHtmlField  "ALIGN" v
  unqtDot (HtmlBAlign v)      = printHtmlField  "BALIGN" v
  unqtDot (HtmlBGColor v)     = printHtmlField  "BGCOLOR" v
  unqtDot (HtmlBorder v)      = printHtmlField  "BORDER" v
  unqtDot (HtmlCellBorder v)  = printHtmlField  "CELLBORDER" v
  unqtDot (HtmlCellPadding v) = printHtmlField  "CELLPADDING" v
  unqtDot (HtmlCellSpacing v) = printHtmlField  "CELLSPACING" v
  unqtDot (HtmlColor v)       = printHtmlField  "COLOR" v
  unqtDot (HtmlColSpan v)     = printHtmlField  "COLSPAN" v
  unqtDot (HtmlFace v)        = printHtmlField' "FACE" $ escapeAttribute v
  unqtDot (HtmlFixedSize v)   = printHtmlField' "FIXEDSIZE" $ printBoolHtml v
  unqtDot (HtmlHeight v)      = printHtmlField  "HEIGHT" v
  unqtDot (HtmlHRef v)        = printHtmlField' "HREF" $ escapeAttribute v
  unqtDot (HtmlPointSize v)   = printHtmlField  "POINTSIZE" v
  unqtDot (HtmlPort v)        = printHtmlField' "PORT" . escapeAttribute $ portName v
  unqtDot (HtmlRowSpan v)     = printHtmlField  "ROWSPAN" v
  unqtDot (HtmlScale v)       = printHtmlField  "SCALE" v
  unqtDot (HtmlSrc v)         = printHtmlField' "SRC" $ escapeAttribute v
  unqtDot (HtmlTarget v)      = printHtmlField' "TARGET" $ escapeAttribute v
  unqtDot (HtmlTitle v)       = printHtmlField' "TITLE" $ escapeAttribute v
  unqtDot (HtmlVAlign v)      = printHtmlField  "VALIGN" v
  unqtDot (HtmlWidth v)       = printHtmlField  "WIDTH" v

  unqtListToDot = hsep . map unqtDot

  listToDot = unqtListToDot

-- | Only to be used when the 'PrintDot' instance of @a@ matches the
--   HTML syntax (i.e. numbers and @Html*@ values; 'Color' values also
--   seem to work).
printHtmlField   :: (PrintDot a) => String -> a -> DotCode
printHtmlField f = printHtmlField' f . unqtDot

printHtmlField'     :: String -> DotCode -> DotCode
printHtmlField' f v = text f <> equals <> doubleQuotes v

instance ParseDot HtmlAttribute where
  parseUnqt = oneOf [ parseHtmlField  HtmlAlign "ALIGN"
                    , parseHtmlField  HtmlBAlign "BALIGN"
                    , parseHtmlField  HtmlBGColor "BGCOLOR"
                    , parseHtmlField  HtmlBorder "BORDER"
                    , parseHtmlField  HtmlCellBorder "CELLBORDER"
                    , parseHtmlField  HtmlCellPadding "CELLPADDING"
                    , parseHtmlField  HtmlCellSpacing "CELLSPACING"
                    , parseHtmlField  HtmlColor "COLOR"
                    , parseHtmlField  HtmlColSpan "COLSPAN"
                    , parseHtmlField' HtmlFace "FACE" unescapeAttribute
                    , parseHtmlField' HtmlFixedSize "FIXEDSIZE" parseBoolHtml
                    , parseHtmlField  HtmlHeight "HEIGHT"
                    , parseHtmlField' HtmlHRef "HREF" unescapeAttribute
                    , parseHtmlField  HtmlPointSize "POINTSIZE"
                    , parseHtmlField' (HtmlPort . PN) "PORT" unescapeAttribute
                    , parseHtmlField  HtmlRowSpan "ROWSPAN"
                    , parseHtmlField  HtmlScale "SCALE"
                    , parseHtmlField' HtmlSrc "SRC" unescapeAttribute
                    , parseHtmlField' HtmlTarget "TARGET" unescapeAttribute
                    , parseHtmlField' HtmlTitle "TITLE" unescapeAttribute
                      `onFail`
                      parseHtmlField' HtmlTitle "TOOLTIP" unescapeAttribute
                    , parseHtmlField  HtmlVAlign "VALIGN"
                    , parseHtmlField  HtmlWidth "WIDTH"
                    ]

  parse = parseUnqt

  parseUnqtList = sepBy parseUnqt allWhitespace -- needs at least one whitespace char

  parseList = parseUnqtList



parseHtmlField     :: (ParseDot a) => (a -> HtmlAttribute) -> String
                      -> Parse HtmlAttribute
parseHtmlField c f = parseHtmlField' c f parseUnqt

parseHtmlField'       :: (a -> HtmlAttribute) -> String -> Parse a
                         -> Parse HtmlAttribute
parseHtmlField' c f p = do string f
                           whitespace'
                           character '='
                           whitespace'
                           liftM c $ quotedParse p

-- | Specifies horizontal placement. When an object is allocated more
--   space than required, this value determines where the extra space
--   is placed left and right of the object.
data HtmlAlign = HLeft
               | HCenter -- ^ Default value.
               | HRight
               | HText -- ^ 'HtmlLabelCell' values only; aligns lines
                       --   of text using the full cell width. The
                       --   alignment of a line is determined by its
                       --   (possibly implicit) associated
                       --   'HtmlNewline' element.
               deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot HtmlAlign where
  unqtDot HLeft   = text "LEFT"
  unqtDot HCenter = text "CENTER"
  unqtDot HRight  = text "RIGHT"
  unqtDot HText   = text "TEXT"

instance ParseDot HtmlAlign where
  parseUnqt = oneOf [ stringRep HLeft "LEFT"
                    , stringRep HCenter "CENTER"
                    , stringRep HRight "RIGHT"
                    , stringRep HText "TEXT"
                    ]

  parse = parseUnqt

-- | Specifies vertical placement. When an object is allocated more
--   space than required, this value determines where the extra space
--   is placed above and below the object.
data HtmlVAlign = HTop
                | HMiddle -- ^ Default value.
                | HBottom
                deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot HtmlVAlign where
  unqtDot HTop    = text "TOP"
  unqtDot HMiddle = text "MIDDLE"
  unqtDot HBottom = text "BOTTOM"

instance ParseDot HtmlVAlign where
  parseUnqt = oneOf [ stringRep HTop "TOP"
                    , stringRep HMiddle "MIDDLE"
                    , stringRep HBottom "BOTTOM"
                    ]

  parse = parseUnqt

-- | Specifies how an image will use any extra space available in its
--   cell.  If undefined, the image inherits the value of the
--   @ImageScale@ attribute.
data HtmlScale = HtmlNaturalSize -- ^ Default value.
               | HtmlScaleUniformly
               | HtmlExpandWidth
               | HtmlExpandHeight
               | HtmlExpandBoth
               deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot HtmlScale where
  unqtDot HtmlNaturalSize    = text "FALSE"
  unqtDot HtmlScaleUniformly = text "TRUE"
  unqtDot HtmlExpandWidth    = text "WIDTH"
  unqtDot HtmlExpandHeight   = text "HEIGHT"
  unqtDot HtmlExpandBoth     = text "BOTH"

instance ParseDot HtmlScale where
  parseUnqt = oneOf [ stringRep HtmlNaturalSize "FALSE"
                    , stringRep HtmlScaleUniformly "TRUE"
                    , stringRep HtmlExpandWidth "WIDTH"
                    , stringRep HtmlExpandHeight "HEIGHT"
                    , stringRep HtmlExpandBoth "BOTH"
                    ]

  parse = parseUnqt

-- -----------------------------------------------------------------------------

escapeAttribute :: String -> DotCode
escapeAttribute = escapeHtml False

escapeValue :: String -> DotCode
escapeValue = escapeHtml True

escapeHtml               :: Bool -> String -> DotCode
escapeHtml quotesAllowed = hcat
                           . concatMap escapeSegment
                           . groupBy ((==) `on` isSpace)
  where
    -- Note: use numeric version of space rather than nbsp, since this
    -- matches what Graphviz does (since Inkscape apparently can't
    -- cope with nbsp).
    escapeSegment (s:sps) | isSpace s = char s : map numEscape sps
    escapeSegment txt                 = map xmlChar txt

    allowQuotes = if quotesAllowed
                  then Map.delete '"'
                  else id

    escs = allowQuotes $ Map.fromList htmlEscapes
    xmlChar c = maybe (char c) escape $ c `Map.lookup` escs

    numEscape = escape' . (<>) (char '#') . int . ord
    escape' e = char '&' <> e <> char ';'
    escape = escape' . text

unescapeAttribute :: Parse String
unescapeAttribute = unescapeHtml False

unescapeValue :: Parse String
unescapeValue = unescapeHtml True

-- | Parses an HTML-compatible 'String', de-escaping known characters.
--   Note: this /will/ fail if an unknown non-numeric HTML-escape is
--   used.
unescapeHtml               :: Bool -> Parse String
unescapeHtml quotesAllowed = liftM concat
                             . many1 . oneOf $ [ parseEscpd
                                               , validChars
                                               ]
  where
    parseEscpd = do character '&'
                    esc <- many1 $ satisfy ((/=) ';')
                    character ';'
                    let c = case esc of
                              ('#':'x':hex) -> readMaybe readHex hex
                              ('#':'X':hex) -> readMaybe readHex hex
                              ('#':dec)     -> readMaybe readInt dec
                              _             -> esc `Map.lookup` escMap
                    return $ maybeToList c

    readMaybe f str = do (n, []) <- listToMaybe $ f str
                         return $ chr n
    readInt :: ReadS Int
    readInt = reads

    allowQuotes = if quotesAllowed
                  then delete '"'
                  else id

    escMap = Map.fromList htmlUnescapes

    validChars = liftM return $ satisfy (`notElem` needEscaping)
    needEscaping = allowQuotes $ map fst htmlEscapes

-- | The characters that need to be escaped and what they need to be
--   replaced with (sans @'&'@).
htmlEscapes :: [(Char, String)]
htmlEscapes = [ ('"', "quot")
              , ('<', "lt")
              , ('>', "gt")
              , ('&', "amp")
              ]
              ++ map numEscape ['-', '\'']
  where
    numEscape c = (c, '#' : show (ord c))

-- | Flip the order and add extra values that might be escaped.  More
--   specifically, provide the escape code for spaces (@\"nbsp\"@) and
--   apostrophes (@\"apos\"@) since they aren't used for escaping.
htmlUnescapes :: [(String, Char)]
htmlUnescapes = maybeEscaped
                ++
                map (uncurry (flip (,))) htmlEscapes
  where
    maybeEscaped = [("nbsp", ' '), ("apos", '\'')]

printBoolHtml :: Bool -> DotCode
printBoolHtml = text . bool "FALSE" "TRUE"

parseBoolHtml :: Parse Bool
parseBoolHtml = stringRep True "TRUE"
                `onFail`
                stringRep False "FALSE"

-- -----------------------------------------------------------------------------

-- | Print something like @<FOO ATTR=\"ATTR_VALUE\">value<\/FOO>@
printHtmlTag        :: DotCode -> HtmlAttributes -> DotCode -> DotCode
printHtmlTag t as v = angled (t <+> toDot as)
                      <> v
                      <> angled (fslash <> t)

printHtmlFontTag :: HtmlAttributes -> DotCode -> DotCode
printHtmlFontTag = printHtmlTag (text "FONT")

-- | Print something like @<FOO ATTR=\"ATTR_VALUE\"\/>@
printHtmlEmptyTag      :: DotCode -> HtmlAttributes -> DotCode
printHtmlEmptyTag t as = angled $ t <+> toDot as <> fslash

-- -----------------------------------------------------------------------------

-- Note: can't use bracket here because we're not completely
-- discarding everything from the opening bracket.

-- | Parse something like @<FOO ATTR=\"ATTR_VALUE\">value<\/FOO>@
parseHtmlTag        :: (HtmlAttributes -> val -> tag) -> String
                       -> Parse val -> Parse tag
parseHtmlTag c t pv = do as <- parseAngled openingTag
                         v <- pv
                         parseAngled $ character '/' >> t' >> allWhitespace'
                         return $ c as v
  where
    t' = string t
    openingTag = do t'
                    as <- tryParseList' $ allWhitespace >> parse
                    allWhitespace'
                    return as

parseHtmlFontTag :: (HtmlAttributes -> val -> tag) -> Parse val -> Parse tag
parseHtmlFontTag = flip parseHtmlTag "FONT"

-- | Parse something like @<FOO ATTR=\"ATTR_VALUE\"\/>@
parseHtmlEmptyTag     :: (HtmlAttributes -> tag) -> String -> Parse tag
parseHtmlEmptyTag c t = parseAngled
                        ( do string t
                             as <- tryParseList' $ allWhitespace >> parse
                             allWhitespace'
                             character '/'
                             return $ c as
                        )
