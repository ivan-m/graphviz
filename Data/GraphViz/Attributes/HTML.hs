{-# LANGUAGE CPP, OverloadedStrings, PatternGuards #-}

{- |
   Module      : Data.GraphViz.Attributes.HTML
   Description : Specification of HTML-like types for Graphviz.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module is written to be imported qualified.  It defines the
   syntax for HTML-like values for use in Graphviz.  Please note that
   these values are /not/ really HTML, but the term \"HTML\" is used
   throughout as it is less cumbersome than \"HTML-like\".  To be able
   to use this, the version of Graphviz must be at least 1.10.  For
   more information, please see:
       <http://graphviz.org/doc/info/shapes.html#html>

   The actual definition of the syntax specifies that these types must
   be valid XML syntax.  As such, this assumed when printing and parsing,
   though the correct escape/descaping for @\"@, @&@, @\<@ and @\>@ are
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
       ( Label(..)
       , Text
       , TextItem(..)
       , Format(..)
       , Table(..)
       , Row(..)
       , Cell(..)
       , Img(..)
       , Attributes
       , Attribute(..)
       , Align(..)
       , VAlign(..)
       , CellFormat(..)
       , Scale(..)
       , Side(..)
       , Style(..)
       ) where

import Data.GraphViz.Attributes.Colors
import Data.GraphViz.Attributes.Internal
import Data.GraphViz.Internal.Util       (bool)
import Data.GraphViz.Parsing
import Data.GraphViz.Printing

import           Data.Char      (chr, isSpace, ord)
import           Data.Function  (on)
import           Data.List      (delete)
import qualified Data.Map       as Map
import           Data.Maybe     (catMaybes, listToMaybe)
import qualified Data.Text.Lazy as T
import           Data.Word      (Word16, Word8)
import           Numeric        (readHex)

#if !MIN_VERSION_base (4,13,0)
import Data.Monoid ((<>))
#endif

-- -----------------------------------------------------------------------------

-- | The overall type for HTML-like labels.  Fundamentally, HTML-like
--   values in Graphviz are either textual (i.e. a single element with
--   formatting) or a table.  Note that 'Label' values can be
--   nested via 'LabelCell'.
data Label = Text  Text
           | Table Table
           deriving (Eq, Ord, Show, Read)

instance PrintDot Label where
  unqtDot (Text txt)  = unqtDot txt
  unqtDot (Table tbl) = unqtDot tbl

instance ParseDot Label where
  -- Try parsing Table first in case of a FONT tag being used.
  parseUnqt = fmap Table parseUnqt
              `onFail`
              fmap Text parseUnqt
              `adjustErr`
              ("Can't parse Html.Label\n\t"++)

  parse = parseUnqt

-- | Represents a textual component of an HTML-like label.  It is
--   assumed that a 'Text' list is non-empty.  It is preferable
--   to \"group\" 'Str' values together rather than have
--   individual ones.  Note that when printing, the individual values
--   are concatenated together without spaces, and when parsing
--   anything that isn't a tag is assumed to be a 'Str': that is,
--   something like \"@\<BR\/\> \<BR\/\>@\" is parsed as:
--
--  > [Newline [], Str " ", Newline []]
type Text = [TextItem]

-- | Textual items in HTML-like labels.
data TextItem = Str T.Text
                -- | Only accepts an optional 'Align'
                --   'Attribute'; defined this way for ease of
                --   printing/parsing.
              | Newline Attributes
              | Font Attributes Text
                -- | Only available in Graphviz >= 2.28.0.
              | Format Format Text
              deriving (Eq, Ord, Show, Read)

instance PrintDot TextItem where
  unqtDot (Str str)        = escapeValue str
  unqtDot (Newline as)     = printEmptyTag (text "BR") as
  unqtDot (Font as txt)    = printFontTag as $ unqtDot txt
  unqtDot (Format fmt txt) = printTag (unqtDot fmt) [] $ unqtDot txt

  unqtListToDot = hcat . mapM unqtDot

  listToDot = unqtListToDot

instance ParseDot TextItem where
  parseUnqt = oneOf [ fmap Str unescapeValue
                    , parseEmptyTag Newline "BR"
                    , parseFontTag Font parseUnqt
                    , parseTagRep Format parseUnqt parseUnqt
                    ]
              `adjustErr`
              ("Can't parse Html.TextItem\n\t"++)

  parse = parseUnqt

  parseUnqtList = many parseUnqt

  parseList = parseUnqtList

data Format = Italics
              | Bold
              | Underline
              | Overline -- ^ Requires Graphviz >= 2.38.0.
              | Subscript
              | Superscript
              deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot Format where
  unqtDot Italics     = text "I"
  unqtDot Bold        = text "B"
  unqtDot Underline   = text "U"
  unqtDot Overline    = text "O"
  unqtDot Subscript   = text "SUB"
  unqtDot Superscript = text "SUP"

instance ParseDot Format where
  parseUnqt = stringValue [ ("I", Italics)
                          , ("B", Bold)
                          , ("U", Underline)
                          , ("O", Overline)
                          , ("SUB", Subscript)
                          , ("SUP", Superscript)
                          ]

-- | A table in HTML-like labels.  Tables are optionally wrapped in
--   overall @FONT@ tags.
data Table = HTable { -- | Optional @FONT@ attributes.  @'Just'
                      --   []@ denotes empty @FONT@ tags;
                      --   @'Nothing'@ denotes no such tags.
                      tableFontAttrs :: Maybe Attributes
                    , tableAttrs     :: Attributes
                      -- | This list is assumed to be non-empty.
                    , tableRows      :: [Row]
                    }
               deriving (Eq, Ord, Show, Read)

instance PrintDot Table where
  unqtDot tbl = case tableFontAttrs tbl of
                  (Just as) -> printFontTag as tbl'
                  Nothing   -> tbl'
    where
      tbl' = printTag (text "TABLE")
                          (tableAttrs tbl)
                          (toDot $ tableRows tbl)

instance ParseDot Table where
  parseUnqt = wrapWhitespace (parseFontTag addFontAttrs pTbl)
              `onFail`
              pTbl
              `adjustErr`
              ("Can't parse Html.Table\n\t"++)
    where
      pTbl = wrapWhitespace $ parseTag (HTable Nothing)
                                       "TABLE"
                                       (wrapWhitespace parseUnqt)
      addFontAttrs fas tbl = tbl { tableFontAttrs = Just fas }

  parse = parseUnqt

-- | A row in a 'Table'.  The list of 'Cell' values is
--   assumed to be non-empty.
data Row = Cells [Cell]
         | HorizontalRule -- ^ Should be between 'Cells' values,
                          --   requires Graphviz >= 2.29.0
         deriving (Eq, Ord, Show, Read)

instance PrintDot Row where
  unqtDot (Cells cs)     = printTag (text "TR") [] $ unqtDot cs
  unqtDot HorizontalRule = printEmptyTag (text "HR") []

  unqtListToDot = align . cat . mapM unqtDot

  listToDot = unqtListToDot

instance ParseDot Row where
  -- To save doing it manually, use 'parseTag' and ignore any
  -- 'Attributes' that it might accidentally parse.
  parseUnqt = wrapWhitespace $ parseTag (const Cells) "TR" parseUnqt
              `onFail`
              parseEmptyTag (const HorizontalRule) "HR"
              `adjustErr`
              ("Can't parse Html.Row\n\t"++)

  parse = parseUnqt

  parseUnqtList = wrapWhitespace $ sepBy1 parseUnqt whitespace

  parseList = parseUnqtList

-- | Cells either recursively contain another 'Label' or else a
--   path to an image file.
data Cell = LabelCell Attributes Label
          | ImgCell Attributes Img
          | VerticalRule -- ^ Should be between 'LabelCell' or
                         --   'ImgCell' values, requires Graphviz >=
                         --   2.29.0
          deriving (Eq, Ord, Show, Read)

instance PrintDot Cell where
  unqtDot (LabelCell as l) = printCell as $ unqtDot l
  unqtDot (ImgCell as img) = printCell as $ unqtDot img
  unqtDot VerticalRule     = printEmptyTag (text "VR") []

  unqtListToDot = hsep . mapM unqtDot

  listToDot = unqtListToDot

printCell :: Attributes -> DotCode -> DotCode
printCell = printTag (text "TD")

instance ParseDot Cell where
  parseUnqt = oneOf [ parseCell LabelCell parse
                    , parseCell ImgCell $ wrapWhitespace parse
                    , parseEmptyTag (const VerticalRule) "VR"
                    ]
              `adjustErr`
              ("Can't parse Html.Cell\n\t"++)
    where
      parseCell = (`parseTag` "TD")

  parse = parseUnqt

  parseUnqtList = wrapWhitespace $ sepBy1 parseUnqt whitespace

  parseList = parseUnqtList

-- | The path to an image; accepted 'Attributes' are 'Scale' and 'Src'.
newtype Img = Img Attributes
            deriving (Eq, Ord, Show, Read)

instance PrintDot Img where
  unqtDot (Img as) = printEmptyTag (text "IMG") as

instance ParseDot Img where
  parseUnqt = wrapWhitespace (parseEmptyTag Img "IMG")
              `adjustErr`
              ("Can't parse Html.Img\n\t"++)

  parse = parseUnqt

-- -----------------------------------------------------------------------------

-- | The various HTML-like label-specific attributes being used.
type Attributes = [Attribute]

-- | Note that not all 'Attribute' values are valid everywhere:
--   see the comments for each one on where it is valid.
data Attribute = Align Align        -- ^ Valid for: 'Table', 'Cell', 'Newline'.
               | BAlign Align       -- ^ Valid for: 'Cell'.
               | BGColor Color      -- ^ Valid for: 'Table' (including 'tableFontAttrs'), 'Cell', 'Font'.
               | Border Word8       -- ^ Valid for: 'Table', 'Cell'.  Default is @1@; @0@ represents no border.
               | CellBorder Word8   -- ^ Valid for: 'Table'.  Default is @1@; @0@ represents no border.
               | CellPadding Word8  -- ^ Valid for: 'Table', 'Cell'.  Default is @2@.
               | CellSpacing Word8  -- ^ Valid for: 'Table', 'Cell'.  Default is @2@; maximum is @127@.
               | Color Color        -- ^ Valid for: 'Table', 'Cell'.
               | ColSpan Word16     -- ^ Valid for: 'Cell'.  Default is @1@.
               | Columns CellFormat -- ^ Valid for: 'Table'.  Requires Graphviz >= 2.40.1
               | Face T.Text        -- ^ Valid for: 'tableFontAttrs', 'Font'.
               | FixedSize Bool     -- ^ Valid for: 'Table', 'Cell'.  Default is @'False'@.
               | GradientAngle Int  -- ^ Valid for: 'Table', 'Cell'.  Default is @0@.  Requires Graphviz >= 2.40.1
               | Height Word16      -- ^ Valid for: 'Table', 'Cell'.
               | HRef T.Text        -- ^ Valid for: 'Table', 'Cell'.
               | ID T.Text          -- ^ Valid for: 'Table', 'Cell'.  Requires Graphviz >= 2.29.0
               | PointSize Double   -- ^ Valid for: 'tableFontAttrs', 'Font'.
               | Port PortName      -- ^ Valid for: 'Table', 'Cell'.
               | Rows CellFormat    -- ^ Valid for: 'Table'.  Requires Graphviz >= 2.40.1
               | RowSpan Word16     -- ^ Valid for: 'Cell'.
               | Scale Scale        -- ^ Valid for: 'Img'.
               | Sides [Side]       -- ^ Valid for: 'Table', 'Cell'.  Default is @['LeftSide', 'TopSide', 'RightSide', 'BottomSide']@.  Requires Graphviz >= 2.40.1
               | Src FilePath       -- ^ Valid for: 'Img'.
               | Style Style        -- ^ Valid for: 'Table', 'Cell'.  Requires Graphviz >= 2.40.1
               | Target T.Text      -- ^ Valid for: 'Table', 'Cell'.
               | Title T.Text       -- ^ Valid for: 'Table', 'Cell'.  Has an alias of @TOOLTIP@.
               | VAlign VAlign      -- ^ Valid for: 'Table', 'Cell'.
               | Width Word16       -- ^ Valid for: 'Table', 'Cell'.
               deriving (Eq, Ord, Show, Read)

instance PrintDot Attribute where
  unqtDot (Align v)         = printHtmlField  "ALIGN" v
  unqtDot (BAlign v)        = printHtmlField  "BALIGN" v
  unqtDot (BGColor v)       = printHtmlField  "BGCOLOR" v
  unqtDot (Border v)        = printHtmlField  "BORDER" v
  unqtDot (CellBorder v)    = printHtmlField  "CELLBORDER" v
  unqtDot (CellPadding v)   = printHtmlField  "CELLPADDING" v
  unqtDot (CellSpacing v)   = printHtmlField  "CELLSPACING" v
  unqtDot (Color v)         = printHtmlField  "COLOR" v
  unqtDot (ColSpan v)       = printHtmlField  "COLSPAN" v
  unqtDot (Columns v)       = printHtmlField  "COLUMNS" v
  unqtDot (Face v)          = printHtmlField' "FACE" $ escapeAttribute v
  unqtDot (FixedSize v)     = printHtmlField' "FIXEDSIZE" $ printBoolHtml v
  unqtDot (GradientAngle v) = printHtmlField  "GRADIENTANGLE" v
  unqtDot (Height v)        = printHtmlField  "HEIGHT" v
  unqtDot (HRef v)          = printHtmlField' "HREF" $ escapeAttribute v
  unqtDot (ID v)            = printHtmlField' "ID" $ escapeAttribute v
  unqtDot (PointSize v)     = printHtmlField  "POINT-SIZE" v
  unqtDot (Port v)          = printHtmlField' "PORT" . escapeAttribute $ portName v
  unqtDot (Rows v)          = printHtmlField  "ROWS" v
  unqtDot (RowSpan v)       = printHtmlField  "ROWSPAN" v
  unqtDot (Scale v)         = printHtmlField  "SCALE" v
  unqtDot (Sides v)         = printHtmlField  "SIDES" v
  unqtDot (Src v)           = printHtmlField' "SRC" . escapeAttribute $ T.pack v
  unqtDot (Style v)         = printHtmlField  "STYLE" v
  unqtDot (Target v)        = printHtmlField' "TARGET" $ escapeAttribute v
  unqtDot (Title v)         = printHtmlField' "TITLE" $ escapeAttribute v
  unqtDot (VAlign v)        = printHtmlField  "VALIGN" v
  unqtDot (Width v)         = printHtmlField  "WIDTH" v

  unqtListToDot = hsep . mapM unqtDot

  listToDot = unqtListToDot

-- | Only to be used when the 'PrintDot' instance of @a@ matches the
--   HTML syntax (i.e. numbers and @Html.*@ values; 'Color' values also
--   seem to work).
printHtmlField   :: (PrintDot a) => T.Text -> a -> DotCode
printHtmlField f = printHtmlField' f . unqtDot

printHtmlField'     :: T.Text -> DotCode -> DotCode
printHtmlField' f v = text f <> equals <> dquotes v

instance ParseDot Attribute where
  parseUnqt = oneOf [ parseHtmlField  Align "ALIGN"
                    , parseHtmlField  BAlign "BALIGN"
                    , parseHtmlField  BGColor "BGCOLOR"
                    , parseHtmlField  Border "BORDER"
                    , parseHtmlField  CellBorder "CELLBORDER"
                    , parseHtmlField  CellPadding "CELLPADDING"
                    , parseHtmlField  CellSpacing "CELLSPACING"
                    , parseHtmlField  Color "COLOR"
                    , parseHtmlField  ColSpan "COLSPAN"
                    , parseHtmlField  Columns "COLUMNS"
                    , parseHtmlField' Face "FACE" unescapeAttribute
                    , parseHtmlField' FixedSize "FIXEDSIZE" parseBoolHtml
                    , parseHtmlField  GradientAngle "GRADIENTANGLE"
                    , parseHtmlField  Height "HEIGHT"
                    , parseHtmlField' HRef "HREF" unescapeAttribute
                    , parseHtmlField' ID "ID" unescapeAttribute
                    , parseHtmlField  PointSize "POINT-SIZE"
                    , parseHtmlField' (Port . PN) "PORT" unescapeAttribute
                    , parseHtmlField  Rows "ROWS"
                    , parseHtmlField  RowSpan "ROWSPAN"
                    , parseHtmlField  Scale "SCALE"
                    , parseHtmlField  Sides "SIDES"
                    , parseHtmlField' Src "SRC" $ fmap T.unpack unescapeAttribute
                    , parseHtmlField  Style "STYLE"
                    , parseHtmlField' Target "TARGET" unescapeAttribute
                    , parseHtmlField' Title "TITLE" unescapeAttribute
                      `onFail`
                      parseHtmlField' Title "TOOLTIP" unescapeAttribute
                    , parseHtmlField  VAlign "VALIGN"
                    , parseHtmlField  Width "WIDTH"
                    ]

  parse = parseUnqt

  parseUnqtList = sepBy parseUnqt whitespace1 -- needs at least one whitespace char

  parseList = parseUnqtList



parseHtmlField     :: (ParseDot a) => (a -> Attribute) -> String
                  -> Parse Attribute
parseHtmlField c f = parseHtmlField' c f parseUnqt

parseHtmlField'       :: (a -> Attribute) -> String -> Parse a
                     -> Parse Attribute
parseHtmlField' c f p = string f
                        *> parseEq
                        *> ( c <$> ( quotedParse p
                                      `adjustErr`
                                      (("Can't parse HTML.Attribute." ++ f ++ "\n\t")++)
                                   )
                           )
-- Can't use liftEqParse, etc. here because it causes backtracking
-- problems when the attributes could apply to multiple constructors.
-- This includes using commit! (Example: if it starts with a FONT tag,
-- is it a Table or Text?

-- | Specifies horizontal placement. When an object is allocated more
--   space than required, this value determines where the extra space
--   is placed left and right of the object.
data Align = HLeft
           | HCenter -- ^ Default value.
           | HRight
           | HText -- ^ 'LabelCell' values only; aligns lines of text
                   --   using the full cell width. The alignment of a
                   --   line is determined by its (possibly implicit)
                   --   associated 'Newline' element.
           deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot Align where
  unqtDot HLeft   = text "LEFT"
  unqtDot HCenter = text "CENTER"
  unqtDot HRight  = text "RIGHT"
  unqtDot HText   = text "TEXT"

instance ParseDot Align where
  parseUnqt = oneOf [ stringRep HLeft "LEFT"
                    , stringRep HCenter "CENTER"
                    , stringRep HRight "RIGHT"
                    , stringRep HText "TEXT"
                    ]

  parse = parseUnqt

-- | Specifies vertical placement. When an object is allocated more
--   space than required, this value determines where the extra space
--   is placed above and below the object.
data VAlign = HTop
            | HMiddle -- ^ Default value.
            | HBottom
            deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot VAlign where
  unqtDot HTop    = text "TOP"
  unqtDot HMiddle = text "MIDDLE"
  unqtDot HBottom = text "BOTTOM"

instance ParseDot VAlign where
  parseUnqt = oneOf [ stringRep HTop "TOP"
                    , stringRep HMiddle "MIDDLE"
                    , stringRep HBottom "BOTTOM"
                    ]

  parse = parseUnqt

data CellFormat = RuleBetween
                deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot CellFormat where
  unqtDot RuleBetween = text "*"

instance ParseDot CellFormat where
  parseUnqt = stringRep RuleBetween "*"

  parse = parseUnqt

-- | Specifies how an image will use any extra space available in its
--   cell.  If undefined, the image inherits the value of the
--   @ImageScale@ attribute.
data Scale = NaturalSize -- ^ Default value.
           | ScaleUniformly
           | ExpandWidth
           | ExpandHeight
           | ExpandBoth
           deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot Scale where
  unqtDot NaturalSize    = text "FALSE"
  unqtDot ScaleUniformly = text "TRUE"
  unqtDot ExpandWidth    = text "WIDTH"
  unqtDot ExpandHeight   = text "HEIGHT"
  unqtDot ExpandBoth     = text "BOTH"

instance ParseDot Scale where
  parseUnqt = oneOf [ stringRep NaturalSize "FALSE"
                    , stringRep ScaleUniformly "TRUE"
                    , stringRep ExpandWidth "WIDTH"
                    , stringRep ExpandHeight "HEIGHT"
                    , stringRep ExpandBoth "BOTH"
                    ]

  parse = parseUnqt

-- | Which sides of a border in a cell or table should be drawn, if a
--   border is drawn.
data Side = LeftSide
          | RightSide
          | TopSide
          | BottomSide
          deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot Side where
  unqtDot LeftSide   = text "L"
  unqtDot RightSide  = text "R"
  unqtDot TopSide    = text "T"
  unqtDot BottomSide = text "B"

  unqtListToDot = hcat . mapM unqtDot

  listToDot = unqtListToDot

instance ParseDot Side where
  parseUnqt = oneOf [ stringRep LeftSide   "L"
                    , stringRep RightSide  "R"
                    , stringRep TopSide    "T"
                    , stringRep BottomSide "B"
                    ]

  parse = parseUnqt

  parseUnqtList = many parseUnqt

  parseList = parseUnqtList

data Style = Rounded  -- ^ Valid for 'Table'
           | Radial   -- ^ Valid for 'Table', 'Cell'.
           deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot Style where
  unqtDot Rounded = text "ROUNDED"
  unqtDot Radial  = text "RADIAL"

instance ParseDot Style where
  parseUnqt = oneOf [ stringRep Rounded "ROUNDED"
                    , stringRep Radial  "RADIAL"
                    ]

  parse = parseUnqt

-- -----------------------------------------------------------------------------

escapeAttribute :: T.Text -> DotCode
escapeAttribute = escapeHtml False

escapeValue :: T.Text -> DotCode
escapeValue = escapeHtml True

escapeHtml               :: Bool -> T.Text -> DotCode
escapeHtml quotesAllowed = hcat . fmap concat
                           . mapM (escapeSegment . T.unpack)
                           . T.groupBy ((==) `on` isSpace)
  where
    -- Note: use numeric version of space rather than nbsp, since this
    -- matches what Graphviz does (since Inkscape apparently can't
    -- cope with nbsp).
    escapeSegment (s:sps) | isSpace s = liftA2 (:) (char s) $ mapM numEscape sps
    escapeSegment txt                 = mapM xmlChar txt

    allowQuotes = if quotesAllowed
                  then Map.delete '"'
                  else id

    escs = allowQuotes $ Map.fromList htmlEscapes
    xmlChar c = maybe (char c) escape $ c `Map.lookup` escs

    numEscape = escape' . (<>) (char '#') . int . ord
    escape' e = char '&' <> e <> char ';'
    escape = escape' . text

unescapeAttribute :: Parse T.Text
unescapeAttribute = unescapeHtml False

unescapeValue :: Parse T.Text
unescapeValue = unescapeHtml True

-- | Parses an HTML-compatible 'String', de-escaping known characters.
--   Note: this /will/ fail if an unknown non-numeric HTML-escape is
--   used.
unescapeHtml               :: Bool -> Parse T.Text
unescapeHtml quotesAllowed = fmap (T.pack . catMaybes)
                             . many1 . oneOf $ [ parseEscpd
                                               , validChars
                                               ]
  where
    parseEscpd :: Parse (Maybe Char)
    parseEscpd = do character '&'
                    esc <- many1Satisfy (';' /=)
                    character ';'
                    let c = case T.uncons $ T.toLower esc of
                              Just ('#',dec) | Just ('x',hex) <- T.uncons dec
                                               -> readMaybe readHex $ T.unpack hex
                                             | otherwise
                                               -> readMaybe readInt $ T.unpack dec
                              _                -> esc `Map.lookup` escMap
                    return c

    readMaybe f str = do (n, []) <- listToMaybe $ f str
                         return $ chr n
    readInt :: ReadS Int
    readInt = reads

    allowQuotes = if quotesAllowed
                  then delete '"'
                  else id

    escMap = Map.fromList htmlUnescapes

    validChars = fmap Just $ satisfy (`notElem` needEscaping)
    needEscaping = allowQuotes $ map fst htmlEscapes

-- | The characters that need to be escaped and what they need to be
--   replaced with (sans @'&'@).
htmlEscapes :: [(Char, T.Text)]
htmlEscapes = [ ('"', "quot")
              , ('<', "lt")
              , ('>', "gt")
              , ('&', "amp")
              ]

-- | Flip the order and add extra values that might be escaped.  More
--   specifically, provide the escape code for spaces (@\"nbsp\"@) and
--   apostrophes (@\"apos\"@) since they aren't used for escaping.
htmlUnescapes :: [(T.Text, Char)]
htmlUnescapes = maybeEscaped
                ++
                map (uncurry $ flip (,)) htmlEscapes
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
printTag        :: DotCode -> Attributes -> DotCode -> DotCode
printTag t as v = angled (t <+> toDot as)
                      <> v
                      <> angled (fslash <> t)

printFontTag :: Attributes -> DotCode -> DotCode
printFontTag = printTag (text "FONT")

-- | Print something like @<FOO ATTR=\"ATTR_VALUE\"\/>@
printEmptyTag      :: DotCode -> Attributes -> DotCode
printEmptyTag t as = angled $ t <+> toDot as <> fslash

-- -----------------------------------------------------------------------------

-- Note: can't use bracket here because we're not completely
-- discarding everything from the opening bracket.

-- Not using parseTagRep for parseTag because open/close case
-- is different; worth fixing?

-- | Parse something like @<FOO ATTR=\"ATTR_VALUE\">value<\/FOO>@
parseTag        :: (Attributes -> val -> tag) -> String
                       -> Parse val -> Parse tag
parseTag c t pv = c <$> parseAngled openingTag
                    <*> wrapWhitespace pv
                    <* parseAngled (character '/' *> t' *> whitespace)
                  `adjustErr`
                  (("Can't parse Html tag: " ++ t ++ "\n\t")++)
  where
    t' = string t
    openingTag :: Parse Attributes
    openingTag = t'
                 *> tryParseList' (whitespace1 >> parse)
                 <* whitespace

parseFontTag :: (Attributes -> val -> tag) -> Parse val -> Parse tag
parseFontTag = (`parseTag` "FONT")

-- Should this just be specialised for tagName ~ Format ?

-- | Parse something like @<FOO>value<\/FOO>@.
parseTagRep :: (tagName -> val -> tag) -> Parse tagName -> Parse val -> Parse tag
parseTagRep c pt pv = c <$> parseAngled (pt `discard` whitespace)
                        <*> pv
                        <* parseAngled (character '/' *> pt *> whitespace)
                    `adjustErr`
                    ("Can't parse attribute-less Html tag\n\t"++)

-- | Parse something like @<FOO ATTR=\"ATTR_VALUE\"\/>@
parseEmptyTag     :: (Attributes -> tag) -> String -> Parse tag
parseEmptyTag c t = c <$> parseAngled
                        ( string t
                          *> tryParseList' (whitespace1 *> parse)
                          <* whitespace
                          <* character '/'
                        )
                    `adjustErr`
                    (("Can't parse empty Html tag: " ++ t ++ "\n\t")++)
