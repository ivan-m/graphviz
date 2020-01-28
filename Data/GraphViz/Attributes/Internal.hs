{-# LANGUAGE CPP, OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
   Module      : Data.GraphViz.Attributes.Internal
   Description : Internal Attribute value definitions
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module is defined so as to avoid exposing internal functions
   in the external API.  These are those that are needed for the
   testsuite.

 -}

module Data.GraphViz.Attributes.Internal
       ( PortName(..)
       , PortPos(..)
       , CompassPoint(..)
       , compassLookup
       , parseEdgeBasedPP
       ) where

import Data.GraphViz.Parsing
import Data.GraphViz.Printing

import           Data.Map       (Map)
import qualified Data.Map       as Map
import           Data.Maybe     (isNothing)
import           Data.Text.Lazy (Text)

#if !MIN_VERSION_base (4,13,0)
import Data.Monoid ((<>))
#endif

-- -----------------------------------------------------------------------------

-- Note that printing and parsing of PortName values is specific to
-- where it's being used: record- and HTML-like labels print/parse
-- them differently from when they're on they're part of PortPos; the
-- default printing and parsing is done for the latter.

-- Should this really be exported from here?  Or in another common module?

-- | Specifies a name for ports (used also in record-based and
--   HTML-like labels).  Note that it is not valid for a 'PortName'
--   value to contain a colon (@:@) character; it is assumed that it
--   doesn't.
newtype PortName = PN { portName :: Text }
                 deriving (Eq, Ord, Show, Read)

instance PrintDot PortName where
  unqtDot = unqtDot . portName

  toDot = toDot . portName

instance ParseDot PortName where
  parseUnqt = PN <$> parseEscaped False [] ['"', ':']

  parse = quotedParse parseUnqt
          `onFail`
          unqtPortName

unqtPortName :: Parse PortName
unqtPortName = PN <$> quotelessString

-- -----------------------------------------------------------------------------

data PortPos = LabelledPort PortName (Maybe CompassPoint)
             | CompassPoint CompassPoint
             deriving (Eq, Ord, Show, Read)

instance PrintDot PortPos where
  unqtDot (LabelledPort n mc) = unqtDot n
                                <> maybe empty (colon <>) (fmap unqtDot mc)
  unqtDot (CompassPoint cp)   = unqtDot cp

  toDot (LabelledPort n Nothing) = toDot n
  toDot lp@LabelledPort{}        = dquotes $ unqtDot lp
  toDot cp                       = unqtDot cp

instance ParseDot PortPos where
  parseUnqt = do n <- parseUnqt
                 mc <- optional $ character ':' >> parseUnqt
                 return $ if isNothing mc
                          then checkPortName n
                          else LabelledPort n mc

  parse = quotedParse parseUnqt
          `onFail`
          fmap checkPortName unqtPortName

checkPortName    :: PortName -> PortPos
checkPortName pn = maybe (LabelledPort pn Nothing) CompassPoint
                   . (`Map.lookup` compassLookup)
                   $ portName pn

-- | When attached to a node in a DotEdge definition, the 'PortName'
--   and the 'CompassPoint' can be in separate quotes.
parseEdgeBasedPP :: Parse PortPos
parseEdgeBasedPP = liftA2 LabelledPort parse (fmap Just $ character ':' *> parse)
                   `onFail`
                   parse

data CompassPoint = North
                  | NorthEast
                  | East
                  | SouthEast
                  | South
                  | SouthWest
                  | West
                  | NorthWest
                  | CenterPoint
                  | NoCP
                  deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot CompassPoint where
  unqtDot NorthEast   = text "ne"
  unqtDot NorthWest   = text "nw"
  unqtDot North       = text "n"
  unqtDot East        = text "e"
  unqtDot SouthEast   = text "se"
  unqtDot SouthWest   = text "sw"
  unqtDot South       = text "s"
  unqtDot West        = text "w"
  unqtDot CenterPoint = text "c"
  unqtDot NoCP        = text "_"

instance ParseDot CompassPoint where
  -- Have to take care of longer parsing values first.
  parseUnqt = oneOf [ stringRep NorthEast "ne"
                    , stringRep NorthWest "nw"
                    , stringRep North "n"
                    , stringRep SouthEast "se"
                    , stringRep SouthWest "sw"
                    , stringRep South "s"
                    , stringRep East "e"
                    , stringRep West "w"
                    , stringRep CenterPoint "c"
                    , stringRep NoCP "_"
                    ]

compassLookup :: Map Text CompassPoint
compassLookup = Map.fromList [ ("ne", NorthEast)
                             , ("nw", NorthWest)
                             , ("n", North)
                             , ("e", East)
                             , ("se", SouthEast)
                             , ("sw", SouthWest)
                             , ("s", South)
                             , ("w", West)
                             , ("c", CenterPoint)
                             , ("_", NoCP)
                             ]

-- -----------------------------------------------------------------------------
