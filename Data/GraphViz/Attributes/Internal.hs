{-# OPTIONS_HADDOCK hide #-}

{- |
   Module      : Data.GraphViz.Attributes.Internal
   Description : Internal Attribute value definitions
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module is defined so as to avoid exposing internal functions
   in the external API.

 -}

module Data.GraphViz.Attributes.Internal
       ( PortName(..)
       ) where

import Data.GraphViz.Parsing
import Data.GraphViz.Printing

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
newtype PortName = PN { portName :: String }
                 deriving (Eq, Ord, Show, Read)

instance PrintDot PortName where
  unqtDot = unqtDot . portName

  toDot = toDot . portName

instance ParseDot PortName where
  parseUnqt = liftM PN . many1
              . orQuote $ satisfy (`notElem` ['"', ':'])

  parse= quotedParse parseUnqt
         `onFail`
         liftM PN quotelessString

-- -----------------------------------------------------------------------------
