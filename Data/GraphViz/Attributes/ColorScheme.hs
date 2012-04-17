{-# OPTIONS_HADDOCK hide #-}

{- |
   Module      : Data.GraphViz.Attributes.ColorScheme
   Description : Specification of color schemes.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This is an internal module designed so that the state can record
   the current color scheme.
-}
module Data.GraphViz.Attributes.ColorScheme where

import Data.Word(Word8)

-- -----------------------------------------------------------------------------

-- | This represents the color schemes that Graphviz accepts.
data ColorScheme = X11
                 | SVG
                 | Brewer BrewerScheme
                 deriving (Eq, Ord, Show, Read)

-- | Specify which colour palette and how many colours it has.  Note
--   the allowed values for the different 'BrewerName's.
data BrewerScheme = BScheme BrewerName Word8
                  deriving (Eq, Ord, Show, Read)

-- | All of these have a minimum level value of @3@, with a maximum
--   of @9@ unless otherwise specified.
data BrewerName = Accent   -- ^ Maximum of @8@.
                | Blues
                | Brbg     -- ^ Maximum of @11@.
                | Bugn
                | Bupu
                | Dark2    -- ^ Maximum of @8@.
                | Gnbu
                | Greens
                | Greys
                | Oranges
                | Orrd
                | Paired   -- ^ Maximum of @12@.
                | Pastel1
                | Pastel2  -- ^ Maximum of @8@.
                | Piyg     -- ^ Maximum of @11@.
                | Prgn     -- ^ Maximum of @11@.
                | Pubu
                | Pubugn
                | Puor     -- ^ Maximum of @11@; note that the last two are listed
                           --   after the @'Purd'@ values in the
                           --   documentation.
                | Purd
                | Purples
                | Rdbu     -- ^ Maximum of @11@; note that the last two are listed
                           --   first.
                | Rdgy     -- ^ Maximum of @11@; note that the last two are listed
                           --   after the @'Rdpu'@ values in the
                           --   documentation.
                | Rdpu
                | Rdylbu   -- ^ Maximum of @11@.
                | Rdylgn   -- ^ Maximum of @11@.
                | Reds
                | Set1
                | Set2     -- ^ Maximum of @8@.
                | Set3     -- ^ Maximum of @12@.
                | Spectral -- ^ Maximum of @11@.
                | Ylgn
                | Ylgnbu
                | Ylorbr
                | Ylorrd
                deriving (Eq, Ord, Bounded, Enum, Show, Read)
