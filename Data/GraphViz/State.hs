{- |
   Module      : Data.GraphViz.State
   Description : Printing and parsing state.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   When printing and parsing Dot code, some items depend on values
   that are set earlier.
-}
module Data.GraphViz.State
       ( GraphvizState(..)
       , initialState
       , setUndirected
       , setLayerSep
       , setColorScheme
       ) where

import Data.GraphViz.Attributes.ColorScheme

-- -----------------------------------------------------------------------------

-- | Several aspects of Dot code are either global or mutable state.
data GraphvizState = GS { directedEdges :: Bool
                        , layerSep      :: [Char]
                        , colorScheme   :: ColorScheme
                        }
                   deriving (Eq, Ord, Show, Read)

initialState :: GraphvizState
initialState = GS { directedEdges = True
                  , layerSep      = defLayerSep
                  , colorScheme   = X11
                  }

setUndirected    :: GraphvizState -> GraphvizState
setUndirected gs = gs { directedEdges = False }

setLayerSep        :: [Char] -> GraphvizState -> GraphvizState
setLayerSep sep gs = gs { layerSep = sep }

setColorScheme       :: ColorScheme -> GraphvizState -> GraphvizState
setColorScheme cs gs = gs { colorScheme = cs }

-- | The default separators for 'LayerSep'.
defLayerSep :: [Char]
defLayerSep = [' ', ':', '\t']



