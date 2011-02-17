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
       ( GraphvizStateM(..)
       , GraphvizState(..)
       , initialState
       , setDirectedness
       , getDirectedness
       , setLayerSep
       , getLayerSep
       , setColorScheme
       , getColorScheme
       ) where

import Data.GraphViz.Attributes.ColorScheme

import Control.Monad(liftM)

-- -----------------------------------------------------------------------------

class (Monad m) => GraphvizStateM m where
  modifyGS :: (GraphvizState -> GraphvizState) -> m ()

  getsGS :: (GraphvizState -> a) -> m a

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

setDirectedness   :: (GraphvizStateM m) => Bool -> m ()
setDirectedness d = modifyGS (\ gs -> gs { directedEdges = d } )

getDirectedness :: (GraphvizStateM m) => m Bool
getDirectedness = getsGS directedEdges

setLayerSep     :: (GraphvizStateM m) => [Char] -> m ()
setLayerSep sep = modifyGS (\ gs -> gs { layerSep = sep } )

getLayerSep :: (GraphvizStateM m) => m [Char]
getLayerSep = getsGS layerSep

setColorScheme    :: (GraphvizStateM m) => ColorScheme -> m ()
setColorScheme cs = modifyGS (\ gs -> gs { colorScheme = cs } )

getColorScheme :: (GraphvizStateM m) => m ColorScheme
getColorScheme = getsGS colorScheme

-- | The default separators for 'LayerSep'.
defLayerSep :: [Char]
defLayerSep = [' ', ':', '\t']



