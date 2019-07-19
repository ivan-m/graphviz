{-# LANGUAGE DeriveDataTypeable #-}
{- |
   Module      : Data.GraphViz.Exception
   Description : Graphviz-specific exceptions
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com
-}
module Data.GraphViz.Exception
       ( GraphvizException(..)
         -- * Re-exported for convenience.
       , mapException
       , throw
       , throwIO
       , handle
       , bracket
       ) where

import Control.Exception
import Data.Typeable

-- -----------------------------------------------------------------------------

-- | Exceptions that arise from using this library fall into four
--   categories:
--
--   * Unable to parse provided Dot code.
--
--   * Dot code is not valid UTF-8.
--
--   * An error when trying to run an external program (e.g. @dot@).
--
--   * Treating a non-custom Attribute as a custom one.
--
data GraphvizException = NotDotCode String
                       | NotUTF8Dot String
                       | GVProgramExc String
                       | NotCustomAttr String
                       deriving (Eq, Ord, Typeable)

instance Show GraphvizException where
  showsPrec _ (NotDotCode str)    = showString $ "Error when parsing Dot code:\n" ++ str
  showsPrec _ (NotUTF8Dot str)    = showString $ "Invalid UTF-8 Dot code: " ++ str
  showsPrec _ (GVProgramExc str)  = showString $ "Error running utility program: " ++ str
  showsPrec _ (NotCustomAttr str) = showString $ "Not a custom Attribute: " ++ str

instance Exception GraphvizException
