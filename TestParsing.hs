{- |
   Module      : TestParsing
   Description : Check if the graphviz parser can parse "real world" Dot code.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines a program that determines if the provided files
   containing Dot code can be properly parsed using graphviz's parsers
   (with the assumption that the provided code is valid).

-}
module Main where

import Data.GraphViz
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Parsing(runParser, parse, discard, allWhitespace', eof)
import Data.GraphViz.PreProcessing(preProcess)

import Data.Either(either)
import Control.Exception(try, ErrorCall(..))
import Control.Monad(liftM)
import System.Environment(getArgs)

-- -----------------------------------------------------------------------------

main :: IO ()
main = tryParsing =<< getArgs
  where
    tryParsing [] = putStrLn "Test that the graphviz library can parse\
                             \ \"real life\" Dot code by passing a list\n\
                             \of files in which contain Dot graphs."
    tryParsing fs = mapM_ tryParseFile fs

-- -----------------------------------------------------------------------------


withParse :: (DotRepr dg n) => (a -> IO String) -> (dg n -> IO ())
             -> (ErrMsg -> String) -> a -> IO ()
withParse toStr withDG cmbErr a = do dc <- toStr a
                                     edg <- tryParse dc
                                     case edg of
                                       (Right dg) -> withDG dg
                                       (Left err) -> do putStrLn "Parsing problem!"
                                                        putStrLn $ cmbErr err
                                                        putStrLn  ""

type DG = DotGraph String
type GDG = GDotGraph String
type ErrMsg = String

tryParseFile    :: FilePath -> IO ()
tryParseFile fp = withParse readFile
                            (tryParseCanon fp)
                            (\ e -> fp ++ ": Cannot parse as a GDotGraph:\n"
                                    ++ e)
                            fp

tryParseCanon    :: FilePath -> GDG -> IO ()
tryParseCanon fp = withParse prettyPrint
                             (const (return ()) . asDG)
                             (\ e -> fp ++ ": Canonical Form not a DotGraph:\n"
                                     ++ e)
  where
    asDG = flip asTypeOf emptDG
    emptDG = DotGraph False False Nothing $ DotStmts [] [] [] [] :: DG

tryParse    :: (DotRepr dg n) => String -> IO (Either ErrMsg (dg n))
tryParse dc = liftM getErrMsg . try
              $ let (dg, rst) = runParser p $ preProcess dc
                in length rst `seq` return dg
  where
    p = parse `discard` (allWhitespace' >> eof)

getErrMsg :: Either ErrorCall a -> Either ErrMsg a
getErrMsg = either getEC Right
  where
    getEC (ErrorCall e) = Left e


