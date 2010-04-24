#!/usr/bin/runhaskell

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

import Control.Exception(try, ErrorCall(..), IOException)
import Control.Monad(liftM)
import System.Environment(getArgs)
import System.IO(openFile, IOMode(ReadMode))

-- -----------------------------------------------------------------------------

main :: IO ()
main = tryParsing =<< getArgs
  where
    tryParsing [] = putStrLn "Test that the graphviz library can parse\
                             \ \"real life\" Dot code by passing a list\n\
                             \of files in which contain Dot graphs.\n\
                             \\n\
                             \One way of using this file:\n\t\
                             \$ locate -r \".*\\.\\(gv\\|dot\\)$\" -0\
                             \ | xargs -0 runhaskell TestParsing.hs"
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

-- tryParseFile ::

tryParseFile    :: FilePath -> IO ()
tryParseFile fp = readFile' fp >>= maybeParse
  where
    maybeParse (Left err)  = putStrLn $ "Error parsing \"" ++ fp ++ "\":\n\t"
                                        ++ err ++ "\n"
    maybeParse (Right dot) = withParse (const $ return dot)
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
              $ let (dg, rst) = runParser parse $ preProcess dc
                in length rst `seq` return dg

getErrMsg :: Either ErrorCall a -> Either ErrMsg a
getErrMsg = either getEC Right
  where
    getEC (ErrorCall e) = Left e

readFile'    :: FilePath -> IO (Either ErrMsg String)
readFile' fp = liftM getMsg . try
               $ openFile fp ReadMode >>= hGetContents'
  where
    getMsg :: Either IOException String -> Either ErrMsg String
    getMsg = either (Left . show) Right
