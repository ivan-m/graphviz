#!/usr/bin/runhaskell

{-# LANGUAGE ScopedTypeVariables #-}

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
import qualified Data.GraphViz.Types.Generalised as G
import Data.GraphViz.Parsing(runParser, parse)
import Data.GraphViz.PreProcessing(preProcess)
import Data.GraphViz.Commands.IO(hGetStrict, toUTF8)
import Data.GraphViz.Exception

import qualified Data.Text.Lazy as T
import Data.Text.Lazy(Text)
import qualified Data.ByteString.Lazy as B
import Control.Exception.Extensible(try, IOException)
import Control.Monad(liftM)
import System.Environment(getArgs)

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


withParse :: (PPDotRepr dg n) => (a -> IO Text) -> (dg n -> IO ())
             -> (ErrMsg -> String) -> a -> IO ()
withParse toStr withDG cmbErr a = do dc <- toStr a
                                     edg <- tryParse dc
                                     case edg of
                                       (Right dg) -> withDG dg
                                       (Left err) -> do putStrLn "Parsing problem!"
                                                        putStrLn $ cmbErr err
                                                        putStrLn  ""

type DG = DotGraph Text
type GDG = G.DotGraph Text
type ErrMsg = String

tryParseFile    :: FilePath -> IO ()
tryParseFile fp = readFile' fp >>= maybeParse
  where
    maybeParse (Left err)  = putStrLn $ "Error parsing \"" ++ fp ++ "\":\n\t"
                                        ++ err ++ "\n"
    maybeParse (Right dot) = withParse (const $ return dot)
                                       (tryParseCanon fp)
                                       ("Cannot parse as a G.DotGraph: "++)
                                       fp

tryParseCanon    :: FilePath -> GDG -> IO ()
tryParseCanon fp = withParse prettyPrint
                             ((`seq` putStrLn "Parsed OK!") . T.length . printDotGraph . asDG)
                             (\ e -> fp ++ ": Canonical Form not a DotGraph:\n"
                                     ++ e)
  where
    asDG = flip asTypeOf emptDG
    emptDG = DotGraph False False Nothing $ DotStmts [] [] [] [] :: DG
    prettyPrint dg = graphvizWithHandle (commandFor dg) dg Canon hGetStrict

tryParse    :: (PPDotRepr dg n) => Text -> IO (Either ErrMsg (dg n))
tryParse dc = handle getErr
              $ let (dg, rst) = runParser parse $ preProcess dc
                in T.length rst `seq` return dg
  where
    getErr :: GraphvizException -> IO (Either ErrMsg a)
    getErr = return . Left . show

readFile' :: FilePath -> IO (Either ErrMsg Text)
readFile' fp = do putStr fp
                  putStr " - "
                  liftM getMsg . try $ readUTF8File fp
  where
    getMsg :: Either IOException Text -> Either ErrMsg Text
    getMsg = either (Left . show) Right

readUTF8File :: FilePath -> IO Text
readUTF8File = liftM toUTF8 . B.readFile
