{-# LANGUAGE MultiParamTypeClasses #-}

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

import           Data.GraphViz
import           Data.GraphViz.Commands.IO       (hGetStrict, toUTF8)
import           Data.GraphViz.Exception
import           Data.GraphViz.Parsing           (runParser)
import           Data.GraphViz.PreProcessing     (preProcess)
import qualified Data.GraphViz.Types.Generalised as G

import           Control.Exception    (SomeException, evaluate, try)
import           Control.Monad        (filterM, liftM)
import qualified Data.ByteString.Lazy as B
import           Data.Text.Lazy       (Text)
import qualified Data.Text.Lazy       as T
import           System.Directory
import           System.Environment   (getArgs)
import           System.FilePath

-- -----------------------------------------------------------------------------

main :: IO ()
main = tryParsing =<< getArgs
  where
    tryParsing []   = putStrLn "Test that the graphviz library can parse\
                               \ \"real life\" Dot code by passing a list\n\
                               \of files in which contain Dot graphs.\n\
                               \\n\
                               \One way of using this file:\n\t\
                               \$ locate -r \".*\\.\\(gv\\|dot\\)$\" -0\
                               \ | xargs -0 TestParsing.hs"
    tryParsing [fp] = do isDir <- doesDirectoryExist fp
                         if isDir
                            then mapM_ tryParseFile =<< getDContents fp
                            else tryParseFile fp
    tryParsing fs = mapM_ tryParseFile fs

getDContents :: FilePath -> IO [FilePath]
getDContents fp = (filterM doesFileExist . map (fp </>)) =<< getDirectoryContents fp

-- -----------------------------------------------------------------------------


withParse :: (PPDotRepr dg n) => (a -> IO Text) -> (dg n -> IO ())
             -> (ErrMsg -> String) -> a -> IO ()
withParse toStr withDG cmbErr a = do dc <- liftM getMsg . try $ toStr a
                                     case dc of
                                       Right dc' -> do edg <- tryParse dc'
                                                       case edg of
                                                         (Right dg) -> withDG dg
                                                         (Left err) -> do putStrLn "Parsing problem!"
                                                                          putStrLn $ cmbErr err
                                                                          putStrLn  ""
                                       Left err  -> do putStrLn "IO problem!"
                                                       putStrLn err
                                                       putStrLn ""
  where
    getMsg :: Either SomeException Text -> Either ErrMsg Text
    getMsg = either (Left . show) Right

type DG = DotGraph Text
type GDG = G.DotGraph Text
type ErrMsg = String

tryParseFile    :: FilePath -> IO ()
tryParseFile fp = withParse readFile'
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
    getErr :: SomeException -> IO (Either ErrMsg a)
    getErr = return . Left . show

readFile' :: FilePath -> IO Text
readFile' fp = do putStr fp
                  putStr " - "
                  readUTF8File fp

-- Force any encoding errors into the IO section rather than when parsing.
readUTF8File    :: FilePath -> IO Text
readUTF8File fp = do cnts <- liftM toUTF8 $ B.readFile fp
                     _ <- evaluate $ T.length cnts
                     return cnts
