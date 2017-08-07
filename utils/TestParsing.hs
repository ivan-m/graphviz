{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}

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
module Main (main) where

import           Data.GraphViz
import           Data.GraphViz.Commands.IO       (toUTF8)
import           Data.GraphViz.Exception
import           Data.GraphViz.Parsing           (runParser)
import           Data.GraphViz.PreProcessing     (preProcess)
import qualified Data.GraphViz.Types.Generalised as G

import           Control.Exception    (SomeException, evaluate, try)
import           Control.Monad        (filterM, liftM)
import qualified Data.ByteString.Lazy as B
import           Data.Either          (either)
import           Data.Monoid          (mappend)
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

withParse :: (Show a, PPDotRepr dg n) => (a -> IO Text) -> (dg n -> IO ())
             -> (ErrMsg -> String) -> a -> IO ()
withParse toStr withDG cmbErr a = do dc <- liftM getMsg . try $ toStr a
                                     case dc of
                                       Right dc' -> do edg <- tryParse dc'
                                                       case edg of
                                                         (Right dg) -> withDG dg
                                                         (Left err) -> do putStr (show a)
                                                                          putStrLn " - Parsing problem!"
                                                                          putStrLn $ cmbErr err
                                                                          putStrLn  ""
                                       Left err  -> do putStr (show a)
                                                       putStrLn " - IO problem!"
                                                       putStrLn err
                                                       putStrLn ""
  where
    getMsg :: Either SomeException Text -> Either ErrMsg Text
    getMsg = either (Left . show) Right

type GDG = G.DotGraph Text
type ErrMsg = String

tryParseFile    :: FilePath -> IO ()
tryParseFile fp = withParse readUTF8File
                            ((`seq` return ()) . T.length . printDotGraph . asGDG)
                            ("Cannot parse as a G.DotGraph: "++)
                            fp
  where
    asGDG :: GDG -> GDG
    asGDG = id

tryParse    :: (PPDotRepr dg n) => Text -> IO (Either ErrMsg (dg n))
tryParse dc = handle getErr
              $ let (dg, rst) = runParser parse $ preProcess dc
                in T.length rst `seq` return (eitherLR (augmentErr rst) id dg)
  where
    getErr :: SomeException -> IO (Either ErrMsg a)
    getErr = return . Left . show

    augmentErr rst err = err ++ "\n\tRemaining input: " ++ show res
      where
        sampleLen = 35

        res | T.length rst <= sampleLen = rst
            | otherwise                 = T.take sampleLen rst `mappend` " ..."

    eitherLR f g = either (Left . f) (Right . g)

-- Force any encoding errors into the IO section rather than when parsing.
readUTF8File    :: FilePath -> IO Text
readUTF8File fp = do cnts <- liftM toUTF8 $ B.readFile fp
                     _ <- evaluate $ T.length cnts
                     return cnts
