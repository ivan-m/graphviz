{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
   Module      : Data.GraphViz.Testing.Instances.Helpers
   Description : Helper functions for graphviz Arbitrary instances.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com
 -}
module Data.GraphViz.Testing.Instances.Helpers where

import Data.GraphViz.Parsing(isNumString)
import Data.GraphViz.State(initialState, layerSep)

import Test.QuickCheck

import qualified Data.Text.Lazy as T
import Data.Text.Lazy(Text)
import Control.Monad(liftM, liftM2)

-- -----------------------------------------------------------------------------
-- Helper Functions

instance Arbitrary Text where
  arbitrary = arbText

  shrink = filter validString
           . map T.pack . nonEmptyShrinks' . T.unpack

arbText :: Gen Text
arbText = suchThat genStr notBool
    where
      genStr = liftM2 T.cons (elements notDigits)
                             (liftM T.concat . listOf $ elements strChr)
      notDigits = ['a'..'z'] ++ ['\'', '"', ' ', '(', ')', ',', ':', '\\']
      strChr = map T.singleton $ notDigits ++ '.' : ['0'..'9']

arbString :: Gen String
arbString = liftM T.unpack arbitrary

fromPositive              :: Positive a -> a
fromPositive (Positive a) = a

posArbitrary :: (Arbitrary a, Num a, Ord a) => Gen a
posArbitrary = liftM fromPositive arbitrary

arbIDString :: Gen Text
arbIDString = suchThat genStr notBool
  where
    genStr = liftM2 T.cons (elements frst)
                           (liftM T.pack . listOf $ elements rest)
    frst = ['a'..'z'] ++ ['_']
    rest = frst ++ ['0'.. '9']

validString :: Text -> Bool
validString = liftM2 (&&) notBool notNumStr

notBool         :: Text -> Bool
notBool "true"  = False
notBool "false" = False
notBool _       = True

shrinkString :: String -> [String]
shrinkString = map T.unpack . shrink . T.pack

notNumStr :: Text -> Bool
notNumStr = not . isNumString

arbBounded :: (Bounded a, Enum a) => Gen a
arbBounded = elements [minBound .. maxBound]

arbLayerName :: Gen Text
arbLayerName = suchThat arbitrary (T.all notLayerSep)
  where
    defLayerSep = layerSep initialState
    notLayerSep = (`notElem` defLayerSep)

arbStyleName :: Gen Text
arbStyleName = suchThat arbitrary (T.all notBrackCom)
  where
    notBrackCom = flip notElem ['(', ')', ',', ' ']

arbList :: (Arbitrary a) => Gen [a]
arbList = listOf1 arbitrary

nonEmptyShrinks :: (Arbitrary a) => [a] -> [[a]]
nonEmptyShrinks = filter (not . null) . shrink

nonEmptyShrinks' :: [a] -> [[a]]
nonEmptyShrinks' = filter (not . null) . shrinkList'

-- Shrink lists with more than one value only by removing values, not
-- by shrinking individual items.
shrinkList     :: (Arbitrary a) => [a] -> [[a]]
shrinkList [a] = map return $ shrink a
shrinkList as  = shrinkList' as

-- Just shrink the size.
shrinkList'     :: [a] -> [[a]]
shrinkList' as  = rm (length as) as
  where
    rm 0 _  = []
    rm 1 _  = [[]]
    rm n xs = xs1
            : xs2
            : ( [ xs1' ++ xs2 | xs1' <- rm n1 xs1, not (null xs1') ]
                `ilv` [ xs1 ++ xs2' | xs2' <- rm n2 xs2, not (null xs2') ]
              )
     where
      n1  = n `div` 2
      xs1 = take n1 xs
      n2  = n - n1
      xs2 = drop n1 xs

    []     `ilv` ys     = ys
    xs     `ilv` []     = xs
    (x:xs) `ilv` (y:ys) = x : y : (xs `ilv` ys)

-- When a Maybe value is a sub-component, and we need shrink to return
-- a value.
shrinkM         :: (Arbitrary a) => Maybe a -> [Maybe a]
shrinkM Nothing = [Nothing]
shrinkM j       = shrink j

shrinkL    :: (Arbitrary a) => [a] -> [[a]]
shrinkL xs = case shrinkList xs of
               []  -> [xs]
               xs' -> xs'

notInt   :: Double -> Bool
notInt d = fromIntegral (round d :: Int) /= d

returnCheck     :: (Eq a) => a -> a -> [a]
returnCheck o n = if o == n
                  then []
                  else [n]
