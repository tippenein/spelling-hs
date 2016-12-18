{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.HashMap.Strict as Map
import           Text.Printf (printf)
import           Spelling
import Data.Text (words)

import           Protolude

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["wordsPre"] -> wordsPreprocess
    ["wordsFast"] -> wordsFast
    ["toHistogram"] -> multiset
    ["proba"] -> proba
    ["all"] -> makeCorrection
    _ -> makeCorrection
  where
    wordsPreprocess =
      (words <$> readFile "big-words.txt") >>= print . head . reverse

    wordsFast =
      (fastWords <$> readFile "big.txt") >>= print . head . reverse

    multiset = do
      hist <- toHistogram . words <$> readFile "big-words.txt"
      traverse_ putText $ Map.keys hist

    proba = do
      hist <- toHistogram . words <$> readFile "big-words.txt"
      let p = prob "the" hist
      printf "probability of 'the' -> %i" p

    makeCorrection = do
      hist <- toHistogram . fastWords <$> readFile "big.txt"
      putText $ correction hist "korrectud" -- "corrected"
