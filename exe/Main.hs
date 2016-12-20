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
    ["toCounter"] -> multiset
    ["proba"] -> proba
    ["all"] -> makeCorrection
    _ -> makeCorrection
  where
    wordsPreprocess =
      (words <$> readFile "big-words.txt") >>= print . head . reverse

    wordsFast =
      (fastWords <$> readFile "big.txt") >>= print . head . reverse

    multiset = do
      counter <- toCounter . words <$> readFile "big-words.txt"
      traverse_ putText $ Map.keys counter

    proba = do
      counter <- toCounter . words <$> readFile "big-words.txt"
      let p = prob "the" counter
      printf "probability of 'the' -> %i" p

    makeCorrection = do
      counter <- toCounter . fastWords <$> readFile "big.txt"
      putText $ correction counter "mississippo" -- "corrected"
