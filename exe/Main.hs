{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.HashMap.Strict as Map
import           Text.Printf (printf)
import           Spelling
import Data.Text (words)

import           Protolude

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
  hist <- toHistogram . words <$> readFile "big-words.txt"
  -- putText $ correction hist "speling" -- spelling
  putText $ correction hist "korrectud" -- "corrected"

main = do
  args <- getArgs
  case args of
    ["wordsPre"] -> wordsPreprocess
    ["wordsFast"] -> wordsFast
    ["toHistogram"] -> multiset
    ["proba"] -> proba
    _ -> makeCorrection

-- main :: IO ()
-- main = do
--   hist <- toHistogram . words <$> readFile "big.txt"
--   putText $ correction hist "speling" -- spelling
--   putText $ correction hist "korrectud" -- "corrected"
--   let p = prob "the" hist
--   printf "probability of 'the' -> %i" p
