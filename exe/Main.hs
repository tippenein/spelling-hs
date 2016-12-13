{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.HashMap.Strict as Map
import           Text.Printf (printf)
import           Spelling

import           Protolude

wordss = do
  words <$> readFile "big.txt"
  pure ()

multiset = do
  hist <- toHistogram . words <$> readFile "big.txt"
  traverse_ putText $ Map.keys hist

proba = do
  hist <- toHistogram . words <$> readFile "big.txt"
  let p = prob "the" hist
  printf "probability of 'the' -> %i" p

makeCorrection = do
  hist <- toHistogram . words <$> readFile "big.txt"
  -- putText $ correction hist "speling" -- spelling
  putText $ correction hist "korrectud" -- "corrected"

main = do
  args <- getArgs
  case args of
    ["words"] -> wordss
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
