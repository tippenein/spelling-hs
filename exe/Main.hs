{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.HashMap.Strict as Map
import           Text.Printf (printf)
import           Spelling

import           Protolude

main :: IO ()
main = do
  hist <- toHistogram . words <$> readFile "big.txt"
  -- let can = candidates hist "watever"
  -- traverse_ putText can
  putText $ correction hist "speling" -- spelling
  putText $ correction hist "korrectud" -- "corrected"
  -- let k = known hist ["hero", "whatever", "nerp"]
  -- traverse_ putText k

  let p = prob "the" hist
  printf "probability of 'the' -> %i" p

  -- dict <- T.lines <$> readFile dictLocation
  -- traverse_ putText  . take 5 dict
