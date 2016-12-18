{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

-- http://norvig.com/spell-correct.html
module Spelling where

import qualified Data.Char as Char
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import           Data.List (foldl')
import qualified Data.Text as T
import Data.Text.Unsafe (Iter(..), iter)
import Data.Text.Internal (Text(..))

import           Protolude hiding (Set)

type Set a = Set.HashSet a
type Hist = Map.HashMap Text Int

-- dictLocation :: FilePath
-- dictLocation = "/usr/share/dict/words"

alphabet :: [Text]
alphabet = fmap T.singleton ['a'..'z']

-- def P(word, N=sum(WORDS.values())):
--     return WORDS[word] / N
-- | Probability of `word`.
prob :: Text -> Hist -> Int
prob t ms = occurences `div` totalWords
  where
    occurences = fromMaybe 0 $ Map.lookup t ms
    totalWords = Map.size ms

-- the intuitive version
slowWords :: Text -> [Text]
slowWords = T.split (not . Char.isAsciiLower) . T.toLower

-- based on https://hackage.haskell.org/package/text-1.2.2.1/docs/src/Data-Text.html#words
fastWords :: Text -> [Text]
fastWords t@(Text arr off len) = loop 0 0
  where
    loop !start !n
        | n >= len = if start == n
                     then []
                     else [Text arr (start+off) (n-start)]
        | not $ Char.isAsciiLower c =
            if start == n
            then loop (start+1) (start+1)
            else Text arr (start+off) (n-start) : loop (n+d) (n+d)
        | otherwise = loop start (n+d)
        where Iter c d = iter t n
{-# INLINE fastWords #-}

-- | create a Multi Set (Histogram) from a collection of words
toHistogram :: [Text] -> Hist
toHistogram = Map.fromListWith (+) . fmap (,1)

-- def correction(word):
--     return max(candidates(word), key=P)
-- | most probable spelling correction for word
correction :: Hist -> Text -> Text
correction hist t = maximumBy (\a b -> prob a hist `compare` prob b hist) $ candidates hist t

-- def candidates(word):
--     return (known([word]) or known(edits1(word)) or known(edits2(word)) or [word])
-- | Generate possible spelling corrections for word.
candidates :: Hist -> Text -> Set Text
candidates hist t = detect
  [ known hist [t]
  , known hist (edits1 t)
  , known hist (edits2 t)
  , Set.fromList [t]
  ]

-- | get first non-empty
detect :: [Set Text] -> Set Text
detect = fromMaybe Set.empty . head . filter (not . Set.null)

-- def known(words):
--     return set(w for w in words if w in WORDS)
-- | The subset of `words` that appear in the dictionary of WORDS.
known :: Hist -> [Text] -> Set Text
known hist ws = Set.fromList $ filter (\w -> Map.member w hist) ws

-- def edits1(word):
--     letters    = 'abcdefghijklmnopqrstuvwxyz'
--     splits     = [(word[:i], word[i:])    for i in range(len(word) + 1)]
--     deletes    = [L + R[1:]               for L, R in splits if R]
--     Transposes = [L + R[1] + R[0] + R[2:] for L, R in splits if len(R)>1]
--     Replaces   = [L + c + R[1:]           for L, R in splits if R for c in letters]
--     inserts    = [L + c + R               for L, R in splits for c in letters]
--     return set(deletes + transposes + replaces + inserts)
-- | All edits that are one edit away from `word`.
edits1 :: Text -> [Text]
edits1 w = nub' $ foldl' mappend mempty [transposes', deletes', replaces', inserts]
  where
    splits = zip (T.inits w) (T.tails w)
    deletes' = deletes splits
    transposes' = transposes splits
    replaces' = replaces splits
    inserts = [l <> c <> r | (l,r) <- splits, c <- alphabet]

-- def edits2(word):
--     return (e2 for e1 in edits1(word) for e2 in edits1(e1))
-- | All edits that are two edits away from `word`.
edits2 :: Text -> [Text]
edits2 w = nub' [ e2 | !e1 <- edits1 w, !e2 <- edits1 e1 ]

nub' :: [Text] -> [Text]
nub' = Set.toList . Set.fromList

type Splits = [(Text, Text)]

-- transposes = [L + R[1] + R[0] + R[2:] for L, R in splits if len(R)>1]
transposes :: Splits -> [Text]
transposes splits =
  [l <> swap' r | x <- splits, (l,r) <- unSplitWith (\a -> T.length a > 1) x]
  where
  swap' w = T.intercalate "" [two, one', rest]
    where
      two  = T.take 1 $ T.drop 1 w
      one'  = T.take 1 w
      rest = T.tail $ T.tail w

-- deletes = [L + R[1:] for L, R in splits if R]
deletes :: Splits -> [Text]
deletes splits =
  [l <> T.tail r | x <- splits, (l,r) <- unSplit x]

-- replaces = [L + c + R[1:] for L, R in splits if R for c in letters]
replaces :: Splits -> [Text]
replaces splits = [l <> c <> T.tail r | x <- splits, (l,r) <- unSplit x, c <- alphabet]

unSplit :: (Monad f, Alternative f) => (Text, Text) -> f (Text,Text)
unSplit = unSplitWith (/= "")

unSplitWith :: (Monad f, Alternative f) => (Text -> Bool) -> (Text, Text) -> f (Text,Text)
unSplitWith f (l, r) = guard (f r) >> pure (l, r)
