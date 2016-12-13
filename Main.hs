{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- http://norvig.com/spell-correct.html

import           Data.List (nub)
import qualified Data.Map as Map
import qualified Data.MultiSet as MultiSet
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Text.Printf (printf)

import           Protolude

type Hist = MultiSet.MultiSet Text

-- dictLocation :: FilePath
-- dictLocation = "/usr/share/dict/words"

alphabet :: [Text]
alphabet = fmap T.singleton ['a'..'z']

inAlphabet :: Char -> Bool
inAlphabet t = t `elem` ['a'..'z']

-- def P(word, N=sum(WORDS.values())):
--     return WORDS[word] / N
-- | Probability of `word`.
prob :: Text -> Hist -> Int
prob t ms = occurences `div` totalWords
  where
    occurences = fromMaybe 0 $ Map.lookup t (MultiSet.toMap ms)
    totalWords = MultiSet.distinctSize ms

words :: Text -> [Text]
words = T.split (not . inAlphabet) . T.toLower

-- | create a Multi Set (Histogram) from a collection of words
toHistogram :: [Text] -> Hist
toHistogram = MultiSet.fromList

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
known hist ws = Set.fromList $ filter (\w -> MultiSet.member w hist) ws
-- member :: Ord a => a -> MultiSet a -> Bool
-- filter :: (a -> Bool) -> MultiSet a -> MultiSet a

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
edits1 w = nub $ mconcat [transposes', deletes', replaces', inserts]
  where
    splits = zip (T.tails w) (T.inits w)
    deletes' = deletes splits
    transposes' = transposes splits
    replaces' = replaces splits
    inserts = [l <> c <> r | (l,r) <- splits, c <- alphabet]

type Splits = [(Text, Text)]

transposes :: Splits -> [Text]
transposes splits =
  [l <> swap' r | x <- splits, (l,r) <- unSplitWith (\a -> T.length a > 1) x]
  where
  swap' w = T.intercalate "" [two, one', rest]
    where
      two  = T.take 1 $ T.drop 1 w
      one'  = T.take 1 w
      rest = T.tail $ T.tail w

deletes :: Splits -> [Text]
deletes splits =
  [l <> T.tail r | x <- splits, (l,r) <- unSplit x]

replaces :: Splits -> [Text]
replaces splits = [l <> c <> r | x <- splits, (l,r) <- unSplit x, c <- alphabet]

unSplit :: (Monad f, Alternative f) => (Text, Text) -> f (Text,Text)
unSplit (l, r) = guard(r /= "") >> pure (l, r)

unSplitWith :: (Monad f, Alternative f) => (Text -> Bool) -> (Text, Text) -> f (Text,Text)
unSplitWith f (l, r) = guard (f r) >> pure (l, r)

-- def edits2(word):
--     return (e2 for e1 in edits1(word) for e2 in edits1(e1))
-- | All edits that are two edits away from `word`.
edits2 :: Text -> [Text]
edits2 w = [ e2 | e1 <- edits1 w, e2 <- edits1 e1 ]

main :: IO ()
main = do
  hist <- toHistogram . words <$> readFile "big.txt"
  -- let can = candidates hist "watever"
  -- traverse_ putText can
  -- putText $ correction hist "speling" -- spelling
  -- putText $ correction hist "korrectud" -- "corrected"
  -- let k = known hist ["hero", "whatever", "nerp"]
  -- traverse_ putText k

  traverse_ (putText . show) $ Map.assocs $ MultiSet.toMap hist
  -- let p = prob "the" hist
  -- printf "probability of 'the' -> %i" p

  -- dict <- T.lines <$> readFile dictLocation
  -- traverse_ putText  . take 5 dict
