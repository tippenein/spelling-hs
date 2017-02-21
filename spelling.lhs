> {-# LANGUAGE NoImplicitPrelude #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE BangPatterns #-}

> import qualified Data.Char as Char
> import qualified Data.HashMap.Strict as Map
> import qualified Data.HashSet as Set
> import           Data.List (foldl')
> import qualified Data.Text as T
> import Data.Text.Unsafe (Iter(..), iter)
> import Data.Text.Internal (Text(..))

> import           Protolude hiding (Set)

> type Set a = Set.HashSet a
> type Counter = Map.HashMap Text Int

We split the raw text into words

> words :: Text -> [Text]
> words = T.split (not . Char.isAsciiLower) . T.toLower
> slowWords = words

This code is the intuitive answer to the problem above, however it's very slow. We'll look at performance later in this post, but for now we can adapt the core of `words` based off https://hackage.haskell.org/package/text-1.2.2.1/docs/src/Data-Text.html#words

> fastWords :: Text -> [Text]
> fastWords t@(Text arr off len) = loop 0 0
>   where
>     loop !start !n
>         | n >= len = if start == n
>                      then []
>                      else [Text arr (start+off) (n-start)]
>         | not $ Char.isAsciiLower c =
>             if start == n
>             then loop (start+1) (start+1)
>             else Text arr (start+off) (n-start) : loop (n+d) (n+d)
>         | otherwise = loop start (n+d)
>         where Iter c d = iter t n
> {-# INLINE fastWords #-}


## Counter / Bag

In python, the `Counter` is implemented as a multiset / "bag". We'll create our own with a Map from `Text` (word) to `Int` (count)

> type Counter = Map Text Int

> toCounter :: [Text] -> Hist
> toCounter = Map.fromListWith (+) . fmap (,1)

> words <- toCounter . words <$> readFile "big.txt"

There is also a package called `multiset` but I didn't know this because the wifi on the plane didn't work.

## Probability and Correction

In order to guess the right way of correcting we need to have probabilities based on the corpus' word counts.

> prob :: Counter -> Text -> Int
> prob counter word = occurences `div` totalWords
>   where
>     occurences = fromMaybe 0 $ Map.lookup t counter
>     totalWords = Map.size ms

> correction :: Counter -> Text -> Text
> correction counter word = maximumBy (\a b -> p a `compare` p b) $ candidates counter word
>   where p = prob counter

> candidates :: Counter -> Text -> Set Text
> candidates counter word = detect
>   [ known counter $ Set.singleton t
>   , known counter (edits1 word)
>   , known counter (edits2 word)
>   , Set.fromList [t]
>   ]

> detect :: [Set Text] -> Set Text
> detect = fromMaybe Set.empty . head . filter (not . Set.null)

> known :: Counter -> Set Text -> Set Text
> known counter = Set.filter (\w -> Map.member w counter)


## Edits / Permutations

I initially squished all the logic into single list-comprehensions, but you'll see I've split the heavier functions out.

> edits1 :: Text -> [Text]
> edits1 w = nub' $ mconcat [transposes', deletes', replaces', inserts]
>   where
>     alphabet    = fmap T.singleton ['a'..'z']
>     splits      = zip (T.inits w) (T.tails w)
>     deletes'    = deletes splits
>     transposes' = transposes splits
>     replaces'   = replaces splits
>     inserts     = [l <> c <> r | (l,r) <- splits, c <- alphabet]

The `splits` gets its own type for cleanliness:

> type Splits = [(Text, Text)]

Instead of `if R` or `if len(R)<1` and such like we have in python, I used a `guard` to skip over splits with contents fitting a certain criteria (e.g (l,r) where r is not empty)

> unSplit :: (Monad f, Alternative f) => (Text, Text) -> f (Text,Text)
> unSplit = unSplitWith (/= "")

> unSplitWith :: (Monad f, Alternative f) => (Text -> Bool) -> (Text, Text) -> f (Text,Text)
> unSplitWith f (l, r) = guard (f r) >> pure (l, r)

> -- | swap the 1st and 2nd letters across our list of splits ("derp" -> "edrp")
> transposes :: Splits -> [Text]
> transposes splits =
>   [l <> swap' r | x <- splits, (l,r) <- unSplitWith (\a -> T.length a > 1) x]
>   where
>     swap' w = T.intercalate "" [two, one', rest]
>       where
>         two  = T.take 1 $ T.drop 1 w
>         one'  = T.take 1 w
>         rest = T.tail $ T.tail w

> -- | remove a letter across all splits "derp" -> ["drp","dep","der"]
> deletes :: Splits -> [Text]
> deletes splits =
>   [l <> T.tail r | x <- splits, (l,r) <- unSplit x]

> -- | try replacing a letter with one from the alphabet in each spot. This one is very large
> replaces :: Splits -> [Text]
> replaces splits = [l <> c <> T.tail r | x <- splits, (l,r) <- unSplit x, c <- alphabet]

I think this comes out reasonably concise.

> edits2 :: Text -> [Text]
> edits2 w = nub' [ e2 | e1 <- edits1 w, e2 <- edits1 e1 ]

> -- Prelude's nub is prrrrrretty bad, so we use this instead.
> nub' :: [Text] -> [Text]
> nub' = Set.toList . Set.fromList
