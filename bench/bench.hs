module Bench where

import Criterion.Main
import qualified Main as M

main = defaultMain [
  bgroup "main"
    [
      bench "words"  $ nfIO M.main "words"
    , bench "multiset" $ nfIO M.main "toCounter"
    , bench "proba" $ nfIO M.main "proba"
    , bench "all" $ nfIO M.main "all"
    ]
  ]
