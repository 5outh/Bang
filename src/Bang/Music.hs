module Bang.Music where

import Bang.Music.Class
import Bang.Music.MDrum
import System.MIDI
import Data.Ratio
import Prelude hiding (foldr, foldl, foldr1, foldl1)
import Data.Monoid

withDuration :: Duration -> Composition -> Composition
withDuration d = (fmap . fmap) (const d)

sumDuration :: Composition -> Rational
sumDuration = go
  where go (a :=: b) = max (go a) (go b)
        go (a :+: b) = go a + go b
        go (Prim a)  = dur a

scanDuration :: Composition -> Composition
scanDuration = go 0
  where go acc p@(Prim a) = withDuration acc p
        go acc (a :+: b) = go acc a :+: go (acc + sumDuration a) b
        go acc (a :=: b) = go acc a :=: go acc b

toList :: Composition -> [Music Duration]
toList (Prim a) = [a]
toList (a :+: b) = toList a <> toList b
toList (a :=: b) = toList a <> toList b

-- mapDurationF = fmap
-- mapDuration  = fmap . fmap