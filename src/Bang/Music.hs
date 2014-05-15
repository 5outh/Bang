module Bang.Music where

import Bang.Music.Class
import Bang.Music.Drum
import System.MIDI
import Data.Ratio
import Prelude hiding (foldr, foldl, foldr1, foldl1)
import Data.Monoid
import Data.List(sortBy)
import Data.Ord(comparing)
import Data.Foldable(toList)

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

bpm :: Integer -> Composition -> Composition
bpm x = scanDuration . (fmap . fmap) (* (240000 % x))

interpret :: Composition -> [Music Duration]
interpret = sortBy (comparing dur) . toList

-- mapDurationF = fmap
-- mapDuration  = fmap . fmap