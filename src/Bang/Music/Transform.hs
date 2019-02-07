{-|
Module      : Bang.Music.Transform
Description : General transformations on compositions
Copyright   : (c) Benjamin Kovach, 2014
License     : MIT
Maintainer  : bkovach13@gmail.com
Stability   : experimental
Portability : Mac OSX

This module exports a number of functions to manipulate compositions in various ways.
-}
module Bang.Music.Transform where

import           Bang.Interface.Base
import           Bang.Music.Class
import           Data.Foldable       (foldMap)

-- |Reverses a composition
reverseMusic :: Music b -> Music b
reverseMusic p@(Prim _ ) = p
reverseMusic (  a :+: b) = reverseMusic b :+: reverseMusic a
reverseMusic (a :=: b)
  | durA < durB = (rest diff :+: reverseMusic a) :=: reverseMusic b
  | durB < durA = reverseMusic a :=: (rest diff :+: reverseMusic b)
  | otherwise   = reverseMusic a :=: reverseMusic b
 where
  (durA, durB) = (musicDuration a, musicDuration b)
  diff         = abs $ durA - durB
reverseMusic (Modify c a) = Modify c (reverseMusic a)

-- |Play a composition forwards, then backwards.
mirror :: Music b -> Music b
mirror m = m <> reverseMusic m

-- |Play a composition backwards, then forwards.
mirrorR :: Music b -> Music b
mirrorR m = reverseMusic m <> m

-- |Play a composition forwards and backwards concurrently.
cross :: Music b -> Music b
cross m = m `cappend` reverseMusic m

-- |Take the first `d` duration units of a composition.
takeDur :: Rational -> Music b -> Music b
takeDur = go
 where
  go d m
    | d <= 0 = rest (abs d)
    | otherwise = case m of
      p@(Prim _      ) -> p
      (  a      :+: b) -> go d a :+: go (d - musicDuration a) b
      (  a      :=: b) -> go d a :=: go d b
      (  Modify c   a) -> Modify c (go d a)

-- |Drop the first `d` duration units of a composition.
dropDur :: Rational -> Music b -> Music b
dropDur = go
 where
  go d m
    | d <= 0 = m
    | otherwise = case m of
      (Prim (Rest _  )) -> rest 0
      (Prim (Note _ _)) -> rest 0
      (a      :+: b   ) -> go d a :+: go (d - musicDuration a) b
      (a      :=: b   ) -> go d a :=: go d b
      (Modify c   a   ) -> Modify c (go d a)

-- |Take the last `d` duration units of a composition
takeLast :: Rational -> Music b -> Music b
takeLast d m = dropDur (musicDuration m - d) m

-- |Drop the last `d` duration units of a composition
dropLast :: Rational -> Music b -> Music b
dropLast d m = takeDur (musicDuration m - d) m

-- |Remove the section of the composition between `d1` and `d2`.
removeBetween :: Rational -> Rational -> Music b -> Music b
removeBetween d1 d2 m = takeDur d1 m <> dropDur d2 m

-- |Split a composition at a specific duration and return the composition
-- before said duration along with the rest of it.
partitionDur :: Rational -> Music b -> (Music b, Music b)
partitionDur d m = (takeDur d m, dropDur d m)

-- |Turn the first `d` duration units of a composition into silence.
hushFor :: Rational -> Music b -> Music b
hushFor d m = rest d <> dropDur d m

-- |Turn the rest of a composition into silence after `d` duration units.
hushFrom :: Rational -> Music b -> Music b
hushFrom d m = takeDur d m <> rest (max (musicDuration m - d) 0)

-- |Turn the last `d` duration units into silence
hushLast :: Rational -> Music b -> Music b
hushLast d m = hushFrom (musicDuration m - d) m

-- |Turn everything but the last `d` duration units into silence
hushUntilLast :: Rational -> Music b -> Music b
hushUntilLast d m = hushFor (musicDuration m - d) m

-- |Turn the section of a composition between `pos` and `d` into silence.
hushBetween :: Rational -> Rational -> Music b -> Music b
hushBetween pos d m = pre <> rest d <> dropDur d post
  where (pre, post) = partitionDur pos m

-- |Play a polyrhythm with 'm' having units of length 1\/x and 'n' with units of length 1\/y
--
-- Example:
--
-- > poly (3, 3 #> bd) (4, 4 #> sn)
poly :: (Rational, Music b) -> (Rational, Music b) -> Music b
poly (x, m) (y, n) = tempo (x / 4) m :=: tempo (y / 4) n

-- |Set the duration of a composition
withDuration :: Rational -> Music b -> Music b
withDuration d m = mapMusicDuration (* (d / d')) m where d' = musicDuration m

-- |Replicate a composition `n` times.
repl :: Int -> Music b -> Music b
repl n = mconcat . replicate n

-- |Infinitely repeat a composition.
rep :: Music b -> Music b
rep = mconcat . repeat

-- |Fit the duration of `b` to the duration of `a`
fitL :: Music b -> Music b -> Music b
fitL a = cappend a . withDuration (musicDuration a)

-- |Fit the duration of `a` into the duration of `b`
fitR :: Music b -> Music b -> Music b
fitR = flip fitL

-- |Normalize the durations of each value in a list of Compositions to `d` and compose them sequentially.
normalize :: Rational -> [Music b] -> Music b
normalize d = foldMap (withDuration d)

-- |Normalize the durations of each value in a list of Compositions to `d` and compose them concurrently.
normalizeC :: Rational -> [Music b] -> Music b
normalizeC d = cconcat . map (withDuration d)

-- |Normalize each composition's duration to the duration of the first element in the list and compose sequentially.
normalize1 :: [Music b] -> Music b
normalize1 []       = mempty
normalize1 (x : xs) = foldMap (withDuration (musicDuration x)) (x : xs)

-- |Normalize each composition's duration to the duration of the first element in the list and compose concurrently.
normalizeC1 :: [Music b] -> Music b
normalizeC1 []       = mempty
normalizeC1 (x : xs) = cconcat $ map (withDuration (musicDuration x)) (x : xs)
