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

import Bang.Music.Class
import Bang.Interface.Base
import Data.Monoid
import Data.Bifunctor
import Data.Foldable(foldMap)

-- |Reverses a composition
reverseMusic :: Music Dur b -> Music Dur b
reverseMusic p@(Prim _) = p
reverseMusic (a :+: b) = reverseMusic b :+: reverseMusic a
reverseMusic (a :=: b)
  | durA < durB = (rest diff :+: reverseMusic a) :=: reverseMusic b
  | durB < durA = reverseMusic a :=: (rest diff :+: reverseMusic b)
  | otherwise = reverseMusic a :=: reverseMusic b
  where (durA, durB) = (duration a, duration b)
        diff = abs $ durA - durB
reverseMusic m@(Modify c a) = Modify c (reverseMusic a)

-- |Play a composition forwards, then backwards.
mirror :: Music Dur b -> Music Dur b
mirror m = m <> reverseMusic m

-- |Play a composition backwards, then forwards.
mirrorR :: Music Dur b -> Music Dur b
mirrorR m = reverseMusic m <> m

-- |Play a composition forwards and backwards concurrently.
cross :: Music Dur b -> Music Dur b
cross m = m `cappend` reverseMusic m

-- |Take the first `d` duration units of a composition.
takeDur :: Dur -> Music Dur b -> Music Dur b
takeDur = go
  where go dr m | dr <= 0 = rest (abs dr)
                | otherwise = case m of
                    p@(Prim _)   -> p
                    (a :+: b)    -> go dr a :+: go (dr - duration a) b
                    (a :=: b)    -> go dr a :=: go dr b
                    (Modify c a) -> Modify c (go dr a)

-- |Drop the first `d` duration units of a composition.
dropDur :: Dur -> Music Dur b -> Music Dur b
dropDur = go
  where go dr m | dr <= 0 = m
                | otherwise = case m of
                    p@(Prim (Rest d'))   -> rest 0
                    p@(Prim (Note d' _)) -> rest 0
                    (a :+: b)    -> go dr a :+: go (dr - duration a) b
                    (a :=: b)    -> go dr a :=: go dr b
                    (Modify c a) -> Modify c (go dr a)

-- |Take the last `d` duration units of a composition
takeLast :: Dur -> Music Dur b -> Music Dur b
takeLast d m = dropDur (duration m - d) m

-- |Drop the last `d` duration units of a composition
dropLast :: Dur -> Music Dur b -> Music Dur b
dropLast d m = takeDur (duration m - d) m

-- |Remove the section of the composition between `d1` and `d2`.
removeBetween :: Dur -> Dur -> Music Dur b -> Music Dur b
removeBetween d1 d2 m = takeDur d1 m <> dropDur d2 m

-- |Split a composition at a specific duration and return the composition
-- before said duration along with the rest of it. 
partitionDur :: Dur -> Music Dur b -> (Music Dur b, Music Dur b)
partitionDur d m = (takeDur d m, dropDur d m)

-- |Turn the first `d` duration units of a composition into silence.
hushFor :: Dur -> Music Dur b -> Music Dur b
hushFor d m = rest d <> dropDur d m

-- |Turn the rest of a composition into silence after `d` duration units.
hushFrom :: Dur -> Music Dur b -> Music Dur b
hushFrom d m = takeDur d m <> rest (max (duration m - d) 0)

-- |Turn the last `d` duration units into silence
hushLast :: Dur -> Music Dur b -> Music Dur b
hushLast d m = hushFrom (duration m - d) m

-- |Turn everything but the last `d` duration units into silence
hushUntil :: Dur -> Music Dur b -> Music Dur b
hushUntil d m = hushFor (duration m - d) m 

-- |Turn the section of a composition between `pos` and `d` into silence.
hushBetween :: Dur -> Dur -> Music Dur b -> Music Dur b
hushBetween pos d m = pre <> rest d <> dropDur d post
  where (pre, post) = partitionDur pos m

-- |Play a polyrhythm with 'm' having units of length 1\/x and 'n' with units of length 1\/y
-- 
-- Example:
--
-- > poly (3, 3 #> bd) (4, 4 #> sn)
poly :: (Dur, Music Dur b) -> (Dur, Music Dur b) -> Music Dur b
poly (x, m) (y, n) = tempo (x/4) m :=: tempo (y/4) n

-- |Set the duration of a composition
withDuration :: Dur -> Music Dur b -> Music Dur b
withDuration d m = first (*(d/d')) m
  where d' = duration m

-- |Replicate a composition `n` times.
repl :: Num a => Int -> Music a b -> Music a b
repl n = mconcat . replicate n

-- |Infinitely repeat a composition.
rep :: Num a => Music a b -> Music a b
rep = mconcat . repeat

-- |Fit the duration of `b` to the duration of `a`
fitL :: Music Dur b -> Music Dur b -> Music Dur b
fitL a = cappend a . withDuration (duration a)

-- |Fit the duration of `a` into the duration of `b`
fitR :: Music Dur b -> Music Dur b -> Music Dur b
fitR = flip fitL

-- |Normalize the durations of each value in a list of Compositions to `d` and compose them sequentially.
normalize :: Dur -> [Music Dur b] -> Music Dur b
normalize d = foldMap (withDuration d)

-- |Normalize the durations of each value in a list of Compositions to `d` and compose them concurrently.
normalizeC :: Dur -> [Music Dur b] -> Music Dur b
normalizeC d = cconcat . map (withDuration d)

-- |Normalize each composition's duration to the duration of the first element in the list and compose sequentially.
normalize1 :: [Music Dur b] -> Music Dur b
normalize1 (x:xs)= foldMap (withDuration (duration x)) (x:xs)

-- |Normalize each composition's duration to the duration of the first element in the list and compose concurrently.
normalizeC1 :: [Music Dur b] -> Music Dur b
normalizeC1 (x:xs) = cconcat $ map (withDuration (duration x)) (x:xs)
