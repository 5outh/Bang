{-|
Module      : Bang.Interface.Base
Description : An interface to the basic, general musical operations in Bang
Copyright   : (c) Benjamin Kovach, 2014
License     : MIT
Maintainer  : bkovach13@gmail.com
Stability   : experimental
Portability : Mac OSX

This module exports a number of utilities for constructing primitive notes, rests, and tempo.
-}
module Bang.Interface.Base where

import           Bang.Music.Class

-- | 'Rest' for a given duration.
rest :: Rational -> Music a
rest d = Prim (Rest d)

-- | Convenience constructor for single 'Note's
note :: Rational -> a -> Music a
note d x = Prim (Note d x)

-- | Set the bpm of a composition
bpm :: Integer -> Music b -> Music b
bpm n = Modify (BPM n)

-- | Set the tempo of a composition
tempo :: Rational -> Music b -> Music b
tempo n = Modify (Tempo (1 / n))

-- | Set the time signature of a composition
-- @n@ is the beat unit, @d@ is the number of beats per measure.
-- For example, @ts 4 4 ==@ Common time
ts :: Rational -> Rational -> Music b -> Music b
ts n d = Modify (Tempo (d / n))

-- | Quadruple the tempo of a composition.
quad :: Music b -> Music b
quad = tempo 4

-- | Double the tempo of a composition.
double :: Music b -> Music b
double = tempo 2

-- | Set the tempo of a composition to 1 (default, typically idempotent).
normal :: Music b -> Music b
normal = tempo 1

-- | Half the tempo of a composition.
half :: Music b -> Music b
half = tempo (1 / 2)

-- | Quarter the tempo of a composition.
quarter :: Music b -> Music b
quarter = tempo (1 / 4)

-- | Convenience constructor for smashing `n` values into a single 1-duration measure.
tuplets :: Rational -> Music b -> Music b
tuplets n = tempo (n / 4)

-- | Play 3 notes per measure.
triplets :: Music b -> Music b
triplets = tuplets 3

-- | Play 5 notes per measure.
quintuplets :: Music b -> Music b
quintuplets = tuplets 5

-- | Sixteenth rest
sr :: Music a
sr = rest (1 / 16)

-- | Eighth rest
er :: Music a
er = rest (1 / 8)

-- | Quarter rest
qr :: Music a
qr = rest (1 / 4)

-- | Half rest
hr :: Music a
hr = rest (1 / 2)

-- | Whole rest
wr :: Music a
wr = rest 1

-- Exported separately because it'll be nice to have for Drum
dots :: Int -> Rational
dots n = 2 - (1 / (2 ^ n))

dottedRest :: Int -> Rational -> Music a
dottedRest n d = rest (d * (dots n))

-- | Shorthand for @dottedRest@
dr :: Int -> Rational -> Music a
dr = dottedRest

-- | Constructor for a singly-dotted rest
oneDotRest :: Rational -> Music a
oneDotRest = dottedRest 1

-- | Eighth Dotted Rest
edr :: Music a
edr = oneDotRest (1 / 8)

-- | Quarter Dotted Rest
qdr :: Music a
qdr = oneDotRest (1 / 4)

-- | Half Dotted Rest
hdr :: Music a
hdr = oneDotRest (1 / 2)

-- | Whole Dotted Rest
wdr :: Music a
wdr = oneDotRest 1

-- |Sequence `k` compositions together without the need for lists.
-- `m` corresponds to `m` in `mappend`, `mconcat`, etc.
m2 :: Monoid a => a -> a -> a
m2 a b = a <> b
m3 :: Monoid a => a -> a -> a -> a
m3 a b c = mconcat [a, b, c]
m4 :: Monoid a => a -> a -> a -> a -> a
m4 a b c d = mconcat [a, b, c, d]
m5 :: Monoid a => a -> a -> a -> a -> a -> a
m5 a b c d e = mconcat [a, b, c, d, e]
m6 :: Monoid a => a -> a -> a -> a -> a -> a -> a
m6 a b c d e f = mconcat [a, b, c, d, e, f]
m7 :: Monoid a => a -> a -> a -> a -> a -> a -> a -> a
m7 a b c d e f g = mconcat [a, b, c, d, e, f, g]
m8 :: Monoid a => a -> a -> a -> a -> a -> a -> a -> a -> a
m8 a b c d e f g h = mconcat [a, b, c, d, e, f, g, h]
m9 :: Monoid a => a -> a -> a -> a -> a -> a -> a -> a -> a -> a
m9 a b c d e f g h i = mconcat [a, b, c, d, e, f, g, h, i]
m10 :: Monoid a => a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a
m10 a b c d e f g h i j = mconcat [a, b, c, d, e, f, g, h, i, j]
m11 :: Monoid a => a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a
m11 a b c d e f g h i j k = mconcat [a, b, c, d, e, f, g, h, i, j, k]
m12 :: Monoid a => a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a
m12 a b c d e f g h i j k l = mconcat [a, b, c, d, e, f, g, h, i, j, k, l]

-- |Play `k` compositions concurrently without the need for lists.
-- `c` corresponds to `c` in `cappend`, `cconcat`, etc.
c2 :: Music a -> Music a -> Music a
c2 a b = a <> b
c3 :: Music a -> Music a -> Music a -> Music a
c3 a b c = cconcat [a, b, c]
c4 :: Music a -> Music a -> Music a -> Music a -> Music a
c4 a b c d = cconcat [a, b, c, d]
c5 :: Music a -> Music a -> Music a -> Music a -> Music a -> Music a
c5 a b c d e = cconcat [a, b, c, d, e]
c6 :: Music a -> Music a -> Music a -> Music a -> Music a -> Music a -> Music a
c6 a b c d e f = cconcat [a, b, c, d, e, f]
c7
  :: Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
c7 a b c d e f g = cconcat [a, b, c, d, e, f, g]
c8
  :: Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
c8 a b c d e f g h = cconcat [a, b, c, d, e, f, g, h]
c9
  :: Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
c9 a b c d e f g h i = cconcat [a, b, c, d, e, f, g, h, i]
c10
  :: Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
c10 a b c d e f g h i j = cconcat [a, b, c, d, e, f, g, h, i, j]
c11
  :: Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
c11 a b c d e f g h i j k = cconcat [a, b, c, d, e, f, g, h, i, j, k]
c12
  :: Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
  -> Music a
c12 a b c d e f g h i j k l = cconcat [a, b, c, d, e, f, g, h, i, j, k, l]
