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

import Bang.Music.Class
import Data.Monoid

-- | 'Rest' for a given duration.
rest :: Dur -> Music Dur a
rest d = Prim (Rest d)

-- | Convenience constructor for single 'Note's
note :: Dur -> a -> Music Dur a
note d x = Prim (Note d x)

-- | Set the bpm of a composition
bpm :: Integer -> Music a b -> Music a b
bpm n = Modify (BPM n)

-- | Set the tempo of a composition
tempo :: Rational -> Music a b -> Music a b
tempo n = Modify (Tempo (1/n))

-- | Set the time signature of a composition
-- @n@ is the beat unit, @d@ is the number of beats per measure.
-- For example, @ts 4 4 ==@ Common time
ts :: Rational -> Rational -> Music a b -> Music a b
ts n d = Modify (Tempo (d/n))

-- | Quadruple the tempo of a composition.
quad :: Music a b -> Music a b
quad = tempo 4

-- | Double the tempo of a composition.
double :: Music a b -> Music a b
double = tempo 2

-- | Set the tempo of a composition to 1 (default, typically idempotent).
normal :: Music a b -> Music a b
normal = tempo 1

-- | Half the tempo of a composition.
half :: Music a b -> Music a b
half = tempo (1/2)

-- | Quarter the tempo of a composition.
quarter :: Music a b -> Music a b
quarter = tempo (1/4)

-- | Convenience constructor for smashing `n` values into a single 1-duration measure.
tuplets :: Rational -> Music a b -> Music a b
tuplets n = tempo (n/4)

-- | Play 3 notes per measure.
triplets :: Music a b -> Music a b
triplets = tuplets 3

-- | Play 5 notes per measure.
quintuplets :: Music a b -> Music a b
quintuplets = tuplets 5

-- | Sixteenth rest
sr :: Music Dur a
sr = rest (1/16)

-- | Eighth rest
er :: Music Dur a
er = rest (1/8)

-- | Quarter rest
qr :: Music Dur a
qr = rest (1/4)

-- | Half rest
hr :: Music Dur a
hr = rest (1/2)

-- | Whole rest
wr :: Music Dur a
wr = rest 1

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
m6 a b c d e f =  mconcat [a, b, c, d, e, f]
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
c2 :: Num d => Music d a -> Music d a -> Music d a
c2 a b = a <> b
c3 :: Num d => Music d a -> Music d a -> Music d a -> Music d a
c3 a b c = cconcat [a, b, c]
c4 :: Num d => Music d a -> Music d a -> Music d a -> Music d a 
            -> Music d a
c4 a b c d = cconcat [a, b, c, d]
c5 :: Num d => Music d a -> Music d a -> Music d a -> Music d a 
              -> Music d a -> Music d a
c5 a b c d e = cconcat [a, b, c, d, e]
c6 :: Num d => Music d a -> Music d a -> Music d a -> Music d a 
            -> Music d a -> Music d a -> Music d a
c6 a b c d e f =  cconcat [a, b, c, d, e, f]
c7 :: Num d => Music d a -> Music d a -> Music d a -> Music d a 
            -> Music d a -> Music d a -> Music d a -> Music d a
c7 a b c d e f g = cconcat [a, b, c, d, e, f, g]
c8 :: Num d => Music d a -> Music d a -> Music d a -> Music d a 
            -> Music d a -> Music d a -> Music d a -> Music d a 
            -> Music d a
c8 a b c d e f g h = cconcat [a, b, c, d, e, f, g, h]
c9 :: Num d => Music d a -> Music d a -> Music d a -> Music d a 
            -> Music d a -> Music d a -> Music d a -> Music d a 
            -> Music d a -> Music d a
c9 a b c d e f g h i = cconcat [a, b, c, d, e, f, g, h, i]
c10 :: Num d => Music d a -> Music d a -> Music d a -> Music d a 
             -> Music d a -> Music d a -> Music d a -> Music d a 
             -> Music d a -> Music d a -> Music d a
c10 a b c d e f g h i j = cconcat [a, b, c, d, e, f, g, h, i, j]
c11 :: Num d => Music d a -> Music d a -> Music d a -> Music d a 
             -> Music d a -> Music d a -> Music d a -> Music d a 
             -> Music d a -> Music d a -> Music d a -> Music d a
c11 a b c d e f g h i j k = cconcat [a, b, c, d, e, f, g, h, i, j, k]
c12 :: Num d => Music d a -> Music d a -> Music d a -> Music d a 
             -> Music d a -> Music d a -> Music d a -> Music d a 
             -> Music d a -> Music d a -> Music d a -> Music d a 
             -> Music d a
c12 a b c d e f g h i j k l = cconcat [a, b, c, d, e, f, g, h, i, j, k, l]