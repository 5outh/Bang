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

-- | Convenience function for concatenating four compositions together
-- sequentially. Most general type signature \'cause why not?
m4 :: Monoid a => a -> a -> a -> a -> a
m4 a b c d = mconcat [a, b, c, d]

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