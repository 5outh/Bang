module Bang.Interface.Base where

import Bang.Music
import Bang.Music.Class
import Control.Monad.Free
import Data.Ratio

-- |Rest for one beat.
rest :: Composition
rest  = liftF $ (Rest $ 1 % 4) ()

-- |Shorthand for `rest`
r :: Composition
r = rest

-- |Rest for a specified number of 32nd notes.
rt :: Duration -> Composition
rt d = liftF $ (Rest d) ()

-- |Create a polyrhythm with durations `n` and `m`
polyrhythm :: (Integer, Composition) -> (Integer, Composition) -> Composition
polyrhythm (n, c) (m, c') = (withDuration (1%n) c) `mergeCompositions` (withDuration (1%m) c')

-- |Sequence 4 compositions in line (special use case: create a 4/4 measure)
measure4 :: Composition -> Composition -> Composition -> Composition -> Composition
measure4 c1 c2 c3 c4 = c1 >> c2 >> c3 >> c4

-- |Shorthand for `measure4`
m4 :: Composition -> Composition -> Composition -> Composition -> Composition
m4 = measure4

-- |Reverses the notes in a `Composition`
reverseComposition :: Composition -> Composition
reverseComposition = flipZeros . go
  where go (Free End) = Free End
        go (Pure ())  = return ()
        go x@(Free _) = go (nextBeat x) >> singleton x

-- |Shorthand for `reverseComposition`
rev :: Composition -> Composition
rev = reverseComposition

-- |Play a `Composition` forwards and then backwards
mirror :: Composition -> Composition
mirror c = c >> rev c

flipZeros :: Composition -> Composition
flipZeros (Free End) = Free End
flipZeros (Pure _)   = return ()
flipZeros x@(Free _) = case nextBeat x of
    Free End -> x >> Free End
    Pure _   -> x
    next     -> case dur (value next) of
      0 -> singleton next >> flipZeros (singleton x >> (nextBeat next))
      _ -> singleton x >> flipZeros next

-- |Speed up by a factor of `x`
speedDiv :: Duration -> Composition -> Composition
speedDiv x = mapDurationF (/x)

-- |Slow down by a factor of `x`
speedMult :: Duration -> Composition -> Composition
speedMult x = mapDurationF (*x)

-- |double the speed of a `Composition`
double :: Composition -> Composition
double = speedDiv 2

-- |quadruple the speed of a `Composition`
quad :: Composition -> Composition
quad = speedDiv 4

-- |Multiply speed of a composition by 8
oct :: Composition -> Composition
oct = speedDiv 8

-- |Shorthand for `double`
dbl :: Composition -> Composition
dbl = double

-- |Shorthand for `quad`
qd :: Composition -> Composition
qd = quad

-- |Shorthand for `speedDiv`
sd :: Duration -> Composition -> Composition
sd = speedDiv

-- |half the speed of a `Composition`
half :: Composition -> Composition
half = speedMult 2

-- |quarter the speed of a `Composition`
quarter :: Composition -> Composition
quarter = speedMult 4

-- |divide the speed of a `Composition` by 8
eighth :: Composition -> Composition
eighth = speedMult 8

-- |divide the speed of a `Composition` by 16
sixteenth :: Composition -> Composition
sixteenth = speedMult 16

-- |divide the sped of a `Composition` by 32
thirtysecond :: Composition -> Composition
thirtysecond = speedMult 32

-- |Shorthand for `half`
sh :: Composition -> Composition
sh   = half

-- |Shorthand for `quarter`
sq :: Composition -> Composition
sq   = quarter

-- |Shorthand for `eighth`
se :: Composition -> Composition
se   = eighth

-- |Shorthand for `sixteenth`
sx :: Composition -> Composition
sx   = sixteenth

-- |Shorthand for `thirtysecond`
sts :: Composition -> Composition
sts  = thirtysecond

-- |Shorthand for `speedMult`
sm :: Duration -> Composition -> Composition
sm  = speedMult

-- |Convert quarter notes to n-tuplets
tuplets :: Integer -> Composition -> Composition
tuplets n = speedMult (4%n)

-- |Turn quarter notes into triplets
triplets :: Composition -> Composition
triplets = tuplets 3

-- |Turn quarter notes into quintuplets
quintuplets :: Composition -> Composition
quintuplets = tuplets 5

-- |Turn quarter notes into septuplets
septuplets :: Composition -> Composition
septuplets = tuplets 7

