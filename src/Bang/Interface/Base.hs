module Bang.Interface.Base where

import Bang.Music
import Bang.Music.Class
import Data.Ratio
import Control.Applicative
import Data.Monoid

{- Operators -}

-- |Play a `Composition` `n` times
($>) :: Int -> Composition -> Composition
n $> m = foldr1 (<>) $ replicate n m

-- |Play a `Composition` at `n` bpm
(!>) :: Integer -> Composition -> Composition
n !> m = bpm n m

-- |Repeat a `Composition` at `n` bpm
(@>) :: Integer -> Composition -> Composition
n @> m = bpm n $ foldr1 (<>) $ repeat m

(&) :: Composition -> Composition -> Composition
(&) = (:=:)

{- Constructors -}
rest :: Composition
rest = pure (Rest $ 1 % 4)

r :: Composition
r = rest

measure4 :: Composition
         -> Composition
         -> Composition
         -> Composition
         -> Composition
measure4 c1 c2 c3 c4 = foldr1 (<>) [c1, c2, c3, c4]

{- Useful functions -}
rev :: Composition -> Composition
rev p@(Prim _) = p
rev (a :+: b) = rev b :+: rev a
rev (a :=: b) = rev b :=: rev a

mirror :: Composition -> Composition
mirror = (<>) <$> id <*> rev

-- |Shorthand for `measure4`
m4 = measure4

speedDiv :: Duration -> Composition -> Composition
speedDiv x = (fmap . fmap) (/x)

speedMult :: Duration -> Composition -> Composition
speedMult x = (fmap . fmap) (*x)

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
