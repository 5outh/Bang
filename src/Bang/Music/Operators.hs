module Bang.Music.Operators where

import Bang.Music.Class
import Bang.Music.Transform
import Bang.Interface.Base

import Data.Monoid

infixr 0 !>
(!>) :: Rational -> Music a b -> Music a b
(!>) = tempo

infixr 0 #>
(#>) :: Num a => Int -> Music a b -> Music a b
n #> m = mconcat $ replicate n m

infixl 1 >>~
(>>~) :: Monoid b => (a -> b) -> [a] -> b
(>>~) = mconcatMap

(~=~) :: (Dur, Music Dur b) -> (Dur, Music Dur b) -> Music Dur b
(~=~) = poly