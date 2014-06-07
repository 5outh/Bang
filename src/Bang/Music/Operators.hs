module Bang.Music.Operators where

import Bang.Music.Class
import Bang.Music.Transform
import Bang.Interface.Base

import Data.Monoid

infixr 0 !>
(!>) :: Rational -> Music a b -> Music a b
(!>) = tempo

infixr 1 #>
(#>) :: Num a => Int -> Music a b -> Music a b
n #> m = repl n m

infixl 1 >>~
(>>~) :: Monoid b => (a -> b) -> [a] -> b
(>>~) = mconcatMap

(~=~) :: (Dur, Music Dur b) -> (Dur, Music Dur b) -> Music Dur b
(~=~) = poly

infixl 2 ~=
(~=) :: Music Dur b -> Music Dur b -> Music Dur b
(~=) = fitL

infixr 2 =~
(=~) :: Music Dur b -> Music Dur b -> Music Dur b
(=~) = fitR

infixr 2 ~~
(~~) :: Dur -> Music Dur b -> Music Dur b
(~~) = withDuration
