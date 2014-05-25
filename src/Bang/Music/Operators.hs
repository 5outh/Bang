module Bang.Music.Operators where

import Bang.Music.Class
import Bang.Interface.Base

import Data.Monoid

infixr 0 %>
(%>) :: Integer -> Music a b -> Music a b
(%>) = bpm

infixr 0 !>
(!>) :: Rational -> Music a b -> Music a b
(!>) = tempo

infixr 0 @>
(@>) :: Num a => Integer -> Music a b -> Music a b
n @> m = bpm n (mconcat $ repeat m)

infixr 0 #>
(#>) :: Num a => Int -> Music a b -> Music a b
n #> m = mconcat $ replicate n m