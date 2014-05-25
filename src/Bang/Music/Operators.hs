module Bang.Music.Operators where

import Bang.Music.Class
import Bang.Interface.Base

import Data.Monoid

(%>) :: Integer -> Music a b -> Music a b
(%>) = bpm

(!>) :: Rational -> Music a b -> Music a b
(!>) = tempo

(@>) :: Num a => Integer -> Music a b -> Music a b
n @> m = bpm n (mconcat $ repeat m)

(#>) :: Num a => Int -> Music a b -> Music a b
n #> m = mconcat $ replicate n m