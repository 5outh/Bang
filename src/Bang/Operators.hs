{-# LANGUAGE NoMonomorphismRestriction #-}
module Bang.Operators(
  (%),
  (&),
  (<&>),
  (<>>),
  (^>)
) where

import Control.Monad
import Bang.Music

infixl 1 %
(%) = (>>)

(&) = concurrent

infixr 0 <&>
m1 <&> m2 = interleave m1 m2 

infixr 0 <>>
x <>> m = bpm x $ forever $ m

infixr 0 ^>
x ^> m = bpm x $ m