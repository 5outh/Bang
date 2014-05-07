{-# LANGUAGE NoMonomorphismRestriction #-}
module Bang.Operators(
  (&),
  (<&>),
  (<>>),
  (^>),
  ($>)
) where

import Control.Monad
import Bang.Music

-- |Concurrently run two `Composition`s
(&) = concurrent

-- |Interleave two `Composition`s
infixr 1 <&>
m1 <&> m2 = interleave m1 m2 

-- |Set the `bpm` for a `Composition` and run it once.
infixr 0 ^>
x ^> m = bpm x $ m

-- |Set the `bpm` for a `Composition` and repeatedly run it
infixr 0 <>>
x <>> m = bpm x $ forever $ m

-- |Repeat a given `Composition` some number of times.
infixr 0 $>
($>) = replicateM_ 