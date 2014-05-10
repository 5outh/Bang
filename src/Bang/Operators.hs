{-# LANGUAGE NoMonomorphismRestriction #-}
module Bang.Operators(
  (&),
  (<&>),
  (<>>),
  (^>),
  ($>)
) where

{- @TODO: Prove the eight laws of polymorphic temporal media:
  
  (!!1)
  >> is associative (for free: http://en.wikibooks.org/wiki/Haskell/Understanding_monads#Monad_Laws)
  m1 >> (m2 >> m3) == (m1 >> m2) >> m3

  (2)
  & is associative
  (m1 & m2) & m3 == m1 & (m2 & m3)

  (3)
  & is commutative
  (m1 & m2) == (m2 & m1)

  (!!4)
  return () >> m == m
  return () >>= \_ -> m
  (\_ -> m) ()
  = m
  
  (5??) ( constrain to Composition () )
  [22:52] <monochrom> Theorem: suppose m >> return() = m >>= \() -> return (), 
  so you know that m is not pulling a ⊥ trick. then m >> return () = m.

  m >> return () == m
  m >>= \_ -> return ()
  
  (6*)
  rest* d & m === m, if d == dur m **
    ** actually d <= dur m in this case.
  
  (7*)
  return d1 >> return d2 
           ===
    return (d1 >> d2)
  
  (8)
  (m1 >> m2) & (m3 >> m4)
            === 
  (m1 & m3) >> (m2 & m4)

  MONAD LAWS:
  (1) return a >>= f == f a
  (2) m >>= return == m
  (3)    (m >>= f) >>= g
               ==
      (m >>= (\x -> f x >>= g))

  m >> k = m >>= \_ -> k

-}

import Control.Monad
import Bang.Music

-- |Concurrently run two `Composition`s
(&) = mergeCompositions

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