module Bang.Music.Transform where

import Bang.Music.Class
import Bang.Interface.Base
import Data.Monoid

reverseMusic :: Music Dur b -> Music Dur b
reverseMusic p@(Prim _) = p
reverseMusic (a :+: b) = reverseMusic b :+: reverseMusic a
reverseMusic (a :=: b) =
  if durA < durB then (rest diff :+: reverseMusic a) :=: reverseMusic b
  else if durB < durA then reverseMusic a :=: (rest diff :+: reverseMusic b)
  else reverseMusic a :=: reverseMusic b
  where (durA, durB) = (duration a, duration b)
        diff = abs $ durA - durB
reverseMusic m@(Modify c a) = Modify c (reverseMusic a)

mirror :: Music Dur b -> Music Dur b
mirror m = m <> reverseMusic m

cross :: Music Dur b -> Music Dur b
cross m = m >< reverseMusic m

takeDur :: Dur -> Music Dur a -> Music Dur a
takeDur d = go d
  where go dr m | dr <= 0 = rest (abs dr)
                | otherwise = case m of
                    p@(Prim _)   -> p
                    (a :+: b)    -> (go dr a) :+: (go (dr - duration a) b)
                    (a :=: b)    -> (go dr a) :=: (go dr b)
                    (Modify c a) -> Modify c (go dr a)

