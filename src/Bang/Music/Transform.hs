module Bang.Music.Transform where

import Bang.Music.Class
import Bang.Interface.Base
import Data.Monoid
import Data.Bifunctor

reverseMusic :: Music Dur b -> Music Dur b
reverseMusic p@(Prim _) = p
reverseMusic (a :+: b) = reverseMusic b :+: reverseMusic a
reverseMusic (a :=: b) =
  if      durA < durB then (rest diff :+: reverseMusic a) :=: reverseMusic b
  else if durB < durA then reverseMusic a :=: (rest diff :+: reverseMusic b)
  else reverseMusic a :=: reverseMusic b
  where (durA, durB) = (duration a, duration b)
        diff = abs $ durA - durB
reverseMusic m@(Modify c a) = Modify c (reverseMusic a)

mirror :: Music Dur b -> Music Dur b
mirror m = m <> reverseMusic m

cross :: Music Dur b -> Music Dur b
cross m = m >< reverseMusic m

takeDur :: Dur -> Music Dur b -> Music Dur b
takeDur d = go d
  where go dr m | dr <= 0 = rest (abs dr)
                | otherwise = case m of
                    p@(Prim _)   -> p
                    (a :+: b)    -> (go dr a) :+: (go (dr - duration a) b)
                    (a :=: b)    -> (go dr a) :=: (go dr b)
                    (Modify c a) -> Modify c (go dr a)

dropDur :: Dur -> Music Dur b -> Music Dur b
dropDur d = go d
  where go dr m | dr <= 0 = m
                | otherwise = case m of
                    p@(Prim (Rest d'))   -> rest 0
                    p@(Prim (Note d' _)) -> rest 0
                    (a :+: b)    -> (go dr a) :+: (go (dr - duration a) b)
                    (a :=: b)    -> (go dr a) :=: (go dr b)
                    (Modify c a) -> Modify c (go dr a)

partitionDur :: Dur -> Music Dur b -> (Music Dur b, Music Dur b)
partitionDur d m = (takeDur d m, dropDur d m)

hushFor :: Dur -> Music Dur b -> Music Dur b
hushFor d m = rest d <> dropDur d m

hushFrom :: Dur -> Music Dur b -> Music Dur b
hushFrom d m = takeDur d m <> rest (max (duration m - d) 0)

mconcatMap :: Monoid b => (a -> b) -> [a] -> b
mconcatMap f = mconcat . map f

poly :: (Dur, Music Dur b) -> (Dur, Music Dur b) -> Music Dur b
poly (x, m) (y, n) = (tempo (4/x) m) :=: (tempo (4/y) n)

withDuration :: Dur -> Music Dur b -> Music Dur b
withDuration d m = first (*(d/d')) m
  where d' = duration m

fitL :: Music Dur b -> Music Dur b -> Music Dur b
fitL a = cappend a . withDuration (duration a) 

fitR :: Music Dur b -> Music Dur b -> Music Dur b
fitR = flip fitL

normalize :: Dur -> [Music Dur b] -> Music Dur b
normalize d = mconcatMap (withDuration d)

normalizeC :: Dur -> [Music Dur b] -> Music Dur b
normalizeC d = cconcat . map (withDuration d)
