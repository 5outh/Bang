module Bang.Interpreter where

import Bang.Music.Class
import Bang.Interface.Drum

import System.MIDI
import Data.Bifunctor
import Data.Ratio
import Data.Monoid

toList :: Music Dur PercussionSound -> [MidiEvent]
toList m = map (drumToMidiEvent . f) itpd
  where itpd@(x:_) = interpret m
        f a = a{ dur = dur a - dur x }

-- @TODO: Finish
interpret :: Music Dur PercussionSound -> [Primitive Dur PercussionSound]
interpret (Modify (Tempo a) m)      = interpret (first (*a) m)
interpret (Modify (BPM n)   m)      = interpret (first (* (240000 % n)) m) -- breaks down when bpm has already been set
interpret (Modify (Instrument _) m) = interpret m -- @TODO
interpret (Prim n@(Note _ _))       = [n]
interpret (Prim n@(Rest _))         = [n]
interpret (a :+: b)                 = interpret a `mappend` (map (\x -> x{dur = dur x + durA}) (interpret b))
  where durA = duration a
interpret (a :=: b)                 = interpret a `merge` interpret b

duration :: (Num a, Ord a) => Music a b -> a
duration (a :+: b) = foldDur (+) 0 a + foldDur (+) 0 b
duration (a :=: b) = max (foldDur (+) 0 a) (foldDur (+) 0 b)
duration a         = foldDur (+) 0 a

merge :: Ord d => [Primitive d a] -> [Primitive d a] -> [Primitive d a]
merge [] ys = ys
merge xs [] = xs
merge (a:xs) (b:ys)
  | dur a <= dur b = a : merge xs (b:ys)
  | otherwise = b : merge (a:xs) ys