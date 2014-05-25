module Bang.Interpreter where

import Bang.Music.Class
import Bang.Interface.Drum

import System.MIDI
import Data.Bifunctor
import Data.Ratio
import Data.Monoid

toList :: Music Dur PercussionSound -> [MidiEvent]
toList m = map drumToMidiEvent itpd
  where itpd@(x:_) = interpret m

interpret :: Music Dur PercussionSound -> [Primitive Dur PercussionSound]
interpret (Modify (Tempo a) m)      = interpret (first (*a) m)
interpret (Modify (BPM n)   m)      = interpret (first (* (240000 % n)) m) -- breaks down when bpm has already been set
interpret (Modify (Instrument _) m) = interpret m -- @TODO
interpret (Prim n@(Note _ _))       = [n]
interpret (Prim n@(Rest _))         = []
interpret (a :+: b)                 = interpret a `mappend` (map (\x -> x{dur = dur x + durA}) (interpret b))
  where durA       = duration a
        intB@(h:_) = interpret b
interpret (a :=: b)                 = interpret a `merge` interpret b

merge :: Ord d => [Primitive d a] -> [Primitive d a] -> [Primitive d a]
merge [] ys = ys
merge xs [] = xs
merge (a:xs) (b:ys)
  | dur a <= dur b = a : merge xs (b:ys)
  | otherwise = b : merge (a:xs) ys