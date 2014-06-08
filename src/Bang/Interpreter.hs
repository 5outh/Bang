module Bang.Interpreter where

import Bang.Music.Class
import Bang.Interface.Drum

import System.MIDI
import Data.Bifunctor
import Data.Ratio
import Data.Monoid


toList :: Music Dur PercussionSound -> [MidiEvent]
toList m = map drumToMidiEvent (interpret m)

merge :: Ord d => [Primitive d a] -> [Primitive d a] -> [Primitive d a]
merge [] ys = ys
merge xs [] = xs
merge (a:xs) (b:ys)
  | dur a <= dur b = a : merge xs (b:ys)
  | otherwise = b : merge (a:xs) ys

interpret :: Music Dur PercussionSound -> [Primitive Dur PercussionSound]
interpret = go 0
  where go d (a :+: b) = go d a `mappend` go (d + duration a) b
        go d (a :=: b) = go d a `merge`   go d b
        go d (Prim n@(Note _ _))       = [n{dur = d}]
        go d (Prim n@(Rest _))         = []
        go d (Modify (Tempo a) m)      = go d (first (*a) m)
        go d (Modify (BPM n)   m)      = go d (first (* (240000 % n)) m) -- breaks down when bpm has already been set
        go d (Modify (Instrument _) m) = go d m -- @TODO

