module Bang.Interpreter where

import           Bang.Interface.Drum
import           Bang.Music.Class

import           Data.Ratio
import           System.MIDI

toList :: Music PercussionSound -> [MidiEvent]
toList m = map drumToMidiEvent (interpret m)

interpret :: Music a -> [Primitive a]
interpret = go 0
 where
  go d (a :+: b                ) = go d a `mappend` go (d + musicDuration a) b
  go d (a :=: b                ) = go d a `merge` go d b
  go d (Prim n@(Note _ _)      ) = [n { duration = d }]
  go _ (Prim (  Rest _  )      ) = []
  go d (Modify (Tempo      a) m) = go d (mapMusicDuration (* a) m)
  go d (Modify (BPM        n) m) = go d (mapMusicDuration (* (240000 % n)) m)
  go d (Modify (Instrument _) m) = go d m

merge :: [Primitive a] -> [Primitive a] -> [Primitive a]
merge [] ys = ys
merge xs [] = xs
merge (a : xs) (b : ys) | duration a <= duration b = a : merge xs (b : ys)
                        | otherwise                = b : merge (a : xs) ys
