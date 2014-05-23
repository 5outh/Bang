module Bang.Interpreter where

import Bang.Music.Class
import Bang.Interface.Drum

import System.MIDI
import Data.Bifunctor
import Data.Ratio

-- @TODO: Finish
interpret :: Music Dur PercussionSound -> [MidiEvent]
interpret (Modify (Tempo a) m)      = interpret (first (*a) m)
interpret (Modify (BPM n)   m)      = interpret (first (* (240000 % n)) m)
interpret (Modify (Instrument i) m) = interpret m -- @TODO
interpret (Prim n@(Note _ _))       = [drumToMidiEvent n]