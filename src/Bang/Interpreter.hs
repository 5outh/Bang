module Bang.Interpreter where

import Bang.Music.Class
import System.MIDI

interpret :: Music a -> [MidiEvent]
interpret = undefined