module Bang.Interface.MDrum where

import Bang.Music
import Bang.Music.MDrum
import Control.Monad.Free
import System.MIDI

snare, bass :: Composition ()
-- |A single snare drum beat
snare = liftF $ (MDrum Snare 32) ()

-- |A single bass drum beat
bass  = liftF $ (MDrum Bass 32) ()

-- |A single tom drum beat, with a given `TomType`
tom :: TomType -> Composition ()
tom t = liftF $ (MDrum (Tom t) 32) ()

-- |A single cymbal hit, with a given `CymbalType`
cymbal :: CymbalType -> Composition ()
cymbal c = liftF $ (MDrum (Cymbal c) 32) ()

-- |A single hi-hat hit, either open or closed
hiHat :: Bool -> Composition ()
hiHat open = liftF $ (MDrum (HiHat open) 32) ()

-- |Shorthand for `snare`
sn = snare

-- |Shorthand for `bass`
bd  = bass

-- |Shorthand for `tom Hang1`
t1 = tom Hang1

-- |Shorthand for `tom Hang2`
t2 = tom Hang2

-- |Shorthand for `tom Floor`
tf = tom Floor

-- |Shorthand for `cymbal Crash`
cc = cymbal Crash

-- |Shorthand for `cymbal Ride`
cr = cymbal Ride

-- |Shorthand for `cymbal Splash`
cs = cymbal Splash

-- |Shorthand for `cymbal China`
ch = cymbal China

-- |Shorthand for `cymbal Bell`
bl = cymbal Bell

-- |Shorthand for `hiHat False` (closed)
hc = hiHat False

-- |Shorthand for `hiHat True` (open)
ho = hiHat True

playBass, playSnare :: Delay -> MidiEvent
-- |A `MidiEvent` for a single bass drum beat with given `Delay`
playBass  d = midiEvent d 35
-- |A `MidiEvent` for a single snare drum beat with given `Delay`
playSnare d = midiEvent d 38


-- |A `MidiEvent` for a single tom drum beat with given `TomType` and `Delay`
playTom :: TomType -> Delay -> MidiEvent
playTom t d = midiEvent d $ case t of
  Floor -> 43
  Hang1 -> 47
  Hang2 -> 50

-- |A `MidiEvent` for a single cymbal hit with given `CymbalType` and `Delay`
playCymbal :: CymbalType -> Delay -> MidiEvent
playCymbal c d = midiEvent d $ case c of
    Crash  -> 49
    Ride   -> 51
    Splash -> 55
    China  -> 52
    Bell   -> 53

-- |A `MidiEvent` for a single hi-hat hit (open or closed) with given `Delay`
playHiHat :: Bool -> Delay -> MidiEvent
playHiHat open d = midiEvent d $
  if open then 46 else 42

-- |Construct a `midiEvent` from a drum beat
drumToMidiEvent :: Music a -> MidiEvent
drumToMidiEvent (Rest d _) = error "No MIDI event associated with rests!"
drumToMidiEvent (MDrum dr d _) = case dr of
  HiHat open -> playHiHat open d
  Cymbal c   -> playCymbal c d
  Tom t      -> playTom t d
  Bass       -> playBass d
  Snare      -> playSnare d