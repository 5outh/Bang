module Bang.Interface.MDrum where

import Bang.Music
import Bang.Music.MDrum
import Control.Monad.Free
import System.MIDI

-- Beats!
snare, bass :: Composition ()
snare = liftF $ (MDrum Snare 32) ()
bass  = liftF $ (MDrum Bass 32) ()

tom :: TomType -> Composition ()
tom t = liftF $ (MDrum (Tom t) 32) ()

cymbal :: CymbalType -> Composition ()
cymbal c = liftF $ (MDrum (Cymbal c) 32) ()

hiHat :: Bool -> Composition ()
hiHat open = liftF $ (MDrum (HiHat open) 32) ()

-- Short names
sn = snare
b  = bass
t1 = tom Hang1
t2 = tom Hang2
tf = tom Floor
cc = cymbal Crash
cr = cymbal Ride
cs = cymbal Splash
ch = cymbal China
bl = cymbal Bell
hc = hiHat False
ho = hiHat True

-- Play the things
playBass, playSnare :: Delay -> MidiEvent
playBass  d = midiEvent d 35
playSnare d = midiEvent d 38

playTom :: TomType -> Delay -> MidiEvent
playTom t d = midiEvent d $ case t of
  Floor -> 43
  Hang1 -> 47
  Hang2 -> 50

playCymbal :: CymbalType -> Delay -> MidiEvent
playCymbal c d = midiEvent d $ case c of
    Crash  -> 49
    Ride   -> 51
    Splash -> 55
    China  -> 52
    Bell   -> 53

playHiHat :: Bool -> Delay -> MidiEvent
playHiHat open d = midiEvent d $
  if open then 46 else 42

drumToMidiEvent :: Music a -> MidiEvent
drumToMidiEvent (Rest d _) = error "No MIDI event associated with rests!"
drumToMidiEvent (MDrum dr d _) = case dr of
  HiHat open -> playHiHat open d
  Cymbal c   -> playCymbal c d
  Tom t      -> playTom t d
  Bass       -> playBass d
  Snare      -> playSnare d