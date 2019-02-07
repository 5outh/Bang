{-|
Module      : Bang.Interface.Drum
Description : An interface to the drum-specific operations in Bang.
Copyright   : (c) Benjamin Kovach, 2014
License     : MIT
Maintainer  : bkovach13@gmail.com
Stability   : experimental
Portability : Mac OSX

This module exports a number of functions that build primitive drum beats from various `PercussionSound`s. It also exports
some convenient shorthands for common drum types (i.e. snare, bass, toms, and cymbals). The more bizarre instruments have
slightly longer construction functions, but you'll find all of the sounds\' constructors here.
-}
module Bang.Interface.Drum where

import           Bang.Interface.Base
import           Bang.Music.Class
import           System.MIDI

-- | An Enum representing the different types of things you can bang on.
data PercussionSound =
   BassDrum2
 | BassDrum1
 | SideStick
 | SnareDrum1
 | HandClap
 | SnareDrum2
 | LowTom2
 | ClosedHihat
 | LowTom1
 | PedalHihat
 | MidTom2
 | OpenHihat
 | MidTom1
 | HighTom2
 | CrashCymbal1
 | HighTom1
 | RideCymbal1
 | ChineseCymbal
 | RideBell
 | Tambourine
 | SplashCymbal
 | Cowbell
 | CrashCymbal2
 | VibraSlap
 | RideCymbal2
 | HighBongo
 | LowBongo
 | MuteHighConga
 | OpenHighConga
 | LowConga
 | HighTimbale
 | LowTimbale
 | HighAgogo
 | LowAgogo
 | Cabasa
 | Maracas
 | ShortWhistle
 | LongWhistle
 | ShortGuiro
 | LongGuiro
 | Claves
 | HighWoodBlock
 | LowWoodBlock
 | MuteCuica
 | OpenCuica
 | MuteTriangle
 | OpenTriangle
    deriving (Show,Eq,Ord,Enum)

-- | Convenience constructor for drum sounds
drum :: PercussionSound -> Rational -> Music PercussionSound
drum ps d = Prim (Note d ps)

dottedDrum :: Int -> PercussionSound -> Rational -> Music PercussionSound
dottedDrum n ps d = drum ps (d * dots n)

oneDotDrum :: PercussionSound -> Rational -> Music PercussionSound
oneDotDrum = dottedDrum 1

ed :: PercussionSound -> Music PercussionSound
ed ps = drum ps (1 / 8)

-- | Simple constructor for a quarter-note drum hit.
qd :: PercussionSound -> Music PercussionSound
qd ps = drum ps (1 / 4)

hd :: PercussionSound -> Music PercussionSound
hd ps = drum ps (1 / 2)

wd :: PercussionSound -> Music PercussionSound
wd ps = drum ps 1

-- | Eighth Dotted Drum
edd :: PercussionSound -> Music PercussionSound
edd ps = oneDotDrum ps (1 / 8)

-- | Quarter Dotted Drum
qdd :: PercussionSound -> Music PercussionSound
qdd ps = oneDotDrum ps (1 / 4)

-- | Half Dotted Drum
hdd :: PercussionSound -> Music PercussionSound
hdd ps = oneDotDrum ps (1 / 2)

-- | Whole Dotted Drum
wdd :: PercussionSound -> Music PercussionSound
wdd ps = oneDotDrum ps 1

-- | Get the MIDI offset number for a 'PercussionSound'
toMIDINum :: PercussionSound -> Int
toMIDINum ps = fromEnum ps + 35

-- | Closed Hi Hat
hc :: Music PercussionSound
hc = closedHihat

-- | Open Hi Hat
ho :: Music PercussionSound
ho = openHihat

-- | Pedal Hi Hat
hco :: Music PercussionSound
hco = pedalHihat

-- | Bass Drum
bd :: Music PercussionSound
bd = bassDrum1

-- | Bass Drum (alt)
bd2 :: Music PercussionSound
bd2 = bassDrum2

-- | Snare
sn :: Music PercussionSound
sn = snareDrum1

-- | Snare (alt)
sn2 :: Music PercussionSound
sn2 = snareDrum2

-- | Snare Sidestick
stick :: Music PercussionSound
stick = sideStick

-- | High Tom
t1 :: Music PercussionSound
t1 = highTom1

-- | High Tom (alt)
t2 :: Music PercussionSound
t2 = highTom2

-- | Mid Tom
t3 :: Music PercussionSound
t3 = midTom1

-- | Mid Tom (alt)
t4 :: Music PercussionSound
t4 = midTom2

-- | Low Tom
t5 :: Music PercussionSound
t5 = lowTom1

-- | Low Tom (alt)
t6 :: Music PercussionSound
t6 = lowTom2

-- | Crash Cymbal
cc :: Music PercussionSound
cc = crashCymbal1

-- | Crash Cymbal (alt)
cc2 :: Music PercussionSound
cc2 = crashCymbal2

-- | Ride Cymbal
rc :: Music PercussionSound
rc = rideCymbal1

-- | Ride Cymbal (alt)
rc2 :: Music PercussionSound
rc2 = rideCymbal2

-- | China Cymbal
china :: Music PercussionSound
china = chineseCymbal

-- | Splash Cymbal
splash :: Music PercussionSound
splash = splashCymbal

-- | Bell
bell :: Music PercussionSound
bell = rideBell

-- | Hand Clap
clap :: Music PercussionSound
clap = handClap

-- | Convert a primitive 'PercussionSound' to a 'MidiEvent'
drumToMidiEvent :: Primitive PercussionSound -> MidiEvent
drumToMidiEvent (Rest _   ) = error "rests cannot be converted to midi events"
drumToMidiEvent (Note d ps) = MidiEvent
  (fromIntegral (round d :: Integer))
  (MidiMessage 10 (NoteOn (fromEnum ps + 35) 64))

bassDrum2 :: Music PercussionSound
bassDrum2 = qd BassDrum2

bassDrum1 :: Music PercussionSound
bassDrum1 = qd BassDrum1

sideStick :: Music PercussionSound
sideStick = qd SideStick

snareDrum1 :: Music PercussionSound
snareDrum1 = qd SnareDrum1

handClap :: Music PercussionSound
handClap = qd HandClap

snareDrum2 :: Music PercussionSound
snareDrum2 = qd SnareDrum2

lowTom2 :: Music PercussionSound
lowTom2 = qd LowTom2

closedHihat :: Music PercussionSound
closedHihat = qd ClosedHihat

lowTom1 :: Music PercussionSound
lowTom1 = qd LowTom1

pedalHihat :: Music PercussionSound
pedalHihat = qd PedalHihat

midTom2 :: Music PercussionSound
midTom2 = qd MidTom2

openHihat :: Music PercussionSound
openHihat = qd OpenHihat

midTom1 :: Music PercussionSound
midTom1 = qd MidTom1

highTom2 :: Music PercussionSound
highTom2 = qd HighTom2

crashCymbal1 :: Music PercussionSound
crashCymbal1 = qd CrashCymbal1

highTom1 :: Music PercussionSound
highTom1 = qd HighTom1

rideCymbal1 :: Music PercussionSound
rideCymbal1 = qd RideCymbal1

chineseCymbal :: Music PercussionSound
chineseCymbal = qd ChineseCymbal

rideBell :: Music PercussionSound
rideBell = qd RideBell

tambourine :: Music PercussionSound
tambourine = qd Tambourine

splashCymbal :: Music PercussionSound
splashCymbal = qd SplashCymbal

cowbell :: Music PercussionSound
cowbell = qd Cowbell

crashCymbal2 :: Music PercussionSound
crashCymbal2 = qd CrashCymbal2

vibraSlap :: Music PercussionSound
vibraSlap = qd VibraSlap

rideCymbal2 :: Music PercussionSound
rideCymbal2 = qd RideCymbal2

highBongo :: Music PercussionSound
highBongo = qd HighBongo

lowBongo :: Music PercussionSound
lowBongo = qd LowBongo

muteHighConga :: Music PercussionSound
muteHighConga = qd MuteHighConga

openHighConga :: Music PercussionSound
openHighConga = qd OpenHighConga

lowConga :: Music PercussionSound
lowConga = qd LowConga

highTimbale :: Music PercussionSound
highTimbale = qd HighTimbale

lowTimbale :: Music PercussionSound
lowTimbale = qd LowTimbale

highAgogo :: Music PercussionSound
highAgogo = qd HighAgogo

lowAgogo :: Music PercussionSound
lowAgogo = qd LowAgogo

cabasa :: Music PercussionSound
cabasa = qd Cabasa

maracas :: Music PercussionSound
maracas = qd Maracas

shortWhistle :: Music PercussionSound
shortWhistle = qd ShortWhistle

longWhistle :: Music PercussionSound
longWhistle = qd LongWhistle

shortGuiro :: Music PercussionSound
shortGuiro = qd ShortGuiro

longGuiro :: Music PercussionSound
longGuiro = qd LongGuiro

claves :: Music PercussionSound
claves = qd Claves

highWoodBlock :: Music PercussionSound
highWoodBlock = qd HighWoodBlock

lowWoodBlock :: Music PercussionSound
lowWoodBlock = qd LowWoodBlock

muteCuica :: Music PercussionSound
muteCuica = qd MuteCuica

openCuica :: Music PercussionSound
openCuica = qd OpenCuica

muteTriangle :: Music PercussionSound
muteTriangle = qd MuteTriangle

openTriangle :: Music PercussionSound
openTriangle = qd OpenTriangle
