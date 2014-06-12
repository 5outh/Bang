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

import Bang.Music.Class
import Bang.Interface.Base
import System.MIDI

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
drum :: PercussionSound -> Dur -> Music Dur PercussionSound
drum ps d = Prim (Note d ps)

dottedDrum :: Int -> PercussionSound -> Dur -> Music Dur PercussionSound
dottedDrum n ps d = drum ps (d * dots n)

oneDotDrum :: PercussionSound -> Dur -> Music Dur PercussionSound
oneDotDrum = dottedDrum 1 

ed :: PercussionSound -> Music Dur PercussionSound
ed ps = drum ps (1/8)

-- | Simple constructor for a quarter-note drum hit.
qd :: PercussionSound -> Music Dur PercussionSound
qd ps = drum ps (1/4)

hd :: PercussionSound -> Music Dur PercussionSound
hd ps = drum ps (1/2)

wd :: PercussionSound -> Music Dur PercussionSound
wd ps = drum ps 1

-- | Eighth Dotted Drum
edd ::  PercussionSound -> Music Dur PercussionSound
edd ps = oneDotDrum ps (1/8)

-- | Quarter Dotted Drum
qdd ::  PercussionSound -> Music Dur PercussionSound
qdd ps = oneDotDrum ps (1/4)

-- | Half Dotted Drum
hdd ::  PercussionSound -> Music Dur PercussionSound
hdd ps = oneDotDrum ps (1/2)

-- | Whole Dotted Drum
wdd :: PercussionSound -> Music Dur PercussionSound
wdd ps = oneDotDrum ps 1

-- | Get the MIDI offset number for a 'PercussionSound'
toMIDINum :: PercussionSound -> Int
toMIDINum ps = fromEnum ps + 35

-- | Closed Hi Hat
hc :: Music Dur PercussionSound
hc = closedHihat

-- | Open Hi Hat
ho :: Music Dur PercussionSound
ho = openHihat

-- | Pedal Hi Hat
hco :: Music Dur PercussionSound
hco = pedalHihat

-- | Bass Drum
bd :: Music Dur PercussionSound
bd = bassDrum1

-- | Bass Drum (alt)
bd2 :: Music Dur PercussionSound
bd2 = bassDrum2

-- | Snare
sn :: Music Dur PercussionSound
sn = snareDrum1

-- | Snare (alt)
sn2 :: Music Dur PercussionSound
sn2 = snareDrum2

-- | Snare Sidestick
stick :: Music Dur PercussionSound
stick = sideStick

-- | High Tom
t1 :: Music Dur PercussionSound
t1 = highTom1

-- | High Tom (alt)
t2 :: Music Dur PercussionSound
t2 = highTom2

-- | Mid Tom
t3 :: Music Dur PercussionSound
t3 = midTom1

-- | Mid Tom (alt)
t4 :: Music Dur PercussionSound
t4 = midTom2

-- | Low Tom
t5 :: Music Dur PercussionSound
t5 = lowTom1

-- | Low Tom (alt)
t6 :: Music Dur PercussionSound
t6 = lowTom2

-- | Crash Cymbal
cc :: Music Dur PercussionSound
cc  = crashCymbal1

-- | Crash Cymbal (alt)
cc2 :: Music Dur PercussionSound
cc2 = crashCymbal2

-- | Ride Cymbal
rc :: Music Dur PercussionSound
rc  = rideCymbal1

-- | Ride Cymbal (alt)
rc2 :: Music Dur PercussionSound
rc2 = rideCymbal2

-- | China Cymbal
china :: Music Dur PercussionSound
china  = chineseCymbal

-- | Splash Cymbal
splash :: Music Dur PercussionSound
splash = splashCymbal

-- | Bell
bell :: Music Dur PercussionSound
bell = rideBell

-- | Hand Clap
clap :: Music Dur PercussionSound
clap = handClap

-- | Convert a primitive 'PercussionSound' to a 'MidiEvent'
drumToMidiEvent :: Primitive Dur PercussionSound -> MidiEvent
drumToMidiEvent (Note d ps) = MidiEvent (fromIntegral (round d)) (MidiMessage 10 (NoteOn (fromEnum ps + 35) 64))

bassDrum2 :: Music Dur PercussionSound
bassDrum2 = qd BassDrum2

bassDrum1 :: Music Dur PercussionSound
bassDrum1 = qd BassDrum1

sideStick :: Music Dur PercussionSound
sideStick = qd SideStick

snareDrum1 :: Music Dur PercussionSound
snareDrum1 = qd SnareDrum1

handClap :: Music Dur PercussionSound
handClap = qd HandClap

snareDrum2 :: Music Dur PercussionSound
snareDrum2 = qd SnareDrum2

lowTom2 :: Music Dur PercussionSound
lowTom2 = qd LowTom2

closedHihat :: Music Dur PercussionSound
closedHihat = qd ClosedHihat

lowTom1 :: Music Dur PercussionSound
lowTom1 = qd LowTom1

pedalHihat :: Music Dur PercussionSound
pedalHihat = qd PedalHihat

midTom2 :: Music Dur PercussionSound
midTom2 = qd MidTom2

openHihat :: Music Dur PercussionSound
openHihat = qd OpenHihat

midTom1 :: Music Dur PercussionSound
midTom1 = qd MidTom1

highTom2 :: Music Dur PercussionSound
highTom2 = qd HighTom2

crashCymbal1 :: Music Dur PercussionSound
crashCymbal1 = qd CrashCymbal1

highTom1 :: Music Dur PercussionSound
highTom1 = qd HighTom1

rideCymbal1 :: Music Dur PercussionSound
rideCymbal1 = qd RideCymbal1

chineseCymbal :: Music Dur PercussionSound
chineseCymbal = qd ChineseCymbal

rideBell :: Music Dur PercussionSound
rideBell = qd RideBell

tambourine :: Music Dur PercussionSound
tambourine = qd Tambourine

splashCymbal :: Music Dur PercussionSound
splashCymbal = qd SplashCymbal

cowbell :: Music Dur PercussionSound
cowbell = qd Cowbell

crashCymbal2 :: Music Dur PercussionSound
crashCymbal2 = qd CrashCymbal2

vibraSlap :: Music Dur PercussionSound
vibraSlap = qd VibraSlap

rideCymbal2 :: Music Dur PercussionSound
rideCymbal2 = qd RideCymbal2

highBongo :: Music Dur PercussionSound
highBongo = qd HighBongo

lowBongo :: Music Dur PercussionSound
lowBongo = qd LowBongo

muteHighConga :: Music Dur PercussionSound
muteHighConga = qd MuteHighConga

openHighConga :: Music Dur PercussionSound
openHighConga = qd OpenHighConga

lowConga :: Music Dur PercussionSound
lowConga = qd LowConga

highTimbale :: Music Dur PercussionSound
highTimbale = qd HighTimbale

lowTimbale :: Music Dur PercussionSound
lowTimbale = qd LowTimbale

highAgogo :: Music Dur PercussionSound
highAgogo = qd HighAgogo

lowAgogo :: Music Dur PercussionSound
lowAgogo = qd LowAgogo

cabasa :: Music Dur PercussionSound
cabasa = qd Cabasa

maracas :: Music Dur PercussionSound
maracas = qd Maracas

shortWhistle :: Music Dur PercussionSound
shortWhistle = qd ShortWhistle

longWhistle :: Music Dur PercussionSound
longWhistle = qd LongWhistle

shortGuiro :: Music Dur PercussionSound
shortGuiro = qd ShortGuiro

longGuiro :: Music Dur PercussionSound
longGuiro = qd LongGuiro

claves :: Music Dur PercussionSound
claves = qd Claves

highWoodBlock :: Music Dur PercussionSound
highWoodBlock = qd HighWoodBlock

lowWoodBlock :: Music Dur PercussionSound
lowWoodBlock = qd LowWoodBlock

muteCuica :: Music Dur PercussionSound
muteCuica = qd MuteCuica

openCuica :: Music Dur PercussionSound
openCuica = qd OpenCuica

muteTriangle :: Music Dur PercussionSound
muteTriangle = qd MuteTriangle

openTriangle :: Music Dur PercussionSound
openTriangle = qd OpenTriangle