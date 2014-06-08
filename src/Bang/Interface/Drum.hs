module Bang.Interface.Drum where

import Bang.Music.Class
import Bang.Interface.Base
import System.MIDI

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
 | HighAgogô
 | LowAgogô
 | Cabasa
 | Maracas
 | ShortWhistle
 | LongWhistle
 | ShortGuiro
 | LongGuiro
 | Claves
 | HighWoodBlock
 | LowWoodBlock
 | MuteCuíca
 | OpenCuíca
 | MuteTriangle
 | OpenTriangle
    deriving (Show,Eq,Ord,Enum)

drum :: PercussionSound -> Dur -> Music Dur PercussionSound
drum ps d = Prim (Note d ps)

qd :: PercussionSound -> Music Dur PercussionSound
qd ps = drum ps (1/4)

toMIDINum :: PercussionSound -> Int
toMIDINum ps = fromEnum ps + 35

bd = bassDrum1
sn = snareDrum1
hc = closedHihat
ho = openHihat
cc = crashCymbal1
t1 = highTom1
t2 = highTom2

drumToMidiEvent :: Primitive Dur PercussionSound -> MidiEvent
drumToMidiEvent (Note d ps) = MidiEvent (fromIntegral (round d)) (MidiMessage 10 (NoteOn (fromEnum ps + 35) 64))

-- ALL THE THINGS
bassDrum2 = qd BassDrum2
bassDrum1 = qd BassDrum1
sideStick = qd SideStick
snareDrum1 = qd SnareDrum1
handClap = qd HandClap
snareDrum2 = qd SnareDrum2
lowTom2 = qd LowTom2
closedHihat = qd ClosedHihat
lowTom1 = qd LowTom1
pedalHihat = qd PedalHihat
midTom2 = qd MidTom2
openHihat = qd OpenHihat
midTom1 = qd MidTom1
highTom2 = qd HighTom2
crashCymbal1 = qd CrashCymbal1
highTom1 = qd HighTom1
rideCymbal1 = qd RideCymbal1
chineseCymbal = qd ChineseCymbal
rideBell = qd RideBell
tambourine = qd Tambourine
splashCymbal = qd SplashCymbal
cowbell = qd Cowbell
crashCymbal2 = qd CrashCymbal2
vibraSlap = qd VibraSlap
rideCymbal2 = qd RideCymbal2
highBongo = qd HighBongo
lowBongo = qd LowBongo
muteHighConga = qd MuteHighConga
openHighConga = qd OpenHighConga
lowConga = qd LowConga
highTimbale = qd HighTimbale
lowTimbale = qd LowTimbale
highAgogo = qd HighAgogô
lowAgogo = qd LowAgogô
cabasa = qd Cabasa
maracas = qd Maracas
shortWhistle = qd ShortWhistle
longWhistle = qd LongWhistle
shortGuiro = qd ShortGuiro
longGuiro = qd LongGuiro
claves = qd Claves
highWoodBlock = qd HighWoodBlock
lowWoodBlock = qd LowWoodBlock
muteCuíca = qd MuteCuíca
openCuíca = qd OpenCuíca
muteTriangle = qd MuteTriangle
openTriangle = qd OpenTriangle