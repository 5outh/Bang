module Bang.Interface.Drum where

import Bang.Music.Class
import Bang.Interface.Base

drum :: PercussionSound -> Dur -> Music PercussionSound
drum ps d = Prim ( (Note d ps ) )

toMIDINum :: PercussionSound -> Int
toMIDINum ps = fromEnum ps + 35

acousticBassDrum, bassDrum1, sideStick, acousticSnare, 
  closedHiHat, openHiHat, highTom, tambourine, vibraslap, 
    muteHiConga, lowTimbale, maracas, longGuiro, muteCuica, 
      openTriangle, handClap, highFloorTom, lowMidTom, rideCymbal1, 
        splashCymbal, rideCymbal2, openHiConga , highAgogo, shortWhistle, 
          claves, openCuica, electricSnare, pedalHiHat, hiMidTom, chineseCymbal, 
            cowbell, hiBongo, lowConga, lowAgogo, longWhistle, hiWoodBlock, 
              muteTriangle, lowFloorTom , lowTom, crashCymbal1 , rideBell, 
                crashCymbal2 , lowBongo, highTimbale, cabasa, shortGuiro, lowWoodBlock
                :: Music PercussionSound
acousticBassDrum = drum AcousticBassDrum (1/4)
bassDrum1 = drum BassDrum1 (1/4)
sideStick = drum SideStick (1/4)
acousticSnare = drum AcousticSnare (1/4)
closedHiHat = drum ClosedHiHat (1/4)
openHiHat = drum OpenHiHat (1/4)
highTom = drum HighTom (1/4)
tambourine = drum Tambourine (1/4)
vibraslap = drum Vibraslap (1/4)
muteHiConga = drum MuteHiConga (1/4)
lowTimbale = drum LowTimbale (1/4)
maracas = drum Maracas (1/4)
longGuiro = drum LongGuiro (1/4)
muteCuica = drum MuteCuica (1/4)
openTriangle = drum OpenTriangle (1/4)
handClap = drum HandClap (1/4)
highFloorTom = drum HighFloorTom  (1/4)
lowMidTom = drum LowMidTom (1/4)
rideCymbal1 = drum RideCymbal1 (1/4)
splashCymbal = drum SplashCymbal  (1/4)
rideCymbal2 = drum RideCymbal2 (1/4)
openHiConga  = drum OpenHiConga  (1/4)
highAgogo = drum HighAgogo (1/4)
shortWhistle = drum ShortWhistle (1/4)
claves = drum Claves (1/4)
openCuica = drum OpenCuica (1/4)
electricSnare = drum ElectricSnare (1/4)
pedalHiHat = drum PedalHiHat (1/4)
hiMidTom = drum HiMidTom (1/4)
chineseCymbal = drum ChineseCymbal  (1/4)
cowbell = drum Cowbell (1/4)
hiBongo = drum HiBongo (1/4)
lowConga = drum LowConga (1/4)
lowAgogo = drum LowAgogo (1/4)
longWhistle = drum LongWhistle  (1/4)
hiWoodBlock = drum HiWoodBlock  (1/4)
muteTriangle = drum MuteTriangle (1/4)
lowFloorTom  = drum LowFloorTom  (1/4)
lowTom = drum LowTom (1/4)
crashCymbal1  = drum CrashCymbal1  (1/4)
rideBell = drum RideBell (1/4)
crashCymbal2  = drum CrashCymbal2  (1/4)
lowBongo = drum LowBongo (1/4)
highTimbale = drum HighTimbale (1/4)
cabasa = drum Cabasa (1/4)
shortGuiro = drum ShortGuiro (1/4)
lowWoodBlock = drum LowWoodBlock (1/4)