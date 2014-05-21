module Bang.Music.Class where

import Data.Ratio
import Data.Monoid
import Data.Foldable

type Dur = Rational

data Primitive a = 
    Note Dur a
  | Rest Dur
    deriving (Show, Eq)

instance Functor Primitive where
  fmap f (Note d a) = Note d (f a)
  fmap f (Rest d)   = Rest d

data Music a = 
    Prim (Primitive a)
  | Music a :+: Music a
  | Music a :=: Music a
  | Modify Control (Music a)
    deriving (Show, Eq)

instance Monoid (Music a) where
  mappend = (:+:)
  mempty = Prim (Rest 0)

instance Functor Music where
  fmap f (Prim m) = Prim (fmap f m)
  fmap f (a :+: b) = fmap f a :+: fmap f b
  fmap f (a :=: b) = fmap f a :=: fmap f b
  fmap f (Modify c a) = Modify c (fmap f a)

instance Foldable Music where
  foldMap f (Prim (Rest _)) = mempty
  foldMap f (Prim (Note _ a)) = f a 
  foldMap f (a :+: b) = foldMap f a `mappend` foldMap f b
  foldMap f (a :=: b) = foldMap f a `mappend` foldMap f b
  foldMap f (Modify c a) = foldMap f a

data Control = 
    BPM Integer               -- set the beats per minute
  | Tempo Rational            -- set the time signature for a section of music (default 1/4)
  | Instrument InstrumentName -- Change the instrument (currently unused)
    deriving (Show, Eq)

data InstrumentName = DrumSet
  deriving (Show, Eq)

data PercussionSound = 
    AcousticBassDrum
  | BassDrum1
  | SideStick
  | AcousticSnare
  | ClosedHiHat
  | OpenHiHat
  | HighTom
  | Tambourine
  | Vibraslap
  | MuteHiConga
  | LowTimbale
  | Maracas
  | LongGuiro
  | MuteCuica
  | OpenTriangle
  | HandClap
  | HighFloorTom 
  | LowMidTom
  | RideCymbal1
  | SplashCymbal 
  | RideCymbal2
  | OpenHiConga 
  | HighAgogo
  | ShortWhistle
  | Claves
  | OpenCuica
  | ElectricSnare
  | PedalHiHat
  | HiMidTom
  | ChineseCymbal 
  | Cowbell
  | HiBongo
  | LowConga
  | LowAgogo
  | LongWhistle 
  | HiWoodBlock 
  | MuteTriangle
  | LowFloorTom 
  | LowTom
  | CrashCymbal1 
  | RideBell
  | CrashCymbal2 
  | LowBongo
  | HighTimbale
  | Cabasa
  | ShortGuiro
  | LowWoodBlock
    deriving (Show,Eq,Ord,Enum)

type MBang = Music Int
