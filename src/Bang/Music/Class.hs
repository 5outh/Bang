module Bang.Music.Class where

import Data.Ratio
import Data.Monoid
import Data.Foldable
import Data.Bifunctor
import Data.Bifoldable

type Dur = Rational

data Primitive dur a = 
    Note dur a
  | Rest dur
    deriving (Show, Eq)

instance Functor (Primitive dur) where
  fmap f (Note d a) = Note d (f a)
  fmap f (Rest d)   = Rest d

data Music dur a = 
    Prim (Primitive dur a)
  | Music dur a :+: Music dur a
  | Music dur a :=: Music dur a
  | Modify Control (Music dur a)
    deriving (Show, Eq)

instance Num dur => Monoid (Music dur a) where
  mappend = (:+:)
  mempty = Prim (Rest 0)

{-
  `fmap` (and `second`) maps over parameterized type (typically a Drum),
  and `first` maps over duration.
-}
instance Functor (Music dur) where
  fmap f (Prim m) = Prim (fmap f m)
  fmap f (a :+: b) = fmap f a :+: fmap f b
  fmap f (a :=: b) = fmap f a :=: fmap f b
  fmap f (Modify c a) = Modify c (fmap f a)

instance Bifunctor Music where
  bimap f g (Prim (Note dur a)) = Prim $ Note (f dur) (g a)
  bimap f g (Prim (Rest dur))   = Prim $ Rest (f dur)
  bimap f g (a :+: b) = bimap f g a :+: bimap f g b
  bimap f g (a :=: b) = bimap f g a :=: bimap f g b
  bimap f g (Modify c a) = Modify c (bimap f g a)

{-
  `foldMap` folds over parameterized type (typically a Drum),
  and `bifoldMap` folds over duration as well.
-}
instance Foldable (Music dur) where
  foldMap f (Prim (Rest _)) = mempty
  foldMap f (Prim (Note _ a)) = f a 
  foldMap f (a :+: b) = foldMap f a `mappend` foldMap f b
  foldMap f (a :=: b) = foldMap f a `mappend` foldMap f b
  foldMap f (Modify c a) = foldMap f a

instance Bifoldable Music where
  bifoldMap f g (Prim (Note dur a)) = f dur `mappend` g a
  bifoldMap f g (Prim (Rest dur)) = f dur
  bifoldMap f g (a :+: b) = bifoldMap f g a `mappend` bifoldMap f g b
  bifoldMap f g (a :=: b) = bifoldMap f g a `mappend` bifoldMap f g b
  bifoldMap f g (Modify c a) = bifoldMap f g a

data Control = 
    BPM Integer               -- set the beats per minute
  | Tempo Rational            -- set the speed for a section of music (default 1)
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
