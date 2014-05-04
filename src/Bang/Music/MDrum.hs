module Bang.Music.MDrum(
  Drum(..),
  TomType(..),
  CymbalType(..)
) where

-- |Data type representing cymbals
data CymbalType = 
    Crash
  | Ride
  | Splash
  | China
  | Bell
    deriving (Show, Eq)

-- |Data type representing tom drums
data TomType = 
    Floor
  | Hang1
  | Hang2
    deriving (Show, Eq)

-- |Data type representing a drum
data Drum = 
    Snare
  | Bass
  | Tom TomType
  | Cymbal CymbalType
  | HiHat Bool
    deriving (Show, Eq)