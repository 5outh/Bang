module Bang.Music.Drum(
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
    deriving (Show, Eq, Ord)

-- |Data type representing tom drums
data TomType = 
    Floor
  | Hang1
  | Hang2
    deriving (Show, Eq, Ord)

-- |Data type representing a drum
data Drum = 
    Snare
  | Bass
  | Tom TomType
  | Cymbal CymbalType
  | HiHat Bool
    deriving (Show, Eq, Ord)