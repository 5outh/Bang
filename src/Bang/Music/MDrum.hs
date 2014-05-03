module Bang.Music.MDrum(
  Drum(..),
  TomType(..),
  CymbalType(..)
) where

data CymbalType = 
    Crash
  | Ride
  | Splash
  | China
  | Bell
    deriving (Show, Eq)

data TomType = 
    Floor
  | Hang1
  | Hang2
    deriving (Show, Eq)

data Drum = 
    Snare
  | Bass
  | Tom TomType
  | Cymbal CymbalType
  | HiHat Bool
    deriving (Show, Eq)