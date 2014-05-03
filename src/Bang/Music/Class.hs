{-# LANGUAGE DeriveFunctor #-}

module Bang.Music.Class (
  Music(..),
  Composition,
  Delay
) where

import Bang.Music.MDrum
import Control.Monad.Free
import System.MIDI

type Delay = Integer

data Music a = 
    MDrum {drum :: Drum, delay :: Delay, next :: a}
  | Rest  {delay :: Delay, next :: a}
  | End
    deriving (Show, Eq, Functor)

type Composition r = Free Music r

midiEvent :: Delay -> Int -> MidiEvent
midiEvent d instrument = MidiEvent (fromIntegral d) (MidiMessage 10 (NoteOn instrument 64))