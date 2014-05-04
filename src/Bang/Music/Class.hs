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

-- |A data type for `Music` representing `Drum` events, `Rest`s and the end of a song.
data Music a = 
    MDrum {drum :: Drum, delay :: Delay, next :: a}
  | Rest  {delay :: Delay, next :: a}
  | End
    deriving (Show, Eq, Functor)

-- |A data type for a `Composition` consisting of a stream of `Music`s
type Composition r = Free Music r