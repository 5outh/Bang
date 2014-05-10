{-# LANGUAGE DeriveFunctor #-}

module Bang.Music.Class (
  Music(..),
  Composition,
  Delay,
  Duration
) where

import Bang.Music.MDrum
import Control.Monad.Free
import System.MIDI
import Data.Ratio

type Delay = Integer
type Duration = Rational

-- |A data type for `Music` representing `Drum` events, `Rest`s and the end of a song.
data Music a = 
   MDrum {drum :: Drum, dur :: Duration, next :: a}
 | Rest  {dur :: Duration, next :: a}
 | End
    deriving (Show, Eq, Functor)

-- |A data type for a `Composition` consisting of a stream of `Music`s
type Composition = Free Music ()