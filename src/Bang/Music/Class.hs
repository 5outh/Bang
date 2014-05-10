{-# LANGUAGE DeriveFunctor, OverlappingInstances, TypeSynonymInstances, FlexibleInstances #-}

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
    deriving (Show, Eq, Ord, Functor)

-- |A data type for a `Composition` consisting of a stream of `Music`s
type Composition = Free Music ()

showComposition :: Composition -> String
showComposition (Pure _)   = "Pure ()"
showComposition (Free End) = "Free End"
showComposition (Free x) = case x of
  MDrum d dur n -> (show d  ++ " : " ++ show dur ++ "\n") ++ showComposition n
  Rest    dur n -> ("Rest " ++ " : " ++ show dur ++ "\n") ++ showComposition n

instance Show Composition where
  show = showComposition