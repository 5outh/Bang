{-# LANGUAGE DeriveFunctor, OverlappingInstances, TypeSynonymInstances, FlexibleInstances #-}

module Bang.Music.Class (
  Music(..),
  Composition,
  Delay,
  Duration,
  takeZeros,
  dropZeros,
  equivZeros,
  elems
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

elems :: Composition -> [Music ()]
elems (Pure ())             = []
elems (Free End)            = [End]
elems (Free (MDrum dr d n)) = MDrum dr d () : elems n
elems (Free (Rest d n))     = Rest d ()     : elems n

-- NOTE: THESE ARE VERY SPECIALIZED FUNCTIONS FOR TESTING EQUIVALENCY, 
--       AND PROBABLY DON'T WORK THE WAY YOU THINK. They are REALLY only
--       useful for testing equivalency and not recommended for use outside
--       of this module.
takeZeros :: Composition -> Composition
takeZeros (Free End) = return ()
takeZeros (Pure _)   = return ()
takeZeros (Free (MDrum dr d n))
  | d == 0    = liftF (MDrum dr d ()) >> takeZeros n
  | otherwise = liftF (MDrum dr 0 ())
takeZeros (Free (Rest d n))
  | d == 0    = liftF (Rest d ()) >> takeZeros n
  | otherwise = liftF (Rest 0 ())

dropZeros :: Composition -> Composition
dropZeros (Free End) = return ()
dropZeros (Pure _)   = return ()
dropZeros (Free (MDrum dr d n))
  | d == 0    = dropZeros n
  | otherwise = n
dropZeros (Free (Rest d n))
  | d == 0    = dropZeros n
  | otherwise = n

equivZeros :: Composition -> Composition -> Bool
equivZeros m n = (length elemsM == length elemsN)
          && all (`elem` elemsM) elemsN
  where elemsM = elems $ takeZeros m
        elemsN = elems $ takeZeros n

instance Show Composition where
  show = showComposition

instance Eq Composition where
  (Free End) == (Free End) = True
  (Free End) == _          = False
  (Pure ())  == (Pure ())  = True
  (Pure _)   == _          = False
  a@(Free (Rest 0 n)) == m = n == m
  m == b@(Free (Rest 0 n)) = m == n
  a@(Free (MDrum dr d n)) == b@(Free (MDrum dr' d' m))
    | d == 0 && d' == 0 = equivZeros a b && (dropZeros a == dropZeros b) 
    | otherwise = dr == dr && d == d' && n == m
  (Free (Rest d n)) == (Free (Rest d' m)) = d == d' && n == m  
  a == b = False
