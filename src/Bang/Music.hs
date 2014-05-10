{-# LANGUAGE DeriveFunctor #-}

module Bang.Music (
  bpm,
  interleave,
  midiEvent,
  nextBeat,
  withDuration,
  scanDurationF,
  mapDuration,
  mapDurationF,
  mergeCompositions,
  module Bang.Music.Class
) where

import Bang.Music.Class
import Bang.Music.MDrum
import Control.Monad.Free
import System.MIDI
import Data.Ratio

-- |Sets the duration for all notes in a `Composition`
withDuration :: Duration -> Composition -> Composition
withDuration d = mapDurationF (const d)

mapDuration :: (Duration -> Duration) -> Music r -> Music ()
mapDuration _ End = End
mapDuration f x = case x of
  (MDrum dr d a) -> MDrum dr (f d) ()
  (Rest d a)     -> Rest (f d) ()

mapDurationF :: (Duration -> Duration) -> Composition -> Composition
mapDurationF _ (Pure r) = return ()
mapDurationF f (Free x) = case x of
  (MDrum dr d a) -> Free (MDrum dr (f d) $ mapDurationF f a)
  (Rest d a)     -> Free (Rest (f d) $ mapDurationF f a)
  End            -> return ()

scanDurationF :: (Duration -> Duration -> Duration) -> Duration -> Composition -> Composition
scanDurationF f acc (Pure r) = Pure ()
scanDurationF f acc a@(Free x) = do
  liftF $ mapDuration (const acc) x
  scanDurationF f (dur (value a) `f` acc) (nextBeat a)

-- @TODO: Handle `Rest`s separately
mergeCompositions :: Composition -> Composition -> Composition
mergeCompositions a' b' = go 0 0 a' b'
  where go :: Duration -> Duration -> Composition -> Composition -> Composition
        go sumA sumB a          (Pure _)   = a >> return ()
        go sumA sumB (Pure _)   b          = b >> return ()
        go sumA sumB a@(Free x) b@(Free y) = do
          let goA = do
                withDuration (sumB - sumA) (singleton a)
                go (sumA + dur (value a)) sumB (nextBeat a) b
              goB = do
                withDuration (sumA - sumB) (singleton b)
                go sumA (sumB + dur (value b)) a (nextBeat b)
              zeroRest r = case r of
                Rest 0 _ -> True
                _        -> False
          case (zeroRest x, zeroRest y) of
            (True, True)  -> go sumA sumB (nextBeat a) (nextBeat b)
            (False, True) -> go sumA sumB a (nextBeat b)
            (True, False) -> go sumA sumB (nextBeat a) b
            (False, False) -> do 
              if all (==0) [sumA, sumB] then do
                -- go with the one with smallest delay, and if they're the same,
                -- use the implicit order
                let da = dur (value a)
                    db = dur (value b)
                if da < db then goA 
                else if da > db then goB
                else -- da == db
                  if value a < value b then goA else goB
              else if sumA <= sumB then goA else goB

value :: Composition -> Music ()
value (Pure _)              = End
value (Free End)            = End
value (Free (MDrum dr d a)) = (MDrum dr d) ()
value (Free (Rest d a))     = (Rest d) ()

singleton :: Composition -> Composition
singleton = liftF . value

-- |Get the next beat in a `Composition`
nextBeat :: Composition -> Composition
nextBeat (Pure r)              = return r
nextBeat (Free (MDrum dr d a)) = a
nextBeat (Free (Rest d a))     = a

-- | NOTE: Still a `Ratio`, so needs to be rounded when the time comes.
-- |Set the beats per minute of a `Composition`
bpm :: Integer -> Composition -> Composition
bpm x song = scanDurationF (+) 0 $ mapDurationF (* (240000 % x)) song

-- |Interleave the beats of two `Composition`s
interleave :: Composition -> Composition -> Composition
interleave (Pure _) (Pure _) = return ()
interleave (Pure r) x = singleton x
interleave x (Pure r) = singleton x
interleave a b = singleton minD >> interleave maxD (nextBeat minD)
  where (minD, maxD) = if dur (value a) <= dur (value b)
                       then (a, b) 
                       else (b, a)

-- |Convenience constructor for a `MidiEvent`
midiEvent :: Delay -> Int -> MidiEvent
midiEvent d instrument = MidiEvent (fromIntegral d) (MidiMessage 10 (NoteOn instrument 64))
