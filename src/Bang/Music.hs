{-# LANGUAGE DeriveFunctor, MultiWayIf #-}

module Bang.Music (
  bpm,
  interleave,
  midiEvent,
  nextBeat,
  withDuration,
  scanDurationF,
  mapDuration,
  mapDurationF,
  singleton,
  value,
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

-- Note: if we have a situation like this:
--        (hc >> (1%3) bd) & (bd >> r)
-- It's ambiguous what the duration of the final note should be.
-- We choose the one with the larger duration. In this case, the (1%3) bd.
mergeCompositions :: Composition -> Composition -> Composition
mergeCompositions a' b' = go 0 0 a' b'
  where go :: Duration -> Duration -> Composition -> Composition -> Composition
        go _    _    (Pure _)   (Pure _)   = return ()
        
        go sumA sumB a          (Pure _)   = a
          --withDuration ((max sumA sumB) - (min sumA sumB)) (singleton a)
          --go (sumA + dur (value a)) sumB (nextBeat a) (Pure ())
        
        go sumA sumB (Pure _)   b          = b
          --withDuration ((max sumA sumB) - (min sumA sumB)) (singleton b)
          --go sumA (sumB + dur (value b)) (Pure ()) (nextBeat b)
        
        go sumA sumB a@(Free x) b@(Free y) = do
          let da = dur (value a)
              db = dur (value b)
              goA = do
                withDuration (sumB - sumA) (singleton a)
                go (sumA + da) sumB (nextBeat a) b
              
              goB = do
                withDuration (sumA - sumB) (singleton b)
                go sumA (sumB + db) a (nextBeat b)
          if | da == 0 -> singleton a >> go sumA sumB (nextBeat a) b
             | db == 0 -> singleton b >> go sumA sumB a (nextBeat b)
             | sumA < sumB       -> goA 
             | sumB < sumA       -> goB
             | da < db           -> goA 
             | da > db           -> goB
             | value a < value b -> goA
             | otherwise         -> goB

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
