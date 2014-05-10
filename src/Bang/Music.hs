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
  polyrhythm,
  mergeCompositions,
  rest, r, rt,
  speedDiv, speedMult,
  double, quad, oct,
  dbl, qd, sm,
  half, quarter, eighth, sixteenth, thirtysecond,
  triplets, quintuplets,
  sh, sq, se, sts, sd,
  module Bang.Music.Class
) where

import Bang.Music.Class
import Bang.Music.MDrum
import Control.Monad.Free
import System.MIDI
import Data.Ratio

-- |Rest for one beat.
rest :: Composition
rest  = liftF $ (Rest $ 1 % 4) ()

-- |Shorthand for `rest`
r = rest

-- |Rest for a specified number of 32nd notes.
rt :: Duration -> Composition
rt d = liftF $ (Rest d) ()

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
        go sumA sumB a@(Free x) b@(Free y) =
          if sumA == 0 && sumB == 0 then do
            -- go with the one with smallest delay
            let da = dur (value a)
                db = dur (value b)
            if da <= db then do
              withDuration (sumB - sumA) (singleton a)
              go (sumA + dur (value a)) sumB (nextBeat a) b
            else do
              withDuration (sumA - sumB) (singleton b)
              go sumA (sumB + dur (value b)) a (nextBeat b)
          else if sumA <= sumB then do
            withDuration (sumB - sumA) (singleton a)
            go (sumA + dur (value a)) sumB (nextBeat a) b
          else do
            withDuration (sumA - sumB) (singleton b)
            go sumA (sumB + dur (value b)) a (nextBeat b)

-- |Create a polyrhythm with durations `n` and `m`
polyrhythm :: (Integer, Composition) -> (Integer, Composition) -> Composition
polyrhythm (n, c) (m, c') = (withDuration (1%n) c) `mergeCompositions` (withDuration (1%m) c')

-- |Speed up by a factor of `x`
speedDiv :: Duration -> Composition -> Composition
speedDiv x = mapDurationF (/x)

-- |Slow down by a factor of `x`
speedMult :: Duration -> Composition -> Composition
speedMult x = mapDurationF (*x)

-- |double the speed of a `Composition`
double = speedDiv 2

-- |quadruple the speed of a `Composition`
quad = speedDiv 4

-- |Multiply speed of a composition by 8
oct = speedDiv 8

-- |Shorthand for `double`
dbl = double

-- |Shorthand for `quad`
qd = quad

-- |Shorthand for `speedDiv`
sd = speedDiv

-- |half the speed of a `Composition`
half = speedMult 2

-- |quarter the speed of a `Composition`
quarter = speedMult 4

-- |divide the speed of a `Composition` by 8
eighth = speedMult 8

-- |divide the speed of a `Composition` by 16
sixteenth = speedMult 16

-- |divide the sped of a `Composition` by 32
thirtysecond = speedMult 32

-- |Shorthand for `half`
sh   = half

-- |Shorthand for `quarter`
sq   = quarter

-- |Shorthand for `eighth`
se   = eighth

-- |Shorthand for `sixteenth`
sx   = sixteenth

-- |Shorthand for `thirtysecond`
sts  = thirtysecond

-- |Shorthand for `speedMult`
sm  = speedMult

-- |Turn quarter notes into triplets
triplets = speedMult (4%3)

-- |Turn quarter notes into quintuplets
quintuplets = speedMult (4%5)

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
