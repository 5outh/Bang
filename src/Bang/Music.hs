{-# LANGUAGE DeriveFunctor #-}

module Bang.Music (
  bpm,
  concurrent,
  interleave,
  midiEvent,
  nextBeat,
  rest, r, rt,
  half, quarter, sixteenth, thirtysecond,
  sh, sq, se, sts, sp,
  module Bang.Music.Class
) where

import Bang.Music.Class
import Bang.Music.MDrum
import Control.Monad.Free
import System.MIDI

rest :: Composition ()
rest  = liftF $ (Rest 32) ()
r  = rest
rt d = liftF (Rest d) ()

mapDelay :: (Delay -> Delay) -> Music r -> Music ()
mapDelay _ End = End
mapDelay f x = case x of
  (MDrum dr d a) -> MDrum dr (f d) ()
  (Rest d a)     -> Rest (f d) ()

mapDelayF :: (Delay -> Delay) -> Composition r -> Composition ()
mapDelayF _ (Pure r) = return ()
mapDelayF f (Free x) = case x of
  (MDrum dr d a) -> Free (MDrum dr (f d) $ mapDelayF f a)
  (Rest d a)     -> Free (Rest (f d) $ mapDelayF f a)
  End            -> return ()

speedDiv :: Delay -> Composition r -> Composition ()
speedDiv x = mapDelayF (`div` x)

half      = speedDiv 2
quarter   = speedDiv 4
eighth    = speedDiv 8
sixteenth = speedDiv 16
thirtysecond = speedDiv 32

sh  = half
sq  = quarter
se  = eighth
sx  = sixteenth
sts = thirtysecond
sp x = speedDiv x

foldDelayF :: (Delay -> Delay -> Delay) -> Delay -> Composition r -> Composition ()
foldDelayF f acc (Pure r) = Pure ()
foldDelayF f acc a@(Free x) = do
  liftF $ mapDelay (+acc) x
  foldDelayF f (delay (value a) + acc) (nextBeat a)

value :: Composition r -> Music ()
value (Pure _)              = End
value (Free End)            = End
value (Free (MDrum dr d a)) = (MDrum dr d) ()
value (Free (Rest d a))     = (Rest d) ()

singleton :: Composition r -> Composition ()
singleton = liftF . value

nextBeat :: Composition r -> Composition r
nextBeat (Pure r)              = return r
nextBeat (Free (MDrum dr d a)) = a
nextBeat (Free (Rest d a))     = a

-- NB. 1875 ~ 60000 / 32
bpm :: Integer -> Composition r -> Composition ()
bpm x song = foldDelayF (+) 0 $ mapDelayF (* (1875 `div` x)) song

concurrent :: Composition r -> Composition r -> Composition ()
concurrent m n = m >> mapDelayF (*0) n

interleave :: Composition r -> Composition r -> Composition ()
interleave (Pure r) x = singleton x
interleave x (Pure r) = singleton x
interleave a b = singleton minD >> interleave maxD (nextBeat minD)
  where (minD, maxD) = if delay (value a) <= delay (value b)
                       then (a, b) 
                       else (b, a)

midiEvent :: Delay -> Int -> MidiEvent
midiEvent d instrument = MidiEvent (fromIntegral d) (MidiMessage 10 (NoteOn instrument 64))
