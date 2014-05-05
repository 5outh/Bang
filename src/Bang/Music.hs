{-# LANGUAGE DeriveFunctor #-}

module Bang.Music (
  bpm,
  concurrent,
  interleave,
  midiEvent,
  nextBeat,
  rest, r, rt,
  speedDiv, speedMult,
  double, quad, oct,
  dbl, qd, sm,
  half, quarter, eighth, sixteenth, thirtysecond,
  sh, sq, se, sts, sd,
  rev, mirror,
  module Bang.Music.Class
) where

import Bang.Music.Class
import Bang.Music.MDrum
import Control.Monad.Free
import System.MIDI

-- |Rest for one beat.
rest :: Composition ()
rest  = liftF $ (Rest 32) ()

-- |Shorthand for `rest`
r = rest

-- |Rest for a specified number of 32nd notes.
rt :: Delay -> Composition ()
rt d = liftF $ (Rest d) ()

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

-- |Speed up by a factor of `x`
speedDiv :: Delay -> Composition r -> Composition ()
speedDiv x = mapDelayF (`div` x)

-- |Slow down by a factor of `x`
speedMult :: Delay -> Composition r -> Composition ()
speedMult x = mapDelayF (*x)

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

-- |Get the next beat in a `Composition`
nextBeat :: Composition r -> Composition r
nextBeat (Pure r)              = return r
nextBeat (Free (MDrum dr d a)) = a
nextBeat (Free (Rest d a))     = a

-- |Set the beats per minute of a `Composition`
bpm :: Integer -> Composition r -> Composition ()
bpm x song = foldDelayF (+) 0 $ mapDelayF (* (1875 `div` x)) song

-- |Run two `Composition`s simultaneously and wait for the longer one to complete.
concurrent :: Composition r -> Composition r -> Composition ()
concurrent (Pure _) (Pure _)     = return ()
concurrent (Free End) (Free End) = return ()
concurrent (Free End) (Pure _)   = return ()
concurrent (Pure _) (Free End)   = return ()
concurrent m (Pure r)            = m >> return ()
concurrent (Pure r) m            = m >> return ()
concurrent m (Free End)          = m >> return ()
concurrent (Free End) m          = m >> return ()
concurrent m@(Free x) n@(Free y) = do
  let d  = delay x
      d' = delay y
  if d' < d then do
    singleton m 
    mapDelayF (*0) (singleton n)
    concurrent (rt (d - d') >> nextBeat m) (nextBeat n)
  else if d < d' then do 
    singleton n 
    mapDelayF (*0) (singleton m)
    concurrent (nextBeat m) (rt (d' - d) >> nextBeat n)
  else do
    singleton m
    mapDelayF (*0) (singleton n)
    concurrent (nextBeat m) (nextBeat n) 

-- |Interleave the beats of two `Composition`s
interleave :: Composition r -> Composition r -> Composition ()
interleave (Pure _) (Pure _) = return ()
interleave (Pure r) x = singleton x
interleave x (Pure r) = singleton x
interleave a b = singleton minD >> interleave maxD (nextBeat minD)
  where (minD, maxD) = if delay (value a) <= delay (value b)
                       then (a, b) 
                       else (b, a)

-- |Reverse a `Composition`
rev :: Composition () -> Composition ()
rev (Pure r)   = return ()
rev (Free End) = return ()
rev f          = rev (nextBeat f) >> singleton f

-- |Play a `Composition` forward, and then in reverse.
mirror :: Composition () -> Composition ()
mirror f = f >> rev f

-- |Convenience constructor for a `MidiEvent`
midiEvent :: Delay -> Int -> MidiEvent
midiEvent d instrument = MidiEvent (fromIntegral d) (MidiMessage 10 (NoteOn instrument 64))
