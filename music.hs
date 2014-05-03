{-# LANGUAGE DeriveFunctor #-}
import Control.Monad
import Control.Monad.Free
import Control.Monad.Trans
import qualified Control.Monad.Trans.Free as F
import Control.Monad.Trans.State
import Control.Concurrent

import qualified System.MacOSX.CoreMIDI as OSX
import System.MIDI

data CymbalType = 
    Crash
  | Ride
  | Splash
  | China
  | Bell
    deriving (Show, Eq)

data TomType = 
    Floor
  | Hang1
  | Hang2
    deriving (Show, Eq)

type Delay = Integer

data Drum a = 
    Snare Delay a
  | Bass Delay a
  | Tom TomType Delay a
  | Cymbal CymbalType Delay a
  | HiHat Bool Delay a -- Open?
  | Rest Delay a -- silence!
  | End
    deriving (Show, Eq, Functor)

sn = snare
b  = bass
t1 = tom Hang1
t2 = tom Hang2
tf = tom Floor
cc = cymbal Crash
cr = cymbal Ride
cs = cymbal Splash
ch = cymbal China
bl = cymbal Bell
hc = hiHat False
ho = hiHat True
r  = rest
rt d = liftF (Rest d) ()

type Music r = Free Drum r

delay :: Music r -> Delay
delay (Pure r) = error "Attempt to get delay from pure value."
delay (Free x) = case x of
  (Bass d a) -> d
  (Snare d a) -> d
  (Tom t d a) -> d
  (Cymbal c d a) -> d
  (HiHat open d a) -> d
  (Rest d a) -> d

mapDelayBase :: (Delay -> Delay) -> Drum r -> Drum ()
mapDelayBase _ End = End
mapDelayBase f x = case x of
  (Bass d a) -> Bass (f d) ()
  (Snare d a) -> Snare (f d) ()
  (Tom t d a) -> Tom t (f d) ()
  (Cymbal c d a) -> Cymbal c (f d) ()
  (HiHat open d a) -> HiHat open (f d) ()
  (Rest d a) -> Rest (f d) ()

mapDelay :: (Delay -> Delay) -> Music r -> Music r
mapDelay _ (Pure r) = return r
mapDelay f (Free x) = case x of
  (Bass d a) -> Free (Bass (f d) $ mapDelay f a)
  (Snare d a) -> Free (Snare (f d) $ mapDelay f a)
  (Tom t d a) -> Free (Tom t (f d) $ mapDelay f a)
  (Cymbal c d a) -> Free (Cymbal c (f d) $ mapDelay f a)
  (HiHat open d a) -> Free (HiHat open (f d) $ mapDelay f a)
  (Rest d a) -> Free (Rest (f d) $ mapDelay f a)

next :: Music r -> Music r
next (Pure r)                  = return r
next (Free (Bass _ a))         = a
next (Free (Snare _ a))        = a
next (Free (Tom _ _ a))        = a
next (Free (Cymbal _ _ a))     = a
next (Free (HiHat _ _ a))      = a
next (Free (Rest _ a))         = a

value :: Music r -> Drum ()
value (Pure _)                = End
value (Free (Bass d a))       = (Bass d) ()
value (Free (Snare d a))      = (Snare d) ()
value (Free (Tom t d a))      = (Tom t d) ()
value (Free (Cymbal c d a))   = (Cymbal c d) ()
value (Free (HiHat open d a)) = (HiHat open d) ()
value (Free (Rest d a))       = (Rest d) ()

snare, bass, rest :: Music ()
snare = liftF $ Snare 1 ()
bass  = liftF $ Bass 1 ()
rest  = liftF $ Rest 1 ()

tom :: TomType -> Music ()
tom t = liftF $ (Tom t 1) ()

cymbal :: CymbalType -> Music ()
cymbal c = liftF $ (Cymbal c 1) ()

hiHat :: Bool -> Music ()
hiHat open = liftF $ (HiHat open 1) ()

mapMusicM_ :: Monad m => (Drum () -> m ()) -> Music r -> m () 
mapMusicM_ f (Pure _) = return ()
mapMusicM_ f music    = f (value music) >> mapMusicM_ f (next music)

singleton :: Music r -> Music ()
singleton = liftF . value

interleave :: Music r -> Music r -> Music ()
interleave (Pure r) x = singleton x
interleave x (Pure r) = singleton x
interleave a b = singleton minD >> interleave maxD (next minD)
  where (minD, maxD) = if delay a <= delay b then (a, b) else (b, a)

foldDelay :: (Delay -> Delay -> Delay) -> Delay -> Music r -> Music ()
foldDelay f acc (Pure r) = Pure ()
foldDelay f acc a@(Free x) = ( liftF $ mapDelayBase (+acc) x ) >> foldDelay f (delay a + acc) (next a)
-- foldr f z (x:xs) = f x (foldr f z xs) 

bpm :: Integer -> Music r -> Music ()
bpm x song = foldDelay (+) 0 $ mapDelay (* (60000 `div` x)) song

concurrent :: Music r -> Music r -> Music r
concurrent m n = m >> mapDelay (*0) n

toMidiMessages :: Music () -> [MidiEvent]
toMidiMessages music = go music
  where go m = case m of 
            (Pure r)                -> []
            (Free (Bass d a))       -> playBass d : go a
            (Free (Snare d a))      -> playSnare d : go a
            (Free (Tom t d a))      -> playTom t d : go a
            (Free (Cymbal c d a))   -> playCymbal c d : go a
            (Free (HiHat open d a)) -> playHiHat open d : go a
            (Free (Rest d a))       -> go a

toMidiMessage :: Music r -> MidiEvent
toMidiMessage m = case m of 
            (Pure r)                -> error "Attempt to extract MidiEvent from pure value"
            (Free (Bass d a))       -> playBass d
            (Free (Snare d a))      -> playSnare d
            (Free (Tom t d a))      -> playTom t d
            (Free (Cymbal c d a))   -> playCymbal c d
            (Free (HiHat open d a)) -> playHiHat open d
            (Free (Rest d a))       -> toMidiMessage a -- ***** must handle this separately *****

midiEvent :: Delay -> Int -> MidiEvent
midiEvent d instrument = MidiEvent (fromIntegral d) (MidiMessage 10 (NoteOn instrument 64))

playBass, playSnare :: Delay -> MidiEvent
playBass  d = midiEvent d 35
playSnare d = midiEvent d 38

playTom :: TomType -> Delay -> MidiEvent
playTom t d = midiEvent d $ case t of
  Floor -> 43
  Hang1 -> 47
  Hang2 -> 50

playCymbal :: CymbalType -> Delay -> MidiEvent
playCymbal c d = midiEvent d $ case c of
    Crash  -> 49
    Ride   -> 51
    Splash -> 55
    China  -> 52
    Bell   -> 53

playHiHat :: Bool -> Delay -> MidiEvent
playHiHat open d = midiEvent d $
  if open then 46 else 42

main :: IO ()
main = do
  dstlist <- enumerateDestinations
  case dstlist of 
    [] -> fail "No MIDI Devices found."
    (dst:_) -> do
      name    <- getName dst
      putStrLn $ "Using MIDI device: " ++ name
      conn    <- openDestination dst
      start conn
      evalStateT player (conn, events)
      close conn

(&) = (>>)
infixl 0 %
(%) = concurrent

infixr 0 <&>
m1 <&> m2 = interleave m1 m2 

infixr 0 <>>
x <>> m = bpm x $ forever $ m

infixr 0 ^>
x ^> m = bpm x $ m

crazy = (100 <>> bl & r & b & r) 
        <&> 
        (300 <>> b)

basic = s1 <&> s2
  where s1 = 300 <>> hc
        s2 = 300 <>> b & r & sn & r

events :: [MidiEvent]
events = toMidiMessages basic

player :: StateT (Connection, [MidiEvent]) IO ()
player = do
  (conn, evs) <- get
  t <- lift $ currentTime conn
  case evs of
    [] -> return ()
    (MidiEvent s ev):evs' -> do
      when (s <= t) $ do -- fire event when event time is less than current time
        put (conn, evs')
        case ev of
          SysEx _   -> return ()
          Undefined -> return ()
          _         -> do
            lift $ print (MidiEvent s ev)
            lift $ send conn ev
      lift $ threadDelay 1000
      player
