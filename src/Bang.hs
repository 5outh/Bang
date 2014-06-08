
module Bang(
  bang
, bangR
, bangWith
, bangRWith
, Options(..)
, defaultOptions
, module Bang.Music.Operators
, module Bang.Music.Class
, module Bang.Music.Transform
, module Bang.Interface.Base
, module Bang.Interface.Drum
) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Concurrent
import Data.Monoid
import System.MIDI

import Bang.Music.Operators
import Bang.Music.Class
import Bang.Music.Transform
import Bang.Interpreter
import Bang.Interface.Base
import Bang.Interface.Drum

data Options = Options {
  o_bpm :: Integer,
  -- ^ BPM of the composition to play
  o_tempo :: Dur
  -- ^ Initial 'Tempo' of the composition to play
  } deriving (Show, Eq)

-- | Default options to 'bang' with.
--
-- > defaultOptions = Options{ o_bpm = 120, o_tempo = 1 }
defaultOptions :: Options
defaultOptions = Options{ o_bpm = 120, o_tempo = 1 }

-- | Play a composition over the first system `Destination` for MIDI events.
--
-- > bang = bangWith defaultOptions
bang :: Music Dur PercussionSound -> IO ()
bang = bangWith defaultOptions

-- | 'bang' a composition repeatedly.
--
-- > bangR = bang . mconcat . repeat
bangR :: Music Dur PercussionSound -> IO ()
bangR = bangRWith defaultOptions

-- | 'bangR' with specified 'Options'.
--
-- > bangRWith opts = bangWith opts . mconcat . repeat
bangRWith :: Options -> Music Dur PercussionSound -> IO ()
bangRWith opts = bangWith opts . mconcat . repeat

-- | 'bang' with specified 'Options'.
bangWith :: Options -> Music Dur PercussionSound -> IO ()
bangWith opts song = do
  dstlist <- enumerateDestinations
  case dstlist of 
    [] -> fail "No MIDI Devices found."
    (dst:_) -> do
      name    <- getName dst
      putStrLn $ "Using MIDI device: " ++ name
      conn    <- openDestination dst
      playWith opts conn song

-- | 'play' with specified 'Options'
playWith :: Options -> Connection -> Music Dur PercussionSound -> IO ()
playWith (Options oBpm oTempo) conn song = do
  start conn
  evalStateT runComposition (conn, interpret (bpm oBpm $ tempo oTempo song))
  close conn

-- | 'play' a composition over a given 'Connection'
play :: Connection -> Music Dur PercussionSound -> IO ()
play conn song = do
  start conn
  evalStateT runComposition (conn, interpret song)
  close conn

-- | Run a composition by repeatedly updating the `Connection` and sending events as they come.
runComposition :: StateT (Connection, [Primitive Dur PercussionSound]) IO ()
runComposition = do
  (conn, evs) <- get
  t <- lift $ currentTime conn
  case evs of
    [] -> return ()
    (e@(Note _ _):xs) -> do
      let x@(MidiEvent s ev) = drumToMidiEvent e
      when (s < t) $ do
        put (conn, xs)
        lift $ print x
        lift $ send conn ev
      lift $ threadDelay 1000
      runComposition
