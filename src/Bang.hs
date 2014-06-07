
module Bang(
  play
, runComposition
, bang
, bangR
, module Bang.Music.Operators
, module Bang.Music.Class
, module Bang.Music.Transform
, module Bang.Interface.Base
, module Bang.Interface.Drum
, module Bang.Interpreter
)where

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
  o_tempo :: Dur
} deriving (Show, Eq)

defaultOptions :: Options
defaultOptions = Options 120 1

-- |Play a `Composition` over the first system `Destination` for MIDI events
bang :: Music Dur PercussionSound -> IO ()
bang = bangWith defaultOptions

bangR :: Music Dur PercussionSound -> IO ()
bangR = bang . mconcat . repeat

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

playWith :: Options -> Connection -> Music Dur PercussionSound -> IO ()
playWith (Options oBpm oTempo) conn song = do
  start conn
  evalStateT runComposition (conn, interpret (bpm oBpm $ tempo oTempo song))
  close conn

-- |`play` a `Composition` over a given `Connection`
play :: Connection -> Music Dur PercussionSound -> IO ()
play conn song = do
  start conn
  evalStateT runComposition (conn, interpret song)
  close conn

-- |Run a `Composition` by repeatedly updating the `Connection` and sending events as they come.
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
