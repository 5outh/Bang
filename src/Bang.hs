
module Bang(
  play
, runComposition
, bang
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
import System.MIDI

import Bang.Music.Operators
import Bang.Music.Class
import Bang.Music.Transform
import Bang.Interpreter
import Bang.Interface.Base
import Bang.Interface.Drum

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

-- |Play a `Composition` over the first system `Destination` for MIDI events
bang :: Music Dur PercussionSound -> IO ()
bang song = do
  dstlist <- enumerateDestinations
  case dstlist of 
    [] -> fail "No MIDI Devices found."
    (dst:_) -> do
      name    <- getName dst
      putStrLn $ "Using MIDI device: " ++ name
      conn    <- openDestination dst
      play conn song