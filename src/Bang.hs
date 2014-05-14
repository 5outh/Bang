{-# LANGUAGE DeriveFunctor, NoMonomorphismRestriction #-}

module Bang(
  play,
  bang,
  runComposition,
  module Bang.Music,
  module Bang.Music.Drum,
  module Bang.Interface.Drum,
  module Bang.Interface.Base
)where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Concurrent

import qualified System.MacOSX.CoreMIDI as OSX
import System.MIDI
import Bang.Music.Class
import Bang.Music
import Bang.Music.Drum
import Bang.Interface.Drum
import Bang.Interface.Base

-- |`play` a `Composition` over a given `Connection`
play :: Connection -> Composition -> IO ()
play conn c = do
  start conn
  evalStateT runComposition (conn, toList c)
  close conn

-- |Run a `Composition` by repeatedly updating the `Connection` and sending events as they come.
runComposition :: StateT (Connection, [Music Duration]) IO ()
runComposition = do
  (conn, evs) <- get
  t <- lift $ currentTime conn
  case evs of
    []   -> return ()
    (x:xs)   -> do
      when (fromIntegral (round (dur x)) < t) $ do
        put (conn, xs)
        case x of
          Rest d -> return ()
          m@(MDrum _ _) -> do
            let MidiEvent s ev = drumToMidiEvent m
            lift $ print x
            lift $ send conn ev
      lift $ threadDelay 1000
      runComposition

-- |Play a `Composition` over the first system `Destination` for MIDI events
bang :: Composition -> IO ()
bang song = do
  dstlist <- enumerateDestinations
  case dstlist of 
    [] -> fail "No MIDI Devices found."
    (dst:_) -> do
      name    <- getName dst
      putStrLn $ "Using MIDI device: " ++ name
      conn    <- openDestination dst
      play conn song
