{-# LANGUAGE DeriveFunctor, NoMonomorphismRestriction #-}

module Bang where

import Control.Monad
import Control.Monad.Free
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Concurrent

import qualified System.MacOSX.CoreMIDI as OSX
import System.MIDI
import Bang.Music
import Bang.Music.MDrum
import Bang.Interface.MDrum

play :: Connection -> Composition () -> IO ()
play conn c = do
  start conn
  evalStateT runComposition (conn, c)
  close conn

runComposition :: StateT (Connection, Composition ()) IO ()
runComposition = do
  (conn, evs) <- get
  t <- lift $ currentTime conn
  case evs of
    Pure _   -> return ()
    Free End -> return ()
    Free x   -> do
      when (fromIntegral (delay x) < t) $ do
        put (conn, nextBeat evs)
        case x of
          Rest d a -> return ()
          m@(MDrum _ _ _) -> do
            let MidiEvent s ev = drumToMidiEvent m
            lift $ print (MidiEvent s ev)
            lift $ send conn ev
      lift $ threadDelay 1000
      runComposition