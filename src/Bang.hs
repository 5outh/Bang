{-|
Module      : Bang
Description : A Domain Specific Language for generating drum compositions
Copyright   : (c) Benjamin Kovach, 2014
License     : MIT
Maintainer  : bkovach13@gmail.com
Stability   : experimental
Portability : Mac OSX

The Bang module exports the main functions to actually play a constructed composition. You can use either 'bang' to
play a composition a single time, or 'bangR' to continuously repeat a composition /ad infinitum/.
-}
module Bang(
  bang
, bangR
, bangWith
, bangRWith
, Options(..)
, defaultOptions
, module Bang.Music
, module Bang.Interface
, (<>)
) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Concurrent
import Data.Monoid
import System.MIDI
import System.Info

import Bang.Music
import Bang.Interface
import Bang.Interpreter

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
  -- Add a dummy note at the end 'cause Windows doesn't play the last one for some reason.
  -- This is stupid, but windows is generally unsupported anyway.
  let song' | os == "mingw32" || os == "mingw" = song <> bd 
            | otherwise = song
  start conn
  evalStateT runComposition (conn, interpret (bpm oBpm $ tempo oTempo song'))
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
        lift $ send conn ev
      lift $ threadDelay 250
      runComposition