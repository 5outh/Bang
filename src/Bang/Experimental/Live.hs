{-| 
Module      : Bang.Experimental.Live
Description : Experimental module for live coding with Bang
Copyright   : (c) Benjamin Kovach, 2014
License     : MIT
Maintainer  : bkovach13@gmail.com
Stability   : experimental
Portability : Mac OSX

An experimental alternative to the base `Bang` module that allows "live coding," a la Overtone or Tidal, via a ghci session.
Note: Very finnicky, pull requests welcome at https://github.com/5outh/Bang.

The following works in GHCI if you fork off @run@ into a new thread
and @readIORef counter@.
We can use an unsafe global variable like this to hold the duration to wait
until the next Bang composition should be played.
Right now, the aim is to be able to *replace* compositions being played during
runtime, but eventually I think this could be extended to include multiple
compositions being played at runtime, and turned on and off at will
with calls to @killThread@, etc. I want to make this more generic than making
everyone learn about concurrency and stuff, but as a first pass getting it working
with the existing concurrency mechanisms is the goal.
-}
module Bang.Experimental.Live where

import Control.Concurrent
import System.IO.Unsafe
import Data.IORef
import Control.Monad(forever, when)
import Data.Time.Clock.POSIX
import Control.Applicative((<$>))

import Bang

waitTime :: IORef Int
waitTime = unsafePerformIO (newIORef 0)

metronome :: MVar ThreadId
metronome = unsafePerformIO newEmptyMVar

-- |Bang live from a GHCi session. Note that the @ThreadId@ involved must be referenced directly
--   in order to replace or add to the currently running track. 
--
-- Example:
--
-- > track <- bangL bd
bangL :: Music Dur PercussionSound -> IO ThreadId
bangL = bangLWith defaultOptions

-- |Bang live with specified options.
bangLWith :: Options -> Music Dur PercussionSound -> IO ThreadId
bangLWith (Options oBpm oTempo) m = do
  startTime <- round . (*1000000) <$> getPOSIXTime
  let spb     = (60 :: Float) / (fromIntegral oBpm)
      beatLen = round $ spb * 1000000 -- length of a single beat, in ns

  -- constantly update waitTime
  met <- forkIO $ forever $ do
    time <- round . (*1000000) <$> getPOSIXTime
    writeIORef waitTime (beatLen - (time `mod` beatLen))

  -- forkIO the music and return threadId
  forkIO $ bangR m

-- |Kill a thread playing music and replace it with a new composition.
-- 
-- Example:
--  
-- @
--   m1 <- bangL hc 
--   m2 <- m1 `killThen` (bd <> hc <> bd <> bd)
-- @
killThen :: ThreadId
          -> Music Dur PercussionSound
          -> IO ThreadId
killThen tid m = do
  ref <- readIORef waitTime
  killThread tid
  forkIO $ threadDelay ref >> bangR m

-- |Add another track to play concurrently with the currently playing track.
--
-- Example:
--  
-- @
--   m1 <- bangL hc 
--   m2 <- m1 `addTrack` (bd <> hc <> bd <> bd)
-- @
addTrack :: ThreadId
         -> Music Dur PercussionSound
         -> IO ThreadId
addTrack tid m = do
  ref <- readIORef waitTime
  forkIO $ threadDelay ref >> bangR m
