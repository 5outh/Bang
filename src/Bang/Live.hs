module Bang.Live where

import Control.Concurrent
import System.IO.Unsafe
import Data.IORef
import Control.Monad(forever, when)
import Data.Time.Clock.POSIX
import Control.Applicative((<$>))

import Bang

-- The following works in GHCI if you fork off `run` into a new thread
-- and readIORef counter (it gets updated!)
-- We can use an unsafe global variable like this to hold the duration to wait
-- until the next Bang composition should be played.
-- Right now, the aim is to be able to *replace* compositions being played during
-- runtime, but eventually I think this could be extended to include multiple
-- compositions being played at runtime, and turned on and off at will
-- with calls to killThread, etc. I want to make this more generic than making
-- everyone learn about concurrency and stuff, but as a first pass getting it working
-- with the existing concurrency mechanisms is the goal.

-- "global" variable holding a counter
-- counter :: IORef Int
-- counter = unsafePerformIO (newIORef 0)

-- run = forever $ do
--   modifyIORef' counter (+1)
--   threadDelay 10000

waitTime :: IORef Int
waitTime = unsafePerformIO (newIORef 0)

bangLWith :: Options -> Music Dur PercussionSound -> IO (ThreadId, ThreadId)
bangLWith (Options oBpm oTempo) m = do
  startTime <- round . (*1000000) <$> getPOSIXTime
  let spb     = (60 :: Float) / (fromIntegral oBpm)
      beatLen = round $ spb * 1000000 -- length of a single beat, in ns

  -- constantly update waitTime
  metronome <- forkIO $ forever $ do
    time <- round . (*1000000) <$> getPOSIXTime
    writeIORef waitTime (beatLen - (time `mod` beatLen))

  -- forkIO the music and return both thread IDs
  music <- forkIO $ bangR m

  return (metronome, music)

-- i.e m2 <- m1 `killThen` (bd <> hc <> bd <> bd)
killThen :: ThreadId
          -> Music Dur PercussionSound
          -> IO ThreadId
killThen tid m = do
  ref <- readIORef waitTime
  killThread tid
  forkIO $ threadDelay ref >> bangR m

-- Add another track to play concurrently with the currently playing track.
addTrack :: ThreadId 
         -> Music Dur PercussionSound
         -> IO ThreadId
addTrack tid m = do
  ref <- readIORef waitTime
  forkIO $ threadDelay ref >> bangR m
