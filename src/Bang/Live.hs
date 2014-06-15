module Bang.Live where

import Control.Concurrent
import System.IO.Unsafe
import Data.IORef
import Control.Monad(forever)
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
waitTime = unsafePerformIO (newIORef 1000000)

bangLWith :: Options -> Music Dur PercussionSound -> IO ThreadId
bangLWith (Options oBpm oTempo) m = do
  startTime <- round . (*1000000) <$> getPOSIXTime
  print startTime
  -- constantly update waitTime (mod bpm -> ns) (fork?)
  -- forkIO the music and return threadId
  undefined

-- i.e m2 <- m1 `killThen` (bangR $ bd <> hc <> bd <> bd)
-- could also make this more generic, auto-repeating.
killThen :: ThreadId 
          -> IO () {- / Music Dur PercussionSound -} 
          -> IO ThreadId
killThen tid io {- / m -} = do
  killThread tid
  ref <- readIORef waitTime
  threadDelay ref
  r <- forkIO io
  {- / r <- forkIO $ bangR m -}
  return r