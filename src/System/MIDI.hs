
--
-- Module      : System.MIDI
-- Version     : 0.1
-- License     : BSD3
-- Author      : Balazs Komuves
-- Maintainer  : bkomuves+hmidi@gmail.com
-- Stability   : experimental
-- Portability : not portable
-- Tested with : GHC 6.8.2
--

-- |A lowest common denominator interface to the Win32 and MacOSX MIDI bindings. 
-- Error handling is via `fail`-s in the IO monad. 

{-# LANGUAGE CPP #-}
module System.MIDI 
  ( module System.MIDI.Base

  , Source
  , Destination
  , Connection
 
  , enumerateSources
  , enumerateDestinations
  
  , getName
  , getModel
  , getManufacturer

  , openSource
  , openDestination
  , close
  , send
  , sendSysEx
  , start
  , stop
  
  , getNextEvent
  , getEvents
  , currentTime
  
  ) where

import Data.Word (Word8,Word32)
import System.MIDI.Base

#ifdef mingw32_HOST_OS
import qualified System.MIDI.Win32 as S
#define HMIDI_SUPPORTED_OS
#endif

#ifdef darwin_HOST_OS
import qualified System.MIDI.MacOSX as S
#define HMIDI_SUPPORTED_OS
#endif

-- this is just to be able to produce a Haddock documentation on a not supported system (eg. Linux)
#ifndef HMIDI_SUPPORTED_OS
import qualified System.MIDI.Placeholder as S
#endif

-- All the definitions in this file are neccessary to be able to have a nice Haddock-generated
-- documentation independently of the platform. Though I still don't know how to generate documentation
-- for a platform-specific module while being on an a different platform (probably not at all possible 
-- at present?) 

-- |The opaque data type representing a MIDI source.
type Source = S.Source

-- |The opaque data type representing a MIDI destination.
type Destination = S.Destination

-- |The opaque data type representing a MIDI connection.
type Connection = S.Connection

-- |Enumerates the MIDI sources present in the system.
enumerateSources :: IO [Source]
enumerateSources = S.enumerateSources

-- |Enumerates the MIDI destinations present in the system.
enumerateDestinations :: IO [Destination]
enumerateDestinations = S.enumerateDestinations

-- |These functions return the name, model and manufacturer of a MIDI source \/ destination.
-- 
-- Note: On Win32, only `getName` returns a somewhat meaningful string at the moment.
getName :: S.MIDIHasName a => a -> IO String
getModel :: S.MIDIHasName a => a -> IO String
getManufacturer :: S.MIDIHasName a => a -> IO String

getName = S.getName
getModel = S.getModel
getManufacturer = S.getManufacturer

-- |Opens a MIDI Source.
-- There are two possibilites to receive MIDI messages. The user can either support a callback function,
-- or get the messages from an asynchronous buffer. However, mixing the two approaches is not allowed.
openSource :: Source -> Maybe ClientCallback -> IO Connection 
openSource = S.openSource

-- |Opens a MIDI Destination.
openDestination :: Destination -> IO Connection 
openDestination = S.openDestination

-- |Gets the next event from a buffered connection (see also `openSource`)
getNextEvent :: Connection -> IO (Maybe MidiEvent)
getNextEvent = S.getNextEvent

-- |Gets all the events from the buffer (see also `openSource`)
getEvents :: Connection -> IO [MidiEvent]
getEvents = S.getEvents
      
-- |Sends a short message. The connection must be a `Destination`.
send :: Connection -> MidiMessage -> IO ()
send = S.send
 
-- |Sends a system exclusive message. You shouldn't include the starting \/ trailing bytes 0xF0 and 0xF7.
-- 
-- Note: On Win32, the connection must be a `Destination`
sendSysEx :: Connection -> [Word8] -> IO ()
sendSysEx = S.sendSysEx
 
-- |Starts a connection. This is required for receiving MIDI messages, and also for starting the clock.
start :: Connection -> IO ()
start = S.start

-- |Stops a connection.
stop :: Connection -> IO ()
stop = S.stop
  
-- |Closes a MIDI Connection.
close :: Connection -> IO ()
close = S.close
 
-- |Returns the time elapsed since the last `start` call, in milisecs.
currentTime :: Connection -> IO Word32
currentTime = S.currentTime
 
