
-- |This is just to be able to produce a Haddock documentation on a Linux system

module System.MIDI.Placeholder
  ( module System.MIDI.Base

  , Source
  , Destination
  , Connection
 
  , enumerateSources
  , enumerateDestinations
  
  , MIDIHasName
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

import Data.Word
import System.MIDI.Base

-- |The opaque data type representing a MIDI source.
data Source 

-- |The opaque data type representing a MIDI destination.
data Destination 

-- |The opaque data type representing a MIDI connection.
data Connection 

class MIDIHasName c

-- |Enumerates the MIDI sources present in the system.
enumerateSources :: IO [Source]
enumerateSources = undefined

-- |Enumerates the MIDI destinations present in the system.
enumerateDestinations :: IO [Destination]
enumerateDestinations = undefined

-- |These functions return the name, model and manufacturer of a MIDI source \/ destination.
-- 
-- Note: On Win32, only `getName` returns a somewhat meaningful string at the moment.
getName :: MIDIHasName a => a -> IO String
getModel :: MIDIHasName a => a -> IO String
getManufacturer :: MIDIHasName a => a -> IO String

getName = undefined
getModel = undefined
getManufacturer = undefined

-- |Opens a MIDI Source.
-- There are two possibilites to receive MIDI messages. The user can either support a callback function,
-- or get the messages from an asynchronous buffer. However, mixing the two approaches is not allowed.
openSource :: Source -> Maybe ClientCallback -> IO Connection 
openSource = undefined

-- |Opens a MIDI Destination.
openDestination :: Destination -> IO Connection 
openDestination = undefined

-- |Gets the next event from a buffered connection (see also `openSource`)
getNextEvent :: Connection -> IO (Maybe MidiEvent)
getNextEvent = undefined

-- |Gets all the events from the buffer (see also `openSource`)
getEvents :: Connection -> IO [MidiEvent]
getEvents = undefined
      
-- |Sends a short message. The connection must be a `Destination`.
send :: Connection -> MidiMessage -> IO ()
send = undefined

-- |Sends a system exclusive message. You shouldn't include the starting \/ trailing bytes 0xF0 and 0xF7.
-- 
-- Note: On Win32, the connection must be a `Destination`
sendSysEx :: Connection -> [Word8] -> IO ()
sendSysEx = undefined
 
-- |Starts a connection. This is required for receiving MIDI messages, and also for starting the clock.
start :: Connection -> IO ()
start = undefined

-- |Stops a connection.
stop :: Connection -> IO ()
stop = undefined
  
-- |Closes a MIDI Connection.
close :: Connection -> IO ()
close = undefined
 
-- |Returns the time elapsed since the last `start` call, in milisecs.
currentTime :: Connection -> IO Word32
currentTime = undefined
 
