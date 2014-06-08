
-- |A lowest common denominator interface to the Win32 and MacOSX MIDI bindings, MacOSX part.

module System.MIDI.MacOSX
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

import System.MIDI.Base

import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import Data.List
import Foreign hiding (unsafePerformIO)
import Foreign.StablePtr
import System.IO.Unsafe

import System.MacOSX.CoreFoundation
import System.MacOSX.CoreAudio
import System.MacOSX.CoreMIDI

-- |Gets all the events from the buffer.
getEvents :: Connection -> IO [MidiEvent]
getEvents conn = do
  m <- getNextEvent conn
  case m of
    Nothing -> return []
    Just ev -> do
      evs <- getEvents conn
      return (ev:evs)

-- |Gets the next event from a buffered connection.
getNextEvent :: Connection -> IO (Maybe MidiEvent)
getNextEvent conn = case cn_fifo_cb conn of
  Right _   -> fail "this is not a buffered connection"
  Left chan -> do
    b <- atomically $ isEmptyTChan chan
    if b
      then return Nothing
      else do
        x <- atomically $ readTChan chan
        return (Just x)

type Client      = MIDIClientRef
type Device      = MIDIDeviceRef
type Port        = MIDIPortRef

-- |The opaque data type representing a MIDI connection
data Connection = Connection
  { cn_isInput     :: Bool
  , cn_port        :: MIDIPortRef
  , cn_endpoint    :: MIDIEndpointRef
  , cn_time        :: MVar UInt64     -- measured in nanosecs
  , cn_alive       :: MVar Bool
  , cn_fifo_cb     :: Either (TChan MidiEvent) ClientCallback
  , cn_midiproc    :: FunPtr (MIDIReadProc () ())
  , cn_mydata      :: StablePtr (MVar Connection)
  }

----- automatic client creation

client = unsafePerformIO $ newEmptyMVar :: MVar Client

{-
#ifdef __GLASGOW_HASKELL__
clientFinalizer :: IO ()
clientFinalizer = do
  c <- readMVar client
  disposeClient c
#endif
-}

getClient :: IO MIDIClientRef
getClient = do
  b <- isEmptyMVar client
  if b
    then do
      x <- newClient "HaskellMidi"
      putMVar client x
{-
#ifdef __GLASGOW_HASKELL__
      addMVarFinalizer client clientFinalizer      -- uh-oh, that's not a good idea (not in the present form)
#endif
-}
      return x
    else readMVar client

-- |Returns the time elapsed since the last `start` call, in milisecs.
currentTime :: Connection -> IO Word32
currentTime conn = do
  t  <- audioGetCurrentTimeInNanos
  t0 <- readMVar (cn_time conn)
  return (nanoToMili $ t-t0)

nanoToMili :: UInt64 -> Word32
nanoToMili n = fromIntegral $ div n 1000000

convertShortMessage :: UInt64 -> (MIDITimeStamp,[Word8]) -> IO MidiEvent
convertShortMessage t0 (ts',bytes) = do
  ts <- audioConvertHostTimeToNanos ts'
  return $ MidiEvent (nanoToMili $ ts-t0) (translateShortMessage $ decodeShortMessage bytes)

myMIDIReadProc :: Ptr MIDIPacket -> Ptr () -> Ptr () -> IO ()
myMIDIReadProc packets myptr _  = do
  let stabptr = castPtrToStablePtr myptr :: StablePtr (MVar Connection)
  mv <- deRefStablePtr stabptr :: IO (MVar Connection)
  mconn <- tryTakeMVar mv  -- we are also "blocking" (handling) further callbacks this way
  case mconn of
    Nothing   -> return ()
    Just conn -> do
      time0 <- readMVar (cn_time conn)
      list1 <- depackMIDIPacketList packets
      let (normal,sysex') = partition (\(_,bytes) -> isShortMessage bytes) list1
      sysexs <- forM sysex' $ \(ts',bytes) -> do
        ts <- audioConvertHostTimeToNanos ts'
        return $ MidiEvent (nanoToMili $ ts-time0) (SysEx $ tail bytes)
      normals <- mapM (convertShortMessage time0) normal
      let events = sysexs ++ normals
      case (cn_fifo_cb conn) of
        Left  chan -> atomically $ mapM_ (writeTChan chan) events
        Right call -> mapM_ call events
      putMVar mv conn      -- do not forget to put it back!

-- |Opens a MIDI Source.
-- There are two possibilites to receive MIDI messages. The user can either support a callback function,
-- or get the messages from an asynchronous buffer. However, mixing the two approaches is not allowed.
openSource :: Source -> Maybe ClientCallback -> IO Connection
openSource src@(Source endpoint) mcallback = do

  client <- getClient

  myData <- newEmptyMVar :: IO (MVar Connection)
  sp <- newStablePtr myData
  the_callback <- mkMIDIReadProc myMIDIReadProc

  time  <- newEmptyMVar
  alive <- newMVar True

  fifo_cb <- case mcallback of
    Just cb -> return $ Right cb
    Nothing -> liftM Left $ atomically newTChan

  inport <- newInputPort client "Input Port" the_callback (castStablePtrToPtr sp)

  let conn = Connection True inport endpoint time alive fifo_cb the_callback sp
  putMVar myData conn
  return conn

-- |Opens a MIDI Destination.
openDestination :: Destination -> IO Connection
openDestination dst@(Destination endpoint) = do

  client <- getClient
  outport <- newOutputPort client "Output Port"
  alive <- newMVar True
  time  <- newEmptyMVar

  let conn = Connection False outport endpoint time alive undefined undefined undefined
  return conn

sendShortMessage :: Connection -> ShortMessage -> IO ()
sendShortMessage conn msg = case cn_isInput conn of
  True  -> fail "sending short messages to midi sources is not supported"
  False -> midiSend (cn_port conn) (Destination $ cn_endpoint conn) msg

-- |Sends a short message. The connection must be a `Destination`.
send :: Connection -> MidiMessage -> IO ()
send conn msg = sendShortMessage conn (untranslateShortMessage msg)

-- |Sends a system exclusive message. You shouldn't include the starting \/ trailing bytes 0xF0 and 0xF7.
sendSysEx :: Connection -> [Word8] -> IO ()
sendSysEx conn dat = midiSendSysEx (cn_endpoint conn) dat

-- |Starts a connection. This is required for receiving MIDI messages, and also for starting the clock.
start :: Connection -> IO ()
start conn = do
  b <- isEmptyMVar (cn_time conn)
  if b
    then do
      hosttime <- audioGetCurrentTimeInNanos
      putMVar (cn_time conn) hosttime
      case cn_isInput conn of
        True  -> connectToSource (cn_port conn) (Source $ cn_endpoint conn) nullPtr
        False -> return ()
    else putStrLn "warning: you shouldn't call start twice"

-- |Stops a connection.
stop :: Connection -> IO ()
stop conn = do
  b <- isEmptyMVar (cn_time conn)
  if not b
    then do
      takeMVar (cn_time conn)
      case cn_isInput conn of
        True  -> disconnectFromSource (cn_port conn) (Source $ cn_endpoint conn)
        False -> return ()
    else putStrLn "warning: you shouldn't call stop twice"

-- |Closes a MIDI Connection
close conn = do
  when (cn_isInput conn) $ do
    b <- isEmptyMVar (cn_time conn)
    when (not b) (stop conn)
  disposePort (cn_port conn)
  cleanup conn

-- called by "close"; not exposed.
cleanup :: Connection -> IO ()
cleanup conn = case (cn_isInput conn) of
  True  -> do
    freeHaskellFunPtr (cn_midiproc conn)
    freeStablePtr     (cn_mydata   conn)
  False -> return ()
