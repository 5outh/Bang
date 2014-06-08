
-- |A lowest common denominator interface to the Win32 and MacOSX MIDI bindings, Win32 part. 

module System.MIDI.Win32 
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

import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Data.List
import Foreign
import Foreign.StablePtr
import System.IO.Unsafe

import System.Win32.Types
import System.Win32.MIDI 

import System.MIDI.Base

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
    b <- isEmptyChan chan
    if b 
      then return Nothing 
      else do
        x <- readChan chan
        return (Just x)

waitFor :: IO Bool -> IO ()
waitFor check = do
  b <- check
  unless b $ waitFor check

-- |The opaque data type representing a MIDI connection.
data Connection = Connection 
  { cn_isInput    :: Bool
  , cn_handle     :: HMIDI
  , cn_time       :: MVar Word32  -- measured in milisecs  
  , cn_fifo_cb    :: Either (Chan MidiEvent) ClientCallback
  , cn_midiproc   :: FunPtr (MIDIINPROC ())
  , cn_mydata     :: StablePtr (MVar Connection)
  , cn_inbuf      :: MVar (Ptr MIDIHDR)
  , cn_sysex      :: Chan Word8   -- channel for temporarily storing sysex messages (they can be arbritrary long, but the buffer has fixed size)
  , cn_alive      :: MVar Bool
  } 

-- |Returns the time elapsed since the last `start` call, in milisecs.
currentTime :: Connection -> IO Word32
currentTime conn = do
  t  <- timeGetTime 
  t0 <- readMVar (cn_time conn)
  return (t-t0)

myMidiCallback :: HMIDIIN -> UINT -> Ptr () -> DWORD -> DWORD -> IO ()
myMidiCallback hmidi msg' myptr param1 param2 = do
  let stabptr = castPtrToStablePtr myptr :: StablePtr (MVar Connection)
  mv <- deRefStablePtr stabptr :: IO (MVar Connection)
  mconn <- tryTakeMVar mv  -- we are also "blocking" (handling) further callbacks this way
  case mconn of 
    Nothing   -> return ()
    Just conn -> do
      let msg = mim msg'
      case msg of
      
        MIM_DATA -> do
          let tmsg  = translateShortMessage $ decodeShortMessage param1
          let event = MidiEvent param2 tmsg
          case (cn_fifo_cb conn) of
            Left  chan -> writeChan chan event 
            Right call -> call event 

        MIM_LONGDATA -> do
          let ptr = wordPtrToPtr (fromIntegral param1) :: Ptr MIDIHDR
          q <- peek (castPtr ptr            ) :: IO (Ptr Word8)
          n <- peek (castPtr ptr `plusPtr` 8) :: IO DWORD
          dat <- peekArray (fromIntegral n) q 
          
          sysexs <- processSysEx (cn_sysex conn) dat
          forM_ sysexs $ \dat -> do
            let event = MidiEvent param2 (SysEx dat)          
            case (cn_fifo_cb conn) of
              Left  chan -> writeChan chan event 
              Right call -> call event 
          
          -- reportedly we are not supposed to call midiInAddBuffer and the like from here, 
          -- but well, this is the simplest solution and it seems to work pretty well...
          -- (may not work on ancient versions of Windows)
          b <- isEmptyMVar (cn_time conn)  -- not really elegant hack, but the emptyness of this is also used for syncing purposes 
          unless b $ do                    -- this is here because midiInReset can block if we want to free them before it returned...
            freeMidiInHeader hmidi ptr    
            new <- midiInAddBuffer hmidi midiInBufferSize  
            swapMVar (cn_inbuf conn) new
            return ()

        MIM_CLOSE -> do
          swapMVar (cn_alive conn) False
          return ()
          
        _ -> return ()
          
      when (msg /= MIM_CLOSE) $ putMVar mv conn      -- do not forget !!!
  
-- I'm not sure, but getChanContents seems to be too lazy for our purposes  
readChanList :: Chan a -> IO [a]
readChanList chan = do
  b <- isEmptyChan chan  
  if b 
    then return []
    else do
      x <- readChan chan
      xs <- readChanList chan
      return (x:xs)

-- the Win32 SysEx support is somewhat brain-dead...
processSysEx :: Chan Word8 -> [Word8] -> IO [[Word8]]
processSysEx _    []  = return []
processSysEx chan dat = do
  case (findIndex (==0xf7) dat) of
    Nothing -> do
      writeList2Chan chan dat
      return []          
    Just k  -> do
      xs <- readChanList chan  
      let (aa,bb) = splitAt k dat
          ys = xs ++ aa
          ev = if (head ys == 0xf0) then tail ys else ys 
      evs <- processSysEx chan (tail bb)
      return (ev:evs)
  
midiInBufferSize = 64

-- |Opens a MIDI source.
-- There are two possibilites to receive MIDI messages. The user can either support a callback function,
-- or get the messages from an asynchronous buffer. However, mixing the two approaches is not allowed.
openSource :: Source -> Maybe ClientCallback -> IO Connection  
openSource src mcallback = do
  myData <- newEmptyMVar :: IO (MVar Connection)
  sp <- newStablePtr myData 
  the_callback <- mkMIDIPROC myMidiCallback
  alive <- newMVar True
  fifo_cb <- case mcallback of
    Just cb -> return $ Right cb
    Nothing -> liftM Left $ newChan 
  time <- newEmptyMVar 
  handle <- midiInOpen src the_callback (castStablePtrToPtr sp) (CALLBACK_FUNCTION False)
  bufmv <- newEmptyMVar 
  sysex <- newChan   -- channel for temporarily storing sysex messages (they can be arbritrary long, but the buffer has fixed size)
  let conn = Connection True handle time fifo_cb the_callback sp bufmv sysex alive 
  putMVar myData conn
  return conn 
  
-- |Opens a MIDI destination.
openDestination :: Destination -> IO Connection  
openDestination dst = do
  alive <- newMVar True
  handle <- midiOutOpen dst nullFunPtr nullPtr CALLBACK_NULL
  time <- newEmptyMVar 
  let conn = Connection False handle time undefined undefined undefined undefined undefined alive
  return conn 

sendShortMessage :: Connection -> ShortMessage -> IO ()
sendShortMessage conn msg = case (cn_isInput conn) of
  True  -> fail "sending short messages to midi sources is not supported"
  False -> midiOutSend (cn_handle conn) msg
 
-- |Sends a short message. The connection must be a `Destination`.
send :: Connection -> MidiMessage -> IO ()
send conn msg = sendShortMessage conn (untranslateShortMessage msg)
  
-- |Sends a System Exclusive message. You shouldn't include the starting/trailing bytes 0xf0 and 0xf7 in the data.
sendSysEx :: Connection -> [Word8] -> IO ()
sendSysEx conn msg = do
  let handle = cn_handle  conn
  case cn_isInput conn of 
    True  -> fail "sending SysEx messages to midi sources is not supported under Win32"
    False -> midiOutSendSysEx handle msg

-- |Starts a connection. This is required for receiving MIDI messages, and also for starting the clock.
start :: Connection -> IO ()
start conn = do
  let handle = cn_handle conn
  b <- isEmptyMVar (cn_time conn)
  if b
    then do
      time <- timeGetTime
      putMVar (cn_time conn) time
      case cn_isInput conn of 
        True  -> do
          buf <- midiInAddBuffer handle midiInBufferSize
          putMVar (cn_inbuf conn) buf
          midiInStart handle
        False -> return ()
    else putStrLn "warning: you shouldn't call start twice"  

-- |Starts a connection. This is required for receiving MIDI messages, and also for starting the clock.
stop :: Connection -> IO ()
stop conn = do 
  let handle = cn_handle conn
  b <- isEmptyMVar (cn_time conn)
  if not b
    then do
      takeMVar (cn_time conn)   -- also used for syncing!
      case cn_isInput conn of 
        True  -> do
          midiInReset handle
          hdr <- takeMVar (cn_inbuf conn)
          freeMidiInHeader handle hdr
          midiInStop  handle
        False -> return ()
      return ()
    else putStrLn "warning: you shouldn't call stop twice"  
  
-- |Resets a `Connection`.
reset :: Connection -> IO ()
reset conn = do
  let handle = cn_handle conn
  case cn_isInput conn of 
    True  -> midiInReset  handle
    False -> midiOutReset handle

-- |Closes a `Connection`
close :: Connection -> IO ()
close conn = do
  let handle = cn_handle conn
  case cn_isInput conn of 
    True  -> midiInClose  handle
    False -> midiOutClose handle

-- called by "close" 
cleanup :: Connection -> IO ()
cleanup conn = case (cn_isInput conn) of
  True  -> do
    waitFor (liftM not $ readMVar $ cn_alive conn)
    freeHaskellFunPtr (cn_midiproc conn)
    freeStablePtr     (cn_mydata   conn)
  False -> return ()
  
