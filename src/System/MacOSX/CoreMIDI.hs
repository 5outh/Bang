
-- |Low-level binding to the CoreMIDI services present in Mac OS X.
-- Error handling is via `fail`-s in the IO monad.

{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module System.MacOSX.CoreMIDI
  (
    enumerateDevices
  , enumerateSources
  , enumerateDestinations
  , MIDIHasName
  , getName
  , getModel
  , getManufacturer
  , newSource
  , newDestination
  , disposeEndpoint
  , newClient
  , disposeClient
  , newInputPort
  , newOutputPort
  , disposePort
  , connectToSource
  , disconnectFromSource
  , midiSend
  , midiSend'
  , midiSendList
  , midiSendList'
  , midiSendSysEx

  -- types
  , OpaqueMIDIClient
  , OpaqueMIDIObject
  , OpaqueMIDIDevice
  , OpaqueMIDIEntity
  , OpaqueMIDIEndpoint
  , OpaqueMIDIPort

  , MIDIClientRef
  , MIDIObjectRef
  , MIDIDeviceRef
  , MIDIEntityRef
  , MIDIEndpointRef
  , MIDIPortRef

  , MIDITimeStamp
  , MIDIReadProc
  , mkMIDIReadProc
  , MIDIPacket
  , Source(..)
  , Destination(..)

  -- helper functions to write callbacks
  , depackMIDIPacketList
  , depackSingleMIDIPacket
  , decodeShortMessage
  , isShortMessage
  ) where

import Control.Monad
import Control.Concurrent.MVar
import Foreign hiding (unsafePerformIO)
import Foreign.Marshal
import System.IO.Unsafe

import System.MIDI.Base
import System.MacOSX.CoreFoundation

data OpaqueMIDIClient
data OpaqueMIDIObject
data OpaqueMIDIDevice
data OpaqueMIDIEntity
data OpaqueMIDIEndpoint
data OpaqueMIDIPort

type MIDIClientRef    = Ptr OpaqueMIDIClient
type MIDIObjectRef    = Ptr OpaqueMIDIObject
type MIDIDeviceRef    = Ptr OpaqueMIDIDevice
type MIDIEntityRef    = Ptr OpaqueMIDIEntity
type MIDIEndpointRef  = Ptr OpaqueMIDIEndpoint
type MIDIPortRef      = Ptr OpaqueMIDIPort

type MIDIUniqueID     = SInt32
type MIDIObjectType   = SInt32

type MIDITimeStamp    = UInt64

data MIDINotification
type MIDINotifyProc a = Ptr MIDINotification -> Ptr a -> IO ()

data MIDIPacket
data MIDISysexSendRequest

-- | 'r' is readProcRefCon (The refCon you passed to MIDIInputPortCreate or MIDIDestinationCreate);
-- 's' is srcConnRefCon (A refCon you passed to MIDIPortConnectSource, which identifies the source of the data).
type MIDIReadProc r s = Ptr MIDIPacket -> Ptr r -> Ptr s -> IO ()

foreign import ccall safe "wrapper"
  mkMIDIReadProc :: MIDIReadProc () () -> IO (FunPtr (MIDIReadProc () ()))

----- Properties -----

foreign import ccall "&kMIDIPropertyName"          ptr_kMIDIPropertyName          :: Ptr CFStringRef
foreign import ccall "&kMIDIPropertyManufacturer"  ptr_kMIDIPropertyManufacturer  :: Ptr CFStringRef
foreign import ccall "&kMIDIPropertyModel"         ptr_kMIDIPropertyModel         :: Ptr CFStringRef

kMIDIPropertyName         = unsafePerformIO $ peek ptr_kMIDIPropertyName
kMIDIPropertyManufacturer = unsafePerformIO $ peek ptr_kMIDIPropertyManufacturer
kMIDIPropertyModel        = unsafePerformIO $ peek ptr_kMIDIPropertyModel

----- Send

foreign import ccall unsafe "MIDIServices.h MIDISend"
  c_MIDISend :: MIDIPortRef -> MIDIEndpointRef -> Ptr MIDIPacket -> IO OSStatus

foreign import ccall unsafe "MIDIServices.h MIDISendSysex"
  c_MIDISendSysex :: Ptr MIDISysexSendRequest -> IO OSStatus

----- Clients

foreign import ccall unsafe "MIDIServices.h MIDIClientCreate"
  c_MIDIClientCreate :: CFStringRef -> FunPtr (MIDINotifyProc a) -> Ptr a -> Ptr MIDIClientRef -> IO OSStatus

foreign import ccall unsafe "MIDIServices.h MIDIClientDispose"
  c_MIDIClientDispose   :: MIDIClientRef -> IO OSStatus

----- Devices

foreign import ccall unsafe "MIDIServices.h MIDIGetNumberOfDevices"
  c_MIDIGetNumberOfDevices      :: IO ItemCount

foreign import ccall unsafe "MIDIServices.h MIDIGetDevice"
  c_MIDIGetDevice      :: ItemCount -> IO MIDIDeviceRef

----- Endpoints

foreign import ccall unsafe "MIDIServices.h MIDIGetNumberOfSources"
  c_MIDIGetNumberOfSources      :: IO ItemCount

foreign import ccall unsafe "MIDIServices.h MIDIGetNumberOfDestinations"
  c_MIDIGetNumberOfDestinations :: IO ItemCount


foreign import ccall unsafe "MIDIServices.h MIDIGetSource"
  c_MIDIGetSource      :: ItemCount -> IO MIDIEndpointRef

foreign import ccall unsafe "MIDIServices.h MIDIGetDestination"
  c_MIDIGetDestination :: ItemCount -> IO MIDIEndpointRef


foreign import ccall unsafe "MIDIServices.h MIDISourceCreate"
  c_MIDISourceCreate      :: MIDIClientRef -> CFStringRef -> Ptr MIDIEndpointRef -> IO OSStatus

foreign import ccall unsafe "MIDIServices.h MIDIDestinationCreate"
  c_MIDIDestinationCreate :: MIDIClientRef -> CFStringRef -> Ptr MIDIEndpointRef -> IO OSStatus

foreign import ccall unsafe "MIDIServices.h MIDIEndpointDispose"
  c_MIDIEndpointDispose   :: MIDIEndpointRef -> IO OSStatus


foreign import ccall unsafe "MIDIServices.h MIDIEndpointGetEntity"
  c_MIDIEndpointGetEntity :: MIDIEndpointRef -> Ptr MIDIEntityRef -> IO OSStatus

------ Ports

foreign import ccall safe "MIDIServices.h MIDIInputPortCreate"
  c_MIDIInputPortCreate  :: MIDIClientRef -> CFStringRef -> FunPtr (MIDIReadProc r s) -> Ptr r
                            -> Ptr MIDIPortRef -> IO OSStatus

foreign import ccall safe "MIDIServices.h MIDIOutputPortCreate"
  c_MIDIOutputPortCreate  :: MIDIClientRef -> CFStringRef -> Ptr MIDIPortRef -> IO OSStatus

foreign import ccall unsafe "MIDIServices.h MIDIPortDispose"
  c_MIDIPortDispose :: MIDIPortRef -> IO OSStatus

foreign import ccall safe "MIDIServices.h MIDIPortConnectSource"
  c_MIDIPortConnectSource  :: MIDIPortRef -> MIDIEndpointRef -> Ptr a -> IO OSStatus

foreign import ccall safe "MIDIServices.h MIDIPortDisconnectSource"
  c_MIDIPortDisconnectSource  :: MIDIPortRef -> MIDIEndpointRef -> IO OSStatus

------ Objects

foreign import ccall unsafe "MIDIServices.h MIDIObjectFindByUniqueID"
  c_MIDIObjectFindByUniqueID :: MIDIUniqueID -> Ptr MIDIObjectRef -> Ptr MIDIObjectType -> IO OSStatus


foreign import ccall unsafe "MIDIServices.h MIDIObjectGetDataProperty"
  c_MIDIObjectGetDataProperty    :: MIDIObjectRef -> CFStringRef -> Ptr CFDataRef -> IO OSStatus

foreign import ccall unsafe "MIDIServices.h MIDIObjectGetIntegerProperty"
  c_MIDIObjectGetIntegerProperty :: MIDIObjectRef -> CFStringRef -> Ptr SInt32 -> IO OSStatus

foreign import ccall unsafe "MIDIServices.h MIDIObjectGetStringProperty"
  c_MIDIObjectGetStringProperty  :: MIDIObjectRef -> CFStringRef -> Ptr CFStringRef -> IO OSStatus


foreign import ccall unsafe "MIDIServices.h MIDIObjectSetDataProperty"
  c_MIDIObjectSetDataProperty    :: MIDIObjectRef -> CFStringRef -> CFDataRef -> IO OSStatus

foreign import ccall unsafe "MIDIServices.h MIDIObjectSetIntegerProperty"
  c_MIDIObjectSetIntegerProperty :: MIDIObjectRef -> CFStringRef -> SInt32 -> IO OSStatus

foreign import ccall unsafe "MIDIServices.h MIDIObjectSetStringProperty"
  c_MIDIObjectSetStringProperty  :: MIDIObjectRef -> CFStringRef -> CFStringRef -> IO OSStatus


midiObjectGetStringProperty :: MIDIObjectRef -> CFStringRef -> IO String
midiObjectGetStringProperty object propertyid =
  alloca $ \ptr_cfstringref -> do
    osstatus <- c_MIDIObjectGetStringProperty object propertyid ptr_cfstringref
    if osstatus /= 0
      then osStatusError osstatus
      else do
        cfstringref <- peek ptr_cfstringref
        string <- peekCFString cfstringref
        releaseCFString cfstringref
        return string

midiObjectGetIntegerProperty :: MIDIObjectRef -> CFStringRef -> IO SInt32
midiObjectGetIntegerProperty object propertyid =
  alloca $ \ptr_sint32 -> do
    osstatus <- c_MIDIObjectGetIntegerProperty object propertyid ptr_sint32
    if osstatus /= 0
      then osStatusError osstatus
      else do
        sint32 <- peek ptr_sint32
        return sint32


---------- exported Haskell functions ----------

newtype Source      = Source      MIDIEndpointRef deriving (Eq,Show)
newtype Destination = Destination MIDIEndpointRef deriving (Eq,Show)

class Endpoint a where endpoint :: a -> MIDIEndpointRef

instance Endpoint Source          where endpoint (Source      src) = src
instance Endpoint Destination     where endpoint (Destination src) = src
instance Endpoint MIDIEndpointRef where endpoint = id

class MIDIObject a where midiObject :: a -> MIDIObjectRef

instance MIDIObject MIDIClientRef    where midiObject = castPtr
instance MIDIObject MIDIDeviceRef    where midiObject = castPtr
instance MIDIObject MIDIPortRef      where midiObject = castPtr
instance MIDIObject MIDIEndpointRef  where midiObject = castPtr
instance MIDIObject MIDIEntityRef    where midiObject = castPtr

instance MIDIObject Source      where midiObject (Source src) = castPtr src
instance MIDIObject Destination where midiObject (Destination dst) = castPtr dst

-- |MIDI objects which can have a name, model name and manufacturer
class MIDIObject a => MIDIHasName a where
  getName         :: a -> IO String
  getModel        :: a -> IO String
  getManufacturer :: a -> IO String

  getName  = genericGetName  . midiObject
  getModel = genericGetModel . midiObject
  getManufacturer = genericGetManufacturer . midiObject

instance MIDIHasName MIDIDeviceRef
instance MIDIHasName MIDIEntityRef
instance MIDIHasName MIDIPortRef
instance MIDIHasName MIDIEndpointRef
instance MIDIHasName Source
instance MIDIHasName Destination

genericGetName obj         = midiObjectGetStringProperty obj kMIDIPropertyName
genericGetModel obj        = midiObjectGetStringProperty obj kMIDIPropertyModel
genericGetManufacturer obj = midiObjectGetStringProperty obj kMIDIPropertyManufacturer

data Notification = Notification NotificationMessageID (Maybe [Word8])

data NotificationMessageID
  = SetupChanged
  | ObjectAdded
  | ObjectRemoved
  | PropertyChanged
  | ThruConnectionsChanged
  | SerialPortOwnerChanged
  | MIDIMsgIOError

----- encode / decode

encodeShortMessageList :: [ShortMessage] -> [Word8]
encodeShortMessageList list = concatMap encodeShortMessage list

encodeShortMessage :: ShortMessage -> [Word8]
encodeShortMessage (ShortMessage chn' msg' bt1 bt2) =
  case msg of
    8  -> [cmd,bt1,bt2]   -- ?!
    9  -> [cmd,bt1,bt2]
    10 -> [cmd,bt1,bt2]
    11 -> [cmd,bt1,bt2]
    12 -> [cmd,bt1]
    13 -> [cmd,bt1]
    14 -> [cmd,bt1,bt2]
    15 -> case chn of
      2 -> [cmd,bt1,bt2]
      3 -> [cmd,bt1]
      0 -> error "SysEx is not a short message!"
      _ -> [cmd]
  where
    chn = 15 .&. chn'
    msg = 15 .&. msg'
    cmd = chn + shiftL msg 4

isShortMessage :: [Word8] -> Bool
isShortMessage msg = (head msg /= 0xf0)

decodeShortMessage :: [Word8] -> ShortMessage
decodeShortMessage bytes = ShortMessage chn msg bt1 bt2 where
  cmd = head bytes
  chn = cmd .&. 15
  msg = shiftR cmd 4
  (bt1,bt2) = case tail bytes of
    []    -> (0,0)
    [a]   -> (a,0)
    [a,b] -> (a,b)
    _     -> error "a short message shouldn't be longer than 3 bytes!"

depackMIDIPacketList :: Ptr MIDIPacket -> IO [ (MIDITimeStamp, [Word8]) ]
depackMIDIPacketList p =
  do
    npackets <- peek (castPtr p) :: IO UInt32
    depack' (p `plusPtr` 4) npackets
  where
    depack' _ 0 = return []
    depack' p k = do
      ( n , ts , msgs ) <- depackSingleMIDIPacket p
      let xs = zip (repeat ts) msgs
      ys <- depack' (p `plusPtr` n) (k-1)
      return (xs++ys)

-- decodes a single MIDIPacket, and returns the length (in bytes), the timestamp, and the list of midi messages
depackSingleMIDIPacket :: Ptr MIDIPacket -> IO ( Int , MIDITimeStamp , [[Word8]] )
depackSingleMIDIPacket p = do
  ts   <- peek (castPtr p            ) :: IO MIDITimeStamp
  len' <- peek (castPtr p `plusPtr` 8) :: IO UInt16
  let len = fromIntegral len'
  msglist <- depackMsgList (castPtr p `plusPtr` 10 :: Ptr Word8) len
  return ( len + 8 + 2, ts, msglist )

-- helper function
depackMsgList :: Ptr Word8 -> Int -> IO [[Word8]]
depackMsgList _ 0 = return []
depackMsgList p n = if n < 0
  then fail "fatal error while depacking MIDI messages"
  else do
    (k,x) <- depackSingleMessage p
    xs <- depackMsgList (p `plusPtr` k) (n-k)
    return (x:xs)

depackSingleMessage :: Ptr Word8 -> IO (Int,[Word8])
depackSingleMessage p = do

  cmd <- peek p

  let hi  = shiftR cmd 4
      lo  = cmd .&. 15

  let ret :: Int -> IO (Int,[Word8])
      ret k = do
        xs <- mapM (peekElemOff p) [0..k-1]
        return $ ( k , xs  )

  case hi of
    8  -> ret 3  -- ?!
    9  -> ret 3
    10 -> ret 3
    11 -> ret 3
    12 -> ret 2
    13 -> ret 2
    14 -> ret 3
    15 -> case lo of
      2 -> ret 3
      3 -> ret 2
      0 -> sysex p
      _ -> ret 1
    _ -> fail "fatal error while interpreting a MIDI message"

-- does not include the terminating 0xf7 byte!
sysex :: Ptr Word8 -> IO (Int,[Word8])
sysex p = do
  n <- sysexloop p 2
  xs <- mapM (peekElemOff p) [0..n]
  return ( n+2 , xs )

sysexloop :: Ptr Word8 -> Int -> IO Int
sysexloop q i = do
  x <- peekElemOff q i
  if x == 0xf7 then return (i-1) else sysexloop q (i+1)

----- Send

-- |Sends a short message with timestamp "now".
midiSend :: MIDIPortRef -> Destination -> ShortMessage -> IO ()
midiSend port dst msg     = midiSend' port dst 0 msg

-- |Sends a list of short messages with timestamp "now".
midiSendList :: MIDIPortRef -> Destination -> [ShortMessage] -> IO ()
midiSendList port dst msglist = midiSendList' port dst 0 msglist

-- |Sends a short message with the given timestamp.
midiSend' :: MIDIPortRef -> Destination -> MIDITimeStamp -> ShortMessage -> IO ()
midiSend' port (Destination dst) ts msg = do
  let encoded = encodeShortMessage msg
      n = length encoded
  allocaBytes (4 + 8 + 2 + n) $ \p -> do
    poke      (        p              :: Ptr UInt32) 1
    poke      (castPtr p `plusPtr`  4 :: Ptr UInt64) ts
    poke      (castPtr p `plusPtr` 12 :: Ptr UInt16) (fromIntegral n)
    pokeArray (castPtr p `plusPtr` 14 :: Ptr Word8 ) encoded
    osstatus <- c_MIDISend port dst (castPtr p)
    when (osstatus /= 0) $ osStatusError osstatus

-- |Sends a list of short messages with the given timestamp.
midiSendList' :: MIDIPortRef -> Destination -> MIDITimeStamp -> [ShortMessage] -> IO ()
midiSendList' port (Destination dst) ts msglist = do
  let encoded = encodeShortMessageList msglist
      n = length encoded
  allocaBytes (4 + 8 + 2 + n) $ \p -> do
    poke      (        p              :: Ptr UInt32) 1
    poke      (castPtr p `plusPtr`  4 :: Ptr UInt64) ts
    poke      (castPtr p `plusPtr` 12 :: Ptr UInt16) (fromIntegral n)
    pokeArray (castPtr p `plusPtr` 14 :: Ptr Word8 ) encoded
    osstatus <- c_MIDISend port dst (castPtr p)
    when (osstatus /= 0) $ osStatusError osstatus

type MIDISendSysExCallback =  Ptr Word8 -> IO ()

foreign import ccall safe "wrapper"
  mkMidiSendSysExCallback :: MIDISendSysExCallback -> IO (FunPtr MIDISendSysExCallback)

midiSendSysExCallback :: MIDISendSysExCallback
midiSendSysExCallback p = do
  free p

-- Sends a system exclusive message. You shouldn't include the starting/trailing bytes 0xF0 and 0xF7.
midiSendSysEx :: Endpoint a => a -> [Word8] -> IO ()
midiSendSysEx dst dat' = do
  let ptrsize = sizeOf (undefined :: Ptr Word8)
      n = length dat
      k = 4*ptrsize + 8
      ep = endpoint dst
      dat = 0xf0 : (dat' ++ [0xf7])
  cb <- mkMidiSendSysExCallback midiSendSysExCallback
  p  <- mallocBytes (k + n)
  let q = (castPtr p `plusPtr` k) :: Ptr Word8
  pokeArray q dat
  poke (castPtr p) ep         ; r <- return (p `plusPtr` ptrsize)
  poke (castPtr r) q          ; r <- return (r `plusPtr` ptrsize)
  poke (castPtr r) n          ; r <- return (r `plusPtr` 4      )
  poke (castPtr r) (0::Int32) ; r <- return (r `plusPtr` 4      )
  poke (castPtr r) cb         ; r <- return (r `plusPtr` ptrsize)
  poke (castPtr r) p             -- not used (?)
  osstatus <- c_MIDISendSysex p  -- this is asynchronous! (returns immediately before data has been sent)
  when (osstatus /= 0) $ osStatusError osstatus

----- Ports

-- |Creates a new input port.
newInputPort :: MIDIClientRef -> String -> FunPtr (MIDIReadProc r s) -> Ptr r -> IO MIDIPortRef
newInputPort client name proc ref = do
  withCFString name $ \cfname -> alloca $ \pport -> do
    osstatus <- c_MIDIInputPortCreate client cfname proc ref pport
    when (osstatus /= 0) $ osStatusError osstatus
    peek pport

-- |Creates a new output port.
newOutputPort :: MIDIClientRef -> String -> IO MIDIPortRef
newOutputPort client name = do
  withCFString name $ \cfname -> alloca $ \pport -> do
    osstatus <- c_MIDIOutputPortCreate client cfname pport
    when (osstatus /= 0) $ osStatusError osstatus
    peek pport

-- |Disposes an existing port.
disposePort :: MIDIPortRef -> IO ()
disposePort port = do
  osstatus <- c_MIDIPortDispose port
  when (osstatus /= 0) $ osStatusError osstatus

-- |Connects an input port to a source.
connectToSource :: MIDIPortRef -> Source -> Ptr a -> IO ()
connectToSource port (Source src) ref = do
  osstatus <- c_MIDIPortConnectSource port src ref
  when (osstatus /= 0) $ osStatusError osstatus

-- |Disconnects an input port from a source.
disconnectFromSource :: MIDIPortRef -> Source -> IO ()
disconnectFromSource port (Source src) = do
  osstatus <- c_MIDIPortDisconnectSource port src
  when (osstatus /= 0) $ osStatusError osstatus

----- Clients

-- |Creates a new MIDI client with the given name.
newClient :: String -> IO MIDIClientRef
newClient name = do
  withCFString name $ \cfname -> alloca $ \pclient -> do
    osstatus <- c_MIDIClientCreate cfname nullFunPtr nullPtr pclient
    when (osstatus /= 0) $ osStatusError osstatus
    peek pclient

-- |Disposes an existing MIDI client.
disposeClient :: MIDIClientRef -> IO ()
disposeClient client = do
  osstatus <- c_MIDIClientDispose client
  when (osstatus /= 0) $ osStatusError osstatus


----- Devices

-- |Note: If a client iterates through the devices and entities in the system, it will not ever visit any virtual sources and destinations created by other clients. Also, a device iteration will return devices which are offline (were present in the past but are not currently present), while iterations through the system's sources and destinations will not include the endpoints of offline devices.
--
-- Thus clients should usually use `enumerateSources` and `enumerateDestinations`, rather iterating through devices and entities to locate endpoints.
enumerateDevices :: IO [MIDIDeviceRef]
enumerateDevices = do
  n <- c_MIDIGetNumberOfDevices
  if n > 0  -- n is unsigned => (n-1)=(2^32)-1  !!
    then forM [0..n-1] c_MIDIGetDevice
    else return []

----- Endpoints

-- |Enumaretes the MIDI sources present.
enumerateSources :: IO [Source]
enumerateSources = do
  n <- c_MIDIGetNumberOfSources
  if n > 0  -- n is unsigned => (n-1)=(2^32)-1  !!
    then forM [0..n-1] $ \i -> liftM Source (c_MIDIGetSource i)
    else return []

-- |Enumaretes the MIDI destinations present.
enumerateDestinations :: IO [Destination]
enumerateDestinations = do
  n <- c_MIDIGetNumberOfSources
  if n > 0  -- n is unsigned => (n-1)=(2^32)-1  !!
    then forM [0..n-1] $ \i -> liftM Destination (c_MIDIGetDestination i)
    else return []

-- a helper function; not exposed.
newEndpoint :: (MIDIClientRef -> CFStringRef -> Ptr MIDIEndpointRef -> IO OSStatus)
               -> MIDIClientRef -> String -> IO MIDIEndpointRef
newEndpoint createEndpoint client name = withCFString name $ \cfname -> do
  alloca $ \ptr_endpoint -> do
    osstatus <- createEndpoint client cfname ptr_endpoint
    if osstatus /= 0
      then osStatusError osstatus
      else peek ptr_endpoint

-- |Creates a new MIDI source with the given name.
newSource :: MIDIClientRef -> String -> IO Source
newSource client name = do
  src <- newEndpoint c_MIDISourceCreate client name
  return $ Source src

-- |Creates a new MIDI destination with the given name.
newDestination ::  MIDIClientRef -> String -> IO Destination
newDestination client name = do
  dst <- newEndpoint c_MIDIDestinationCreate client name
  return $ Destination dst

-- |Disposes an existing MIDI endpoint.
disposeEndpoint :: Endpoint a => a -> IO ()
disposeEndpoint x = do
  osstatus <- c_MIDIEndpointDispose (endpoint x)
  when (osstatus /= 0) $ osStatusError osstatus
