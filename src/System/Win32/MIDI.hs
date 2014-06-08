
-- |Low-level binding to the Win32 MIDI services.
-- Error handling is via `fail`-s in the IO monad. 

{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module System.Win32.MIDI 
  ( Source(..)
  , Destination(..)
  , enumerateSources
  , enumerateDestinations
  , MIDIHasName
  , getName
  , getModel
  , getManufacturer
  , getMidiInCaps
  , getMidiOutCaps
  , MidiInCaps(..)
  , MidiOutCaps(..)
  , midiInOpen
  , midiOutOpen
  , Callback(..)
  , midiInClose
  , midiOutClose
  , midiInStart
  , midiInStop
  , midiInReset
  , midiOutReset
  , midiOutSend

  , midiInAddBuffer
  , midiOutSendSysEx
  , withMidiInHeader
  , withMidiOutHeader
  , newMidiInHeader
  , freeMidiInHeader
  
  , MIDIHDR
  , HMIDI
  , HMIDIIN
  , HMIDIOUT
  , MIDIPROC
  , MIDIINPROC
  , MIDIOUTPROC
  , mkMIDIPROC
  
  , decodeShortMessage
  , encodeShortMessage
  , MIM(..)
  , MOM(..)
  , mim
  , mom
  
  , timeGetTime  -- not strictly MIDI, nevertheless quite useful in this context 
  ) where

import Control.Monad
import Data.Bits
import Data.Word
import Foreign
import Foreign.Storable

import System.Win32.Types
import System.MIDI.Base

maxPNAMELEN    = 32  :: Int
maxERRORLENGTH = 256 :: UINT

midiMapperID = -1 :: UINT

type HMIDI    = HANDLE
type HMIDIIN  = HANDLE
type HMIDIOUT = HANDLE

type MIDIPROC a    = HMIDI    -> UINT -> Ptr a -> DWORD -> DWORD -> IO ()
type MIDIINPROC a  = HMIDIIN  -> UINT -> Ptr a -> DWORD -> DWORD -> IO ()
type MIDIOUTPROC a = HMIDIOUT -> UINT -> Ptr a -> DWORD -> DWORD -> IO ()

foreign import stdcall safe "wrapper" 
  mkMIDIPROC :: MIDIPROC () -> IO (FunPtr (MIDIPROC ()))

-----

type MMRESULT     = UINT
type MMVERSION    = UINT

newtype Source       = Source      UINT deriving (Show,Eq)
newtype Destination  = Destination UINT deriving (Show,Eq)

fromMMVersion :: MMVERSION -> (Int,Int)
fromMMVersion mmversion = (major,minor) where
  major = fromIntegral $ shiftR mmversion 8
  minor = fromIntegral $ mmversion .&. 255

----- Time

-- Returns the system time in miliseconds.
foreign import stdcall unsafe "mmsystem.h timeGetTime"
  timeGetTime :: IO DWORD

----- Errors

{-
-- not really informative conversion (fallback)
mmErrorString :: MMRESULT -> String
mmErrorString mmresult = "MMRESULT = " ++ show mmresult

-- not really informative "fail"
mmError :: MMRESULT -> IO a
mmError mmresult = fail $ mmErrorString mmresult
-}

mmInError :: MMRESULT -> IO a
mmInError mmresult = do { txt <- midiInGetErrorText mmresult ; fail txt }

mmOutError :: MMRESULT -> IO a
mmOutError mmresult = do { txt <- midiOutGetErrorText mmresult ; fail txt }

foreign import stdcall unsafe "mmsystem.h midiInGetErrorTextW"
  c_midiInGetErrorText :: MMRESULT -> LPTSTR -> UINT -> IO UINT

foreign import stdcall unsafe "mmsystem.h midiOutGetErrorTextW"
  c_midiOutGetErrorText :: MMRESULT -> LPTSTR -> UINT -> IO UINT
  
midiInGetErrorText :: MMRESULT -> IO String
midiInGetErrorText errcode = do
  allocaArray0 (fromIntegral maxERRORLENGTH) $ \p -> do
    mmr <- c_midiInGetErrorText errcode p maxERRORLENGTH
    if mmr /= 0 
      then return "invalid MMRESULT"
      else peekTString p 

midiOutGetErrorText :: MMRESULT -> IO String  
midiOutGetErrorText errcode = do
  allocaArray0 (fromIntegral maxERRORLENGTH) $ \p -> do
    mmr <- c_midiOutGetErrorText errcode p maxERRORLENGTH
    if mmr /= 0 
      then return "invalid MMRESULT"
      else peekTString p 
  
----- Devices
  
foreign import stdcall unsafe "mmsystem.h midiInGetNumDevs"
  c_midiInGetNumDevs :: IO UINT

foreign import stdcall unsafe "mmsystem.h midiOutGetNumDevs"
  c_midiOutGetNumDevs :: IO UINT

-- |Eumerates the MIDI sources.
enumerateSources :: IO [Source]
enumerateSources = do
  n <- c_midiInGetNumDevs
  return $ if n>0 then [ Source i | i<-[0..n-1] ] else []  -- n is unsigned!

-- |Eumerates the MIDI destinations.
enumerateDestinations :: IO [Destination]
enumerateDestinations = do
  n <- c_midiOutGetNumDevs
  return $ if n>0 then [ Destination i | i<-[0..n-1] ] else []  -- n is unsigned!

----- MIDI capabilities

manufacturerIDToString :: WORD -> String
manufacturerIDToString mid = "Manufacturer ID = " ++ show mid

productIDToString :: WORD -> WORD -> String
productIDToString mid  pid = "Product ID = " ++ show mid ++ ":" ++ show pid

data MIDIINCAPS
data MIDIOUTCAPS

type LPMIDIINCAPS  = Ptr MIDIINCAPS
type LPMIDIOUTCAPS = Ptr MIDIOUTCAPS

data MidiInCaps = MidiInCaps
  { mic_Mid :: WORD
  , mic_Pid :: WORD
  , mic_DriverVersion :: MMVERSION
  , mic_Pname :: String   -- TCHAR [MAXPNAMELEN]
  , mic_Support :: DWORD  -- reserved
  } deriving Show

instance Storable MidiInCaps where
  sizeOf    _  = 12 + maxPNAMELEN * sizeOf (undefined::TCHAR)
  alignment _  = 4 
  peek p = do
    mid <- peek (castPtr p) ; q <- return (p `plusPtr` 2)
    pid <- peek (castPtr q) ; q <- return (q `plusPtr` 2)
    ver <- peek (castPtr q) ; q <- return (q `plusPtr` 4)
    nam <- peekTString (castPtr q) ; q <- return (q `plusPtr` (4*maxPNAMELEN))
    sup <- peek (castPtr q) 
    return $ MidiInCaps mid pid ver nam sup
  poke p mic = fail "MidiInCaps/poke not implemented yet"

data MidiOutCaps = MidiOutCaps
  { moc_Mid :: WORD
  , moc_Pid :: WORD
  , moc_DriverVersion :: MMVERSION
  , moc_Pname :: String       -- TCHAR [MAXPNAMELEN]
  , moc_Technology  :: WORD
  , moc_Voices      :: WORD
  , moc_Notes       :: WORD
  , moc_ChannelMask :: WORD
  , moc_Support     :: DWORD  -- reserved
  } deriving Show

instance Storable MidiOutCaps where
  sizeOf    _  = 20 + maxPNAMELEN * sizeOf (undefined::TCHAR) 
  alignment _  = 4 
  peek p = do
    mid <- peek (castPtr p) ; q <- return (p `plusPtr` 2)
    pid <- peek (castPtr q) ; q <- return (q `plusPtr` 2)
    ver <- peek (castPtr q) ; q <- return (q `plusPtr` 4)
    nam <- peekTString (castPtr q) ; q <- return (q `plusPtr` (4*maxPNAMELEN))
    tec <- peek (castPtr p) ; q <- return (p `plusPtr` 2)
    voi <- peek (castPtr p) ; q <- return (p `plusPtr` 2)
    not <- peek (castPtr p) ; q <- return (p `plusPtr` 2)
    chm <- peek (castPtr p) ; q <- return (p `plusPtr` 2)
    sup <- peek (castPtr q) 
    return $ MidiOutCaps mid pid ver nam tec voi not chm sup
  poke p mic = fail "not implemented yet"
     
foreign import stdcall unsafe "mmsystem.h midiInGetDevCapsW"
  c_midiInGetDevCaps :: UINT -> LPMIDIINCAPS -> UINT -> IO MMRESULT 

foreign import stdcall unsafe "mmsystem.h midiOutGetDevCapsW"
  c_midiOutGetDevCaps :: UINT -> LPMIDIOUTCAPS -> UINT -> IO MMRESULT 

-- |`Source`'s and `Destinations` have names, models and manufacturers (though the last two is not really supported...)
class MIDIHasName a where 
  getName :: a -> IO String
  getModel :: a -> IO String
  getManufacturer :: a -> IO String
  
instance MIDIHasName Source where
  getName src = do 
    caps <- getMidiInCaps src
    return $ mic_Pname caps
  getModel src = do
    caps <- getMidiInCaps src
    return $ productIDToString (mic_Mid caps) (mic_Pid caps)
  getManufacturer src = do
    caps <- getMidiInCaps src
    return $ manufacturerIDToString (mic_Mid caps)
 
instance MIDIHasName Destination where
  getName dst = do 
    caps <- getMidiOutCaps dst  
    return $ moc_Pname caps
  getModel dst = do
    caps <- getMidiOutCaps dst
    return $ productIDToString (moc_Mid caps) (moc_Pid caps)
  getManufacturer dst = do
    caps <- getMidiOutCaps dst
    return $ manufacturerIDToString (moc_Mid caps)
 
getMidiInCaps :: Source -> IO MidiInCaps 
getMidiInCaps (Source device) = do
  let cast = castPtr :: Ptr MidiInCaps -> LPMIDIINCAPS
  alloca $ \ptr -> do
    mmresult <- c_midiInGetDevCaps device (cast ptr) (fromIntegral $ sizeOf (undefined::MidiInCaps))
    if mmresult /= 0
      then mmInError mmresult
      else peek ptr

getMidiOutCaps :: Destination -> IO MidiOutCaps 
getMidiOutCaps (Destination device) = do
  let cast = castPtr :: Ptr MidiOutCaps -> LPMIDIOUTCAPS
  alloca $ \ptr -> do
    mmresult <- c_midiOutGetDevCaps device (cast ptr) (fromIntegral $ sizeOf (undefined::MidiOutCaps))
    if mmresult /= 0
      then mmOutError mmresult
      else peek ptr
 
----- Open / Close (these should be "safe", as they are generating immediate callbacks!)

foreign import stdcall safe "mmsystem.h midiInOpen"
  c_midiInOpen  :: Ptr HMIDIIN -> UINT -> FunPtr (MIDIINPROC a) -> Ptr a -> DWORD -> IO MMRESULT  

foreign import stdcall safe "mmsystem.h midiOutOpen"
  c_midiOutOpen :: Ptr HMIDIOUT -> UINT -> FunPtr (MIDIOUTPROC a) -> Ptr a -> DWORD -> IO MMRESULT  
  
foreign import stdcall safe "mmsystem.h midiInClose"
  c_midiInClose  :: HMIDIIN -> IO MMRESULT  
  
foreign import stdcall safe "mmsystem.h midiOutClose"
  c_midiOutClose  :: HMIDIOUT -> IO MMRESULT  
  
----- Start / Stop / Reset

foreign import stdcall unsafe "mmsystem.h midiInStart"
  c_midiInStart  :: HMIDIIN -> IO MMRESULT  
  
foreign import stdcall unsafe "mmsystem.h midiInStop"
  c_midiInStop  :: HMIDIIN -> IO MMRESULT  

foreign import stdcall safe "mmsystem.h midiInReset"
  c_midiInReset  :: HMIDIIN -> IO MMRESULT    -- the resetting functions must be safe, as they are doing callbacks!!!

foreign import stdcall safe "mmsystem.h midiOutReset"
  c_midiOutReset  :: HMIDIIN -> IO MMRESULT  
  
----- Send Messages  
 
data MIDIHDR 

foreign import stdcall unsafe "mmsystem.h midiInAddBuffer"  -- this should be called midiInLongMsg ???
  c_midiInAddBuffer  :: HMIDIIN -> Ptr MIDIHDR -> UINT -> IO MMRESULT  
  
foreign import stdcall unsafe "mmsystem.h midiOutShortMsg"
  c_midiOutShortMsg  :: HMIDIOUT -> DWORD -> IO MMRESULT  
  
foreign import stdcall unsafe "mmsystem.h midiOutLongMsg"
  c_midiOutLongMsg  :: HMIDIOUT -> Ptr MIDIHDR -> UINT -> IO MMRESULT  
  
foreign import stdcall unsafe "mmystem.h midiInPrepareHeader"
  c_midiInPrepareHeader :: HMIDIIN -> Ptr MIDIHDR -> UINT -> IO MMRESULT

foreign import stdcall unsafe "mmystem.h midiOutPrepareHeader"
  c_midiOutPrepareHeader :: HMIDIOUT -> Ptr MIDIHDR -> UINT -> IO MMRESULT

foreign import stdcall unsafe "mmystem.h midiInUnprepareHeader"
  c_midiInUnprepareHeader :: HMIDIIN -> Ptr MIDIHDR -> UINT -> IO MMRESULT

foreign import stdcall unsafe "mmystem.h midiOutUnprepareHeader"
  c_midiOutUnprepareHeader :: HMIDIOUT -> Ptr MIDIHDR -> UINT -> IO MMRESULT
  
type CPrepare   = HMIDI -> Ptr MIDIHDR -> UINT -> IO MMRESULT 
type CUnprepare = HMIDI -> Ptr MIDIHDR -> UINT -> IO MMRESULT 
  
waitFor :: IO Bool -> IO ()
waitFor check = do
  b <- check
  unless b $ waitFor check  
  
while :: (a -> Bool) -> IO a -> IO a
while cond act = do
  x <- act
  if cond x then while cond act else return x  
  
withMidiHeader :: (MMRESULT -> IO ()) -> CPrepare -> CUnprepare -> HMIDI -> [Word8] -> (HMIDI -> Ptr MIDIHDR -> UINT -> IO a) -> IO a
withMidiHeader mmError prepare unprepare handle bytes action = do
  let n = fromIntegral (length bytes) :: DWORD 
      size = 48 :: UINT  -- size of the MIDIHDR structure
  allocaBytes (fromIntegral size) $ \p -> allocaBytes (fromIntegral n) $ \q -> do
    poke (castPtr p             ) q           -- pointer to the data
    poke (castPtr p `plusPtr`  4) n           -- length of the data
    poke (castPtr p `plusPtr` 16) (0::DWORD)  -- flags, must be zero
    mmresult <- prepare   handle p size ; when (mmresult /=0) $ mmError mmresult   
    pokeArray q bytes
    x        <- action    handle p size
{-
    waitFor $ do 
      flags <- peek (castPtr p `plusPtr` 16) :: IO DWORD 
      return $ (flags .&. 1) /= 0  -- DONE FLAG
    mmresult <- unprepare handle p size ; when (mmresult /=0) $ mmError mmresult   
-}
    mmresult <- while (==65) (unprepare handle p size)    -- 65 is MIDIERR_STILLPLAYING
    return x

newMidiHeader :: (MMRESULT -> IO ()) -> CPrepare -> HMIDI -> Int -> IO (Ptr MIDIHDR)
newMidiHeader mmError prepare handle n = do
  let size = 48 :: UINT  -- size of the MIDIHDR structure
  p <- mallocBytes (fromIntegral size + n)
  let q = castPtr p `plusPtr` (fromIntegral size) :: Ptr Word8
  poke (castPtr p             ) q           -- pointer to the data
  poke (castPtr p `plusPtr`  4) n           -- length of the data
  poke (castPtr p `plusPtr` 16) (0::DWORD)  -- flags, must be zero
  mmresult <- prepare handle p size ; when (mmresult /=0) $ mmError mmresult   
  return p
  
freeMidiHeader :: (MMRESULT -> IO ()) -> CUnprepare -> HMIDI -> Ptr MIDIHDR -> IO ()
freeMidiHeader mmError unprepare handle p = do
  mmresult <- unprepare handle p 48 ; when (mmresult /=0) $ mmError mmresult 
  free p

withMidiInHeader  = withMidiHeader mmInError  c_midiInPrepareHeader  c_midiInUnprepareHeader   
withMidiOutHeader = withMidiHeader mmOutError c_midiOutPrepareHeader c_midiOutUnprepareHeader   
newMidiInHeader   = newMidiHeader  mmInError  c_midiInPrepareHeader 
freeMidiInHeader  = freeMidiHeader mmInError  c_midiInUnprepareHeader  
  
midiInAddBuffer :: HMIDIIN -> Int -> IO (Ptr MIDIHDR)
midiInAddBuffer handle n = do
  p <- newMidiInHeader handle n 
  mmresult <- c_midiInAddBuffer handle p 48 
  when (mmresult /=0) $ mmInError mmresult   
  return p

-- |you shouldn't include the starting/trailing bytes 0xf0 and 0xf7
midiOutSendSysEx :: HMIDIOUT -> [Word8] -> IO ()
midiOutSendSysEx handle msg' = do
  let msg = 0xf0 : (msg' ++ [0xf7])
  --print msg
  mmresult <- withMidiOutHeader handle msg c_midiOutLongMsg 
  when (mmresult /=0) $ mmOutError mmresult   
    
midiOutSend :: HMIDIOUT -> ShortMessage -> IO ()
midiOutSend handle msg = do
  mmresult <- c_midiOutShortMsg handle (encodeShortMessage msg)
  when (mmresult /=0) $ mmOutError mmresult   
    
---------- exported Haskell functions ---------

encodeRunningMessages :: [ShortMessage] -> [DWORD]
encodeRunningMessages msgs = encodeShortMessage (head msgs) : map encode' (tail msgs) where
  encode' msg = fromIntegral (sm_byte1 msg) + (shiftL (fromIntegral $ sm_byte2 msg) 8)
   
encodeShortMessage :: ShortMessage -> DWORD
encodeShortMessage (ShortMessage chn' msg' bt1' bt2') 
  = chn + shiftL msg 4 + shiftL bt1 8 + shiftL bt2 16  
  where 
    chn = 15 .&. fromIntegral chn'
    msg = 15 .&. fromIntegral msg'
    bt1 = fromIntegral bt1'
    bt2 = fromIntegral bt2'
    
decodeShortMessage :: DWORD -> ShortMessage
decodeShortMessage x = ShortMessage chn msg bt1 bt2 
  where
    chn = fromIntegral $ (       x   ) .&. 15
    msg = fromIntegral $ (shiftR x  4) .&. 15
    bt1 = fromIntegral $ (shiftR x  8) .&. 255
    bt2 = fromIntegral $ (shiftR x 16) .&. 255

midiInStart :: HMIDIIN -> IO ()
midiInStart handle = do
  mmresult <- c_midiInStart handle
  when (mmresult /=0) $ mmInError mmresult   

midiInStop :: HMIDIIN -> IO ()
midiInStop handle = do
  mmresult <- c_midiInStop handle
  when (mmresult /=0) $ mmInError mmresult   

midiInClose :: HMIDIIN -> IO ()
midiInClose handle = do
  mmresult <- c_midiInClose handle
  when (mmresult /=0) $ mmInError mmresult   

midiOutClose :: HMIDIOUT -> IO ()
midiOutClose handle = do
  mmresult <- c_midiOutClose handle
  when (mmresult /=0) $ mmOutError mmresult   

midiInReset :: HMIDIIN -> IO ()
midiInReset handle = do
  mmresult <- c_midiInReset handle
  when (mmresult /=0) $ mmInError mmresult   

midiOutReset :: HMIDIOUT -> IO ()
midiOutReset handle = do
  mmresult <- c_midiOutReset handle
  when (mmresult /=0) $ mmOutError mmresult   

data MIM 
  = MIM_OPEN
  | MIM_CLOSE
  | MIM_DATA
  | MIM_LONGDATA
  | MIM_ERROR
  | MIM_LONGERROR
  deriving (Eq,Show)
  
mim :: UINT -> MIM
mim 0x3C1 = MIM_OPEN
mim 0x3C2 = MIM_CLOSE
mim 0x3C3 = MIM_DATA
mim 0x3C4 = MIM_LONGDATA
mim 0x3C5 = MIM_ERROR
mim 0x3C6 = MIM_LONGERROR

data MOM 
  = MOM_OPEN
  | MOM_CLOSE
  | MOM_DONE
  deriving (Eq,Show)
  
mom :: UINT -> MOM
mom 0x3c7 = MOM_OPEN
mom 0x3c8 = MOM_CLOSE
mom 0x3c9 = MOM_DONE
             
data Callback 
  = CALLBACK_NULL            -- ^ no callback
  | CALLBACK_WINDOW          -- ^ callback is window (needs a HWND)
  | CALLBACK_THREAD          -- ^ callback is a thread (needs the thread id)
  | CALLBACK_FUNCTION Bool   -- ^ callback is a function; the boolean is MIDI_IO_STATUS (input only)
  | CALLBACK_EVENT           -- ^ callback is an event (needs an event handle; output only)

callback :: Callback -> DWORD
callback CALLBACK_NULL           = 0x00000000
callback CALLBACK_WINDOW         = 0x00010000
callback CALLBACK_THREAD         = 0x00020000
callback (CALLBACK_FUNCTION ios) = 0x00030000 + if ios then 0x20 else 0
callback CALLBACK_EVENT          = 0x00050000

midiInOpen :: Source -> FunPtr (MIDIINPROC a) -> Ptr a -> Callback -> IO HMIDIIN
midiInOpen (Source dev) proc ref cbtype = 
  alloca $ \phandle -> do
    mmresult <- c_midiInOpen phandle dev proc ref (callback cbtype)
    when (mmresult /=0) $ mmInError mmresult
    peek phandle    

midiOutOpen :: Destination -> FunPtr (MIDIOUTPROC a) -> Ptr a -> Callback -> IO HMIDIOUT 
midiOutOpen dst@(Destination dev) proc ref cbtype = do
  alloca $ \phandle -> do
    mmresult <- c_midiOutOpen phandle dev proc ref (callback cbtype)
    when (mmresult /=0) $ mmInError mmresult
    peek phandle    
