
-- |The hardware-independent part of the MIDI binding.

{-# LANGUAGE CPP #-}
module System.MIDI.Base 
  ( TimeStamp
  , MidiMessage'(..)
  , MidiMessage(..)
  , MidiEvent(..)
  , ClientCallback
  , ShortMessage(..)
  , translateShortMessage
  , untranslateShortMessage
  , shortMessage
  ) where

import Data.Bits
import Data.Word

type TimeStamp = Word32 

-- |A \"regular\" MIDI message.
--
-- Remark: `NoteOff` not having a velocity field is a design decision, and a questionable one. According to the
-- MIDI standard, NoteOff also has a velocity. However, most keyboards do not use this feature (send the default
-- value 64), and there are keyboards which do not send NoteOff messages at all, but send NoteOn messages with
-- zero velocity instead (for example the EMU Xboard series). I don't know what would be a good solution. 
-- At the moment, the code auto-translates NoteOn messages with zero velocity to NoteOff messages, and the
-- NoteOff velocity is ignored. This behaviour can be inverted with the Cabal flag NoNoteOff.
data MidiMessage' 
  = NoteOff         !Int          -- ^ Note Off (key)
  | NoteOn          !Int !Int     -- ^ Note On (key, velocity)
  | PolyAftertouch  !Int !Int     -- ^ Polyphonic key pressure (key, pressure)
  | CC              !Int !Int     -- ^ Control Change (controller, value)
  | ProgramChange   !Int          -- ^ Program Change (program)
  | Aftertouch      !Int          -- ^ Global aftertouch (pressure)
  | PitchWheel      !Int          -- ^ Pitch wheel (value, from -8192..+8191)
  deriving (Show,Eq)
  
-- |The type representing a MIDI message.  
data MidiMessage 
  = MidiMessage  !Int !MidiMessage'    -- ^ first argument is the MIDI channel (1..16)
  | SysEx        [Word8]               -- ^ not including the bytes 0xf0, 0xf7
  | SongPosition !Int
  | SongSelect   !Int 
  | TuneRequest
  | SRTClock
  | SRTStart
  | SRTContinue 
  | SRTStop
  | ActiveSensing
  | Reset
  | Undefined
  deriving (Show,Eq)
  
-- |The type representing a timestamped MIDI message. 
-- Time is measured in milisecs elapsed since the last call to `System.MIDI.start`.
data MidiEvent = MidiEvent !TimeStamp !MidiMessage deriving (Show,Eq)

-- |Type of the user callback function.  
type ClientCallback = MidiEvent -> IO ()
  
translateShortMessage :: ShortMessage -> MidiMessage
translateShortMessage (ShortMessage chn msg bt1 bt2) =
  if msg < 15 
    then MidiMessage (fromIntegral chn + 1) $ translate' msg k v
    else translate'' chn k v
  where
    k = fromIntegral bt1
    v = fromIntegral bt2

translate' msg k v = case msg of
#ifdef HMIDI_NO_NOTEOFF
   8  -> NoteOn k 0
   9  -> NoteOn k v
#else
   8  -> NoteOff k
   9  -> if v>0 then NoteOn k v else NoteOff k
#endif
   10 -> PolyAftertouch k v
   11 -> CC k v
   12 -> ProgramChange k
   13 -> Aftertouch k
   14 -> PitchWheel (k + shiftL v 7 - 8192)

translate'' lo a b = case lo of
  0  -> Undefined
  1  -> Undefined
  2  -> SongPosition (a + shiftL b 7)
  3  -> SongSelect a 
  4  -> Undefined
  5  -> Undefined
  6  -> TuneRequest
  7  -> Undefined
  8  -> SRTClock
  9  -> Undefined
  10 -> SRTStart
  11 -> SRTContinue
  12 -> SRTStop
  13 -> Undefined
  14 -> ActiveSensing
  15 -> Reset
 
untranslateShortMessage :: MidiMessage -> ShortMessage
untranslateShortMessage (MidiMessage chn msg') = 
  case msg' of
    NoteOff k           -> shortMessage chn  8 k 64
    NoteOn  k v         -> shortMessage chn  9 k v
    PolyAftertouch k v  -> shortMessage chn 10 k v
    CC k v              -> shortMessage chn 11 k v
    ProgramChange k     -> shortMessage chn 12 k 0
    Aftertouch k        -> shortMessage chn 13 k 0
    PitchWheel n        -> let m = min 16383 $ max 0 $ n + 8192 in shortMessage chn 14 (m.&.127) (shiftR m 7) 

untranslateShortMessage (SongPosition p) = shortMessage 15  3 (p.&.7) (shiftR p 7) 
untranslateShortMessage (SongSelect   s) = shortMessage 15  3 (fromIntegral s) 0 
untranslateShortMessage  TuneRequest     = shortMessage 15  6 0 0 
untranslateShortMessage  SRTClock        = shortMessage 15  8 0 0 
untranslateShortMessage  SRTStart        = shortMessage 15 10 0 0 
untranslateShortMessage  SRTContinue     = shortMessage 15 11 0 0 
untranslateShortMessage  SRTStop         = shortMessage 15 12 0 0 
untranslateShortMessage  ActiveSensing   = shortMessage 15 14 0 0 
untranslateShortMessage  Reset           = shortMessage 15 15 0 0 
untranslateShortMessage  Undefined       = error "cannot untranslate Undefined" 
untranslateShortMessage (SysEx _)        = error "cannot untranslate SysEx" 
 
shortMessage :: Int -> Int -> Int -> Int -> ShortMessage
shortMessage chn msg bt1 bt2 = 
  ShortMessage (fromIntegral chn - 1) (fromIntegral msg) (fromIntegral bt1) (fromIntegral bt2)
 
-- |Low level stuff.
data ShortMessage = ShortMessage 
  { sm_channel :: Word8
  , sm_msg     :: Word8 
  , sm_byte1   :: Word8
  , sm_byte2   :: Word8 
  } deriving Show

