
-- |Partial binding to CoreAudio, as required for `System.MIDI`.
-- At the moment only HostTime is supported.
-- In the future this module should grow into a separate entity.

{-# LANGUAGE ForeignFunctionInterface #-}
module System.MacOSX.CoreAudio
  ( audioGetCurrentHostTime
  , audioConvertHostTimeToNanos
  , audioConvertNanosToHostTime
  , audioGetCurrentTimeInNanos
  )
  where

import System.MacOSX.CoreFoundation

foreign import ccall unsafe "HostTime.h AudioGetCurrentHostTime"
  audioGetCurrentHostTime :: IO UInt64

foreign import ccall unsafe "HostTime.h AudioConvertHostTimeToNanos"
  audioConvertHostTimeToNanos :: UInt64 -> IO UInt64

foreign import ccall unsafe "HostTime.h AudioConvertNanosToHostTime"
  audioConvertNanosToHostTime :: UInt64 -> IO UInt64

audioGetCurrentTimeInNanos :: IO UInt64
audioGetCurrentTimeInNanos = ( audioGetCurrentHostTime >>= audioConvertHostTimeToNanos )
