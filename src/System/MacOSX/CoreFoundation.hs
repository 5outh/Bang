
-- |Partial binding to CoreFoundation, as required for `System.MIDI`.
-- At the moment only CFString is supported.
-- In the future this module should grow into a separate entity.

{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module System.MacOSX.CoreFoundation
  ( newCFString
  , releaseCFString
  , peekCFString
  , withCFString

  , osStatusString
  , osStatusError

  , UInt8
  , UInt16
  , UInt32
  , UInt64
  , SInt8
  , SInt16
  , SInt32
  , SInt64
  , OSErr
  , OSStatus
  , UniChar
  , CFIndex
  , ItemCount
  , ByteCount
  , CFDataRef
  , CFStringRef
  , CFAllocatorRef
  ) where

import Data.Bits
import Data.Word
import Data.Int

import Control.Monad

import Foreign hiding (unsafePerformIO)
import Foreign.C
import Foreign.Marshal

type UInt8    = Word8
type UInt16   = Word16
type UInt32   = Word32
type UInt64   = Word64

type SInt8    = Int8
type SInt16   = Int16
type SInt32   = Int32
type SInt64   = Int64

type OSErr    = SInt16
type OSStatus = SInt32

type UniChar   = Char
type CFIndex   = SInt32
type ItemCount = UInt32
type ByteCount = UInt32

data CFData
data CFString
data CFAllocator

type CFDataRef      = Ptr CFData
type CFStringRef    = Ptr CFString
type CFAllocatorRef = Ptr CFAllocator

kCFAllocatorDefault = nullPtr

----- error "handling" :) -----

osStatusString :: OSStatus -> String
osStatusString osstatus = "OSStatus = " ++ show osstatus

osStatusError :: OSStatus -> IO a
osStatusError osstatus = fail $ osStatusString osstatus

----- Base -----

foreign import ccall unsafe "CFBase.h CFRelease"
  c_CFRelease :: Ptr a -> IO ()

----- CFStrings -----

foreign import ccall unsafe "CFString.h CFStringGetLength"
  c_CFStringGetLength :: CFStringRef -> IO CFIndex

foreign import ccall unsafe "CFString.h CFStringGetCharactersPtr"
  c_CFStringGetCharactersPtr :: CFStringRef -> IO (Ptr UniChar)

foreign import ccall unsafe "CFString.h CFStringGetCharacterAtIndex"
  c_CFStringGetCharacterAtIndex :: CFStringRef -> CFIndex -> IO UniChar

foreign import ccall unsafe "CFString.h CFStringCreateWithCharacters"
  c_CFStringCreateWithCharacters :: CFAllocatorRef -> Ptr UniChar -> CFIndex -> IO CFStringRef

-- | Manually releasing a CFString.
releaseCFString :: CFStringRef -> IO ()
releaseCFString = c_CFRelease

-- | Peeks a CFString.
peekCFString :: CFStringRef -> IO String
peekCFString cfstring = do
  n <- c_CFStringGetLength cfstring
  p <- c_CFStringGetCharactersPtr cfstring
  if p /= nullPtr
    then forM [0..n-1] $ \i -> peekElemOff p (fromIntegral i)
    else forM [0..n-1] $ \i -> c_CFStringGetCharacterAtIndex cfstring i

-- | Creates a new CFString. You have to release it manually.
newCFString :: String -> IO CFStringRef
newCFString string =
  let n = length string in allocaArray n $ \p ->
  c_CFStringCreateWithCharacters kCFAllocatorDefault p (fromIntegral n)

-- | Safe passing of a CFString to the OS (releases it afterwards).
withCFString :: String -> (CFStringRef -> IO a) -> IO a
withCFString string action = do
  cfstring <- newCFString string
  x <- action cfstring
  releaseCFString cfstring
  return x
