import Bang
import Bang.Music
import Bang.Operators
import Bang.Interface.MDrum

import qualified System.MacOSX.CoreMIDI as OSX
import System.MIDI

import Control.Monad

comp = (400 <>> (cc & hc & b) % b % b % hc % b % b)
       <&> 
       (400 <>> cc % replicateM_ 3 r)

main :: IO ()
main = do
  dstlist <- enumerateDestinations
  case dstlist of 
    [] -> fail "No MIDI Devices found."
    (dst:_) -> do
      name    <- getName dst
      putStrLn $ "Using MIDI device: " ++ name
      conn    <- openDestination dst
      play conn comp