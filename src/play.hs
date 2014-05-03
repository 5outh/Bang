import Bang
import Bang.Music
import Bang.Operators
import Bang.Interface.MDrum

import qualified System.MacOSX.CoreMIDI as OSX
import System.MIDI

basic :: Composition ()
basic = s1 <&> s2
  where s1 = 300 <>> hc
        s2 = 300 <>> b % r % sn % r

main :: IO ()
main = do
  dstlist <- enumerateDestinations
  case dstlist of 
    [] -> fail "No MIDI Devices found."
    (dst:_) -> do
      name    <- getName dst
      putStrLn $ "Using MIDI device: " ++ name
      conn    <- openDestination dst
      play conn basic