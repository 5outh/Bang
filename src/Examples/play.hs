import Bang
import Data.Ratio

simple = bang $ 120 <>> (4 $> hc) & (bd >> r >> sn >> r)

complex = bang $ 240 ^> do
  bass & cc
  half $ do
    sn
    quad $ 4 $> hc & bd
    sn & ho
    bass & ch
  double $ 9 $> ((sn & hc) >> bd >> (sn & ho))

test = bang $ 120 <>> do
  double $ 4  $> (sn & hc)      >> bd
  quad   $ 8  $> (sn & hc & bd) >> bd
  oct    $ 16 $> (sn & hc & bd) >> bd

doubleBass = bang $ 240 <>> (withDuration (1%12) $ 3 $> bd) & cc

poly = bang $ 120 ^> polyrhythm (3, 3 $> bd) (4, 4 $> sn)

quints = bang $ 480 <>> quintuplets $ (hc & bd) >> (4 $> bd)

amanda = bang $ 120 <>> do
  4 $> bd
  4 $> double $ bd >> hc
  4 $> quad   $ bd >> hc
  2 $> bd

wonko = bang $ 120 ^> do
  bass & cc
  sn & bd
  bass & hc
  sn & ho