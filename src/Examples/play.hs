import Bang

simple = bang $ 120 <>> (4 $> hc) & (bd >> r >> sn >> r)

complex = bang $ 240 <>> mirror $ do
  bass & cc
  half $ do
    sn
    quad $ 4 $> hc & bd
    sn & ho
    bass & ch
  double $ 3 $> ((sn & hc) >> bd >> (sn & ho))

test = bang $ 120 <>> do
  double $ 4 $> (sn & hc) >> bd
  quad   $ 8 $> (sn & hc & bd) >> bd

poly = polyrhythm (3, 3 $> bd) (4, hc >> hc >> (hc & sn) >> hc)

amanda = bang $ 120 <>> do
  4 $> bd
  4 $> double $ bd >> hc
  4 $> quad   $ bd >> hc
  2 $> bd
