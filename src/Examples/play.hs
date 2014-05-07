import Bang

simple = playIO $ (120 <>> hc) <&> (120 <>> (bd >> r >> sn >> r))

complex = playIO $ 240 <>> mirror $ do
  bass & cc
  half $ do
    sn
    quad $ 4 $> hc & bd
    sn & ho
    bass & ch
  double $ 3 $> ((sn & hc) >> bd >> (sn & ho))

test = playIO $ 240 <>> do
  double $ 4 $> (sn & hc) >> bd
  quad   $ 8 $> (sn & hc & bd) >> bd
