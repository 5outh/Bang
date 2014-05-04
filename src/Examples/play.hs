import Bang

simple = playIO $ (120 <>> hc) <&> (120 <>> (bd >> r >> sn >> r))

complex = playIO $ 240 <>> mirror $ do
  bass & cc
  half $ do
    sn
    quad $ 4 $> hc & bd
    sn & ho
    (bass & ch)
  double $ 3 $> ((sn & hc) >> bd >> (sn & ho))
