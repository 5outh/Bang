import Bang
import Data.Monoid
import Data.Ratio

simple = bang $ 120 @> ( (4 #> hc) >< (bd <> qr <> sn <> qr) )

complex = bang $ 240 %>
  mconcat [
    (bd >< cc)
  , half $ mconcat [
    sn
    , quad (4 #> (hc >< bd) )
    , sn >< ho
    , bd >< chineseCymbal
    ]
  , quad $ triplets ( 8 #> ((sn >< hc) <> bd <> bd) )
  , bd >< crashCymbal1
  ]

doubleBass = bang $ 240 @> double $ triplets ( hc >< (3 #> bassDrum2) )

--poly = bang $ 120 <>> polyrhythm (3, 3 $> bd) (4, 4 $> sn)

--quints = bang $ 480 <>> quintuplets $ (hc & bd) >> (4 $> bd)

amanda = 240 @> mconcat
  [ 2 #> lowAgogo
  , double $ 4 #> (bd >< hc)
  ]

--wonko = bang $ 120 ^> do
--  bass & cc
--  sn & bd
--  bass & hc
--  sn & ho

toxicity = 
  let sh = sn >< hc 
  in bang $ 240 @>
     bd
  <> (double $ mconcat [
      mconcat [sh, bd, qr, bd, sh, qr, bd, qr, sh, qr]
    , mconcat [ 
        (2 #>) >>~ [sn, t1, t2]
      , double $ 4 #> sn
      , (2 #>) >>~ [sn, t1, t2] 
      ]
    , mconcat [
        m4 (bd >< cc) qr  hc         sn
      , m4 hc          bd sh         qr
      , m4 hc          sn (bd >< hc) qr
      , m4 (bd >< hc)  qr hc         sh
      , m4 hc          bd sh         qr
      , m4 bd          qr sh         qr 
      ] 
    ])

--mirrorify = bang $ 480 <>> mirror $ 2 $> mapM_ (4 $>) [sn, t2, t1, tf, bd]
