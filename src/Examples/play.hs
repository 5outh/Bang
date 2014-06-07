import Bang
import Data.Monoid
import Data.Ratio

simple = bang $ double $ (4 #> hc) >< (bd <> qr <> sn <> qr)

complex = bang $
  mirror $ double $ 
  mconcat [
    bd >< cc
  , cross $ half $ mconcat [
    sn
    , quad ( 4 #> (hc >< bd) )
    , sn >< ho
    , bd >< chineseCymbal
    ]
  , quad $ triplets ( 8 #> ((sn >< hc) <> bd <> bd) )
  , bd >< crashCymbal1
  ]

doubleBass = bang $ double $ triplets ( hc >< (3 #> bassDrum2) )

polyrhythm = bang $ (3, 3 #> bd) ~=~ (4, 4 #> sn)

polyFast = bang $ (3 #> bd) ~= (4 #> sn)

polySlow = bang $ (3 #> bd) =~ (4 #> sn)

crazyPoly = bang $ 
  ( 7 #> bd ) ~= ( 5 #> hc <> sn )

quints = bangR $ tempo 4 $ quintuplets $ (hc >< bd) <> (4 #> bd)

amanda = mconcat
  [ 2 #> lowAgogo
  , double $ 4 #> (bd >< hc)
  ]

toxicity =
  let sh = sn >< hc 
  in bangR $ double $ 
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
