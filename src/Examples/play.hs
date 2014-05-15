import Bang
import Data.Monoid
import Data.Ratio

simple = bang $ 120 !> ( (4 $> hc) & (bd <> r <> sn <> r) )

complex = bang $ 240 !>
     (bass & cc)
  <> half
    (  sn
    <> (quad (4 $> (hc & bd) ))
    <> sn & ho
    <> bass & ch )
  <> double ( 9 $> ((sn & hc) <> bd <> (sn & ho)) )

--test = bang $ 120 <>> do
--  double $ 4  $> (sn & hc)      >> bd
--  quad   $ 8  $> (sn & hc & bd) >> bd
--  oct    $ 16 $> (sn & hc & bd) >> bd

--doubleBass = bang $ 120 <>> double $ cr & (triplets $ 3 $> bd)

--poly = bang $ 120 <>> polyrhythm (3, 3 $> bd) (4, 4 $> sn)

--quints = bang $ 480 <>> quintuplets $ (hc & bd) >> (4 $> bd)

--amanda = bang $ 120 <>> do
--  4 $> bd
--  4 $> double $ bd >> hc
--  4 $> quad   $ bd >> hc
--  2 $> bd

--wonko = bang $ 120 ^> do
--  bass & cc
--  sn & bd
--  bass & hc
--  sn & ho

--supernova = bang $ 240 <>> do
--  2 $> quintuplets $ (bd >> sn >> (double $ 2$> bd >> sn) >> ho) & (4 $> hc)

--toxicity = bang $ 240 <>> do
--  let sh = sn & hc
--  bd
--  double $ sh >> bd >> r >> bd >> sh >> r >> bd >> r >> sh >> r
--  double $ do 
--    mapM_ (2 $>) [sn, t1, t2]
--    double $ 4 $> sn
--    mapM_ (2 $>) [sn, t1, t2]
--  double $ do
--    m4 (bd & cc) r  hc        sn
--    m4 hc        bd sh        r
--    m4 hc        sn (bd & hc) r
--    m4 (bd & hc) r  hc        sh
--    m4 hc        bd sh        r
--    m4 bd        r  sh        r

--mirrorify = bang $ 480 <>> mirror $ 2 $> mapM_ (4 $>) [sn, t2, t1, tf, bd]
