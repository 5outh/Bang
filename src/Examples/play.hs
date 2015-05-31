import Bang
import Data.Monoid
import Data.Ratio

-- | The very first rhythm most drummers learn to play.
simple :: Music Dur PercussionSound
simple = double $ (4 #> hc) >< (bd <> qr <> sn <> qr)

-- | An example of taking simple pieces and doing crazy stuff with it.
complex :: Music Dur PercussionSound
complex = mirror $
  double $
  mconcat [
    bd >< cc
  , cross $ half $ mconcat [
    sn
    , quad ( 4 #> (hc >< bd) )
    , sn >< ho
    , 3 #> bd >< ho
    ]
  , quad $ triplets ( 4 #> ((sn >< hc) <> bd <> bd) )
  , bd >< cc
  ]

-- A double-bass (sort of) beat in triplets
doubleBass :: Music Dur PercussionSound
doubleBass = 6 !> triplets ( hc >< (3 #> bassDrum2) )

-- | A simple 3/4 polyrhythm, using the ~=~ operator.
polyrhythm :: Music Dur PercussionSound
polyrhythm = (3, 3 #> bd) ~=~ (4, 4 #> sn)

-- | Some quintuplets!
quints :: Music Dur PercussionSound
quints = tempo 4 $ quintuplets $ (hc >< bd) <> (4 #> bd)

-- | Intro to 'Toxicity' by System of a Down.
toxicityIntro :: Music Dur PercussionSound
toxicityIntro =
  let sh = sn >< hc -- snare and closed hi-hat combo
      bc = bd >< hc -- bass and closed hi-hat combo
      cd = bd >< cc -- bass and crash cymbal combo
  in double $
     bd
     <> ( double $
          mconcat [
            mconcat [sh, bd, qr, bd, sh, qr, bd, qr, sh, qr]
          , mconcat [
              (2 #>) >>~ [sn, t1, t2] -- play each element of the list twice
            , double $ 4 #> sn
            , (2 #>) >>~ [sn, t1, t2]
            , m4 cd qr hc sn -- groups of measures with 4 beats
            , m4 hc bd sh qr
            , m4 hc sn bc qr
            , m4 bc qr hc sh
            , m4 hc bd sh qr
            , m4 bd qr sh qr
            ]
          ] )

-- | Plays for 2 measures, with original duration of 6 measures.
changeDurationEx :: Music Dur PercussionSound
changeDurationEx = 2 ~~ mconcat [
    16 #> bd
  , 4 #> sn
  , wr
  ]

-- | A 3/4 polyrhythm fitted to the duration of the shorter (3-beat) section
polyFast :: Music Dur PercussionSound
polyFast = (3 #> bd) ~= (4 #> sn)

-- | A 3/4 polyrhythm fitted to the duration of the longer (4-beat) section
polySlow :: Music Dur PercussionSound
polySlow = (3 #> bd) =~ (4 #> sn)

-- | Play each element of the list sequentially with duration 1
normalizeEx :: Music Dur PercussionSound
normalizeEx = 1 <!> [
    12 #> bd
  , 4 #> hc
  , 3 #> sn
  ]

-- Play each element of the list concurrently with duration 1/2
normalizeCEx :: Music Dur PercussionSound
normalizeCEx = (1/2) >!< [
    12 #> bd
  , 4  #> hc
  , 3  #> sn
  , crashCymbal1
  ]
