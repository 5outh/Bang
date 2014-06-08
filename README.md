Bang
====

An <b>E</b>mbedded <b>D</b>omain <b>S</b>pecific <b>L</b>anguage for writing drum machine patterns in Haskell.

Bang interfaces with your system MIDI device in order to play drum compositions, directly written in and 
interpreted by the Haskell programming language.

Example:

```haskell
-- | The first few measures of 'Toxicity' by System of a Down.
toxicityIntro =
  let sh = sn >< hc -- snare and closed hi-hat combo
      bc = bd >< hc -- bass and closed hi-hat combo
      cd = bd >< cc -- bass and crash cymbal combo
  in bang $ 
     double $ -- play at double tempo
        bd
     <> ( double $ 
          mconcat [ -- concatenate into a single sequential composition
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
```