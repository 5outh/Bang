Bang
====

An <b>E</b>mbedded <b>D</b>omain <b>S</b>pecific <b>L</b>anguage for writing drum machine patterns in Haskell.

Bang interfaces with your system MIDI device in order to play drum compositions, directly written in and 
interpreted by the Haskell programming language.

Currently, the only OS Bang has been tested on is Mac OSX Mavericks, but it is entirely possible that it will work on other operating systems. Please let me know if you manage to get it building and working elsewhere; I'd love to know!

### Installing

Installation of the library is simple.

```
> cabal update
> cabal install Bang
```

On OSX, some additional setup is required to output MIDI sounds. First, you'll need to download and install [SimpleSynth](http://notahat.com/simplesynth/). Next, you'll need to set up a MIDI IAC driver. Open Audio MIDI Setup (in Applications > Utilities) and press ⌘2 (or go Window -> Show MIDI Window). You should see a slightly greyed out IAC Driver icon. Double click it, then check the box labeled "Device is online." Launch SimpleSynth and set the MIDI Source to "IAC Driver Bus 1" (or whatever you named your IAC driver) using the drop-down box at the top of the window. Audio from Bang should now feed into and play through SimpleSynth.

### Getting Started

TODO

### Implementation Details

TODO

Example:

```haskell
-- | The first few measures of 'Toxicity' by System of a Down.
toxicityIntro =
  let sh = sn >< hc -- snare and closed hi-hat combo
      bc = bd >< hc -- bass and closed hi-hat combo
      cd = bd >< cc -- bass and crash cymbal combo
  in bang $ 
     double $ -- play at double tempo
       bd <>
       ( double $ 
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
