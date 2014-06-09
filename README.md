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

On OSX, some additional setup is required to output MIDI sounds. First, you'll need to download and install [SimpleSynth](http://notahat.com/simplesynth/). Next, you'll need to set up a MIDI IAC driver. Open Audio MIDI Setup (in `Applications -> Utilities`) and press ⌘2 (or go `Window -> Show MIDI Window`). You should see a slightly greyed out **IAC Driver** icon. Double click it, then check the box labeled "Device is online." Launch SimpleSynth and set the MIDI Source to "**IAC Driver Bus 1**" (or whatever you named your IAC driver) using the drop-down box at the top of the window. Audio from `Bang` should now feed into and play through SimpleSynth.

### Getting Started

Bang exports two main entry points for playing compositions, `bang` to play compositions once and stop, and `bangR`, to play compositions on repeat. In all of the following examples, `bang` can be replaced by `bangR`.

To play a single bass drum hit:

```haskell
> bang bd
```

To play two compositions sequentially, use `<>` (from `Data.Monoid`):

```haskell
> :m + Data.Monoid
> bang $ bd <> sn -- bass, then snare.
```

To play two compositions in parallel, use '><':

```haskell
> bang $ bd >< sn -- bass and snare at the same time.
```

To play a composition multiple times in sequence, use `#>`:

```haskell
> bang $ 4 #> bd -- play 4 bass drum hits in sequence
```

`bang` plays a composition at 120 BPM by default. To change the tempo, use `!>`, which expects a fraction as the first argument:

```haskell
> bang $ (1/2) !> (bd <> sn) -- play at half speed
```

By default, each primitive note in `Bang` has duration `1/4`. To set the duration for a composition, use `~~`:

```haskell
> bang $ 1 ~~ (bd <> bd) -- play two bass drum hits with a total duration of 1.
```

Composing polyrhythms can be done in three ways. The first, using `~=~`:

```haskell
-- play a 3/4 polyrhythm with bass drum triplets and snare quarter notes.
> bang $ (3, 3 #> bd) ~=~ (4, 4 #> sn)
```

The other two ways are by using `~=` and `=~`. These are slightly more general than the above case, and can be used for more than just polyrhythms. `a ~= b` smashes (or elongates) `b` into the duration of `a`, while `=~` does the same thing, but in the other direction. For example, the polyrhythm above can be more concisely represented like this:

```haskell
> bang $ (3 #> bd) =~ (4 #> sn)
```

To play the same rhythm in `3/4` duration, just use the other operator:

```haskell
> bang $ (3 #> bd) ~= (4 #> sn)
```

To play only the first part of a composition, use `takeDur` or `<<~`. To chop off a section of the end of a composition, use `dropDur` or `~>>`:

```haskell
> bang $ (1/4) <<~ ( 2 #> (bd <> sn) ) -- Play only `bd`
> bang $ (1/4) ~>> ( 2 #> (bd <> sn) ) -- Play bd, sn, bd
```

To instead silence parts of a composition, use `hushFor` (`~@>`) to silence fron the beginning, or `hushFrom` (`<@~`) to silence from some point until the end of a composition:

```haskell
> bang $ (1/4) ~@> ( 2 #> (bd <> sn) ) -- Play a quarter rest, then bd, sn, bd
> bang $ (1/4) <@~ ( 2 #> (bd <> sn) ) -- Play `bd`, then a 3/4 rest.
```

We can normalize a list of compositions to the same duration and play them sequentially using `normalize` or `<!>`:

```haskell
-- play each composition sequentially with duration 1.
> bang $ 1 <!> [bd, 3 #> sn, 5 #> hc]
```

We can do the same, but play each in parallel using `normalizeC` (C for 'Concurrent') or `>!<`:

```haskell
-- play each composition concurrently with duration 1.
> bang $ 1 >!< [bd, 3 #> sn, 5 #> hc]
```

We can map an operator over a list of compositions using `>>~`:

```haskell
-- Play each note in the list twice, sequentially.
> bang $ (2 #>) >>~ [sn, bd, hc]
```

#### Other transformations

`reverseMusic` does just what you'd expect:

```haskell
> bang $ reverseMusic $ bd <> sn -- snare, then bass.
```

`mirror` plays a composition forward, then backward. `mirrorR` plays it backwards, then forwards:

```haskell
> bang $ mirror  $ bd <> sn -- bd, sn, sn, bd
> bang $ mirrorR $ bd <> sn -- sn, bd, bd, sn
```

`cross` plays a composition both forward and backward at the same time:

```haskell
> bang $ cross $ bd <> sn -- (bd & sn), (bd & sn)
```

`rep` repeats a composition ad infinitum:

```haskell
> bang $ rep bd -- bd, bd, bd, bd ...
```

`m4` is a convenience constructor for 4-element compositions (useful for piecing together 4-element measures):

```haskell
> bang $ m4 bd hc bd sn -- bd, hc, bd, sn
```

Compositions form two monoids: one under `<>` (sequential composition) and one under `><` (parallel composition). To avoid wrapping everything in newtypes, the `<>` monoid is the 'real' one, and the `><` one uses similar names to the "real" monoid names.

Of particular note are the "concat" functions. `mconcat` plays a list of compositions in sequence, while `cconcat` plays a list of compositions in parallel:

```haskell
> bang $ mconcat [bd, sn, hc, sn] -- bd, sn, hc, sn in sequence
> bang $ cconcat [bd, sn, hc]     -- bd, sn, hc all at once 
```

All of the operators reside in [Bang.Music.Operators](https://github.com/5outh/Bang/blob/master/src/Bang/Music/Operators.hs), with most underlying implementations (and plain text functions) in [Bang.Music.Transform](https://github.com/5outh/Bang/blob/master/src/Bang/Music/Transform.hs).

All of the primitive sounds (such as `bd`, `sn` and `hc` are implemented in [Bang.Interface.Drum](https://github.com/5outh/Bang/blob/master/src/Bang/Interface/Drum.hs). This includes all of the MIDI percussion sounds, with the more common ones having shortform and longform names. All of these can be used in Bang compositions.


### Implementation Details and Acknowlegements

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
