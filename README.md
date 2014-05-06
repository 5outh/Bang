Bang
====

An *E*mbedded *D*omain *s*pecific *L*anguage for wwriting drum machine patterns in Haskell.

Todos:
- Move notes to rational durations
- Interpolation of sequences ("stretch to fit")
- Toy with folds and see if things like this are possible:

```haskell
foldM_ (&) $ do
  (snare >> bass)
  hc
```

...and see if it's the same as `(snare >> bass) & hc`

- Prove stuff about the associativity of operators and draw inspiration from the polymorphic media paper
- Add more instruments.
