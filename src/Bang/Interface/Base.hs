module Bang.Interface.Base where

import Bang.Music.Class
import Data.Monoid

showMusic :: (Show a, Show b) => Music a b -> String
showMusic = go ""
  where go spcs (Prim m)     = spcs <> show m
        go spcs (a :+: b)    = mconcat [spcs, go (' ': spcs) a, "\n", spcs, ":+:", "\n", spcs, go (' ': spcs) b]
        go spcs (a :=: b)    = mconcat [spcs, go (' ': spcs) a, "\n", spcs, ":=:", "\n", spcs, go (' ': spcs) b]
        go spcs (Modify c a) = show c <> "\n" <> go spcs a

printMusic :: (Show a, Show b) => Music a b -> IO ()
printMusic = putStrLn . showMusic

rest :: Dur -> Music Dur a
rest d = Prim (Rest d)

note :: Dur -> a -> Music Dur a
note d x = Prim (Note d x)

bpm :: Integer -> Music a b -> Music a b
bpm n = Modify (BPM n)

sr, er, qr, hr, wr :: Music Dur a
sr = rest (1/16)
er = rest (1/8)
qr = rest (1/4)
hr = rest (1/2)
wr = rest 1