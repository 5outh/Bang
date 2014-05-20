module Bang.Interface.Base where

import Bang.Music.Class

rest :: Dur -> Music a
rest d = Prim (Rest d)

note :: Dur -> a -> Music a
note d x = Prim (Note d x)

bpm :: Integer -> Music a -> Music a
bpm n = Modify (BPM n)