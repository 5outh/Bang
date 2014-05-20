module Bang.Interface.Drum where

import Bang.Music.Class

drum :: PercussionSound -> Dur -> Music Int
drum ps d = Prim ( (Note d (fromEnum ps + 35) ) )