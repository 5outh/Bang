module Bang.Interface.Drum where

import Bang.Music
import Bang.Music.Drum
import Bang.Music.Class
import Data.Ratio
import Control.Applicative

snare, bass :: Composition
snare = pure (MDrum Snare (1%4))
bass  = pure (MDrum Bass (1%4))

tom :: TomType -> Composition
tom t = pure (MDrum (Tom t) (1%4))

cymbal :: CymbalType -> Composition
cymbal c = pure (MDrum (Cymbal c) (1%4))

hiHat :: Bool -> Composition
hiHat open = pure (MDrum (HiHat open) (1%4))