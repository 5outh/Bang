module Bang.Music where

import Bang.Music.Class
import Bang.Music.MDrum
import System.MIDI
import Data.Ratio

withDuration :: Duration -> Composition -> Composition
withDuration d = (fmap . fmap) (const d)

-- mapDurationF = fmap
-- mapDuration  = fmap . fmap
