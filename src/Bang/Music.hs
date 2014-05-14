module Bang.Music where

import Bang.Music.Class
import Bang.Music.MDrum
import System.MIDI
import Data.Ratio
import Prelude hiding (foldr, foldl, foldr1, foldl1)
import Data.Foldable
import Data.Traversable

withDuration :: Duration -> Composition -> Composition
withDuration d = (fmap . fmap) (const d)

-- mapDurationF = fmap
-- mapDuration  = fmap . fmap
