module Bang.Interface.Base where

import Bang.Music
import Bang.Music.Class
import Data.Ratio
import Control.Applicative
import Data.Monoid

quarter :: (Rational -> Music Rational) -> Composition
quarter m = pure $ m (1%4)

rest :: Composition
rest = pure (Rest $ 1 % 4)

r :: Composition
r = rest

measure4 :: Composition
         -> Composition
         -> Composition
         -> Composition
         -> Composition
measure4 c1 c2 c3 c4 = foldr1 (<>) [c1, c2, c3, c4]