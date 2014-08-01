{-# LANGUAGE RebindableSyntax, NoMonomorphismRestriction #-}

import Bang
import Data.Monoid
import Data.Ratio
import Prelude hiding ((>>))

music = do
  m4 bd bd bd bd
  hc
  m4 bd bd bd bd >< m4 hc hc hc hc
  where (>>) = (<>)

music2 = bang $ do
  bd <> bd
  hc <> cc
  where (>>) = (><)