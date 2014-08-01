{-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-}
module Bang.Experimental.ConcurrentDo((>>), module Bang) where

import Bang
import Prelude hiding ((>>))
import Data.Monoid

(>>) :: Music dur a -> Music dur a -> Music dur a
(>>) = (><)