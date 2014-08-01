{-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-}
module Bang.Experimental.SequentialDo((>>), module Bang) where

import Bang
import Prelude hiding ((>>))
import Data.Monoid

(>>) :: Monoid m => m -> m -> m
(>>) = (<>)