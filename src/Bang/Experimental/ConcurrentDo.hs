{-|
Module      : Bang.Experimental.SequentialDo
Description : Experimental module for concurrent music composition with do-notation
Copyright   : (c) Benjamin Kovach, 2014
License     : MIT
Maintainer  : bkovach13@gmail.com
Stability   : experimental
Portability : Mac OSX

An experimental alternative to the base `Bang` module that allows you to compose music with `do` notation,
representing concurrent application, e.g. @do{hc; bd} = hc >< bd@
-}
{-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-}
module Bang.Experimental.ConcurrentDo((>>), module Bang) where

import Bang
import Prelude hiding ((>>))
import Data.Monoid

(>>) :: Music dur a -> Music dur a -> Music dur a
(>>) = (><)