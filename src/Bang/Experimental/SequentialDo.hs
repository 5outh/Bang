{-|
Module      : Bang.Experimental.SequentialDo
Description : Experimental module for sequential music composition with do-notation
Copyright   : (c) Benjamin Kovach, 2014
License     : MIT
Maintainer  : bkovach13@gmail.com
Stability   : experimental
Portability : Mac OSX

An experimental alternative to the base `Bang` module that allows you to compose music with `do` notation,
representing sequential application, e.g. @do{hc; bd} = hc <> bd@
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax  #-}
module Bang.Experimental.SequentialDo
  ( (>>)
  , module Bang
  )
where

import           Bang
import           Prelude hiding ((>>))

(>>) :: Monoid m => m -> m -> m
(>>) = (<>)
