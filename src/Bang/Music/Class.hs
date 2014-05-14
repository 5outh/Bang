{-# LANGUAGE DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}

module Bang.Music.Class (
  Music(..),
  Media(..),
  Composition,
  Duration
) where

import Bang.Music.MDrum
import Data.Monoid
import Data.Foldable
import Data.Traversable
import Control.Applicative

type Duration = Rational

-- |A data type for `Music` representing `Drum` events, `Rest`s and the end of a song.
data Music a = 
   Rest  {dur :: a}
 | MDrum {drum :: Drum, dur :: a}
    deriving (Show, Eq, Ord, Functor)

instance Applicative Music where
  pure = Rest
  (Rest f) <*> d     = f <$> d
  (MDrum dr f) <*> d = f <$> d

{-| LAWS:
 - a :+: (b :+: c) = (a :+: b) :+: c
 - c :=: (b :=: c) = (a :=: b) :=: c
 - (a :=: b) = (b :=: a)
 - 0 :+: m = m
 - m :+: 0 = 0
 - m :=: 0 = m
 - 0 :=: m = m
 - rest 0 :+: m = m
 - m :+: rest 0 = m
 - rest d :=: m = m iff d == dur m
 - (a :+: b) :=: (c :+: d) 
 -            =
 - (a :=: c) :+: (b :=: d) iff dur a == dur c and dur b == dur d
-}
data Media a =
    Media a :+: Media a -- sequential composition
  | Media a :=: Media a -- parallel composition
  | Prim a              -- Only one
  | Zero                -- Nothing!
    deriving (Show, Eq, Ord, Functor)

type Composition = Media (Music Duration)

instance Monoid Composition where
  mempty  = Zero
  mappend = (:+:)

instance Foldable Media where
  foldMap f Zero     = mempty
  foldMap f (Prim a) = f a
  foldMap f (a :+: b) = foldMap f a `mappend` foldMap f b
  foldMap f (a :=: b) = foldMap f a `mappend` foldMap f b

-- @TODO: Figure out what the hell this means.
-- NB. Right now, inherits the first argument's operator.
instance Applicative Media where
  pure = Prim
  (Prim f)   <*> (Prim a)  = Prim $ f a
  x@(Prim f) <*> (a :=: b) = (x <*> a) :=: (x <*> b)
  x@(Prim f) <*> (a :+: b) = (x <*> a) :+: (x <*> b)
  -- these are still confusing, the above are correct I believe
  (a :+: b)  <*> (c :+: d) = (a <*> c) :+: (b <*> d)
  (a :+: b)  <*> (c :=: d) = (a <*> c) :+: (b <*> d)
  (a :=: b)  <*> (c :+: d) = (a <*> c) :=: (b <*> d)
  (a :=: b)  <*> (c :=: d) = (a <*> c) :=: (b <*> d)

instance Traversable Media where
  traverse f (Prim a)  = Prim  <$> f a
  traverse f (a :+: b) = (:+:) <$> traverse f a <*> traverse f b
  traverse f (a :=: b) = (:=:) <$> traverse f a <*> traverse f b

