{-# LANGUAGE NoMonomorphismRestriction #-}
module Bang.Music.Class where

import Prelude hiding(foldr)

import Data.Ratio
import Data.Monoid
import Data.Foldable
import Data.Bifunctor
import Data.Bifoldable

type Dur = Rational

-- |Primitive objects in music are simply notes with duration and type, or rests with only duration.
data Primitive d a = 
    -- | A `Note` with duration `dur` and type `ntype`
    Note {dur :: d, ntype :: a}
    -- | A `Rest` with duration `dur`
  | Rest {dur :: d}
    deriving (Show, Eq)

instance Functor (Primitive dur) where
  fmap f (Note d a) = Note d (f a)
  fmap f (Rest d)   = Rest d

-- | A musical composition with duration type dur (typically `Dur`) and instrument type `a` (typically a `PercussionSound`)
data Music dur a = 
    -- | A Primitive musical object.
    Prim (Primitive dur a)
    -- | Sequential composition of music
  | Music dur a :+: Music dur a
    -- | Parallel composition of music
  | Music dur a :=: Music dur a
    -- | Modifier (typically 'BPM' or 'Tempo' change)
  | Modify Control (Music dur a)
    deriving (Show, Eq)

-- | Simple data type representing different control structures for compositions.
data Control = 
    BPM Integer               
    -- ^ Set the beats per minute (WARNING: Only set this once, or BPM will multiply!)
  | Tempo Rational            
    -- ^ Set the speed for a section of music (default 1)
  | Instrument InstrumentName 
    -- ^ Change the instrument (currently unused)
    deriving (Show, Eq)

{-
  NB. `Music` under `:=:` also forms a monoid, so we'll give these similar names...
-}
instance Num dur => Monoid (Music dur a) where
  mappend = (:+:)
  mempty = Prim (Rest 0)

{-
  `fmap` (and `second`) maps over parameterized type (typically a Drum),
  and `first` maps over duration.
-}
instance Functor (Music dur) where
  fmap f (Prim m) = Prim (fmap f m)
  fmap f (a :+: b) = fmap f a :+: fmap f b
  fmap f (a :=: b) = fmap f a :=: fmap f b
  fmap f (Modify c a) = Modify c (fmap f a)

instance Bifunctor Music where
  bimap f g (Prim (Note dur a)) = Prim $ Note (f dur) (g a)
  bimap f g (Prim (Rest dur))   = Prim $ Rest (f dur)
  bimap f g (a :+: b) = bimap f g a :+: bimap f g b
  bimap f g (a :=: b) = bimap f g a :=: bimap f g b
  bimap f g (Modify c a) = Modify c (bimap f g a)

{-
  `foldMap` folds over parameterized type (typically a Drum),
  and `bifoldMap` folds over duration as well.
-}
instance Foldable (Music dur) where
  foldMap f (Prim (Rest _)) = mempty
  foldMap f (Prim (Note _ a)) = f a 
  foldMap f (a :+: b) = foldMap f a `mappend` foldMap f b
  foldMap f (a :=: b) = foldMap f a `mappend` foldMap f b
  foldMap f (Modify c a) = foldMap f a

instance Bifoldable Music where
  bifoldMap f g (Prim (Note dur a)) = f dur `mappend` g a
  bifoldMap f g (Prim (Rest dur)) = f dur
  bifoldMap f g (a :+: b) = bifoldMap f g a `mappend` bifoldMap f g b
  bifoldMap f g (a :=: b) = bifoldMap f g a `mappend` bifoldMap f g b
  bifoldMap f g (Modify c a) = bifoldMap f g a

-- | Simple data type representing the types of instruments Bang supports.
-- 
-- Currently, the only value is 'DrumSet'.
data InstrumentName = DrumSet
  deriving (Show, Eq)

-- | Get the duration of a full composition
duration :: (Fractional a, Ord a) => Music a b -> a
duration (a :+: b) = duration a + duration b
duration (a :=: b) = max (duration a) (duration b)
duration (Modify (Tempo n) m) = duration (first (* fromRational n) m)
duration (Modify _ m) = duration m
duration (Prim (Note d a)) = d
duration (Prim (Rest d)) = d

-- | Parallel 'mappend'
--
-- Part of a second 'Monoid' "instance" for 'Music'
cappend :: Music dur a -> Music dur a -> Music dur a
cappend = (:=:)

-- | Parallel 'mempty'
--
-- Part of a second 'Monoid' "instance" for 'Music'
cempty :: Num dur => Music dur a
cempty  = Prim (Rest 0)

-- | Parallel 'mconcat'
-- 
-- Part of a second 'Monoid' "instance" for 'Music'
cconcat :: Num dur => [Music dur a] -> Music dur a
cconcat = foldr cappend cempty
