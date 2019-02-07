{-|
Module      : Bang.Music.Class
Description : Data declarations for Bang
Copyright   : (c) Benjamin Kovach, 2014
License     : MIT
Maintainer  : bkovach13@gmail.com
Stability   : experimental
Portability : Mac OSX

Implements the core data structures for use in the Bang library.
-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Bang.Music.Class where

import           Prelude       hiding (foldr)

import           Data.Foldable


-- | Primitive objects in music are simply notes with duration and type, or rests with only duration.
data Primitive a =
    -- | A `Note` with duration `dur` and type `ntype`
    Note {duration :: Rational, noteType :: a}
    -- | A `Rest` with a duration
  | Rest {duration :: Rational}
    deriving (Show, Eq)

mapDuration :: (Rational -> Rational) -> Primitive a -> Primitive a
mapDuration f (Note dur a) = Note (f dur) a
mapDuration f (Rest dur  ) = Rest (f dur)

instance Functor Primitive where
  fmap f (Note d a) = Note d (f a)
  fmap _ (Rest d)   = Rest d

-- | A musical composition with duration type dur (typically `Dur`) and instrument type `a` (typically a `PercussionSound`)
data Music a =
    -- | A Primitive musical object.
    Prim (Primitive a)
    -- | Sequential composition of music
  | Music a :+: Music a
    -- | Parallel composition of music
  | Music a :=: Music a
    -- | Modifier (typically 'BPM' or 'Tempo' change)
  | Modify Control (Music a)
    deriving (Show, Eq)

mapMusicDuration :: (Rational -> Rational) -> Music a -> Music a
mapMusicDuration f = \case
  Prim p        -> Prim $ mapDuration f p
  ma     :+: mb -> mapMusicDuration f ma :+: mapMusicDuration f mb
  ma     :=: mb -> mapMusicDuration f ma :+: mapMusicDuration f mb
  Modify c   ma -> Modify c (mapMusicDuration f ma)

-- | Simple data type representing different control structures for compositions.
data Control =
    BPM Integer
    -- ^ Set the beats per minute (WARNING: Only set this once, or BPM will multiply!)
  | Tempo Rational
    -- ^ Set the speed for a section of music (default 1)
  | Instrument InstrumentName
    -- ^ Change the instrument (currently unused)
    deriving (Show, Eq)


instance Semigroup (Music a) where
  (<>) = (:+:)

instance Monoid (Music a) where
  mempty = Prim (Rest 0)

instance Functor Music where
  fmap f (Prim m)     = Prim (fmap f m)
  fmap f (a :+: b)    = fmap f a :+: fmap f b
  fmap f (a :=: b)    = fmap f a :=: fmap f b
  fmap f (Modify c a) = Modify c (fmap f a)

{-
  `foldMap` folds over parameterized type (typically a Drum),
  and `bifoldMap` folds over duration as well.
-}
instance Foldable Music where
  foldMap _ (Prim (Rest _))   = mempty
  foldMap f (Prim (Note _ a)) = f a
  foldMap f (a :+: b)         = foldMap f a `mappend` foldMap f b
  foldMap f (a :=: b)         = foldMap f a `mappend` foldMap f b
  foldMap f (Modify _ a)      = foldMap f a

-- | Simple data type representing the types of instruments Bang supports.
--
-- Currently, the only value is 'DrumSet'.
data InstrumentName = DrumSet
  deriving (Show, Eq)

-- | Get the duration of a full composition
musicDuration :: Music a -> Rational
musicDuration (a :+: b) = musicDuration a + musicDuration b
musicDuration (a :=: b) = max (musicDuration a) (musicDuration b)
musicDuration (Modify (Tempo n) m) =
  musicDuration (mapMusicDuration (* fromRational n) m)
musicDuration (Modify _ m     ) = musicDuration m
musicDuration (Prim (Note d _)) = d
musicDuration (Prim (Rest d  )) = d

-- | Parallel 'mappend'
--
-- Part of a second 'Monoid' "instance" for 'Music'
cappend :: Music a -> Music a -> Music a
cappend = (:=:)

-- | Parallel 'mempty'
--
-- Part of a second 'Monoid' "instance" for 'Music'
cempty :: Music a
cempty = Prim (Rest 0)

-- | Parallel 'mconcat'
--
-- Part of a second 'Monoid' "instance" for 'Music'
cconcat :: [Music a] -> Music a
cconcat = foldr cappend cempty
