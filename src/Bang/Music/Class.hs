{-# LANGUAGE DeriveFunctor, OverlappingInstances, TypeSynonymInstances, FlexibleInstances #-}

module Bang.Music.Class (
  Music(..),
  Composition,
  Delay,
  Duration,
  takeZeros,
  dropZeros,
  equivZeros,
  sortComposition,
  interpret,
  normalizeConcurrents,
  catRests,
  elems
) where

import Bang.Music.MDrum
import Control.Monad.Free
import System.MIDI
import Data.Ratio
import Data.List(sortBy, sort, foldl')

type Delay = Integer
type Duration = Rational

-- |A data type for `Music` representing `Drum` events, `Rest`s and the end of a song.
data Music a = 
   Rest  {dur :: Duration, next :: a}
 | MDrum {drum :: Drum, dur :: Duration, next :: a}
 | End
    deriving (Show, Eq, Ord, Functor)

-- |A data type for a `Composition` consisting of a stream of `Music`s
type Composition = Free Music ()

showComposition :: Composition -> String
showComposition (Pure _)   = "Pure ()"
showComposition (Free End) = "Free End"
showComposition (Free x) = case x of
  MDrum d dur n -> (show d  ++ " : " ++ show dur ++ "\n") ++ showComposition n
  Rest    dur n -> ("Rest " ++ " : " ++ show dur ++ "\n") ++ showComposition n

elems :: Composition -> [Music ()]
elems (Pure ())             = []
elems (Free End)            = [End]
elems (Free (MDrum dr d n)) = MDrum dr d () : elems n
elems (Free (Rest d n))     = Rest d ()     : elems n

fromList :: [Music ()] -> Composition
fromList ms = foldr (>>) (return ()) $ map liftF ms

-- NOTE: THESE ARE VERY SPECIALIZED FUNCTIONS FOR TESTING EQUIVALENCY, 
--       AND PROBABLY DON'T WORK THE WAY YOU THINK. They are REALLY only
--       useful for testing equivalency and not recommended for use outside
--       of this module.
takeZeros :: Composition -> Composition
takeZeros (Free End) = return ()
takeZeros (Pure _)   = return ()
takeZeros (Free (MDrum dr d n))
  | d == 0    = liftF (MDrum dr d ()) >> takeZeros n
  | otherwise = liftF (MDrum dr 0 ())
takeZeros (Free (Rest d n))
  | d == 0    = takeZeros n -- ignore 0-rests!
  | otherwise = liftF (Rest 0 ())

dropZeros :: Composition -> Composition
dropZeros (Free End) = return ()
dropZeros (Pure _)   = return ()
dropZeros (Free (MDrum dr d n))
  | d == 0    = dropZeros n
  | otherwise = n
dropZeros (Free (Rest d n))
  | d == 0    = dropZeros n
  | otherwise = n

equivZeros :: Composition -> Composition -> Bool
equivZeros m n = (length elemsM == length elemsN)
          && all (`elem` elemsM) elemsN
  where elemsM = elems $ takeZeros m
        elemsN = elems $ takeZeros n

takeZeros' :: Composition -> Composition
takeZeros' (Free End) = return ()
takeZeros' (Pure _)   = return ()
takeZeros' x@(Free (MDrum dr d n))
  | d == 0    = liftF (MDrum dr d ()) >> takeZeros' n
  | otherwise = liftF (MDrum dr d ())
takeZeros' r@(Free (Rest d n))
  | d == 0    = takeZeros' n
  | otherwise = liftF (Rest d ())

mapDurationF :: (Duration -> Duration) -> Composition -> Composition
mapDurationF _ (Pure r) = return ()
mapDurationF f (Free x) = case x of
  (MDrum dr d a) -> Free (MDrum dr (f d) $ mapDurationF f a)
  (Rest d a)     -> Free (Rest (f d) $ mapDurationF f a)
  End            -> return ()

-- |Sets the duration for all notes in a `Composition`
withDuration :: Duration -> Composition -> Composition
withDuration d = mapDurationF (const d)

sortComposition :: Composition -> Composition
sortComposition (Pure _)   = return ()
sortComposition (Free End) = return ()
sortComposition x@(Free (MDrum dr d n)) = do
  let els = elems (takeZeros' x)
      sorted = sort els
      lst = last els
  withDuration 0 (fromList $ init sorted)
  withDuration (dur lst) (liftF $ last sorted)
  sortComposition (dropZeros x)

sortComposition x@(Free (Rest d n)) = do
  let els = elems (takeZeros' x)
      sorted = sort els
      lst = last els
  withDuration 0 (fromList $ init sorted)
  withDuration (dur lst) (liftF $ last sorted)
  sortComposition (dropZeros x)

mCompare :: Music () -> Music () -> Ordering
mCompare End End = EQ
mCompare a   End = LT
mCompare End b   = GT
mCompare (Rest d _) (Rest d' _) = compare d d'
mCompare (MDrum dr d _) (MDrum dr' d' _) = 
  let cmp = compare d d'
  in case cmp of
     EQ -> compare dr dr'
     _  -> cmp
mCompare a@(Rest d _) b@(MDrum dr d' _) = 
  let cmp = compare d d'
  in case cmp of
     EQ -> compare a b
     _  -> cmp
mCompare a@(MDrum dr d _) b@(Rest d' _) = 
  let cmp = compare d d'
  in case cmp of
     EQ -> compare a b
     _  -> cmp

-- |Converts a `Composition` to a normalized, increasing sequence of notes
interpret :: Composition -> [Music ()]
interpret = catRests
          . normalizeConcurrents
          . scanl1 addDuration 
          . elems
  where isRest (Rest _ _) = True
        isRest _          = False
        addDuration End            End = End
        addDuration End c = c
        addDuration c     (Rest d n) = Rest (d + dur c) n
        addDuration c (MDrum dr d n) = MDrum dr (d + dur c) n
        addDuration a b = addDuration b a

--NB. Doesn't account for trailing rests yet
catRests ((Rest _ _):xs)               = catRests xs
catRests (x:xs)                        = x : catRests xs
catRests []                            = []

normalizeConcurrents []       = []
normalizeConcurrents a@(x:xs) =
  case x of
    End ->  []
    _   ->  let (z, ys) = span (\y -> case y of 
                                      End -> False
                                      _   -> dur y == dur x) $ a
            in case ys of 
                  [] -> sortBy mCompare z
                  (b:zs) -> let d = dur b 
                                sorted = sortBy mCompare (z ++ [b{ dur = dur x }])
                            in init sorted ++ [(last sorted){ dur = d }] ++ normalizeConcurrents zs
                    

instance Show Composition where
  show = showComposition

instance Eq Composition where
  x == y = interpret x == interpret y
