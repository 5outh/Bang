module Bang.Music.Operators where

import Bang.Music.Class
import Bang.Music.Transform
import Bang.Interface.Base
import Data.Foldable(foldMap)

import Data.Monoid

-- |Infix operator for `cappend`
infixr 6 ><
(><) :: Music dur a -> Music dur a -> Music dur a
(><) = cappend

infixr 0 !>
-- |Set the `Tempo` of a composition (default 1)
-- 
-- 
(!>) :: Rational -> Music a b -> Music a b
(!>) = tempo

infixr 1 #>
-- |Infix operator for 'repl'
--
-- Example (play a bass drum twice):
--
-- > 2 #> bd
(#>) :: Num a => Int -> Music a b -> Music a b
(#>) = repl

infixl 1 >>~
-- |Map a function over a list of compositions and sequentially compose them.
-- Note: This is just 'foldMap' specialized to the list Monoid.
--
-- Example (play 'sn', 't1' and 't2' all twice): 
--
-- > (2 #>) >>~ [sn, t1, t2]
(>>~) :: Monoid b => (a -> b) -> [a] -> b
(>>~) = foldMap

-- |Infix operator for 'poly'
-- 
-- Example (A 3\/4 polyrhythm):
--
-- > (3, 3 #> bd) ~=~ (4, 4 #> sn)
(~=~) :: (Dur, Music Dur b) -> (Dur, Music Dur b) -> Music Dur b
(~=~) = poly

infixl 2 ~=
-- |Infix operator for 'fitL'
--
-- Example (a 3\/4 polyrhythm with duration 3\/4):
-- 
-- > (3 #> bd) ~= (4 #> sn)
(~=) :: Music Dur b -> Music Dur b -> Music Dur b
(~=) = fitL

infixr 2 =~
-- |Infix operator for 'fitR'
-- 
-- Example (a 3\/4 polyrhythm with duration 1:
-- 
-- > (3 #> bd) =~ (4 #> sn)
(=~) :: Music Dur b -> Music Dur b -> Music Dur b
(=~) = fitR

infixr 2 ~~
-- |Infix operator for 'withDuration'
--
-- Example:
--
-- @
-- 2 ~~ mconcat [
--    16 #> bd
--  , 4 #> sn
--  , wr
--  ]
-- @
(~~) :: Dur -> Music Dur b -> Music Dur b
(~~) = withDuration

infixr 2 <<~
-- |Infix operator for 'takeDur'
--
-- Example (Only play 2 bass drum hits):
-- 
-- > (1/2) <<~ (4 #> bd)
(<<~) :: Dur -> Music Dur b -> Music Dur b
(<<~) = takeDur

infixr 2 ~>>
-- |Infix operator for 'dropDur'
--
-- Example (play 2 closed hi-hats):
-- 
-- > (1/2) ~>> ( (2 #> bd) <> (2 #> hc) )
(~>>) :: Dur -> Music Dur b -> Music Dur b
(~>>) = dropDur

infixr 2 <@~
-- |Infix operator for 'hushFor'
--
-- Example (half rest, then two closed hi-hats):
--
-- > (1/2) ~@> ( (2 #> bd) <> (2 #> hc) )
(~@>) :: Dur -> Music Dur b -> Music Dur b
(~@>) = hushFor

infixr 2 ~@>
-- |Infix operator for 'hushFrom'
--
-- Example (two bass drum hits, then a half rest):
--
-- > (1/2) <@~ ( (2 #> bd) <> (2 #> hc) )
(<@~) :: Dur -> Music Dur b -> Music Dur b
(<@~) = hushFrom

infixr 0 <!>
-- |Infix operator for 'normalize'
-- 
-- Example (Play 12 bass drum hits, then 4 closed hi-hats, then 3 snares, each within a single measure's time):
--
-- @
-- 1 \<!\> [
--     12 #> bd
--   , 4  #> hc
--   , 3  #> sn
--   ]
-- @
(<!>) :: Dur -> [Music Dur b] -> Music Dur b
(<!>) = normalize

infixr 0 >!<
-- |Infix operator for 'normalizeC'
-- 
-- Example: (Play 12 bass drum hits, then 4 closed hi-hats, then 3 snares, 
-- all concurrently within a single measure's time):
--
-- @
-- 1 >!< [
--     12 #> bd
--   , 4  #> hc
--   , 3  #> sn
--   ]
-- @
(>!<) :: Dur -> [Music Dur b] -> Music Dur b
(>!<) = normalizeC
