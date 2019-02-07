{-|
Module      : Bang.Music.Operators
Description : The DSL part of the Bang library
Copyright   : (c) Benjamin Kovach, 2014
License     : MIT
Maintainer  : bkovach13@gmail.com
Stability   : experimental
Portability : Mac OSX

Defines a number of operators to effectively piece together Bang compositions.
-}
module Bang.Music.Operators where

import           Bang.Interface.Base
import           Bang.Music.Class
import           Bang.Music.Transform
import           Data.Foldable        (foldMap)

-- |Infix operator for `cappend`
infixr 6 ><
(><) :: Music a -> Music a -> Music a
(><) = cappend

infixr 0 !>
-- |Set the `Tempo` of a composition (default 1)
--
--  Example (play 4 bass drum hits at double speed):
--
-- > 2 !> (4 #> bd)
(!>) :: Rational -> Music a -> Music a
(!>) = tempo

infixr 1 #>
-- |Infix operator for 'repl'
--
-- Example (play a bass drum twice):
--
-- > 2 #> bd
(#>) :: Int -> Music a -> Music a
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
(~=~) :: (Rational, Music b) -> (Rational, Music b) -> Music b
(~=~) = poly

infixl 2 ~=
-- |Infix operator for 'fitL'
--
-- Example (a 3\/4 polyrhythm with duration 3\/4):
--
-- > (3 #> bd) ~= (4 #> sn)
(~=) :: Music b -> Music b -> Music b
(~=) = fitL

infixr 2 =~

-- |Infix operator for 'fitR'
--
-- Example (a 3\/4 polyrhythm with duration 1:
--
-- > (3 #> bd) =~ (4 #> sn)
(=~) :: Music b -> Music b -> Music b
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
(~~) :: Rational -> Music b -> Music b
(~~) = withDuration

infixr 2 <<~
-- |Infix operator for 'takeDur'
--
-- Example (Only play 2 bass drum hits):
--
-- > (1/2) <<~ (4 #> bd)
(<<~) :: Rational -> Music b -> Music b
(<<~) = takeDur

infixr 2 ~>>
-- |Infix operator for 'dropDur'
--
-- Example (play 2 closed hi-hats):
--
-- > (1/2) ~>> ( (2 #> bd) <> (2 #> hc) )
(~>>) :: Rational -> Music b -> Music b
(~>>) = dropDur

infixr 2 <@~
-- |Infix operator for 'hushFor'
--
-- Example (half rest, then two closed hi-hats):
--
-- > (1/2) ~@> ( (2 #> bd) <> (2 #> hc) )
(~@>) :: Rational -> Music b -> Music b
(~@>) = hushFor

infixr 2 ~@>
-- |Infix operator for 'hushFrom'
--
-- Example (two bass drum hits, then a half rest):
--
-- > (1/2) <@~ ( (2 #> bd) <> (2 #> hc) )
(<@~) :: Rational -> Music b -> Music b
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
(<!>) :: Rational -> [Music b] -> Music b
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
(>!<) :: Rational -> [Music b] -> Music b
(>!<) = normalizeC
