{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
import Test.QuickCheck
import Bang

import Control.Applicative
import Data.Ratio

{- @TODO: Prove the eight laws of polymorphic temporal media:
  
  (!!1)
  >> is associative (for free: http://en.wikibooks.org/wiki/Haskell/Understanding_monads#Monad_Laws)
  m1 >> (m2 >> m3) == (m1 >> m2) >> m3

  (2)
  & is associative
  (m1 & m2) & m3 == m1 & (m2 & m3)

  (!!3)
  & is commutative
  (m1 & m2) == (m2 & m1)

  (!!4)
  return () >> m == m
  return () >>= \_ -> m
  (\_ -> m) ()
  = m
  
  (!!5) 
  m >> return () == m

  m >> return () == m
  m >>= \_ -> return ()
  
  (6*)
  rest* d & m === m, if d == dur m **
    ** actually d <= dur m in this case.
  
  (7??)
  rest d1 >> rest d2 
         ===
    rest (d1 >> d2)
  
  (8) seq/concurrent switch
  (m1 >> m2) & (m3 >> m4)
            === 
  (m1 & m3) >> (m2 & m4)

  MONAD LAWS:
  (1) return a >>= f == f a
  (2) m >>= return == m
  (3)    (m >>= f) >>= g
               ==
      (m >>= (\x -> f x >>= g))

  m >> k = m >>= \_ -> k
-}


compos  = [snare, bass, rest, hc]
seqs xs = (>>) <$> xs <*> xs
ands xs = (&)  <$> xs <*> xs

allCompos :: [Composition]
allCompos = go 1 compos
  where go 0 xs = xs
        go n xs = go (n-1) $ ( (++) <$> seqs <*> ands ) (xs ++ map triplets xs)

instance Arbitrary Composition where
  arbitrary = elements allCompos

-- check
associative_seq :: Composition -> Composition -> Composition -> Bool
associative_seq c1 c2 c3 = ((c1 >> c2) >> c3) == (c1 >> (c2 >> c3))

-- incorrect, check a & (b & b) vs (a & b) & b
associative_and :: Composition -> Composition -> Composition -> Bool
associative_and c1 c2 c3 = ((c1 & c2) & c3) == (c1 & (c2 & c3))

-- check
symmetric_and :: Composition -> Composition -> Bool
symmetric_and c1 c2 = (c1 & c2) == (c2 & c1)

-- check
right_zero :: Composition -> Bool
right_zero c = c == (c >> return ())

-- check
left_zero :: Composition -> Bool
left_zero c = c == (return () >> c)

-- This is absolutely incorrect,
-- (withDuration 0 bd >> bd) & rt 1 == withDuration 0 (bd >> bd) >> rt 1
rest_idempotency :: Composition -> Duration -> Bool
rest_idempotency c d = (c & rt d) == c

switch :: Composition -> Composition -> Composition -> Composition -> Bool
switch c1 c2 c3 c4 = ( (c1 >> c2) & (c3 >> c4) ) == ( (c1 & c3) >> (c2 & c4) )

runTests = do
  putStrLn "Testing associative_seq"
  quickCheck associative_seq
  putStrLn "Testing associative_and"
  quickCheck associative_and
  putStrLn "Testing symmetric_and"
  quickCheck symmetric_and
  putStrLn "Testing right_zero"
  quickCheck right_zero
  putStrLn "Testing left_zero"
  quickCheck left_zero
  putStrLn "Testing rest_idempotency"
  quickCheck rest_idempotency
  putStrLn "Testing switch"
  quickCheck switch
