module Main (main) where

import Control.Monad (void)
import Criterion.Main
import Series.Types
import Series.Combinators
import Series.Prelude.Fused 
import qualified Series.Prelude.Naive as N 
import Prelude hiding (map, filter, drop, take, sum
                      , iterate, repeat, replicate
                      , splitAt, mapM, takeWhile)
import qualified Prelude as P
import Data.Functor.Identity
import Pipes
import qualified Pipes.Prelude as PP
import qualified Data.Vector.Unboxed as V

value :: Int
value = 100
big :: Int
big = 10000000

-- -------------------
-- long composition
-- -------------------


long_fused :: Int -> Int
long_fused  n = runIdentity $ sum ( 
             (take n
              (drop 100
                (map (\x -> 3*x + 1)
                (filter even
               ((iterate (\x -> x+1) (10 :: Int) ) :: Series (Of Int) Identity ())
              )))))  
{-# INLINE long_fused  #-}  

long_naive  :: Int -> Int
long_naive  n = runIdentity $ N.sum (
    (N.take n
     (N.drop 100
       (N.map (\x -> 3*x + 1)
       (N.filter even
      ((N.iterate (\x -> x+1) (10 :: Int) ) )
     ))))) 
{-# INLINE long_naive  #-}


long_list  :: Int -> Int
long_list  n = P.sum (
    (P.take n
     (P.drop 100
       (P.map (\x -> 3*x + 1)
       (P.filter even
      ((P.iterate (\x -> x+1) (10 :: Int) ) )
     ))))) 
{-# INLINE long_list  #-}

long_vector :: Int -> Int
long_vector n =  V.sum (
    (V.take n
     (V.drop 100
       (V.map (\x -> 3*x + 1)
       (V.filter even
      ((V.iterateN (n*2+300) (\x -> x+1) (10 :: Int) ) )
     ))))) 
{-# INLINE long_vector #-}

pipe :: Int -> Int
pipe n = runIdentity $ 
         PP.sum $ each (P.iterate (\x -> x+1) (10 :: Int) ) 
                  >-> PP.filter even
                  >-> PP.map (\x -> 3*x + 1)
                  >-> PP.drop 100
                  >-> PP.take n
{-# INLINE pipe #-}


-- -------------------
-- shorter composition
-- -------------------
short_naive :: Int -> Int
short_naive = \n -> runIdentity $ N.sum (N.take n (N.iterate (\x -> x+1) (10 :: Int) :: Series (Of Int) Identity ()))
{-# INLINE short_naive #-}

short_fused :: Int -> Int
short_fused = \n -> runIdentity $ sum (take n (iterate (\x -> x+1) (10 :: Int) :: Series (Of Int) Identity ()))
{-# INLINE short_fused #-}

short_list :: Int -> Int
short_list = \n -> P.sum (P.take n (P.iterate (\x -> x+1) (10 :: Int)))
{-# INLINE short_list #-}

short_vector :: Int -> Int
short_vector = \n -> V.sum (V.take n (V.iterateN (n*2) (\x -> x+1) (10 :: Int)))
{-# INLINE short_vector #-}

short_pipe :: Int -> Int 
short_pipe = \n -> runIdentity $ 
           PP.sum (each (P.iterate (\x -> x+1) (10 :: Int) ) 
                   >-> PP.take n
                   )
{-# INLINE short_pipe #-}                

-- -------------------
-- simple sum
-- -------------------

rN :: Int -> Int 
rN n = runIdentity (N.sum (N.replicate n 1))
{-# INLINE rN #-}    

rF :: Int -> Int 
rF n = runIdentity (sum (replicate n 1))
{-# INLINE rF #-}

rL :: Int -> Int
rL n =  P.sum (P.replicate n 1)
{-# INLINE rL #-}

rV :: Int -> Int
rV n = V.sum (V.replicate n 1)
{-# INLINE rV #-}

-- -----
-- enum
-- -----
z :: Int
z = 0

enum_naive n = runIdentity (N.sum (N.map (+7) (N.enumFromStepN z 10 (n*3))))
{-# INLINE enum_naive #-}
enum_fused n = runIdentity (sum (map (+7) (enumFromStepN z 10 (n*3))))
{-# INLINE enum_fused #-}
enum_vector n = V.sum (V.map (+7) (V.enumFromStepN z 10 (n*3)))
{-# INLINE enum_vector #-}
enum_list n = P.sum (P.map (+7) (P.take (n*3) [z, 10 ..]))
{-# INLINE enum_list #-}
--
enum_naive_dot  = runIdentity . N.sum . N.map (+7) . N.enumFromStepN z 10 . (*3)
{-# INLINE enum_naive_dot #-}
enum_fused_dot = runIdentity . sum . map (+7) . enumFromStepN z 10 . (*3)
{-# INLINE enum_fused_dot #-}
enum_vector_dot = V.sum . V.map (+7) . V.enumFromStepN z 10 . (*3)
{-# INLINE enum_vector_dot #-}
enum_list_dot = P.sum . P.map (+7) . (\n -> [z, 10 .. n*30])
{-# INLINE enum_list_dot #-}

main :: IO ()
main =
  defaultMain
  [ bgroup "sum.take.map.drop.filter.iterate"
      [ bench "vector" $ whnf long_vector value
      , bench "fused" $ whnf long_fused value
      , bench "naive" $ whnf long_naive value
      , bench "list" $ whnf long_list value
      ]

  , bgroup "sum.take.iterate"
      [ bench "vector" $ whnf short_vector value
      , bench "fused" $ whnf short_fused value
      , bench "naive" $ whnf short_naive value
      , bench "list" $ whnf short_list value
      ]
  , bgroup "sum.replicate"
       [ bench "vector" $ whnf rV value
       , bench "fused" $ whnf rF value
       , bench "naive" $ whnf rN value
       , bench "list" $ whnf  rL value
       ]
   , bgroup "sum.map.enumFromTo"
         [ bench "vector" $ whnf enum_vector value
         , bench "fused" $ whnf enum_fused value
         , bench "naive" $ whnf enum_naive value
         , bench "list" $ whnf enum_list value
         ]
   , bgroup "sum.map.enumFromTo.pointfree"
         [ bench "vector" $ whnf enum_vector_dot value
         , bench "fused" $ whnf enum_fused_dot value
         , bench "naive" $ whnf enum_naive_dot value
         , bench "list" $ whnf enum_list_dot value
         ]
             
  ]
  
