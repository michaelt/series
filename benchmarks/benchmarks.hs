module Main (main) where

import Control.Monad (void)
import Criterion.Main
import Series.Types
import Series.Combinators
import Series.Prelude
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
wrap :: Int -> Int
wrap n = runIdentity $ sumF ( 
             (takeF n
              (dropF 100
                (mapF (\x ->  3*x + 1)
                (filterF even
               (iterateF (\x -> x+1) (10 :: Int) )
             )))) :: Series (Of Int) Identity ())  
{-# INLINE wrap #-}


listM :: Int -> Int
listM n = runIdentity $ sum ( 
             (take n
              (drop 100
                (map (\x -> 3*x + 1)
                (filter even
               ((iterate (\x -> x+1) (10 :: Int) ) :: Series (Of Int) Identity ())
              )))))  
{-# INLINE listM #-}  
            
list :: Int -> Int
list n = P.sum (
    (P.take n
     (P.drop 100
       (P.map (\x -> 3*x + 1)
       (P.filter even
      ((P.iterate (\x -> x+1) (10 :: Int) ) )
     ))))) 
{-# INLINE list #-}

vector :: Int -> Int
vector n =  V.sum (
    (V.take n
     (V.drop 100
       (V.map (\x -> 3*x + 1)
       (V.filter even
      ((V.iterateN (n*2+300) (\x -> x+1) (10 :: Int) ) )
     ))))) 
{-# INLINE vector #-}

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
short_wrap :: Int -> Int
short_wrap = \n -> runIdentity $ sumF (takeF n (iterateF (\x -> x+1) (10 :: Int) :: Series (Of Int) Identity ()))
{-# INLINE short_wrap #-}

short_series :: Int -> Int
short_series = \n -> runIdentity $ sum (take n (iterate (\x -> x+1) (10 :: Int) :: Series (Of Int) Identity ()))
{-# INLINE short_series #-}

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

rw :: Int -> Int 
rw n = runIdentity (sumF (replicateF n 1))
{-# INLINE rw #-}    

rlm :: Int -> Int 
rlm n = runIdentity (sum (replicate n 1))
{-# INLINE rlm #-}

rl :: Int -> Int
rl n =  P.sum (P.replicate n 1)
{-# INLINE rl #-}

rv :: Int -> Int
rv n = V.sum (V.replicate n 1)
{-# INLINE rv #-}

-- -----
-- enum
-- -----
z :: Int
z = 0

enum_wrap n = runIdentity (sumF (mapF (+7) (enumFromStepN z 10 (n*3))))
{-# INLINE enum_wrap #-}
enum_series n = runIdentity (sum (map (+7) (enumFromStepN z 10 (n*3))))
{-# INLINE enum_series #-}
enum_vector n = V.sum (V.map (+7) (V.enumFromStepN z 10 (n*3)))
{-# INLINE enum_vector #-}
enum_list n = P.sum (P.map (+7) (P.take (n*3) [z, 10 ..]))
{-# INLINE enum_list #-}
--
enum_wrap_dot  = runIdentity . sumF . mapF (+7) . enumFromStepN z 10 . (*3)
{-# INLINE enum_wrap_dot #-}
enum_series_dot = runIdentity . sum . map (+7) . enumFromStepN z 10 . (*3)
{-# INLINE enum_series_dot #-}
enum_vector_dot = V.sum . V.map (+7) . V.enumFromStepN z 10 . (*3)
{-# INLINE enum_vector_dot #-}
enum_list_dot = P.sum . P.map (+7) . (\n -> [z, 10 .. n*30])
{-# INLINE enum_list_dot #-}

main :: IO ()
main =
  defaultMain
  [ bgroup "sum.take.map.drop.filter.iterate"
      [ bench "vector" $ whnf vector value
      , bench "seriesbuildfoldr" $ whnf wrap value
      , bench "seriesnaive" $ whnf listM value
      , bench "list" $ whnf list value
      ]

  , bgroup "sum.take.iterate"
      [ bench "vector" $ whnf short_vector value
      , bench "seriesbuildfoldr" $ whnf short_wrap value
      , bench "seriesnaive" $ whnf short_series value
      , bench "list" $ whnf short_list value
      ]
  , bgroup "sum.replicate"
       [ bench "vector" $ whnf rv value
       , bench "seriesbuildfoldr" $ whnf rw value
       , bench "seriesnaive" $ whnf rlm value
       , bench "list" $ whnf  rl value
       ]
   , bgroup "sum.map.enumFromTo"
         [ bench "vector" $ whnf enum_vector value
         , bench "seriesbuildfoldr" $ whnf enum_wrap value
         , bench "seriesnaive" $ whnf enum_series value
         , bench "list" $ whnf enum_list value
         ]
   , bgroup "sum.map.enumFromTo.pointfree"
         [ bench "vector" $ whnf enum_vector_dot value
         , bench "seriesbuildfoldr" $ whnf enum_wrap_dot value
         , bench "seriesnaive" $ whnf enum_series_dot value
         , bench "list" $ whnf enum_list_dot value
         ]
             
  ]