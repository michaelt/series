module Main (main) where

import Control.Monad (void)
import Criterion.Main
import Series.Types
import Series.Combinators
import Series.Fusion
import Prelude hiding (map, filter, drop, take, sum
                      , iterate, repeat, replicate
                      , splitAt, mapM, takeWhile)
import qualified Prelude as P
import Data.Functor.Identity
import Pipes
import qualified Pipes.Prelude as PP


value :: Int
value = 10

-- long composition --

wrap :: Int -> Int
wrap = \n -> runIdentity $ sumF ( 
             (takeF n
              (dropF 100
                (mapF (\x ->  3*x + 1)
                (filterF even
               (iterateF (\x -> x+1) (10 :: Int) )
             )))) :: Series (Of Int) Identity ())  
{-# INLINE wrap #-}

raw :: Int -> Int
raw = \n -> runIdentity $ sumG ( 
             (takeG n
              (dropG 100
                (mapG (\x ->  3*x + 1)
                (filterG even
               (iterateG (\x -> x+1) (10 :: Int) )
             )))) :: Series (Of Int) Identity ()) 
{-# INLINE raw #-}

listM :: Int -> Int
listM = \n -> runIdentity $ sum ( 
             (take n
              (drop 100
                (map (\x -> 3*x + 1)
                (filter even
               ((iterate (\x -> x+1) (10 :: Int) ) :: Series (Of Int) Identity ())
              )))))  
{-# INLINE listM #-}  
            
list :: Int -> Int
list = \n ->  P.sum (
    (P.take n
     (P.drop 100
       (P.map (\x -> 3*x + 1)
       (P.filter even
      ((P.iterate (\x -> x+1) (10 :: Int) ) )
     ))))) 
{-# INLINE list #-}

pipe :: Int -> Int
pipe = \n -> runIdentity $ 
         PP.sum $ each (P.iterate (\x -> x+1) (10 :: Int) ) 
                  >-> PP.filter even
                  >-> PP.map (\x -> 3*x + 1)
                  >-> PP.drop 100
                  >-> PP.take n
{-# INLINE pipe #-}

-- shorter composition --

sh_wrap :: Int -> Int
sh_wrap = \n -> runIdentity $ sumF (takeF n (iterateF (\x -> x+1) (10 :: Int) :: Series (Of Int) Identity ()))
{-# INLINE sh_wrap #-}

sh_raw :: Int -> Int
sh_raw = \n -> runIdentity $ sumG (takeG n (iterateG (\x -> x+1) (10 :: Int) :: Series (Of Int) Identity ()))
{-# INLINE sh_raw #-}

sh_series :: Int -> Int
sh_series = \n -> runIdentity $ sum (take n (iterate (\x -> x+1) (10 :: Int) :: Series (Of Int) Identity ()))
{-# INLINE sh_series #-}

sh_list :: Int -> Int
sh_list = \n -> P.sum (P.take n (P.iterate (\x -> x+1) (10 :: Int)))
{-# INLINE sh_list #-}

sh_pipe :: Int -> Int 
sh_pipe = \n -> runIdentity $ 
           PP.sum (each (P.iterate (\x -> x+1) (10 :: Int) ) 
                   >-> PP.take n
                   )
{-# INLINE sh_pipe #-}                

-- simple sum --

rr :: Int -> Int -- Series (Of Int) Identity ()
rr = \n -> runIdentity (sumG (replicateG n 1))
{-# INLINE rr #-}

rw :: Int -> Int -- Series (Of Int) Identity ()
rw = \n -> runIdentity (sumF (replicateF n 1))
{-# INLINE rw #-}    

rlm :: Int -> Int -- Series (Of Int) Identity ()
rlm = \n -> runIdentity (sum (replicate n 1))
{-# INLINE rlm #-}

rl :: Int -> Int
rl = \n -> P.sum (P.replicate n 1)
{-# INLINE rl #-}

main :: IO ()
main =
  defaultMain
  [ bgroup "fusion"
      [ bench "raw" $ whnf raw value
      , bench "wrap" $ whnf wrap value
      , bench "listM" $ whnf listM value
      , bench "list" $ whnf list value
      , bench "pipe" $ whnf pipe value
      ]
  , bgroup "short"
      [ bench "raw" $ whnf sh_raw  value
      , bench "wrap" $ whnf sh_wrap value
      , bench "listM" $ whnf sh_series value
      , bench "list" $ whnf sh_list value
      , bench "pipe" $ whnf sh_pipe value
      ]
  , bgroup "shorter"
       [ bench "raw" $ whnf rr value
       , bench "wrap" $ whnf rw value
       , bench "listM" $ whnf rlm value
       , bench "list" $ whnf  rl value
       ]

  ]