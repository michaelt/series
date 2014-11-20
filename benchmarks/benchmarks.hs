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

wrap :: Int -> Int
wrap n =  runIdentity $ sumF ( 
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

shwrap :: Int -> Int
shwrap n = runIdentity $ sumF (takeF n (iterateF (\x -> x+1) (10 :: Int) :: Series (Of Int) Identity ()))
{-# INLINE shwrap #-}
shraw :: Int -> Int
shraw = \n -> runIdentity $ sumG (takeG n (iterateG (\x -> x+1) (10 :: Int) :: Series (Of Int) Identity ()))
{-# INLINE shraw #-}
shSeries :: Int -> Int
shSeries = \n -> runIdentity $ sum (take n (iterate (\x -> x+1) (10 :: Int) :: Series (Of Int) Identity ()))
{-# INLINE shSeries #-}
shlist :: Int -> Int
shlist = \n -> P.sum (P.take n (P.iterate (\x -> x+1) (10 :: Int)))
{-# INLINE shlist #-}

shpipe :: Int -> Int 
shpipe = \n -> runIdentity $ 
           PP.sum (each (P.iterate (\x -> x+1) (10 :: Int) ) 
                   >-> PP.take n
                   )
{-# INLINE shpipe #-}                
rr :: Int -> Series (Of Int) Identity ()
rr n = takeG (n-2) (replicateG n 1)
{-# INLINE rr #-}
rw :: Int -> Series (Of Int) Identity ()
rw = \n -> takeF (n-2) (replicateF n 1)
{-# INLINE rw #-}
rlm :: Int -> Series (Of Int) Identity ()
rlm = \n -> takeF (n-2) (replicate n 1)
{-# INLINE rlm #-}
rl :: Int -> [Int]
rl = \n -> P.take (n-2) (P.replicate n 1)
{-# INLINE rl #-}

main :: IO ()
main =
  defaultMain
  [ bgroup "fusion"
      [ bench "raw" $ whnf (\x -> raw x) value
      , bench "wrap" $ whnf wrap value
      , bench "listM" $ whnf listM value
      , bench "list" $ whnf (\x -> list x) value
      , bench "pipe" $ whnf pipe value
      ]
  , bgroup "short"
      [ bench "raw" $ whnf (\x -> shraw x) value
      , bench "wrap" $ whnf shwrap value
      , bench "listM" $ whnf shSeries value
      , bench "list" $ whnf (\x -> shlist x) value
      , bench "pipe" $ whnf shpipe value
      ]
  , bgroup "shorter"
       [ bench "raw" $ whnf (\x -> rr x)value
       , bench "wrap" $ whnf rw value
       , bench "listM" $ whnf rlm value
       , bench "list" $ whnf (\x -> rl x) value
       ]

  ]