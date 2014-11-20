module Main (main) where

import Control.Monad (void)
import Criterion.Main
import ListM.Types
import ListM.Combinators
import ListM.Fusion
import Prelude hiding (map, filter, drop, take, sum
                      , iterate, repeat, replicate
                      , splitAt, mapM, takeWhile)
import qualified Prelude as P
import Data.Functor.Identity
-- import Control.Monad.Morph
import System.Environment


value :: Int
value = 1000

wrap :: Int -> Int
wrap n = runIdentity $ sumF ( 
             (takeWhileF (< n)
              (dropF 100
                (mapF (\x ->  3*x + 1)
                (filterF even
               (iterateF (\x -> x+1) (10 :: Int) )
             )))) :: ListM (Of Int) Identity ())  

raw :: Int -> Int
raw n = runIdentity $ sumG ( 
             (takeWhileG (< n)
              (dropG 100
                (mapG (\x ->  3*x + 1)
                (filterG even
               (iterateG (\x -> x+1) (10 :: Int) )
             )))) :: ListM (Of Int) Identity ()) 

listM :: Int -> Int
listM n = runIdentity $ sum ( 
             (takeWhile (< n)
              (drop 100
                (map (\x -> 3*x + 1)
                (filter even
               ((iterate (\x -> x+1) (10 :: Int) ) :: ListM (Of Int) Identity ())
              )))))  
              
list :: Int -> Int
list n = P.sum (
    (P.takeWhile (< n)
     (P.drop 100
       (P.map (\x -> 3*x + 1)
       (P.filter even
      ((P.iterate (\x -> x+1) (10 :: Int) ) )
     ))))) 

--
shwrap :: Int -> Int
shwrap n = runIdentity $ sumF (takeF n (iterateF (\x -> x+1) (10 :: Int) :: ListM (Of Int) Identity ()))

shraw :: Int -> Int
shraw n = runIdentity $ sumG (takeG n (iterateG (\x -> x+1) (10 :: Int) :: ListM (Of Int) Identity ()))

shListM :: Int -> Int
shListM n = runIdentity $ sum (take n (iterate (\x -> x+1) (10 :: Int) :: ListM (Of Int) Identity ()))

shlist :: Int -> Int
shlist n = P.sum (P.take n (P.iterate (\x -> x+1) (10 :: Int)))

rr :: Int -> ListM (Of Int) Identity ()
rr = \n -> takeG (n-2) (replicateG n 1)
rw :: Int -> ListM (Of Int) Identity ()
rw = \n -> takeF (n-2) (replicateF n 1)
rlm :: Int -> ListM (Of Int) Identity ()
rlm = \n -> takeF (n-2) (replicate n 1)
rl :: Int -> [Int]
rl = \n -> P.take (n-2) (P.replicate n 1)

main :: IO ()
main =
  defaultMain
  [ bgroup "fusion"
      [ bench "raw" $ whnf raw value
      , bench "wrap" $ whnf wrap value
      , bench "listM" $ whnf listM value
      , bench "list" $ whnf list value
      
      ]
 , bgroup "short"
      [ bench "raw" $ whnf shraw value
      , bench "wrap" $ whnf shwrap value
      , bench "listM" $ whnf shListM value
      , bench "list" $ whnf shlist value
      ]
  , bgroup "shorter"
       [ bench "raw" $ whnf rr value
       , bench "wrap" $ whnf rw value
       , bench "listM" $ whnf rlm value
       , bench "list" $ whnf rl value
       ]
  -- , bgroup "dropWhile"
  --     [ bench "machines" $ whnf drainM (M.droppingWhile (<= value))
  --     , bench "pipes" $ whnf drainP (P.dropWhile (<= value))
  --     ]
  -- , bgroup "scan"
  --     [ bench "machines" $ whnf drainM (M.scan (+) 0)
  --     , bench "pipes" $ whnf drainP (P.scan (+) 0 id)
  --     , bench "conduit" $ whnf drainC (C.scanl (\a s -> let b = a+s in (b,b)) 0)
  --     ]
  -- , bgroup "take"
  --     [ bench "machines" $ whnf drainM (M.taking value)
  --     , bench "pipes" $ whnf drainP (P.take value)
  --     , bench "conduit" $ whnf drainSC (C.take value)
  --     ]
  -- , bgroup "takeWhile"
  --     [ bench "machines" $ whnf drainM (M.takingWhile (<= value))
  --     , bench "pipes" $ whnf drainP (P.takeWhile (<= value))
  --     ]
  -- , bgroup "fold"
  --     [ bench "machines" $ whnf drainM (M.fold (+) 0)
  --     , bench "pipes" $ whnf (P.fold (+) 0 id) sourceP
  --     ]
  ]