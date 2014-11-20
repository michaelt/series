import ListM.Types
import ListM.Combinators
import ListM.Fusion
import Prelude hiding (map, filter, drop, take, sum
                      , iterate, repeat, replicate
                      , splitAt, mapM, takeWhile)
import qualified Prelude as P
import Data.Functor.Identity
import Control.Monad.Morph
import System.Environment
main = do (a:sn:_) <- getArgs -- pain where
          let n = 1000 * read sn :: Int
          case a of  "f" -> f n
                     "g" -> g n
                     "x" -> x n
                     "l" -> l n
                     _      -> putStrLn "f g x l [Int]" 
  where
    f :: Int -> IO ()
    f n = sumF ( 
                 (takeWhileF (< n)
                  (dropF 100
                    (mapF (\x ->  3*x + 1)
                    (filterF even
                   (iterateF (\x -> x+1) (10 :: Int) )
                 )))) :: ListM (Of Int) IO ())  >>= print

    g :: Int -> IO ()
    g n = print $ sumG ( 
                 (takeWhileG (< n)
                  (dropG 100
                    (mapG (\x ->  3*x + 1)
                    (filterG even
                   (iterateG (\x -> x+1) (10 :: Int) )
                 )))) :: ListM (Of Int) Identity ()) 

    x :: Int -> IO ()
    x n = sum ( 
                 (takeWhile (< n)
                  (drop 100
                    (map (\x -> 3*x + 1)
                    (filter even
                   ((iterate (\x -> x+1) (10 :: Int) ) :: ListM (Of Int) IO ())
                  )))))  >>= print
                  
                  
    l n = print $ P.sum (
      (P.takeWhile (< n)
       (P.drop 100
         (P.map (\x -> 3*x + 1)
         (P.filter even
        ((P.iterate (\x -> x+1) (10 :: Int) ) )
       )))))  + 1