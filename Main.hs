import Series.Types
import Series.Combinators
import Series.Prelude
import Prelude hiding (map, filter, drop, take, sum
                      , iterate, repeat, replicate
                      , splitAt, mapM, takeWhile)
import qualified Prelude as P
import Data.Functor.Identity
import Control.Monad
import Control.Monad.Morph
import System.Environment
import System.IO 
main = getFold (foldSeries stdinLn) 
                   (\(str :> x) -> putStrLn str >> x) 
                   join 
                   return
{-
gain = do (a:sn:_) <- getArgs -- pain where
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
                 )))) :: Series (Of Int) IO ())  >>= print

    g :: Int -> IO ()
    g n = print $ sumG ( 
                 (takeWhileG (< n)
                  (dropG 100
                    (mapG (\x ->  3*x + 1)
                    (filterG even
                   (iterateG (\x -> x+1) (10 :: Int) )
                 )))) :: Series (Of Int) Identity ()) 

    x :: Int -> IO ()
    x n = sum ( 
                 (takeWhile (< n)
                  (drop 100
                    (map (\x -> 3*x + 1)
                    (filter even
                   ((iterate (\x -> x+1) (10 :: Int) ) :: Series (Of Int) IO ())
                  )))))  >>= print

    l n = print $ P.sum (
      (P.takeWhile (< n)
       (P.drop 100
         (P.map (\x -> 3*x + 1)
         (P.filter even
        ((P.iterate (\x -> x+1) (10 :: Int) ) )
       )))))  + 1


b = mapF show $ takeWhileF (< 14) $ dropF 1 
              $ filterF even $ iterateF (\x -> x+1) (0 :: Int) 
a = mapG show $ takeWhileG (< 14) $ dropG 1 
              $ filterG even $ iterateG (\x -> x+1) (0 :: Int) 
              
-}