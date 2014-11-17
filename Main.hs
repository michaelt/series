import ListT.Types
import ListT.Combinators
import ListT.Fusion
import Prelude hiding (map, filter, drop, take, sum
                      , iterate, repeat, replicate, splitAt)


main :: IO ()
main = print $ buildListH
            ( 
             (take 10
              (drop 0
                (filter even
               (iterate (\x -> 3*x+1) (0 :: Int) :: Fold_ (Of Int) Identity ())
             ))))  
