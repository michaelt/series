import ListM.Types
import ListM.Combinators
import ListM.Fusion
import Prelude hiding (map, filter, drop, take, sum
                      , iterate, repeat, replicate, splitAt)
import Data.Functor.Identity
import Control.Monad.Morph

main :: IO ()
main = sumF ( 
             (takeF 1000000
              (dropF 100
                (mapF (\x -> 3*x + 1)
                (filterF even
               (iterateF (\x -> x+1) (10 :: Int) )
             )))))  >>= print
