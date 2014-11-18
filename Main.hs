import ListM.Types
import ListM.Combinators
import ListM.Fusion
import Prelude hiding (map, filter, drop, take, sum
                      , iterate, repeat, replicate, splitAt)
import Data.Functor.Identity
import Control.Monad.Morph

main :: IO ()
main = sumF ( 
             (takeF 1000
              (dropF 100
              
                (mapMF (\x -> let y = 3*x in print y >> return y)
                (filterF even
               (iterateF (\x -> x+1) (10 :: Int) )
             )))))  >>= print
