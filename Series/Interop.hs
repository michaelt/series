{-# LANGUAGE LambdaCase, RankNTypes #-}

module Series.Interop where
import Series.Types
import Control.Monad
import Data.Functor.Identity
import qualified Control.Monad.Trans.Free as Free  
import Control.Monad.Trans.Free ( FreeT(..), FreeF(Free) )
import Pipes 
import Pipes.Internal 
import GHC.Exts ( build )
-- ----------------
-- Producer interop
-- ----------------

producerToSeries :: Monad m => Producer a m r -> Series (Of a) m r
producerToSeries =  loop where
  loop = \case M mp         -> Wrap (liftM loop mp)
               Pure r       -> Done r
               Respond a go -> Construct (a :> loop (go ()))
               Request x f  -> closed x


seriesToProducer :: Monad m =>  Series (Of a) m r -> Producer a m r 
seriesToProducer = loop where
  loop = \case Wrap m              -> M (liftM loop m)  
               Construct (a :> as) -> Respond a (\() -> loop as)
               Done r              -> Pure r

-- -----

producerToFold_ :: Monad m => Producer a m r -> Fold_ (Of a) m r
producerToFold_ prod = \construct wrap done -> 
  let loop = \case M mp         -> wrap (liftM loop mp)
                   Pure r       -> done r
                   Respond a go -> construct (a :> loop (go ()))
                   Request x f  -> closed x
  in loop prod 

buildProducer_ :: Monad m =>  Fold_ (Of a) m r -> Producer a m r 
buildProducer_ phi = phi (\(a :> p) -> yield a >> p) M return

-- -----

producerToFold :: Monad m => Producer a m r -> Fold (Of a) m r
producerToFold  p = Fold (producerToFold_ p)

buildProducer :: Monad m =>  Fold (Of a) m r -> Producer a m r 
buildProducer phi = buildProducer_ (getFold phi)

-- ----------------
-- FreeT interop
-- ----------------

foldFree_ :: (Functor f, Monad m) => FreeT f m t -> Fold_ f m t
foldFree_ f construct wrap done = outer f where
   outer = wrap
         . liftM (\case Free.Pure r -> done r
                        Free fr     -> construct (fmap outer fr)) 
         . runFreeT


buildFree_ :: Monad m => Fold_ f m r -> FreeT f m r 
buildFree_ phi = phi (FreeT . return . Free) 
                     (FreeT . (>>= runFreeT )) 
                     (FreeT . return . Free.Pure)
                     
-- standard foldr order
freeFold
  :: (Functor f, Monad m) =>
      (f r' -> r') -> (m r' -> r') -> (t -> r') -> FreeT f m t -> r'
freeFold construct wrap done = 
  wrap 
  . liftM (\case Free.Pure r -> done r
                 Free free_  -> construct (fmap (freeFold construct wrap done) free_)) 
  . runFreeT

-- ---------------------
-- haskell list interop
-- ---------------------
seriesToList :: Series (Of t) Identity () -> [t]
seriesToList  = \case Wrap (Identity ls)  -> seriesToList ls
                      Construct (a :> ls) -> a : seriesToList ls
                      Done ()             -> []

foldToList, foldToList' :: Fold (Of a) Identity () -> [a]
foldToList  phi = buildList (getFold phi)
foldToList' phi = buildList' (getFold phi)

buildList, buildList' :: Fold_ (Of a) Identity () -> [a]
buildList phi = build (\cons nil -> phi (\(x :> xs) -> cons x xs) 
                                        runIdentity 
                                        (\() -> nil))
buildList' phi = phi (\(x :> xs) -> x : xs) 
                     runIdentity 
                     (\() -> [])



seriesFromList :: [a] -> Series (Of a) Identity ()
seriesFromList  []    = Done ()
seriesFromList  (x:xs) = Construct (x :> seriesFromList xs)

toSeriesM :: Monad m => Fold_ (Of a) m r -> m ([a],r)
toSeriesM phi = phi construct join (\r -> return ([], r)) where
  construct (a :> mls) = do (as,r) <- mls
                            return (a : as, r)
