{-# LANGUAGE LambdaCase, RankNTypes #-}
module ListM.Combinators where
import ListM.Types

import Control.Monad
import Control.Monad.Trans
import Control.Applicative 
import Data.Data ( Data, Typeable )
import Data.Foldable ( Foldable )
import Data.Traversable 
import Pipes 
import Pipes.Internal 
import Control.Monad.Morph

import Data.Functor.Identity
import GHC.Exts ( build )
import Control.Monad.Trans.Free ( FreeT(..), FreeF(Free) )
import qualified Control.Monad.Trans.Free as Free  

iterFold_ :: (Monad m) =>  Fold_ f m a -> (f (m a) -> m a) -> m a
iterFold_ phi alg = phi alg join return

iterFold :: (Monad m) =>  Fold f m a -> (f (m a) -> m a) -> m a
iterFold phi alg = getFold phi alg join return

--

augmentFold :: Fold (Of a) m () -> Fold (Of a) m r -> Fold (Of a) m r
augmentFold phi psi = Fold (augmentFold_ (getFold phi) (getFold psi))

augmentsFold :: Fold f m r -> Fold f m s -> Fold f m (r,s)
augmentsFold phi psi = Fold (augmentsFold_ (getFold phi) (getFold psi))


augmentFold_ ::
     (forall r'.  (f r' -> r') -> (m r' -> r') -> (() -> r') -> r')
  -> (forall r'.  (f r' -> r') -> (m r' -> r') -> (s -> r') -> r')
  -> (forall r'.  (f r' -> r') -> (m r' -> r') -> (s -> r') -> r')
augmentFold_ = \phi psi construct wrap done -> 
          phi construct 
              wrap 
              (\() -> psi construct 
                          wrap 
                          done)

augmentsFold_ :: 
        (forall r'.  (f r' -> r') -> (m r' -> r') -> (r -> r') -> r')
     -> (forall r'.  (f r' -> r') -> (m r' -> r') -> (s -> r') -> r')
     -> (forall r'.  (f r' -> r') -> (m r' -> r') -> ((r,s) -> r') -> r')
augmentsFold_ = \phi psi construct wrap done -> 
         phi construct 
             wrap 
             (\r -> psi construct 
                        wrap 
                        (\s -> done (r,s)))



-- -----
-- -----

producerToListM :: Monad m => Producer a m r -> ListM (Of a) m r
producerToListM =  loop where
  loop = \case M mp         -> Wrap (liftM loop mp)
               Pure r       -> Done r
               Respond a go -> Construct (a :> loop (go ()))
               Request x f  -> closed x


listMMToProducer :: Monad m =>  ListM (Of a) m r -> Producer a m r 
listMMToProducer = loop where
  loop = \case Wrap m              -> M (liftM loop m)  
               Construct (a :> as) -> Respond a (\() -> loop as)
               Done r              -> Pure r

-- -----

foldProducer_ :: Monad m => Producer a m r -> Fold_ (Of a) m r
foldProducer_ prod = \construct wrap done -> 
  let loop = \case M mp         -> wrap (liftM loop mp)
                   Pure r       -> done r
                   Respond a go -> construct (a :> loop (go ()))
                   Request x f  -> closed x
  in loop prod 

buildProducer_ :: Monad m =>  Fold_ (Of a) m r -> Producer a m r 
buildProducer_ phi = phi (\(a :> p) -> yield a >> p) M return

-- -----
foldProducer :: Monad m => Producer a m r -> Fold (Of a) m r
foldProducer p = Fold (foldProducer_ p)

buildProducer :: Monad m =>  Fold (Of a) m r -> Producer a m r 
buildProducer phi = buildProducer_ (getFold phi)
-- -----

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
  




-- -----
-- connections to standard haskell lists, listH
-- -----
listMToListH :: ListM (Of t) Identity () -> [t]
listMToListH  = \case Wrap (Identity ls)  -> listMToListH ls
                      Construct (a :> ls) -> a : listMToListH ls
                      Done ()             -> []

foldToListH :: Fold (Of a) Identity () -> [a]
foldToListH  phi = buildListH (getFold phi)
foldToListH' phi = buildListH' (getFold phi)

buildListH' :: Fold_ (Of a) Identity () -> [a]
buildListH'  phi = phi (\(x :> xs) -> x : xs) 
                       runIdentity 
                       (\() -> [])

buildListH :: Fold_ (Of a) Identity () -> [a]
buildListH phi = build $ \cons nil ->  
                  phi (\(x :> xs) -> cons x xs) 
                      runIdentity 
                      (\() -> nil)

listMFromListH :: [a] -> ListM (Of a) Identity ()
listMFromListH  []    = Done ()
listMFromListH (x:xs) = Construct (x :> listMFromListH xs)

toListMM :: Monad m => Fold_ (Of a) m r -> m ([a],r)
toListMM phi = phi construct join (\r -> return ([], r)) where
  construct (a :> mls) = do (as,r) <- mls
                            return (a : as, r)

-- -------
-- unfolds
-- -------

unfold :: (Functor f)
    => (a -> Either r (f a))  -> a  -> ListM f m r
unfold f = let go = either Done (Construct . fmap go) . f in go

unfold_ ::  (Functor f)
    => (a -> Either r (f a)) -> a -> Fold_ f m r
unfold_ f a = \frr mrr rr -> 
            let loop = either rr (frr . fmap loop) . f in loop a
            

unfoldM :: (Functor f, Monad m)
      => (a -> m (Either r (f a))) -> a -> ListM f m r
unfoldM f = let loop = Wrap . liftM (either Done (Construct . fmap loop)) . f 
            in loop

unfoldM_ :: (Functor f, Monad m)
      => (a -> m (Either r (f a))) -> a -> Fold_ f m r
unfoldM_ f a frr mrr rr = loop a where
  loop = mrr . liftM (either rr (frr . fmap loop)) . f 


-- --------------------
-- diverse combinators
-- --------------------

-- cp Atkey & co
effectfulFold :: (Functor f, Monad m) =>
                 (m a -> a)
              -> (r -> a)
              -> (f a -> a)
              -> ListM f m r
              -> a
effectfulFold  malg nil falg = loop where 
  loop = \case Wrap m      -> malg (liftM loop m)
               Construct f -> falg (fmap loop f)
               Done r      -> nil r

efold :: (Functor f, Monad m) 
      => (m b -> b) -> (Either a (f b) -> b) -> ListM f m a -> b
efold malg ealg = loop where
  loop = \case Wrap m      -> malg (liftM loop m)
               Construct f -> ealg (Right (fmap loop f))
               Done r      -> ealg (Left r)



uncons :: (Monad m, Functor f) 
       => ListM f m r -> m (Either r (f (ListM f m r)))
uncons = \case Wrap m       -> m >>= uncons
               Construct ff -> return (Right ff)
               Done r       -> return (Left r)
  
  
next :: (Monad m) 
        => ListM (Of a) m r -> m (Either r (a, ListM (Of a) m r))
next  = liftM (fmap (\(a:>b) -> (a,b))). uncons 
crush :: (Monad m, Functor f) => ListM f m r -> m (ListM f m r)
crush = \case Wrap m -> m
              a     -> return a



pr :: Functor f => f r -> Fold_ f m r 
pr fr frr mrr rr = frr (fmap rr fr)
sing :: a -> Fold_ (Of a) m ()
sing a frr mrr rr = frr (a :> rr ())
ret :: r -> Fold_ f m r
ret r frr mrr rr = rr r

consFold_ :: a -> Fold_ (Of a) m r  -> Fold_ (Of a) m r
consFold_ x fold_ falg  = fold_ (falg . (x :>) . falg)  

consFold :: a -> Fold (Of a) m r  -> Fold (Of a) m r
consFold a (Fold fold_)  = Fold (consFold_ a fold_)

consFB :: (Functor f) => f x -> Fold_ f m x -> Fold_ f m x
consFB fx phi frr mrr rr = frr (fmap rr fx)

consFB_ :: a -> Fold_ (Of a) m x -> Fold_ (Of a) m x
consFB_ a phi frr mrr rr = phi (\ofar -> frr (a :> frr ofar)) mrr rr

-- --------- 
maps :: (forall x . f x -> g x) -> Fold f m a -> Fold g m a
maps morph fold = Fold (maps_ morph (getFold fold))

maps_ :: (forall x . f x -> g x) -> Fold_ f m a -> Fold_ g m a
maps_ morph phi = \construct wrap done ->
   phi (construct . morph) 
       wrap 
       done



iterT2 :: (Monad m) => (f (m a) -> m a) ->  Fold_ f m a -> m a
iterT2 phi fold = fold phi join return
-- 
-- folded'' :: (Functor f, Monad m) => (f (m a) -> m a) -> ListM f m a -> m a
-- folded'' phi ls = iterT2 phi (foldListMx ls)

-- ---------------

-- ----------------------------------
-- ill-fated 'distribution' principles
-- ----------------------------------

  -- | Distribute 'Proxy' over a monad transformer
  -- distribute
  --     ::  ( Monad m , MonadTrans t , MFunctor t
  --         , Monad (t m) , Monad (t (Proxy a' a b' b m)) )
  --     => Proxy a' a b' b (t m) r
  --     -> t (Proxy a' a b' b m) r

freeFoldDist2
  :: (MFunctor t, MonadTrans t, Functor f, Monad (t (FreeT f m)),
      Monad (t m), Monad m) =>
     FreeT f (t m) a -> t (FreeT f m) a
freeFoldDist2  = freeFold (join . lift . FreeT. return . Free . fmap return) 
                          (join .  hoist lift) return

newtype D m f = D {unD :: m (f (D m f))}

dist22
  :: (MFunctor t, Monad (t (Fold f m)), Monad m) 
  => Fold f (t m) a 
  -> (f (t (Fold f m) a) -> t (Fold f m) a) 
  -> t (Fold f m) a
dist22 (Fold phi) construct = phi construct
                                  (join .  hoist lift)
                                  return

dd (Fold phi) = phi join join (lift . return) 
d3 (Fold phi) y  x = phi y (join .  hoist lift) x -- (lift . return) 
