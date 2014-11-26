{-# LANGUAGE LambdaCase, RankNTypes, BangPatterns #-}
module Series.Combinators where

import Series.Types
import Control.Applicative 
import Control.Monad hiding (foldM)
import Control.Monad.Trans
import Control.Monad.Morph
import Data.Functor.Identity
import qualified Control.Monad.Trans.Free as Free  
import Control.Monad.Trans.Free ( FreeT(..), FreeF(Free) )
import qualified Control.Foldl as L


jfold op seed out = \(Folding phi) ->
  do x <- phi (\(a :> mx) -> mx >>= \x -> return (op x a)) 
              join 
              (\_ -> return seed)
     return (out x)

jfoldM op seed out = \(Folding phi) -> 
  do x <- phi (\(a :> mx) -> mx >>= \x -> op x a)
              join
              (const seed)
     out x

-- purely fold ((,) <$> L.product <*> L.sum )
fold op seed out ls = liftM out (loop seed ls) where
  loop !x = \case Construct (a :> as) -> loop (op x a) as
                  Wrap mas    -> mas >>= \as -> loop x as
                  Done r      -> return x
{-# INLINE[0] fold #-} 

-- impurely foldM (generalize $ (,) <$> L.product <*> L.sum )
foldM op seed out ls = seed >>= \y -> loop y ls >>= out where
  loop !x = \case Construct (a :> as) -> op x a >>= \y -> loop y as
                  Wrap mas    -> mas >>= \as -> loop x as
                  Done r      -> return x
{-# INLINE[0] foldM #-} 

{-# RULES 
  "fold/buildSeries" 
   forall op seed out phi.
    fold op seed out (buildSeries phi) = jfold op seed out phi
    #-}

{-# RULES 
  "foldLM/buildSeries" 
   forall op seed out phi.
    foldM op seed out (buildSeries phi) = jfoldM op seed out phi
    #-}


iterFolding_ :: (Monad m) =>  Folding_ f m a -> (f (m a) -> m a) -> m a
iterFolding_ phi alg = phi alg join return

iterFolding :: (Monad m) =>  Folding f m a -> (f (m a) -> m a) -> m a
iterFolding phi alg = getFolding phi alg join return


-- -------
-- unfolds
-- -------

unfold :: (Functor f)
    => (a -> Either r (f a))  -> a  -> Series f m r
unfold f = let go = either Done (Construct . fmap go) . f in go

unfold_ ::  (Functor f)
    => (a -> Either r (f a)) -> a -> Folding_ f m r
unfold_ f a = \construct wrap done -> 
            let loop = either done (construct . fmap loop) . f in loop a


unfoldM :: (Functor f, Monad m)
      => (a -> m (Either r (f a))) -> a -> Series f m r
unfoldM f = let loop = Wrap . liftM (either Done (Construct . fmap loop)) . f 
            in loop

unfoldM_ :: (Functor f, Monad m)
      => (a -> m (Either r (f a))) -> a -> Folding_ f m r
unfoldM_ f a construct wrap done = loop a where
  loop = wrap . liftM (either done (construct . fmap loop)) . f 

-- -------------------
-- unfoldM uncons = id 
-- -------------------
uncons :: (Monad m, Functor f) 
       => Series f m r -> m (Either r (f (Series f m r)))
uncons = \case Wrap m       -> m >>= uncons
               Construct ff -> return (Right ff)
               Done r       -> return (Left r)

next :: (Monad m) 
        => Series (Of a) m r -> m (Either r (a, Series (Of a) m r))
next  = liftM (fmap (\(a:>b) -> (a,b))). uncons 

-- --------------------
-- diverse combinators
-- --------------------

-- cp Atkey & co
effectfulFolding :: (Functor f, Monad m) =>
                 (m a -> a)
              -> (r -> a)
              -> (f a -> a)
              -> Series f m r
              -> a
effectfulFolding malg nil falg = loop where 
  loop = \case Wrap m      -> malg (liftM loop m)
               Construct f -> falg (fmap loop f)
               Done r      -> nil r

efold :: (Functor f, Monad m) 
      => (m b -> b) -> (Either a (f b) -> b) -> Series f m a -> b
efold malg ealg = loop where
  loop = \case Wrap m      -> malg (liftM loop m)
               Construct f -> ealg (Right (fmap loop f))
               Done r      -> ealg (Left r)


crush :: (Monad m, Functor f) => Series f m r -> m (Series f m r)
crush = \case Wrap m -> m; a     -> return a


pr :: Functor f => f r -> Folding_ f m r 
pr fr = \construct wrap done -> construct (fmap done fr)
sing :: a -> Folding_ (Of a) m () --yield
sing a = \construct wrap done -> construct (a :> done ())
ret :: r -> Folding_ f m r
ret r = \construct wrap done -> done r


consFolding_ :: a -> Folding_ (Of a) m r  -> Folding_ (Of a) m r
consFolding_ a phi construct = phi (construct . (a :>) . construct)  

consFolding :: a -> Folding (Of a) m r  -> Folding (Of a) m r
consFolding a = \(Folding phi) -> Folding (consFolding_ a phi)

consFB :: (Functor f) => f x -> Folding_ f m x -> Folding_ f m x
consFB fx phi construct wrap done = construct (fmap done fx)

consFB_ :: a -> Folding_ (Of a) m x -> Folding_ (Of a) m x
consFB_ a phi construct wrap done = 
  phi (\_Ofar -> construct (a :> construct _Ofar)) 
      wrap 
      done
      
-- ---------

augmentFolding :: Folding (Of a) m () -> Folding (Of a) m r -> Folding (Of a) m r
augmentFolding phi psi = Folding (augmentFolding_ (getFolding phi) (getFolding psi))

augmentsFolding :: Folding f m r -> Folding f m s -> Folding f m (r,s)
augmentsFolding phi psi = Folding (augmentsFolding_ (getFolding phi) (getFolding psi))

augmentFolding_ ::
     (forall r'.  (f r' -> r') -> (m r' -> r') -> (() -> r') -> r')
  -> (forall r'.  (f r' -> r') -> (m r' -> r') -> (s -> r') -> r')
  -> (forall r'.  (f r' -> r') -> (m r' -> r') -> (s -> r') -> r')
augmentFolding_ = \phi psi construct wrap done -> 
          phi construct 
              wrap 
              (\() -> psi construct 
                          wrap 
                          done)

augmentsFolding_ :: 
        (forall r'.  (f r' -> r') -> (m r' -> r') -> (r -> r') -> r')
     -> (forall r'.  (f r' -> r') -> (m r' -> r') -> (s -> r') -> r')
     -> (forall r'.  (f r' -> r') -> (m r' -> r') -> ((r,s) -> r') -> r')
augmentsFolding_ = \phi psi construct wrap done -> 
         phi construct 
             wrap 
             (\r -> psi construct 
                        wrap 
                        (\s -> done (r,s)))


-- --------- 
maps :: (forall x . f x -> g x) -> Folding f m a -> Folding g m a
maps morph fold = Folding (maps_ morph (getFolding fold))

maps_ :: (forall x . f x -> g x) -> Folding_ f m a -> Folding_ g m a
maps_ morph phi = \construct wrap done ->
   phi (construct . morph) 
       wrap 
       done



iterT2 :: (Monad m) => (f (m a) -> m a) ->  Folding_ f m a -> m a
iterT2 phi fold = fold phi join return
-- 
-- folded'' :: (Functor f, Monad m) => (f (m a) -> m a) -> Series f m a -> m a
-- folded'' phi ls = iterT2 phi (foldSeriesx ls)

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

freeFoldingDist2
  :: (MFunctor t, MonadTrans t, Functor f, Monad (t (FreeT f m)),
      Monad (t m), Monad m) =>
     FreeT f (t m) a -> t (FreeT f m) a
freeFoldingDist2  = freeFolding (join . lift . FreeT. return . Free . fmap return) 
                          (join .  hoist lift) return
  where 
    freeFolding construct wrap done = 
      wrap 
      . liftM (\case Free.Pure r -> done r
                     Free free_  -> construct (fmap (freeFolding construct wrap done) free_)) 
      . runFreeT
newtype D m f = D {unD :: m (f (D m f))}

dist22
  :: (MFunctor t, Monad (t (Folding f m)), Monad m) 
  => Folding f (t m) a 
  -> (f (t (Folding f m) a) -> t (Folding f m) a) 
  -> t (Folding f m) a
dist22 (Folding phi) construct = phi construct
                                  (join .  hoist lift)
                                  return

dd (Folding phi) = phi join join (lift . return) 
d3 (Folding phi) y  x = phi y (join .  hoist lift) x -- (lift . return) 
