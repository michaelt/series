{-# LANGUAGE LambdaCase, RankNTypes, EmptyCase, 
             StandaloneDeriving, FlexibleContexts,
             DeriveDataTypeable, DeriveFoldable, 
             DeriveFunctor, DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-} -- for Series show instance
module Series.Types where
  
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

-- some instances might better be explicit
data Of a b = !a :> b
    deriving (Data, Eq, Foldable, Functor, Ord,
              Read, Show, Traversable, Typeable)
infixr 4 :>
headOf (a :> b) = a
tailOf (a :> b) = b
mapHead f (a :> b) = f a :> b

-- explicit Series/FreeT data type
data Series f m r = Construct (f (Series f m r))
                 | Wrap (m (Series f m r))
                 | Done r

deriving instance (Show r, Show (m (Series f m r))
                  , Show (f (Series f m r))) => Show (Series f m r)

instance (Functor f, Monad m) => Functor (Series f m) where
  fmap f = loop where
    loop = \case Construct f  -> Construct (fmap loop f)
                 Wrap m       -> Wrap (liftM loop m)
                 Done r       -> Done (f r)
                 
instance (Functor f, Monad m) => Monad (Series f m) where
  return = Done
  lst >>= f = loop lst where
    loop = \case Construct f -> Construct (fmap loop f)
                 Wrap m      -> Wrap (liftM loop m)
                 Done r      -> f r
                 
instance (Functor f, Monad m) => Applicative (Series f m) where
  pure = Done; (<*>) = ap
  
instance Functor f => MonadTrans (Series f) where
  lift = Wrap . liftM Done

instance Functor f => MFunctor (Series f) where
  hoist trans = loop where
    loop = \case Construct f -> Construct (fmap loop f)
                 Wrap m      -> Wrap (trans (liftM loop m))
                 Done r      -> Done r

-- church encodings:
-- ----- unwrapped synonym:
type Fold_ f m r = forall r'
                   .  (f r' -> r') 
                   -> (m r' -> r')
                   -> (r -> r') 
                   -> r'
-- ------ wrapped:
newtype Fold f m r = Fold {getFold :: Fold_ f m r  }

-- these should perhaps be expressed with
-- predefined combinators for Fold_
instance Functor (Fold f m) where
  fmap f phi = Fold (\construct wrap done -> 
    getFold phi construct 
                wrap 
                (done . f))

instance Monad (Fold f m) where
  return r = Fold (\construct wrap done -> done r) 
  phi >>= f = Fold (\construct wrap done -> 
    getFold phi construct 
                wrap  
                (\a -> getFold (f a) construct 
                                     wrap 
                                     done))

instance Applicative (Fold f m) where
  pure r = Fold (\construct wrap done -> done r) 
  phi <*> psi = Fold (\construct wrap done -> 
    getFold phi construct 
                wrap 
                (\f -> getFold psi construct 
                                   wrap 
                                   (\a -> done (f a))))

instance MonadTrans (Fold f) where
  lift ma = Fold (\constr wrap done -> 
    wrap (liftM done ma))


instance Functor f => MFunctor (Fold f) where
  hoist trans phi = Fold (\construct wrap done -> 
    getFold phi construct (wrap . trans) done)

buildSeries :: Fold f m r -> Series f m r 
buildSeries = \(Fold phi) -> phi Construct Wrap Done
{-# INLINE[0] buildSeries #-}

foldSeries  :: (Functor f, Monad m) => Series f m t -> Fold f m t
foldSeries = \lst -> Fold (\construct wrap done ->
  let loop = \case Wrap mlst      -> wrap (liftM loop mlst) 
                   Construct flst -> construct (fmap loop flst)
                   Done r         -> done r
  in  loop lst)
{-# INLINE[0] foldSeries  #-}

-- The compiler has no difficulty with the rule for the wrapped case.
-- I have not investigated whether the remaining newtype
-- constructor is acting as an impediment. 

{-# RULES
  "foldSeries/buildSeries" forall phi.
    foldSeries (buildSeries phi) = phi
    #-}
    
-- ----
buildSeriesx
  :: ((f (Series f m r) -> Series f m r)
      -> (m1 (Series f1 m1 r1) -> Series f1 m1 r1)
      -> (r2 -> Series f2 m2 r2)
      -> t)
     -> t
buildSeriesx = \phi -> phi Construct Wrap Done
{-# INLINE[1] buildSeriesx #-}

foldSeriesx
  :: (Functor f, Monad m) =>
     Series f m t -> (f b -> b) -> (m b -> b) -> (t -> b) -> b
foldSeriesx = \lst construct wrap done ->
   let loop = \case Wrap mlst  -> wrap (liftM loop mlst) 
                    Construct flst -> construct (fmap loop flst)
                    Done r     -> done r
   in  loop lst 
{-# INLINE[1] foldSeriesx #-}

-- the compiler seems to have trouble seeing these rules as applicable,
-- unlike those for foldSeries & buildSeries
{-# RULES
 
  "foldSeriesx/buildSeriesx" forall phi.
    foldSeriesx (buildSeriesx phi) = phi
    
    #-}


{-# RULES
 
  "foldSeriesx/buildSeriesxsat" forall phi x.
    foldSeriesx (buildSeriesx phi) x = phi x
    
    #-}
    

{-# RULES
 
  "foldSeriesx/buildSeriesxsat2" forall phi x y.
    foldSeriesx (buildSeriesx phi) x y = phi x y
    
    #-}

{-# RULES

      "foldSeriesx/buildSeriesxsat3" forall phi x y z.
        foldSeriesx (buildSeriesx phi) x y z = phi x y z

        #-}
-- -----

