{-# LANGUAGE LambdaCase, RankNTypes, EmptyCase, 
             StandaloneDeriving, FlexibleContexts,
             DeriveDataTypeable, DeriveFoldable, 
             DeriveFunctor, DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-} -- for ListM show instance
module ListM.Types where
  
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

-- explicit ListM/FreeT data type
data ListM f m r = Construct (f (ListM f m r))
                 | Wrap (m (ListM f m r))
                 | Done r

deriving instance (Show r, Show (m (ListM f m r))
                  , Show (f (ListM f m r))) => Show (ListM f m r)

instance (Functor f, Monad m) => Functor (ListM f m) where
  fmap f = loop where
    loop = \case Construct f  -> Construct (fmap loop f)
                 Wrap m       -> Wrap (liftM loop m)
                 Done r       -> Done (f r)
                 
instance (Functor f, Monad m) => Monad (ListM f m) where
  return = Done
  lst >>= f = loop lst where
    loop = \case Construct f -> Construct (fmap loop f)
                 Wrap m      -> Wrap (liftM loop m)
                 Done r      -> f r
                 
instance (Functor f, Monad m) => Applicative (ListM f m) where
  pure = Done; (<*>) = ap
  
instance Functor f => MonadTrans (ListM f) where
  lift = Wrap . liftM Done

instance Functor f => MFunctor (ListM f) where
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
