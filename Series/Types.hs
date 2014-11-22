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

-- explicit Series/FreeT data type
data Series f m r = Construct (f (Series f m r))
                 | Wrap (m (Series f m r))
                 | Done r
                 deriving (Typeable)


deriving instance (Show r, Show (m (Series f m r))
                  , Show (f (Series f m r))) => Show (Series f m r)
deriving instance (Eq r, Eq (m (Series f m r))
                  , Eq (f (Series f m r))) => Eq (Series f m r)

instance (Functor f, Monad m) => Functor (Series f m) where
  fmap f = loop where
    loop = \case Construct f  -> Construct (fmap loop f)
                 Wrap m       -> Wrap (liftM loop m)
                 Done r       -> Done (f r)

instance (Functor f, Monad m) => Monad (Series f m) where
  return = Done
  {-# INLINE return #-}
  (>>=) = flip seriesBind where
    seriesBind f = buildSeries . foldBind (foldSeries . f) . foldSeries 
    {-# INLINE seriesBind #-}
  {-# INLINE (>>=) #-}
    -- loop lst where
    -- loop = \case Construct f -> Construct (fmap loop f)
    --              Wrap m      -> Wrap (liftM loop m)
    --              Done r      -> f r

instance (Functor f, Monad m) => Applicative (Series f m) where
  pure = Done; (<*>) = ap
  
instance Functor f => MonadTrans (Series f) where
  lift = Wrap . liftM Done

instance Functor f => MFunctor (Series f) where
  hoist trans = loop where
    loop = \case Construct f -> Construct (fmap loop f)
                 Wrap m      -> Wrap (trans (liftM loop m))
                 Done r      -> Done r

instance (MonadIO m, Functor f) => MonadIO (Series f m) where
  liftIO = Wrap . liftM Done . liftIO 
  
-- church encodings:
-- ----- unwrapped synonym:
type Folding_ f m r = forall r'
                   .  (f r' -> r') 
                   -> (m r' -> r')
                   -> (r -> r') 
                   -> r'
-- ------ wrapped:
newtype Folding f m r = Folding {getFolding :: Folding_ f m r  }

-- these should perhaps be expressed with
-- predefined combinators for Folding_
instance Functor (Folding f m) where
  fmap f phi = Folding (\construct wrap done -> 
    getFolding phi construct 
                wrap 
                (done . f))

instance Monad (Folding f m) where
  return r = Folding (\construct wrap done -> done r) 
  (>>=) = flip foldBind
  {-# INLINE (>>=) #-}

foldBind f phi = Folding (\construct wrap done -> 
  getFolding phi construct 
              wrap  
              (\a -> getFolding (f a) construct 
                                   wrap 
                                   done))
{-# INLINE foldBind #-}
instance Applicative (Folding f m) where
  pure r = Folding (\construct wrap done -> done r) 
  phi <*> psi = Folding (\construct wrap done -> 
    getFolding phi construct 
                wrap 
                (\f -> getFolding psi construct 
                                   wrap 
                                   (\a -> done (f a))))

instance MonadTrans (Folding f) where
  lift ma = Folding (\constr wrap done -> 
    wrap (liftM done ma))


instance Functor f => MFunctor (Folding f) where
  hoist trans phi = Folding (\construct wrap done -> 
    getFolding phi construct (wrap . trans) done)

instance (MonadIO m, Functor f) => MonadIO (Folding f m) where
  liftIO io = Folding (\construct wrap done -> 
             wrap (liftM done (liftIO io))
                )
  {-# INLINE liftIO #-}

type List a = Series (Of a) Identity ()


-- -------------------------------------
-- optimization operations: wrapped case
-- -------------------------------------

-- 

-- `foldSeries` is a flipped and wrapped variant of Atkey's 
-- effectfulFolding :: (Functor f, Monad m) =>
--    (m x -> x) -> (r -> x) -> (f x -> x) -> Series f m r -> x
-- modulo the 'Done' constructor, which implicitly restricts the 
-- available class of Functors. 
-- See http://bentnib.org/posts/2012-01-06-streams.html and 
-- the (nightmarish) associated paper.

-- Our plan is thus where possible to replace the datatype Series with
-- the associated effectfulFolding itself, wrapped as Folding

foldSeries  :: (Functor f, Monad m) => Series f m t -> Folding f m t
foldSeries = \lst -> Folding (\construct wrap done ->
  let loop = \case Wrap mlst      -> wrap (liftM loop mlst) 
                   Construct flst -> construct (fmap loop flst)
                   Done r         -> done r
  in  loop lst)
{-# INLINE[0] foldSeries  #-}

buildSeries :: Folding f m r -> Series f m r 
buildSeries = \(Folding phi) -> phi Construct Wrap Done
{-# INLINE[0] buildSeries #-}


-- The compiler has no difficulty with the rule for the wrapped case.
-- I have not investigated whether the remaining newtype
-- constructor is acting as an impediment. The stage [0] or [1]
-- seems irrelevant in either case.

{-# RULES
  "foldSeries/buildSeries" forall phi.
    foldSeries (buildSeries phi) = phi
    #-}

-- -------------------------------------
-- optimization operations: wrapped case
-- -------------------------------------

foldSeriesx
  :: (Functor f, Monad m) =>
     Series f m t -> (f b -> b) -> (m b -> b) -> (t -> b) -> b
foldSeriesx = \lst construct wrap done ->
   let loop = \case Wrap mlst  -> wrap (liftM loop mlst) 
                    Construct flst -> construct (fmap loop flst)
                    Done r     -> done r
   in  loop lst 
{-# INLINE[1] foldSeriesx #-}


buildSeriesx = \phi -> phi Construct Wrap Done
{-# INLINE[1] buildSeriesx #-}

-- The compiler seems to have trouble seeing these rules as applicable,
-- unlike those for foldSeries & buildSeries. Opaque arity is
-- a plausible hypothesis when you know nothing yet.
-- When additional arguments are given to a rule, 
-- the most saturated is the one that fires, 
-- but it only fires where this one would.

{-# RULES
 
  "foldSeriesx/buildSeriesx" forall phi.
    foldSeriesx (buildSeriesx phi) = phi
    
    #-}


