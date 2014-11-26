{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables #-}
module Series.Folding.Prelude where
import Series.Types
import Control.Monad hiding (filterM, mapM)
import Data.Functor.Identity
import Control.Monad.Trans
import qualified System.IO as IO
import Prelude hiding (map, filter, drop, take, sum
                      , iterate, repeat, replicate, splitAt
                      , takeWhile, enumFrom, enumFromTo)


-- ---------------
-- ---------------
-- Prelude
-- ---------------
-- ---------------

lyield :: Monad m => a -> Folding (Of a) m ()
lyield r = Folding (\construct wrap done -> construct (r :> done ()))
{-# INLINE lyield #-}

-- ---------------
-- sum 
-- ---------------
lsum :: (Monad m, Num a) => Folding (Of a) m () -> m a
lsum = \(Folding phi) -> phi (\(n :> mm) -> mm >>= \m -> return (m+n))
                             join
                             (\_ -> return 0)
{-# INLINE lsum #-}


-- ---------------
-- replicate 
-- ---------------

jreplicate :: Monad m => Int -> a -> Folding_ (Of a) m ()
jreplicate n a c w d = take_ n (repeat_ a) c w d
{-# INLINE jreplicate #-}

lreplicate :: Monad m => Int -> a -> Folding (Of a) m ()
lreplicate n a = Folding (take_ n (repeat_ a))
{-# INLINE lreplicate #-}

lreplicateM :: Monad m => Int -> m a -> Folding (Of a) m ()
lreplicateM n a = Folding (take_ n (repeatM_ a))
{-# INLINE lreplicateM #-}


-- ---------------
-- iterate
-- ---------------
jiterate :: (a -> a) -> a -> Folding_ (Of a) m r
jiterate = \f a construct wrap done -> 
       construct (a :> jiterate f (f a) construct wrap done) 
{-# INLINE jiterate #-}

literate :: (a -> a) -> a -> Folding (Of a) m r
literate f a = Folding (jiterate f a)
{-# INLINE literate #-}


jiterateM :: Monad m => (a -> m a) -> m a -> Folding_ (Of a) m r
jiterateM f ma = \construct wrap done -> 
     let loop mx = wrap $ liftM (\x -> construct (x :> loop (f x))) mx
     in loop ma
{-# INLINE jiterateM #-}

literateM :: Monad m => (a -> m a) -> m a -> Folding (Of a) m r
literateM f a = Folding (jiterateM f a)
{-# INLINE literateM #-}


-- ---------------
-- repeat
-- ---------------

repeat_ :: a -> Folding_ (Of a) m r
repeat_ = \a construct wrap done -> 
  let loop = construct (a :> loop) in loop
{-# INLINE repeat_ #-}


repeatM_ :: Monad m => m a -> Folding_ (Of a) m r
repeatM_ ma = \construct wrap done -> 
  let loop = liftM (\a -> construct (a :> wrap loop)) ma in wrap loop
{-# INLINE repeatM_ #-}

lrepeatM :: Monad m => m a -> Folding (Of a) m r
lrepeatM ma = Folding (repeatM_ ma)
{-# INLINE lrepeatM #-}

-- ---------------
-- filter 
-- ---------------

jfilter :: (Monad m) => (a -> Bool) -> Folding_ (Of a) m r -> Folding_ (Of a) m r
jfilter = \pred phi construct wrap done ->
   phi (\aa@(a :> x) -> if pred a then construct aa else x)
       wrap 
       done
{-# INLINE jfilter #-}

lfilter :: Monad m => (a -> Bool) -> Folding (Of a) m r -> Folding (Of a) m r
lfilter pred = \phi -> Folding (jfilter pred (getFolding phi))
{-# INLINE lfilter #-}

filter_ :: (Monad m) => Folding_ (Of a) m r -> (a -> Bool) -> Folding_ (Of a) m r
filter_ phi pred0 = \construct wrap done ->
   phi (\aa@(a :> x) pred-> if pred a then construct (a :> x pred) else x pred)
       (\mp pred -> wrap $ liftM ($pred) mp)
       (\r pred -> done r)
       pred0
{-# INLINE filter_ #-}

jfilterM :: (Monad m) => (a -> m Bool) -> Folding_ (Of a) m r -> Folding_ (Of a) m r
jfilterM pred = \phi construct wrap done ->
   phi (\aa@(a :> x) -> wrap $ liftM (\b -> if b then construct aa else x) (pred a))
       wrap 
       done
{-# INLINE jfilterM #-}

lfilterM :: Monad m => (a -> m Bool) -> Folding (Of a) m r -> Folding (Of a) m r
lfilterM pred phi = Folding (jfilterM pred (getFolding phi))

-- ---------------
-- drop
-- ---------------

ldrop :: Monad m => Int -> Folding (Of a) m r -> Folding (Of a) m r
ldrop n = \phi -> Folding (jdrop n (getFolding phi))
{-# INLINE ldrop #-}

jdrop :: Monad m => Int -> Folding_ (Of a) m r -> Folding_ (Of a) m r
jdrop = \m phi construct wrap done -> 
   phi  
    (\(a :> fn) n -> if n <= m then fn (n+1) else construct (a :> (fn (n+1))))
    (\m n -> wrap (m >>= \fn -> return (fn n)))
    (\r _ -> done r)
    1
{-# INLINE jdrop #-}

jdrop_ :: Monad m => Folding_ (Of a) m r -> Int -> Folding_ (Of a) m r
jdrop_ phi n0 = \construct wrap done -> 
   phi  
    (\(a :> fn) n -> if n >= 0 then fn (n-1) else construct (a :> (fn (n-1))))
    (\m n -> wrap (m >>= \fn -> return (fn n)))
    (\r _ -> done r)
    n0
{-# INLINE jdrop_ #-}

ldrop_ :: Monad m => Int -> Folding (Of a) m r -> Folding (Of a) m r
ldrop_ n = \phi -> Folding (jdrop_ (getFolding phi) n)
{-# INLINE ldrop_ #-}

-- ---------------
-- concats
-- ---------------

jconcats :: Monad m => Folding (Folding (Of a) m) m r -> Folding (Of a) m r
jconcats (Folding phi) = Folding $ \construct wrap done ->
  phi (\(Folding phi') -> phi' construct wrap id)  
      wrap
      done

jconcats_ :: Monad m => Folding_ (Folding (Of a) m) m r -> Folding_ (Of a) m r
jconcats_ phi = \construct wrap done ->
  phi (\(Folding phi') -> phi' construct wrap id)  
      wrap
      done

jconcats__ :: Monad m => Folding_ (Folding_ (Of a) m) m r -> Folding_ (Of a) m r
jconcats__ phi = \construct wrap done ->
  phi (\phi' -> phi' construct wrap id)  
      wrap
      done

-- ---------------
-- map
-- ---------------

lmap :: (a -> b) -> Folding (Of a) m r -> Folding (Of b) m r
lmap f = \fold -> Folding (jmap f (getFolding fold))
{-# INLINE lmap #-}

jmap :: (a -> b) -> Folding_ (Of a) m r -> Folding_ (Of b) m r
jmap = \f phi construct wrap done -> 
      phi (\(a :> x) -> construct (f a :> x)) 
          wrap 
          done 
{-# INLINE jmap #-}

map_ :: Monad m => Folding_ (Of a) m r -> (a -> b) -> Folding_ (Of b) m r
map_ phi f0 = \construct wrap done -> 
      phi (\(a :> x) f -> construct (f a :> x f)) 
          (\mf f -> wrap (liftM ($f) mf))
          (\r f -> done r)
          f0
{-# INLINE map_ #-}


jmapM :: Monad m => (a -> m b) -> Folding_ (Of a) m r -> Folding_ (Of b) m r
jmapM f = \phi construct wrap done -> 
      phi (\(a :> x) -> wrap (liftM (construct . (:> x)) (f a)))
          wrap 
          done        
{-# INLINE jmapM #-}

lmapM :: Monad m => (a -> m b) -> Folding (Of a) m r -> Folding (Of b) m r
lmapM f = \(Folding phi) -> Folding (jmapM f phi)
{-# INLINE lmapM #-}

-- ---------------
-- take
-- ---------------



take_ :: (Monad m, Functor f) => Int -> Folding_ f m r -> Folding_ f m ()
take_ n phi = \construct wrap done -> phi 
      (\fx n -> if n <= 0 then done () else construct (fmap ($(n-1)) fx))
      (\mx n -> if n <= 0 then done () else wrap (liftM ($n) mx)) 
      (\r n -> done ()) 
      n
{-# INLINE take_ #-}




jtake_ :: (Monad m, Functor f) => Folding_ f m r -> Int -> Folding_ f m ()
jtake_ phi = \m construct wrap done-> phi 
      (\fx n -> if n <= 0 then done () else construct (fmap ($(n-1)) fx))
      (\mx n -> if n <= 0 then done () else wrap (liftM ($n) mx)) 
      (\r n -> done ()) 
      m
{-# INLINE jtake_ #-}

ltake :: (Monad m, Functor f) => Int -> Folding f m r -> Folding f m ()
ltake n = \(Folding phi)  -> Folding (take_ n phi)
{-# INLINE ltake #-}

jtakeWhile :: Monad m => (a -> Bool) -> Folding_ (Of a) m r -> Folding_ (Of a) m ()
jtakeWhile = \pred phi construct wrap done -> phi
  (\(a :> fn) p -> if not (pred a) then done () 
                                   else construct (a :> (fn True)))
  (\m p -> if not p then done () else wrap (liftM ($p) m)) 
  (\r p -> done ()) 
  True 
{-# INLINE jtakeWhile #-}



ltakeWhile :: Monad m => (a -> Bool) -> Folding (Of a) m r -> Folding (Of a) m ()
ltakeWhile pred = \(Folding fold) -> Folding (jtakeWhile pred fold)
{-# INLINE ltakeWhile #-}


jtakeWhile_ :: Monad m => Folding_ (Of a) m r -> (a -> Bool) -> Folding_ (Of a) m ()
jtakeWhile_ phi pred0 =  \construct wrap done -> 
  phi (\(a :> fn) p pred_ -> if not (pred_ a) 
                                then done () 
                                else construct (a :> (fn True pred_)))
      (\m p pred_ -> if not p then done () else wrap (liftM (\fn -> fn p pred_) m)) 
      (\r p pred_ -> done ()) 
      True 
      pred0
{-# INLINE jtakeWhile_ #-}



-- ------- 
lenumFrom n = \construct wrap done -> 
      let loop m = construct (m :> loop (succ m)) in loop n
        
lenumFromTo n m = \construct wrap done -> 
      let loop k = if k <= m then construct (k :> loop (succ k)) 
                             else done ()
      in loop n

lenumFromToStep n m k = \construct wrap done -> 
            let loop p = if p <= k then construct (p :> loop (p + m)) 
                                   else done ()
            in loop n

lenumFromStepN start step n = \construct wrap done -> 
               let loop p 0 = done ()
                   loop p now = construct (p :> loop (p + step) (now-1)) 
               in loop start n



foldl_ ::  Monad m => (b -> a -> b) -> b -> Folding_ (Of a) m r -> m b
foldl_ op b0 = \phi -> 
  phi (\(a :> fn) b -> fn $! flip op a $! b)
      (\mf b -> mf >>= \f -> f b)
      (\_ b -> return $! b)
      b0
{-# INLINE foldl_ #-}

jscanr :: Monad m => (a -> b -> b) -> b -> Folding_ (Of a) m r -> Folding_ (Of b) m r
jscanr op b phi = phi 
      (\(a :> fx) b c w d -> c (b :> fx (op a b) c w d))
      (\mfx b c w d ->  w (liftM (\fx -> c (b :> fx b c w d)) mfx))
      (\r b c w d -> c (b :> d r))
      b
{-# INLINE jscanr #-}


lscanr_ :: Monad m =>  Folding_ (Of a) m r -> (a -> b -> b) -> b -> Folding_ (Of b) m r
lscanr_ phi  = phi 
      (\(a :> fx) op b c w d -> c (b :> fx op (op a b) c w d))
      (\mfx op b c w d ->  w (liftM (\fx -> c (b :> fx op b c w d)) mfx))
      (\r op b c w d -> c (b :> d r))
{-# INLINE lscanr_ #-}