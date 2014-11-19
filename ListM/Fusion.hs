{-# LANGUAGE LambdaCase, RankNTypes #-}
module ListM.Fusion where
import ListM.Types
import ListM.Combinators
import Control.Monad hiding (filterM, mapM)
import Prelude hiding (map, filter, drop, take, sum
                      , iterate, repeat, replicate, splitAt)


-- ---------------
-- ---------------
-- Data.ListM 
-- ---------------
-- ---------------

-- ---------------
-- sum 
-- ---------------
lsum :: (Monad m, Num a) => Fold (Of a) m () -> m a
lsum = \phi -> jsum (getFold phi)
{-# INLINE lsum #-}
jsum :: (Monad m, Num a) => Fold_ (Of a) m () -> m a
jsum  = \phi -> phi (\(n :> mm) -> mm >>= \m -> return (m+n))
                    join
                    (\_ -> return 0) 
{-# INLINE jsum #-}
sum :: (Monad m, Num a) => ListM (Of a) m () -> m a
sum = loop where
  loop = \case Construct (a :> as) -> liftM (a+) (loop as)
               Wrap m -> m >>= loop
               Done r -> return 0

sumF :: (Monad m, Num a) => ListM (Of a) m () -> m a
sumF  = lsum . foldListM 
-- ---------------
-- replicate 
-- ---------------
jreplicate :: Monad m => Int -> a -> Fold_ (Of a) m ()
jreplicate n a = jtake n (jrepeat a)

lreplicate :: Monad m => Int -> a -> Fold (Of a) m ()
lreplicate n a = Fold (jtake n (jrepeat a))

replicate :: Monad m => Int -> a -> ListM (Of a) m ()
replicate n a = loop n where
  loop 0 = Done ()
  loop m = Construct (a :> loop (m-1))

replicateF :: Monad m => Int -> a -> ListM (Of a) m ()
replicateF n a = buildListM (lreplicate n a)

jreplicateM :: Monad m => Int -> m a -> Fold_ (Of a) m ()
jreplicateM n a = jtake n (jrepeatM a)

lreplicateM :: Monad m => Int -> m a -> Fold (Of a) m ()
lreplicateM n a = Fold (jtake n (jrepeatM a))

replicateM :: Monad m => Int -> m a -> ListM (Of a) m ()
replicateM n ma = loop n where 
  loop 0 = Done ()
  loop n = Wrap $ ma >>= \a -> return (Construct $ a :> loop (n-1))



-- ---------------
-- iterate
-- ---------------
literate :: (a -> a) -> a -> Fold (Of a) m r
literate f a = Fold (jiterate f a)
{-# INLINE literate #-}

jiterate :: (a -> a) -> a -> Fold_ (Of a) m r
jiterate f a = \construct wrap done -> 
       construct (a :> jiterate f (f a) construct wrap done) 
{-# INLINE jiterate #-}
iterate :: (a -> a) -> a -> ListM (Of a) m r
iterate f = loop where
  loop a' = Construct (a' :> loop (f a'))

iterateF :: (a -> a) -> a -> ListM (Of a) m r
iterateF f  = buildListM . literate f 
{-# INLINE iterateF #-}

jiterateM :: Monad m => (a -> m a) -> m a -> Fold_ (Of a) m r
jiterateM f ma = \construct wrap done -> 
     let loop mx = wrap $ liftM (\x -> construct (x :> loop (f x))) mx
     in loop ma

literateM :: Monad m => (a -> m a) -> m a -> Fold (Of a) m r
literateM f a = Fold (jiterateM f a)

iterateM :: Monad m => (a -> m a) -> m a -> ListM (Of a) m r
iterateM f = loop where
  loop ma  = Wrap $ do a <- ma
                       return (Construct (a :> loop (f a)))

-- -- | 'iterate' @f x@ returns an infinite list of repeated applications
-- -- of @f@ to @x@:
-- --
-- -- > iterate f x == [x, f x, f (f x), ...]
-- 
-- {-# NOINLINE [1] iterate #-}
-- iterate :: (a -> a) -> a -> [a]
-- iterate f x =  x : iterate f (f x)
-- 
-- {-# NOINLINE [0] iterateFB #-}
-- iterateFB :: (a -> b -> b) -> (a -> a) -> a -> b
-- iterateFB c f x = x `c` iterateFB c f (f x)
-- 
-- {-# RULES
-- "iterate"    [~1] forall f x.   iterate f x = build (\c _n -> iterateFB c f x)
-- "iterateFB"  [1]                iterateFB (:) = iterate
--  #-}


-- ---------------
-- repeat
-- ---------------

jrepeat :: a -> Fold_ (Of a) m r
jrepeat a = \construct wrap done -> let loop = construct (a :> loop) in loop

lrepeat :: a -> Fold (Of a) m r
lrepeat a = Fold (jrepeat a)

repeat :: a -> ListM (Of a) m r
repeat a = loop where
  loop = Construct (a :> loop)

repeatF = buildListM . lrepeat

jrepeatM :: Monad m => m a -> Fold_ (Of a) m r
jrepeatM ma = \construct wrap done -> 
      let loop = wrap $ liftM (construct . (:> loop)) ma
      in loop

lrepeatM :: Monad m => m a -> Fold (Of a) m r
lrepeatM ma = Fold (jrepeatM ma)

repeatM :: Monad m => m a -> ListM (Of a) m r
repeatM ma = loop where
  loop = Wrap $ ma >>= \a -> return (Construct (a :> loop))
  
repeatMF = buildListM . lrepeat
-- -- | 'repeat' @x@ is an infinite list, with @x@ the value of every element.
-- repeat :: a -> [a]
-- {-# INLINE [0] repeat #-}
-- -- The pragma just gives the rules more chance to fire
-- repeat x = xs where xs = x : xs
-- repeat x = loop where loop = Construct (x :> loop)
-- repeat_ fx = loop where loop = Construct (fmap (\_ -> loop) fx)
-- {-# INLINE [0] repeatFB #-}     -- ditto
-- repeatFB :: (a -> b -> b) -> a -> b
-- repeatFB c x = xs where xs = x `c` xs

-- {-# RULES
-- "repeat"    [~1] forall x. repeat x = build (\c _n -> repeatFB c x)
-- "repeatFB"  [1]  repeatFB (:)       = repeat
--  #-}

-- ---------------
-- filter 
-- ---------------

jfilter :: (Monad m) => (a -> Bool) -> Fold_ (Of a) m r -> Fold_ (Of a) m r
jfilter pred = \phi construct wrap done ->
   phi (\aa@(a :> x) -> if pred a then construct aa else x)
       wrap 
       done
{-# INLINE jfilter #-}

lfilter :: Monad m => (a -> Bool) -> Fold (Of a) m r -> Fold (Of a) m r
lfilter pred = \phi -> Fold (jfilter pred (getFold phi))
{-# INLINE lfilter #-}

filter  :: (Monad m) => (a -> Bool) -> ListM (Of a) m r -> ListM (Of a) m r
filter pred = loop where
  loop = \case Construct (a :> as) -> if pred a then Construct (a :> loop as)
                                                else loop as
               Wrap m -> Wrap $ liftM loop m
               Done r -> Done r
               
filterF pred = buildListM . lfilter pred . foldListM
{-# INLINE filterF #-}

jfilterM :: (Monad m) => (a -> m Bool) -> Fold_ (Of a) m r -> Fold_ (Of a) m r
jfilterM pred = \phi construct wrap done ->
   phi (\aa@(a :> x) -> wrap $ liftM (\b -> if b then construct aa else x) (pred a))
       wrap 
       done
{-# INLINE jfilterM #-}

lfilterM :: Monad m => (a -> m Bool) -> Fold (Of a) m r -> Fold (Of a) m r
lfilterM pred phi = Fold (jfilterM pred (getFold phi))


filterM  :: (Monad m) => (a -> m Bool) -> ListM (Of a) m r -> ListM (Of a) m r
filterM pred = loop where
  loop = \case 
     Construct (a:>as) -> Wrap $ do b <- pred a
                                    if b then return $ Construct (a :> loop as)
                                         else return $ loop as
     Wrap m            -> Wrap $ liftM loop m
     Done r            -> Done r

filterMF  :: (Monad m) => (a -> m Bool) -> ListM (Of a) m r -> ListM (Of a) m r
filterMF pred = buildListM . lfilterM pred . foldListM
-- | 'filter', applied to a predicate and a list, returns the list of
-- those elements that satisfy the predicate; i.e.,
--
-- > filter p xs = [ x | x <- xs, p x]

-- {-# NOINLINE [1] filter #-}
-- filter :: (a -> Bool) -> [a] -> [a]
-- filter _pred []    = []
-- filter pred (x:xs)
--   | pred x         = x : filter pred xs
--   | otherwise      = filter pred xs
-- 
-- {-# NOINLINE [0] filterFB #-}
-- filterFB :: (a -> b -> b) -> (a -> Bool) -> a -> b -> b
-- filterFB c p x r | p x       = x `c` r
--                  | otherwise = r
-- 
-- {-# RULES
-- "filter"     [~1] forall p xs.  filter p xs = build 
--       (\c n -> foldr (filterFB c p) n xs)
-- "filterListM" [1]  forall p.     foldr (filterFB (:) p) [] = filter p
-- "filterFB"        forall c p q. filterFB (filterFB c p) q = 
-- filterFB c (\x -> q x && p x)
--  #-}
-- 
-- Note the filterFB rule, which has p and q the "wrong way round" in the RHS.
--     filterFB (filterFB c p) q a b
--   = if q a then filterFB c p a b else b
--   = if q a then (if p a then c a b else b) else b
--   = if q a && p a then c a b else b
--   = filterFB c (\x -> q x && p x) a b
-- I originally wrote (\x -> p x && q x), which is wrong, and actually
-- gave rise to a live bug report.  SLPJ.

-- ---------------
-- drop
-- ---------------

ldrop :: Monad m => Int -> Fold (Of a) m r -> Fold (Of a) m r
ldrop n = \phi -> Fold (jdrop n (getFold phi))
{-# INLINE ldrop #-}

jdrop :: Monad m => Int -> Fold_ (Of a) m r -> Fold_ (Of a) m r
jdrop m = \phi construct wrap done -> 
   phi  
    (\(a :> fn) n -> if n <= m then fn (n+1) else construct (a :> (fn (n+1))))
    (\m n -> wrap (m >>= \fn -> return (fn n)))
    (\r _ -> done r)
    1
{-# INLINE jdrop #-}

drop :: (Monad m) => Int -> ListM (Of a) m r -> ListM (Of a) m r
drop = loop where
  loop 0 p = p
  loop n p = case p of
     Construct (a :> as) -> loop (n-1) as
     Wrap ma      -> Wrap (liftM (drop n) ma)
     Done r       -> Done r

dropF n = buildListM . ldrop n . foldListM
{-# INLINE dropF #-}

-- ---------------
-- map
-- ---------------
lmap f = \fold -> Fold (jmap f (getFold fold))
{-# INLINE lmap #-}

jmap :: (a -> b) -> Fold_ (Of a) m r -> Fold_ (Of b) m r
jmap f = \phi construct wrap done -> 
      phi (\(a :> x) -> construct (f a :> x)) 
          wrap 
          done 
{-# INLINE jmap #-}
map f = loop where
  loop = \case Construct (a :> as) -> Construct (f a :> loop as)
               Wrap m -> Wrap (liftM (map f) m)
               Done r -> Done r

mapF f = buildListM . lmap f . foldListM
{-# INLINE mapF #-}

jmapM :: Monad m => (a -> m b) -> Fold_ (Of a) m r -> Fold_ (Of b) m r
jmapM f = \phi construct wrap done -> 
      phi (\(a :> x) -> wrap (liftM (construct . (:> x)) (f a)))
          wrap 
          done        
{-# INLINE jmapM #-}
lmapM :: Monad m => (a -> m b) -> Fold (Of a) m r -> Fold (Of b) m r
lmapM f = \(Fold phi) -> Fold (jmapM f phi)
{-# INLINE lmapM #-}
mapMF :: Monad m => (a -> m b) -> ListM (Of a) m r -> ListM (Of b) m r
mapMF f = buildListM . lmapM f . foldListM
{-# INLINE mapMF #-}
mapM :: Monad m => (a -> m b) -> ListM (Of a) m r -> ListM (Of b) m r
mapM f = loop where
  loop = \case Construct (a :> as) -> Wrap $ liftM (Construct.(:> loop as)) (f a)
-- map :: (a -> b) -> [a] -> [b]
-- {-# NOINLINE [1] map #-}    -- We want the RULE to fire first.
--                             -- It's recursive, so won't inline anyway,
--                             -- but saying so is more explicit
-- map _ []     = []
-- map f (x:xs) = f x : map f xs
-- 
-- -- Note eta expanded
-- mapFB ::  (elt -> lst -> lst) -> (a -> elt) -> a -> lst -> lst
-- {-# INLINE [0] mapFB #-}
-- mapFB c f = \x ys -> c (f x) ys

-- The rules for map work like this.
--
-- Up to (but not including) phase 1, we use the "map" rule to
-- rewrite all saturated applications of map with its build/fold
-- form, hoping for fusion to happen.
-- In phase 1 and 0, we switch off that rule, inline build, and
-- switch on the "mapListM" rule, which rewrites the foldr/mapFB
-- thing back into plain map.
--
-- It's important that these two rules aren't both active at once
-- (along with build's unfolding) else we'd get an infinite loop
-- in the rules.  Hence the activation control below.
--
-- The "mapFB" rule optimises compositions of map.
--
-- This same pattern is followed by many other functions:
-- e.g. append, filter, iterate, repeat, etc.

-- {-# RULES
-- "map"       [~1] forall f xs.   map f xs                = build 
--                                   (\c n -> foldr (mapFB c f) n xs)
-- "mapListM"   [1]  forall f.      foldr (mapFB (:) f) []  = map f
-- "mapFB"     forall c f g.       mapFB (mapFB c f) g     = mapFB c (f.g)
--   #-}

-- See Breitner, Eisenberg, Peyton Jones, and Weirich, "Safe Zero-cost
-- Coercions for Haskell", section 6.5:
--   http://research.microsoft.com/en-us/um/people/simonpj/papers
--   /ext-f/coercible.pdf

-- {-# RULES "map/coerce" [1] map coerce = coerce #-}

-- ---------------
-- take
-- ---------------

ltake :: Monad m => Int -> Fold (Of a) m r -> Fold (Of a) m ()
ltake n = \(Fold phi)  -> Fold (jtake n phi)
{-# INLINE ltake #-}
--      phi 
--      (\(a :> fn) n -> if n <= 0 then rout () else fout (a :> (fn (n-1))))
--      (\m n -> mout (liftM ($n) m)) 
--      (\r n -> rout ()) 
--      n)
jtake :: Monad m => Int -> Fold_ (Of a) m r -> Fold_ (Of a) m ()
jtake n phi = \construct wrap done -> phi 
      (\(a :> fn) n -> if n <= 0 then done () else construct (a :> (fn (n-1))))
      (\m n -> if n <= 0 then done () else wrap (liftM ($n) m)) 
      (\r n -> done ()) 
      n
{-# INLINE jtake #-}

take :: Monad m => Int -> ListM (Of a) m r -> ListM (Of a) m ()
take = loop where
  loop 0 p = return ()
  loop n p = 
    case p of Construct (a :> as) -> Construct (a :> loop (n-1) as)
              Wrap m -> Wrap (liftM (loop n) m)
              Done r -> Done ()

takeF n = buildListM . ltake n . foldListM
{-# INLINE takeF #-}
-- 
-- 

-- replicate :: Monad m => Int -> a -> ListM (Of a) m ()
-- replicate n x = loop n where
--   loop 0 = Done ()
--   loop n = Construct (x :> loop (n-1))
-- 
-- replicate_ :: (Functor f) => Int -> f a -> ListM f m ()
-- replicate_ n fx = loop n where
--   loop 0 = Done ()
--   loop n = Construct (fmap (\_ -> loop (n-1)) fx)
--   
-- splitAt :: (Functor f, Monad m) => Int -> ListM f m r -> ListM f m (ListM f m r)
-- splitAt 0 ls = return ls
-- splitAt n ls = case ls of 
--   Wrap m -> Wrap (liftM (splitAt n) m)

-- 
-- -- | 'replicate' @n x@ is a list of length @n@ with @x@ the value of
-- -- every element.
-- -- It is an instance of the more general 'Data.ListM.genericReplicate',
-- -- in which @n@ may be of any integral type.
-- {-# INLINE replicate #-}
-- replicate               :: Int -> a -> [a]
-- replicate n x           =  take n (repeat x)




