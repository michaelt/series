
{-# LANGUAGE LambdaCase, RankNTypes #-}
module ListM.Fusion where
import ListM.Types
import ListM.Combinators
import Control.Monad
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
lsum (Fold phi) = sum phi
{-# INLINE lsum #-}

sum :: (Monad m, Num a) => Fold_ (Of a) m () -> m a
sum  = \phi -> phi (\(n :> mm) -> mm >>= \m -> return (m+n))
                    join
                    (\_ -> return 0) 
{-# INLINE sum #-}

-- ---------------
-- replicate 
-- ---------------
replicate :: Monad m => Int -> a -> Fold_ (Of a) m ()
replicate n a = take n (repeat a)
{-# INLINE replicate #-}

lreplicate :: Monad m => Int -> a -> Fold (Of a) m ()
lreplicate n a = Fold (take n (repeat a))
{-# INLINE lreplicate #-}

-- ---------------
-- iterate
-- ---------------
literate :: (a -> a) -> a -> Fold (Of a) m r
literate f a = Fold (iterate f a)
{-# INLINE literate #-}

iterate :: (a -> a) -> a -> Fold_ (Of a) m r
iterate f a = \construct wrap done -> 
     let loop x = construct (x :> loop (f x)) in loop a
{-# INLINE iterate #-}

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

repeat :: a -> Fold_ (Of a) m r
repeat a = \construct wrap done -> let loop = construct (a :> loop) in loop

lrepeat :: a -> Fold (Of a) m r
lrepeat a = Fold (repeat a)

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

filter :: (Monad m) => (a -> Bool) -> Fold_ (Of a) m r -> Fold_ (Of a) m r
filter pred phi construct wrap done = 
   phi (\aa@(a :> x) -> if pred a then construct aa else x)
       wrap 
       done
{-# INLINE filter #-}


lfilter :: Monad m => (a -> Bool) -> Fold (Of a) m r -> Fold (Of a) m r
lfilter pred phi = Fold (filter pred (getFold phi))
{-# INLINE lfilter #-}


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
ldrop n (Fold phi) = Fold (drop n phi)
{-# INLINE ldrop #-}

drop :: Monad m => Int -> Fold_ (Of a) m r -> Fold_ (Of a) m r
drop m phi fout mout rout = 
   phi  
    (\(a :> fn) n -> if n <= m then fn (n+1) else fout (a :> (fn (n+1))))
    (\m n -> mout (m >>= \fn -> return (fn n)))
    (\r _ -> rout r)
    0
{-# INLINE drop #-}


-- ---------------
-- map
-- ---------------
lmap f fold =  Fold (map f (getFold fold))

map :: (a -> b) -> Fold_ (Of a) m r -> Fold_ (Of b) m r
map f phi  = \construct wrap done -> 
      phi (\(a :> x) -> construct (f a :> x)) 
          wrap 
          done 
          
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
ltake n = \(Fold phi)  -> Fold (take n phi)
{-# INLINE ltake #-}
--      phi 
--      (\(a :> fn) n -> if n <= 0 then rout () else fout (a :> (fn (n-1))))
--      (\m n -> mout (liftM ($n) m)) 
--      (\r n -> rout ()) 
--      n)
take :: Monad m => Int -> Fold_ (Of a) m r -> Fold_ (Of a) m ()
take n phi = \fout mout rout -> phi 
      (\(a :> fn) n -> if n <= 0 then rout () else fout (a :> (fn (n-1))))
      (\m n -> mout (liftM ($n) m)) 
      (\r n -> rout ()) 
      n
{-# INLINE take #-}




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




