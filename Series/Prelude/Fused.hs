{-# LANGUAGE LambdaCase, RankNTypes #-}
module Series.Prelude.Fused where
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
-- 
-- yield :: Monad m => a -> Series (Of a) m ()
-- yield r = Construct (r :> Done ())

jyield :: Monad m => a -> Folding_ (Of a) m ()
jyield r = \construct wrap done -> construct (r :> done ())
{-# INLINE jyield #-}

lyield :: Monad m => a -> Folding (Of a) m ()
lyield r = Folding (jyield r)
{-# INLINE lyield #-}

yield :: Monad m => a -> Series (Of a) m ()
yield = buildSeries . lyield
{-# INLINE yield #-}

-- ---------------
-- sum 
-- ---------------
lsum :: (Monad m, Num a) => Folding (Of a) m () -> m a
lsum = \phi -> jsum (getFolding phi)
{-# INLINE lsum #-}

jsum :: (Monad m, Num a) => Folding_ (Of a) m () -> m a
jsum  = \phi -> phi (\(n :> mm) -> mm >>= \m -> return (m+n))
                    join
                    (\_ -> return 0) 
{-# INLINE jsum #-}
-- sum :: (Monad m, Num a) => Series (Of a) m () -> m a
-- sum = loop where
--   loop = \case Construct (a :> as) -> liftM (a+) (loop as)
--                Wrap m -> m >>= loop
--                Done r -> return 0

sum :: (Monad m, Num a) => Series (Of a) m () -> m a
sum  = lsum . foldSeries 
{-# INLINE sum #-}
-- 
-- sumG :: (Monad m, Num a) => Series (Of a) m () -> m a
-- sumG  = \ls -> jsum (foldSeriesx ls)
-- {-# INLINE sumG #-}

-- ---------------
-- replicate 
-- ---------------

jreplicate :: Monad m => Int -> a -> Folding_ (Of a) m ()
jreplicate n a = jtake n (jrepeat a)
{-# INLINE jreplicate #-}

lreplicate :: Monad m => Int -> a -> Folding (Of a) m ()
lreplicate n a = Folding (jtake n (jrepeat a))
{-# INLINE lreplicate #-}

-- replicate :: Monad m => Int -> a -> Series (Of a) m ()
-- replicate n a = loop n where
--   loop 0 = Done ()
--   loop m = Construct (a :> loop (m-1))

replicate :: Monad m => Int -> a -> Series (Of a) m ()
replicate n = take n . repeat
 -- (Folding (\construct wrap done -> 
 --   let loop 0 = done ()
 --       loop m = construct (a :> loop (m-1))
 --   in loop n))
{-# INLINE replicate #-}

-- replicateG :: Monad m => Int -> a -> Series (Of a) m ()
-- replicateG = \n a -> buildSeriesx (jreplicate n a)
-- {-# INLINE replicateG #-}

jreplicateM :: Monad m => Int -> m a -> Folding_ (Of a) m ()
jreplicateM n a = jtake n (jrepeatM a)
{-# INLINE jreplicateM #-}

lreplicateM :: Monad m => Int -> m a -> Folding (Of a) m ()
lreplicateM n a = Folding (jtake n (jrepeatM a))
{-# INLINE lreplicateM #-}

replicateM :: Monad m => Int -> m a -> Series (Of a) m ()
replicateM n a = buildSeries (lreplicateM n a)
{-# INLINE replicateM #-}
-- replicateM :: Monad m => Int -> m a -> Series (Of a) m ()
-- replicateM n ma = loop n where 
--   loop 0 = Done ()
--   loop n = Wrap $ ma >>= \a -> return (Construct $ a :> loop (n-1))
-- 


-- ---------------
-- iterate
-- ---------------
literate :: (a -> a) -> a -> Folding (Of a) m r
literate f a = Folding (jiterate f a)
{-# INLINE literate #-}

jiterate :: (a -> a) -> a -> Folding_ (Of a) m r
jiterate = \f a construct wrap done -> 
       construct (a :> jiterate f (f a) construct wrap done) 
{-# INLINE jiterate #-}
-- iterate :: (a -> a) -> a -> Series (Of a) m r
-- iterate f = loop where
--   loop a' = Construct (a' :> loop (f a'))

iterate :: (a -> a) -> a -> Series (Of a) m r
iterate f  = buildSeries . literate f 
{-# INLINE iterate #-}

-- iterateG :: (a -> a) -> a -> Series (Of a) m r
-- iterateG = \f a -> buildSeriesx (jiterate f a)
-- {-# INLINE iterateG #-}

jiterateM :: Monad m => (a -> m a) -> m a -> Folding_ (Of a) m r
jiterateM f ma = \construct wrap done -> 
     let loop mx = wrap $ liftM (\x -> construct (x :> loop (f x))) mx
     in loop ma
{-# INLINE jiterateM #-}

literateM :: Monad m => (a -> m a) -> m a -> Folding (Of a) m r
literateM f a = Folding (jiterateM f a)
{-# INLINE literateM #-}
-- iterateM :: Monad m => (a -> m a) -> m a -> Series (Of a) m r
-- iterateM f = loop where
--   loop ma  = Wrap $ do a <- ma
--                        return (Construct (a :> loop (f a)))

iterateM :: Monad m => (a -> m a) -> m a -> Series (Of a) m r
iterateM = \f m -> buildSeries (literateM f m)
{-# INLINE iterateM #-}
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

jrepeat :: a -> Folding_ (Of a) m r
jrepeat = \a construct wrap done -> let loop = construct (a :> loop) in loop
{-# INLINE jrepeat #-}

lrepeat :: a -> Folding (Of a) m r
lrepeat a = Folding (jrepeat a)
{-# INLINE lrepeat #-}
-- repeat :: a -> Series (Of a) m r
-- repeat a = loop where
--   loop = Construct (a :> loop)
--   
repeat :: a -> Series (Of a) m r
repeat = buildSeries . lrepeat
{-# INLINE repeat #-}
-- repeatG :: a -> Series (Of a) m r
-- repeatG = \x -> buildSeriesx (jrepeat x)
-- {-# INLINE repeatG #-}

jrepeatM :: Monad m => m a -> Folding_ (Of a) m r
jrepeatM ma = \construct wrap done -> 
      let loop = wrap $ liftM (construct . (:> loop)) ma
      in loop
{-# INLINE jrepeatM #-}

lrepeatM :: Monad m => m a -> Folding (Of a) m r
lrepeatM ma = Folding (jrepeatM ma)
{-# INLINE lrepeatM #-}
-- repeatM :: Monad m => m a -> Series (Of a) m r
-- repeatM ma = loop where
--   loop = Wrap $ ma >>= \a -> return (Construct (a :> loop))

repeatM :: Monad m => m a -> Series (Of a) m r
repeatM = buildSeries . lrepeatM
{-# INLINE repeatM #-}

-- repeatMG :: Monad m => m a -> Series (Of a) m r
-- repeatMG = \x -> buildSeriesx (jrepeatM x)

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

jfilter :: (Monad m) => (a -> Bool) -> Folding_ (Of a) m r -> Folding_ (Of a) m r
jfilter = \pred phi construct wrap done ->
   phi (\aa@(a :> x) -> if pred a then construct aa else x)
       wrap 
       done
{-# INLINE jfilter #-}

lfilter :: Monad m => (a -> Bool) -> Folding (Of a) m r -> Folding (Of a) m r
lfilter pred = \phi -> Folding (jfilter pred (getFolding phi))
{-# INLINE lfilter #-}

-- filter  :: (Monad m) => (a -> Bool) -> Series (Of a) m r -> Series (Of a) m r
-- filter pred = loop where
--   loop = \case Construct (a :> as) -> if pred a then Construct (a :> loop as)
--                                                 else loop as
--                Wrap m -> Wrap $ liftM loop m
--                Done r -> Done r
-- --
filter  :: (Monad m) => (a -> Bool) -> Series (Of a) m r -> Series (Of a) m r               
filter pred = buildSeries . lfilter pred . foldSeries
{-# INLINE filter #-}

-- filterG  :: (Monad m) => (a -> Bool) -> Series (Of a) m r -> Series (Of a) m r
-- filterG = \pred phi -> buildSeriesx (jfilter pred  (foldSeriesx phi))
-- {-# INLINE filterG #-}

jfilterM :: (Monad m) => (a -> m Bool) -> Folding_ (Of a) m r -> Folding_ (Of a) m r
jfilterM pred = \phi construct wrap done ->
   phi (\aa@(a :> x) -> wrap $ liftM (\b -> if b then construct aa else x) (pred a))
       wrap 
       done
{-# INLINE jfilterM #-}

lfilterM :: Monad m => (a -> m Bool) -> Folding (Of a) m r -> Folding (Of a) m r
lfilterM pred phi = Folding (jfilterM pred (getFolding phi))


-- filterM  :: (Monad m) => (a -> m Bool) -> Series (Of a) m r -> Series (Of a) m r
-- filterM pred = loop where
--   loop = \case 
--      Construct (a:>as) -> Wrap $ do b <- pred a
--                                     if b then return $ Construct (a :> loop as)
--                                          else return $ loop as
--      Wrap m            -> Wrap $ liftM loop m
--      Done r            -> Done r
-- 
filterM  :: (Monad m) => (a -> m Bool) -> Series (Of a) m r -> Series (Of a) m r
filterM pred = buildSeries . lfilterM pred . foldSeries
{-# INLINE filterM #-}
-- filterMG  :: (Monad m) => (a -> m Bool) -> Series (Of a) m r -> Series (Of a) m r
-- filterMG pred = \phi -> buildSeries (lfilterM pred (foldSeries phi))

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
-- "filterSeries" [1]  forall p.     foldr (filterFB (:) p) [] = filter p
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

-- drop :: (Monad m) => Int -> Series (Of a) m r -> Series (Of a) m r
-- drop = loop where
--   loop 0 p = p
--   loop n p = case p of
--      Construct (a :> as) -> loop (n-1) as
--      Wrap ma      -> Wrap (liftM (drop n) ma)
--      Done r       -> Done r

drop :: (Monad m) => Int -> Series (Of a) m r -> Series (Of a) m r
drop n = buildSeries . ldrop n . foldSeries
{-# INLINE drop #-}
-- 
-- dropG :: (Monad m) => Int -> Series (Of a) m r -> Series (Of a) m r
-- dropG = \n phi -> buildSeriesx (jdrop n (foldSeriesx phi))
-- {-# INLINE dropG #-}
-- ---------------
-- map
-- ---------------

lmap f = \fold -> Folding (jmap f (getFolding fold))
{-# INLINE lmap #-}

jmap :: (a -> b) -> Folding_ (Of a) m r -> Folding_ (Of b) m r
jmap = \f phi construct wrap done -> 
      phi (\(a :> x) -> construct (f a :> x)) 
          wrap 
          done 
{-# INLINE jmap #-}
-- 
-- map f = loop where
--   loop = \case Construct (a :> as) -> Construct (f a :> loop as)
--                Wrap m -> Wrap (liftM (map f) m)
--                Done r -> Done r

map f = buildSeries . lmap f . foldSeries
{-# INLINE map #-}
-- mapG
--   :: Monad m2 => (a -> b) -> Series (Of a) m2 r2 -> Series (Of b) m2 r2
-- mapG = \f phi -> buildSeriesx (jmap f (foldSeriesx phi))
-- {-# INLINE mapG #-}

jmapM :: Monad m => (a -> m b) -> Folding_ (Of a) m r -> Folding_ (Of b) m r
jmapM f = \phi construct wrap done -> 
      phi (\(a :> x) -> wrap (liftM (construct . (:> x)) (f a)))
          wrap 
          done        
{-# INLINE jmapM #-}
lmapM :: Monad m => (a -> m b) -> Folding (Of a) m r -> Folding (Of b) m r
lmapM f = \(Folding phi) -> Folding (jmapM f phi)
{-# INLINE lmapM #-}

-- mapM :: Monad m => (a -> m b) -> Series (Of a) m r -> Series (Of b) m r
-- mapM f = loop where
--   loop = \case Construct (a :> as) -> Wrap $ liftM (Construct.(:> loop as)) (f a)

mapM :: Monad m => (a -> m b) -> Series (Of a) m r -> Series (Of b) m r
mapM f = buildSeries . lmapM f . foldSeries
{-# INLINE mapM #-}
--
-- mapMG :: Monad m => (a -> m b) -> Series (Of a) m r -> Series (Of b) m r
-- mapMG f = \phi -> buildSeriesx (jmapM f (foldSeriesx phi))
-- {-# INLINE mapMG #-}
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
-- switch on the "mapSeries" rule, which rewrites the foldr/mapFB
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
-- "mapSeries"   [1]  forall f.      foldr (mapFB (:) f) []  = map f
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



jtake :: (Monad m, Functor f) => Int -> Folding_ f m r -> Folding_ f m ()
jtake n phi = \construct wrap done -> phi 
      (\fx n -> if n <= 0 then done () else construct (fmap ($(n-1)) fx))
      (\mx n -> if n <= 0 then done () else wrap (liftM ($n) mx)) 
      (\r n -> done ()) 
      n
{-# INLINE jtake #-}

ltake :: (Monad m, Functor f) => Int -> Folding f m r -> Folding f m ()
ltake n = \(Folding phi)  -> Folding (jtake n phi)
{-# INLINE ltake #-}
-- 
-- take :: (Monad m, Functor f) => Int -> Series f m r -> Series f m ()
-- take = loop where
--   loop 0 p = return ()
--   loop n p = case p of Construct fas -> Construct (fmap (loop (n-1)) fas)
--                        Wrap m -> Wrap (liftM (loop n) m)
--                        Done r -> Done ()

take :: (Monad m, Functor f) => Int -> Series f m r -> Series f m ()
take n = buildSeries . ltake n . foldSeries
{-# INLINE take #-}
-- 
-- takeG :: (Monad m, Functor f) => Int -> Series f m r -> Series f m ()
-- takeG = \n phi -> buildSeriesx (jtake n  (foldSeriesx phi))
-- {-# INLINE takeG #-}


-- replicate :: Monad m => Int -> a -> Series (Of a) m ()
-- replicate n x = loop n where
--   loop 0 = Done ()
--   loop n = Construct (x :> loop (n-1))
-- 
-- replicate_ :: (Functor f) => Int -> f a -> Series f m ()
-- replicate_ n fx = loop n where
--   loop 0 = Done ()
--   loop n = Construct (fmap (\_ -> loop (n-1)) fx)
--   
-- splitAt :: (Functor f, Monad m) => Int -> Series f m r -> Series f m (Series f m r)
-- splitAt 0 ls = return ls
-- splitAt n ls = case ls of 
--   Wrap m -> Wrap (liftM (splitAt n) m)

-- 
-- -- | 'replicate' @n x@ is a list of length @n@ with @x@ the value of
-- -- every element.
-- -- It is an instance of the more general 'Data.Series.genericReplicate',
-- -- in which @n@ may be of any integral type.
-- {-# INLINE replicate #-}
-- replicate               :: Int -> a -> [a]
-- replicate n x           =  take n (repeat x)
-- 
-- takeWhile :: Monad m => (a -> Bool) -> Series (Of a) m r -> Series (Of a) m ()
-- takeWhile pred = loop where
--   loop = \case Construct (a :> as) -> if pred a then Construct (a :> loop as)
--                                                 else return () 
--                Wrap m              -> Wrap (liftM loop m)
--                Done r              -> Done ()

jtakeWhile :: Monad m => (a -> Bool) -> Folding_ (Of a) m r -> Folding_ (Of a) m ()
jtakeWhile = \pred phi construct wrap done -> phi
  (\(a :> fn) p -> if not (pred a) 
                      then done () 
                      else construct (a :> (fn True)))
  (\m p -> if not p then done () else wrap (liftM ($p) m)) 
  (\r p -> done ()) 
  True 
{-# INLINE jtakeWhile #-}

ltakeWhile :: Monad m => (a -> Bool) -> Folding (Of a) m r -> Folding (Of a) m ()
ltakeWhile pred = \(Folding fold) -> Folding (jtakeWhile pred fold)
{-# INLINE ltakeWhile #-}
takeWhile :: Monad m => (a -> Bool) -> Series (Of a) m r -> Series (Of a) m ()
takeWhile pred = buildSeries . ltakeWhile pred . foldSeries 
{-# INLINE takeWhile #-}
-- takeWhileG :: Monad m => (a -> Bool) -> Series (Of a) m r -> Series (Of a) m ()
-- takeWhileG = \pred phi -> buildSeriesx  (jtakeWhile pred  (foldSeriesx phi))
-- {-# INLINE takeWhileG #-}




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
--


enumFrom n = buildSeries (Folding (lenumFrom n))
enumFromTo n m = buildSeries (Folding (lenumFromTo n m))
enumFromToStep n m k = buildSeries (Folding (lenumFromToStep n m k))

enumFromStepN :: (Monad m, Num a) => a -> a -> Int -> Series (Of a) m ()
enumFromStepN start step n = buildSeries (Folding (lenumFromStepN start step n))
{-# INLINE enumFromStepN #-}
--
lenumFromStepN start step n = \construct wrap done -> 
               let loop p 0 = done ()
                   loop p now = construct (p :> loop (p + step) (now-1)) 
               in loop start n


-- ---------------------------------------
-- IO fripperies copped from Pipes.Prelude
-- ---------------------------------------


-- stdinLn = Wrap loop where
--   loop = getLine >>= \str -> return (Construct (str :> Wrap loop))
-- 
-- jstdinLn = \construct wrap done -> 
--      let loop = wrap $ getLine >>= \str -> return (construct (str :> loop))
--      in loop 
--
stdinLn :: MonadIO m => Series (Of String) m ()
stdinLn = fromHandle IO.stdin
{-# INLINABLE stdinLn #-}

-- | 'read' values from 'IO.stdin', ignoring failed parses
readLn :: (MonadIO m, Read a) => Series (Of a) m ()
readLn = map read stdinLn
{-# INLINABLE readLn #-}

{-| Read 'String's from a 'IO.Handle' using 'IO.hGetLine'

    Terminates on end of input
-}
fromHandle :: MonadIO m => IO.Handle -> Series (Of String) m ()
fromHandle h = go
  where
    go = do
        eof <- liftIO $ IO.hIsEOF h
        unless eof $ do
            str <- liftIO $ IO.hGetLine h
            yield str
            go
{-# INLINABLE fromHandle #-}     
--

