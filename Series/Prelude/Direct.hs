{-# LANGUAGE LambdaCase, RankNTypes, BangPatterns #-}
module Series.Prelude.Direct where
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

yield :: Monad m => a -> Series (Of a) m ()
yield r = Construct (r :> Done ())
{-# INLINEABLE yield #-}
-- ---------------
-- sum 
-- ---------------
sum :: (Monad m, Num a) => Series (Of a) m () -> m a
sum = loop where
  loop = \case Construct (a :> as) -> liftM (a+) (loop as)
               Wrap m -> m >>= loop
               Done r -> return 0
{-# INLINEABLE sum #-}
-- ---------------
-- replicate 
-- ---------------

replicate :: Monad m => Int -> a -> Series (Of a) m ()
replicate n a = loop n where
  loop 0 = Done ()
  loop m = Construct (a :> loop (m-1))
{-# INLINEABLE replicate #-}

replicateM :: Monad m => Int -> m a -> Series (Of a) m ()
replicateM n ma = loop n where 
  loop 0 = Done ()
  loop n = Wrap $ ma >>= \a -> return (Construct $ a :> loop (n-1))
{-# INLINEABLE replicateM #-}


-- ---------------
-- iterate
-- ---------------

iterate :: (a -> a) -> a -> Series (Of a) m r
iterate f = loop where
  loop a' = Construct (a' :> loop (f a'))
{-# INLINEABLE iterate #-}

iterateM :: Monad m => (a -> m a) -> m a -> Series (Of a) m r
iterateM f = loop where
  loop ma  = Wrap $ do a <- ma
                       return (Construct (a :> loop (f a)))
{-# INLINEABLE iterateM #-}

-- ---------------
-- repeat
-- ---------------

repeat :: a -> Series (Of a) m r
repeat a = loop where
  loop = Construct (a :> loop)
{-# INLINE repeat #-}

repeatM :: Monad m => m a -> Series (Of a) m r
repeatM ma = loop where
  loop = Wrap $ ma >>= \a -> return (Construct (a :> loop))
{-# INLINEABLE repeatM #-}

-- ---------------
-- filter 
-- ---------------

filter  :: (Monad m) => (a -> Bool) -> Series (Of a) m r -> Series (Of a) m r
filter pred = loop where
  loop = \case Construct (a :> as) -> if pred a then Construct (a :> loop as)
                                                else loop as
               Wrap m -> Wrap $ liftM loop m
               Done r -> Done r
{-# INLINEABLE filter #-}

filterM  :: (Monad m) => (a -> m Bool) -> Series (Of a) m r -> Series (Of a) m r
filterM pred = loop where
  loop = \case 
     Construct (a:>as) -> Wrap $ do b <- pred a
                                    if b then return $ Construct (a :> loop as)
                                         else return $ loop as
     Wrap m            -> Wrap $ liftM loop m
     Done r            -> Done r
{-# INLINEABLE filterM #-}

-- ---------------
-- drop
-- ---------------

drop :: (Monad m) => Int -> Series (Of a) m r -> Series (Of a) m r
drop = loop where
  loop 0 p = p
  loop n p = case p of
     Construct (a :> as) -> loop (n-1) as
     Wrap ma      -> Wrap (liftM (drop n) ma)
     Done r       -> Done r
{-# INLINEABLE drop #-}

-- ---------------
-- map
-- ---------------

map f = loop where
  loop = \case Construct (a :> as) -> Construct (f a :> loop as)
               Wrap m -> Wrap (liftM (map f) m)
               Done r -> Done r
{-# INLINEABLE map #-}

mapM :: Monad m => (a -> m b) -> Series (Of a) m r -> Series (Of b) m r
mapM f = loop where
  loop = \case Construct (a :> as) -> Wrap $ liftM (Construct.(:> loop as)) (f a)
{-# INLINEABLE mapM #-}

-- ---------------
-- take
-- ---------------



take :: (Monad m, Functor f) => Int -> Series f m r -> Series f m ()
take = loop where
  loop 0 p = return ()
  loop n p = case p of Construct fas -> Construct (fmap (loop (n-1)) fas)
                       Wrap m -> Wrap (liftM (loop n) m)
                       Done r -> Done ()
{-# INLINEABLE take #-}

takeWhile :: Monad m => (a -> Bool) -> Series (Of a) m r -> Series (Of a) m ()
takeWhile pred = loop where
  loop = \case Construct (a :> as) -> if pred a then Construct (a :> loop as)
                                                else return () 
               Wrap m              -> Wrap (liftM loop m)
               Done r              -> Done ()
{-# INLINEABLE takeWhile #-}



-- ------- 
-- lenumFrom n = \construct wrap done -> 
--       let loop m = construct (m :> loop (succ m)) in loop n
--         
-- lenumFromTo n m = \construct wrap done -> 
--       let loop k = if k <= m then construct (k :> loop (succ k)) 
--                              else done ()
--       in loop n
-- 
-- lenumFromToStep n m k = \construct wrap done -> 
--             let loop p = if p <= k then construct (p :> loop (p + m)) 
--                                    else done ()
--             in loop n
-- --
-- lenumFromStepN a b k = \construct wrap done -> 
--             let loop 0 p = done ()
--                 loop j p = construct (p :> loop (j-1) (p + b)) 
--             in loop a k
-- {-# INLINE lenumFromStepN #-}
-- enumFrom n = buildSeries (Folding (lenumFrom n))
-- enumFromTo n m = buildSeries (Folding (lenumFromTo n m))
-- enumFromToStep n m k = buildSeries (Folding (lenumFromToStep n m k))
enumFromStepN :: (Monad m, Num a) => a -> a -> Int -> Series (Of a) m ()
enumFromStepN start step = loop start
  where
    loop !s 0 = Done ()
    loop s m = Construct (s :> loop (s+step) (m-1))
{-# INLINEABLE enumFromStepN #-}




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

