
{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables #-}
module Series.Producer.Prelude where
import Series.Types
import Series.Folding.Prelude
import Control.Monad hiding (filterM, mapM)
import Data.Functor.Identity
import Control.Monad.Trans
import qualified System.IO as IO
import Pipes hiding (yield)
import Prelude hiding (map, filter, drop, take, sum
                      , iterate, repeat, replicate, splitAt
                      , takeWhile, enumFrom, enumFromTo)


-- ---------------
-- ---------------
-- Prelude
-- ---------------
-- ---------------

yield :: Monad m => a -> Producer a m ()
yield = buildProducer . lyield
{-# INLINE yield #-}

-- -------
-- foldl'
-- -------
foldl' :: Monad m => forall a b . (b -> a -> b) -> b -> Producer a m r -> m b
foldl' op b0 = (\(Folding phi) -> foldl_ op b0 phi)
              . foldProducer
{-# INLINE foldl' #-}

-- -------
-- scanr
-- -------

scanr :: Monad m => (a -> b -> b) -> b -> Producer a m r -> Producer b m r
scanr op b = buildProducer 
           . (\(Folding phi) -> Folding (jscanr op b phi)) 
           . foldProducer 
{-# INLINE scanr #-}


scanr2 :: Monad m => (a -> b -> b) -> b -> Producer a m r -> Producer b m r
scanr2 op b = buildProducer 
              . (\(Folding phi) -> Folding (lscanr_ phi op b)) 
              . foldProducer 
{-# INLINE scanr2 #-}

-- ---------------
-- sum 
-- ---------------

sum :: (Monad m, Num a) => Producer a m () -> m a
sum  = lsum . foldProducer 
{-# INLINE sum #-}

sum2 :: (Monad m, Num n) => Producer n m r -> m n
sum2 = foldl' (+) 0 
{-# INLINE sum2 #-}

-- ---------------
-- replicate 
-- ---------------

replicate :: Monad m => Int -> a -> Producer a m ()
replicate n = take n . repeat
{-# INLINE replicate #-}

replicateM :: Monad m => Int -> m a -> Producer a m ()
replicateM n a = buildProducer (lreplicateM n a)
{-# INLINE replicateM #-}

-- ---------------
-- iterate
-- ---------------

iterate :: Monad m => (a -> a) -> a -> Producer a m r
iterate f  = buildProducer . literate f 
{-# INLINE iterate #-}

iterateM :: Monad m => (a -> m a) -> m a -> Producer a m r
iterateM = \f m -> buildProducer (literateM f m)
{-# INLINE iterateM #-}

-- ---------------
-- repeat
-- ---------------

repeat :: Monad m => a -> Producer a m r
repeat = buildProducer . (\a -> Folding (repeat_ a))
{-# INLINE repeat #-}

repeatM :: Monad m => m a -> Producer a m r
repeatM = buildProducer . lrepeatM
{-# INLINE repeatM #-}

-- ---------------
-- filter 
-- ---------------
filter2 :: (Monad m) => (a -> Bool) -> Producer a m r -> Producer a m r 
filter2 pred = buildProducer . (\(Folding phi) -> Folding (filter_ phi pred)) . foldProducer

filter  :: (Monad m) => (a -> Bool) -> Producer a m r -> Producer a m r               
filter pred = buildProducer . lfilter pred . foldProducer
{-# INLINE filter #-}

filterM  :: (Monad m) => (a -> m Bool) -> Producer a m r -> Producer a m r
filterM pred = buildProducer . lfilterM pred . foldProducer
{-# INLINE filterM #-}

-- ---------------
-- drop
-- ---------------

drop :: (Monad m) => Int -> Producer a m r -> Producer a m r
drop n = buildProducer . ldrop n . foldProducer
{-# INLINE drop #-}


-- ---------------
-- take
-- ---------------


take2 :: (Monad m) => Int -> Producer a m r -> Producer a m ()
take2 n = buildProducer . (\(Folding phi)  -> Folding (jtake_ phi n)) . foldProducer
{-# INLINE take2 #-}

take :: (Monad m) => Int -> Producer a m r -> Producer a m ()
take n = buildProducer . ltake n . foldProducer
{-# INLINE take #-}

takeWhile :: Monad m => (a -> Bool) -> Producer a m r -> Producer a m ()
takeWhile pred = buildProducer . ltakeWhile pred . foldProducer 
{-# INLINE takeWhile #-}

takeWhile2 :: Monad m => (a -> Bool) -> Producer a m r -> Producer a m ()
takeWhile2 pred = buildProducer 
               . (\(Folding fold) -> Folding (jtakeWhile_ fold pred)) 
               . foldProducer
{-# INLINE takeWhile2#-}

-- ---------------
-- map
-- ---------------

map2 :: Monad m => (a -> b) -> Producer a m r -> Producer b m r
map2 f = buildProducer . (\(Folding phi) -> Folding (map_ phi f)) . foldProducer
{-# INLINE map2 #-}

map :: Monad m => (a -> b) -> Producer a m r -> Producer b m r
map f = buildProducer . lmap f . foldProducer
{-# INLINE map #-}

mapM :: Monad m => (a -> m b) -> Producer a m r -> Producer b m r
mapM f = buildProducer . lmapM f . foldProducer
{-# INLINE mapM #-}




enumFrom n = buildProducer (Folding (lenumFrom n))
enumFromTo n m = buildProducer (Folding (lenumFromTo n m))
enumFromToStep n m k = buildProducer (Folding (lenumFromToStep n m k))

enumFromStepN :: (Monad m, Num a) => a -> a -> Int -> Producer a m ()
enumFromStepN start step n = buildProducer (Folding (lenumFromStepN start step n))
{-# INLINE enumFromStepN #-}


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
stdinLn :: MonadIO m => Producer String m ()
stdinLn = fromHandle IO.stdin
{-# INLINABLE stdinLn #-}

-- | 'read' values from 'IO.stdin', ignoring failed parses
readLn :: (MonadIO m, Read a) => Producer a m ()
readLn = map read stdinLn
{-# INLINABLE readLn #-}

{-| Read 'String's from a 'IO.Handle' using 'IO.hGetLine'

    Terminates on end of input
-}
fromHandle :: MonadIO m => IO.Handle -> Producer String m ()
fromHandle h = go
  where
    go = do
        eof <- liftIO $ IO.hIsEOF h
        unless eof $ do
            str <- liftIO $ IO.hGetLine h
            yield str
            go
{-# INLINABLE fromHandle #-}     

