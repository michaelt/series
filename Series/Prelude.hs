{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables #-}
module Series.Prelude where
import Series.Folding.Prelude
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
yield = buildSeries . lyield
{-# INLINE yield #-}

-- -------
-- foldl'
-- -------
foldl' :: Monad m => forall a b . (b -> a -> b) -> b -> Series (Of a) m r -> m b
foldl' op b0 = (\(Folding phi) -> foldl_ op b0 phi)
              . foldSeries
{-# INLINE foldl' #-}

-- -------
-- scanr
-- -------

scanr :: Monad m => (a -> b -> b) -> b -> Series (Of a) m r -> Series (Of b) m r
scanr op b = buildSeries 
           . (\(Folding phi) -> Folding (jscanr op b phi)) 
           . foldSeries 
{-# INLINE scanr #-}


scanr2 :: Monad m => (a -> b -> b) -> b -> Series (Of a) m r -> Series (Of b) m r
scanr2 op b = buildSeries 
              . (\(Folding phi) -> Folding (lscanr_ phi op b)) 
              . foldSeries 
{-# INLINE scanr2 #-}

-- ---------------
-- sum 
-- ---------------

sum :: (Monad m, Num a) => Series (Of a) m () -> m a
sum  = lsum . foldSeries 
{-# INLINE sum #-}

sum2 :: (Monad m, Num n) => Series (Of n) m r -> m n
sum2 = foldl' (+) 0 
{-# INLINE sum2 #-}

-- ---------------
-- replicate 
-- ---------------

replicate :: Monad m => Int -> a -> Series (Of a) m ()
replicate n = take n . repeat
{-# INLINE replicate #-}

replicateM :: Monad m => Int -> m a -> Series (Of a) m ()
replicateM n a = buildSeries (lreplicateM n a)
{-# INLINE replicateM #-}

-- ---------------
-- iterate
-- ---------------

iterate :: (a -> a) -> a -> Series (Of a) m r
iterate f  = buildSeries . literate f 
{-# INLINE iterate #-}

iterateM :: Monad m => (a -> m a) -> m a -> Series (Of a) m r
iterateM = \f m -> buildSeries (literateM f m)
{-# INLINE iterateM #-}

-- ---------------
-- repeat
-- ---------------

repeat :: a -> Series (Of a) m r
repeat = buildSeries . (\a -> Folding (repeat_ a))
{-# INLINE repeat #-}

repeatM :: Monad m => m a -> Series (Of a) m r
repeatM = buildSeries . lrepeatM
{-# INLINE repeatM #-}

-- ---------------
-- filter 
-- ---------------
filter2 :: (Monad m) => (a -> Bool) -> Series (Of a) m r -> Series (Of a) m r 
filter2 pred = buildSeries . (\(Folding phi) -> Folding (filter_ phi pred)) . foldSeries

filter  :: (Monad m) => (a -> Bool) -> Series (Of a) m r -> Series (Of a) m r               
filter pred = buildSeries . lfilter pred . foldSeries
{-# INLINE filter #-}

filterM  :: (Monad m) => (a -> m Bool) -> Series (Of a) m r -> Series (Of a) m r
filterM pred = buildSeries . lfilterM pred . foldSeries
{-# INLINE filterM #-}

-- ---------------
-- drop
-- ---------------

drop :: (Monad m) => Int -> Series (Of a) m r -> Series (Of a) m r
drop n = buildSeries . ldrop n . foldSeries
{-# INLINE drop #-}


-- ---------------
-- take
-- ---------------


take2 :: (Monad m, Functor f) => Int -> Series f m r -> Series f m ()
take2 n = buildSeries . (\(Folding phi)  -> Folding (jtake_ phi n)) . foldSeries
{-# INLINE take2 #-}

take :: (Monad m, Functor f) => Int -> Series f m r -> Series f m ()
take n = buildSeries . ltake n . foldSeries
{-# INLINE take #-}

takeWhile :: Monad m => (a -> Bool) -> Series (Of a) m r -> Series (Of a) m ()
takeWhile pred = buildSeries . ltakeWhile pred . foldSeries 
{-# INLINE takeWhile #-}

takeWhile2 :: Monad m => (a -> Bool) -> Series (Of a) m r -> Series (Of a) m ()
takeWhile2 pred = buildSeries 
               . (\(Folding fold) -> Folding (jtakeWhile_ fold pred)) 
               . foldSeries
{-# INLINE takeWhile2#-}


-- ---------------
-- map
-- ---------------

map2 :: Monad m => (a -> b) -> Series (Of a) m r -> Series (Of b) m r
map2 f = buildSeries . (\(Folding phi) -> Folding (map_ phi f)) . foldSeries
{-# INLINE map2 #-}

map :: Monad m => (a -> b) -> Series (Of a) m r -> Series (Of b) m r
map f = buildSeries . lmap f . foldSeries
{-# INLINE map #-}

mapM :: Monad m => (a -> m b) -> Series (Of a) m r -> Series (Of b) m r
mapM f = buildSeries . lmapM f . foldSeries
{-# INLINE mapM #-}




enumFrom n = buildSeries (Folding (lenumFrom n))
enumFromTo n m = buildSeries (Folding (lenumFromTo n m))
enumFromToStep n m k = buildSeries (Folding (lenumFromToStep n m k))

enumFromStepN :: (Monad m, Num a) => a -> a -> Int -> Series (Of a) m ()
enumFromStepN start step n = buildSeries (Folding (lenumFromStepN start step n))
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


