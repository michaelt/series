{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables #-}
module Series.FreeT.Prelude where
import Series.Types
import Series.Folding.Prelude
import Control.Monad hiding (filterM, mapM)
import Data.Functor.Identity
import Control.Monad.Trans
import qualified System.IO as IO
import Control.Monad.Trans.Free
import Prelude hiding (map, filter, drop, take, sum
                      , iterate, repeat, replicate, splitAt
                      , takeWhile, enumFrom, enumFromTo)



-- ---------------
-- ---------------
-- Prelude
-- ---------------
-- ---------------

yield :: Monad m => a -> FreeT (Of a) m ()
yield = buildFreeT . lyield
{-# INLINE yield #-}

-- -------
-- foldl'
-- -------
foldl' :: Monad m => forall a b . (b -> a -> b) -> b -> FreeT (Of a) m r -> m b
foldl' op b0 = (\(Folding phi) -> foldl_ op b0 phi)
              . foldFreeT
{-# INLINE foldl' #-}

-- -------
-- scanr
-- -------

scanr :: Monad m => (a -> b -> b) -> b -> FreeT (Of a) m r -> FreeT (Of b) m r
scanr op b = buildFreeT 
           . (\(Folding phi) -> Folding (jscanr op b phi)) 
           . foldFreeT 
{-# INLINE scanr #-}


scanr2 :: Monad m => (a -> b -> b) -> b -> FreeT (Of a) m r -> FreeT (Of b) m r
scanr2 op b = buildFreeT 
              . (\(Folding phi) -> Folding (lscanr_ phi op b)) 
              . foldFreeT 
{-# INLINE scanr2 #-}

-- ---------------
-- sum 
-- ---------------

sum :: (Monad m, Num a) => FreeT (Of a) m () -> m a
sum  = lsum . foldFreeT 
{-# INLINE sum #-}

sum2 :: (Monad m, Num n) => FreeT (Of n) m r -> m n
sum2 = foldl' (+) 0 
{-# INLINE sum2 #-}

-- ---------------
-- replicate 
-- ---------------

replicate :: Monad m => Int -> a -> FreeT (Of a) m ()
replicate n = take n . repeat
{-# INLINE replicate #-}

replicateM :: Monad m => Int -> m a -> FreeT (Of a) m ()
replicateM n a = buildFreeT (lreplicateM n a)
{-# INLINE replicateM #-}

-- ---------------
-- iterate
-- ---------------

iterate :: Monad m => (a -> a) -> a -> FreeT (Of a) m r
iterate f  = buildFreeT . literate f 
{-# INLINE iterate #-}

iterateM :: Monad m => (a -> m a) -> m a -> FreeT (Of a) m r
iterateM = \f m -> buildFreeT (literateM f m)
{-# INLINE iterateM #-}

-- ---------------
-- repeat
-- ---------------

repeat :: Monad m => a -> FreeT (Of a) m r
repeat = buildFreeT . (\a -> Folding (repeat_ a))
{-# INLINE repeat #-}

repeatM :: Monad m => m a -> FreeT (Of a) m r
repeatM = buildFreeT . lrepeatM
{-# INLINE repeatM #-}

-- ---------------
-- filter 
-- ---------------
filter2 :: (Monad m) => (a -> Bool) -> FreeT (Of a) m r -> FreeT (Of a) m r 
filter2 pred = buildFreeT . (\(Folding phi) -> Folding (filter_ phi pred)) . foldFreeT

filter  :: (Monad m) => (a -> Bool) -> FreeT (Of a) m r -> FreeT (Of a) m r               
filter pred = buildFreeT . lfilter pred . foldFreeT
{-# INLINE filter #-}

filterM  :: (Monad m) => (a -> m Bool) -> FreeT (Of a) m r -> FreeT (Of a) m r
filterM pred = buildFreeT . lfilterM pred . foldFreeT
{-# INLINE filterM #-}

-- ---------------
-- drop
-- ---------------

drop :: (Monad m) => Int -> FreeT (Of a) m r -> FreeT (Of a) m r
drop n = buildFreeT . ldrop n . foldFreeT
{-# INLINE drop #-}


-- ---------------
-- take
-- ---------------


take2 :: (Monad m, Functor f) => Int -> FreeT f m r -> FreeT f m ()
take2 n = buildFreeT . (\(Folding phi)  -> Folding (jtake_ phi n)) . foldFreeT
{-# INLINE take2 #-}

take :: (Monad m, Functor f) => Int -> FreeT f m r -> FreeT f m ()
take n = buildFreeT . ltake n . foldFreeT
{-# INLINE take #-}

takeWhile :: Monad m => (a -> Bool) -> FreeT (Of a) m r -> FreeT (Of a) m ()
takeWhile pred = buildFreeT . ltakeWhile pred . foldFreeT 
{-# INLINE takeWhile #-}

takeWhile2 :: Monad m => (a -> Bool) -> FreeT (Of a) m r -> FreeT (Of a) m ()
takeWhile2 pred = buildFreeT 
               . (\(Folding fold) -> Folding (jtakeWhile_ fold pred)) 
               . foldFreeT
{-# INLINE takeWhile2#-}


-- ---------------
-- map
-- ---------------

map2 :: Monad m => (a -> b) -> FreeT (Of a) m r -> FreeT (Of b) m r
map2 f = buildFreeT . (\(Folding phi) -> Folding (map_ phi f)) . foldFreeT
{-# INLINE map2 #-}

map :: Monad m => (a -> b) -> FreeT (Of a) m r -> FreeT (Of b) m r
map f = buildFreeT . lmap f . foldFreeT
{-# INLINE map #-}

mapM :: Monad m => (a -> m b) -> FreeT (Of a) m r -> FreeT (Of b) m r
mapM f = buildFreeT . lmapM f . foldFreeT
{-# INLINE mapM #-}




enumFrom n = buildFreeT (Folding (lenumFrom n))
enumFromTo n m = buildFreeT (Folding (lenumFromTo n m))
enumFromToStep n m k = buildFreeT (Folding (lenumFromToStep n m k))

enumFromStepN :: (Monad m, Num a) => a -> a -> Int -> FreeT (Of a) m ()
enumFromStepN start step n = buildFreeT (Folding (lenumFromStepN start step n))
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
stdinLn :: MonadIO m => FreeT (Of String) m ()
stdinLn = fromHandle IO.stdin
{-# INLINABLE stdinLn #-}

-- | 'read' values from 'IO.stdin', ignoring failed parses
readLn :: (MonadIO m, Read a) => FreeT (Of a) m ()
readLn = map read stdinLn
{-# INLINABLE readLn #-}

{-| Read 'String's from a 'IO.Handle' using 'IO.hGetLine'

    Terminates on end of input
-}
fromHandle :: MonadIO m => IO.Handle -> FreeT (Of String) m ()
fromHandle h = go
  where
    go = do
        eof <- liftIO $ IO.hIsEOF h
        unless eof $ do
            str <- liftIO $ IO.hGetLine h
            yield str
            go
{-# INLINABLE fromHandle #-}     
