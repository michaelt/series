{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables #-}
module Series.Prelude 
  (concats, 
   cons, 
   drop, 
   filter,
   filterM,
   foldl',
   yield,
   iterate,
   iterateM,
   map,
   mapM,
   repeat,
   repeatM,
   replicate,
   scanr,
   span, 
   splitAt, 
   sum,
   take,
   takeWhile,
   enumFromStepN
   ) where
import qualified Series.Folding.Prelude as F
import Series.Types
import Control.Monad hiding (filterM, mapM)
import Data.Functor.Identity
import Control.Monad.Trans
import qualified System.IO as IO
import Prelude hiding (map, filter, drop, take, sum
                      , iterate, repeat, replicate, splitAt
                      , takeWhile, enumFrom, enumFromTo
                      , mapM, scanr, span, break)

-- ---------------
-- Prelude
-- ---------------
-- ---------------

-- ------
-- concats
-- ------

concats_ :: Monad m =>  Series (Folding (Of a) m) m r -> Series (Of a) m r
concats_  = buildSeries 
                . F.concats 
                . foldSeries
{-# INLINE concats_ #-}

concats :: Monad m =>  Series (Series (Of a) m) m r -> Series (Of a) m r
concats  = buildSeries 
                . F.concats 
                . (\(Folding phi) -> 
                       Folding (\c w d -> phi (c . foldSeries) w d))
                . foldSeries
{-# INLINE concats #-}


-- ------
-- cons
-- ------
cons :: Monad m => a -> Series (Of a) m r -> Series (Of a) m r 
cons a = buildSeries . F.cons a . foldSeries
{-# INLINE cons #-}


-- ------
-- yield
-- ------
yield :: Monad m => a -> Series (Of a) m ()
yield = buildSeries . F.yield
{-# INLINE yield #-}

-- -------
-- foldl'
-- -------
foldl' :: Monad m => forall a b . (b -> a -> b) -> b -> Series (Of a) m r -> m b
foldl' op b0 = F.foldl op b0 . foldSeries 
{-# INLINE foldl' #-}

-- -------
-- scanr
-- -------

scanr :: Monad m => (a -> b -> b) -> b -> Series (Of a) m r -> Series (Of b) m r
scanr op b = buildSeries 
           . F.scanr op b
           . foldSeries 
{-# INLINE scanr #-}


-- ---------------
-- sum 
-- ---------------

sum :: (Monad m, Num a) => Series (Of a) m () -> m a
sum  = F.sum . foldSeries 
{-# INLINE sum #-}


-- ---------------
-- replicate 
-- ---------------

replicate :: Monad m => Int -> a -> Series (Of a) m ()
replicate n = take n . repeat
{-# INLINE replicate #-}

replicateM :: Monad m => Int -> m a -> Series (Of a) m ()
replicateM n a = buildSeries (F.replicateM n a)
{-# INLINE replicateM #-}

-- ---------------
-- iterate
-- ---------------

iterate :: (a -> a) -> a -> Series (Of a) m r
iterate f  = buildSeries . F.iterate f 
{-# INLINE iterate #-}

iterateM :: Monad m => (a -> m a) -> m a -> Series (Of a) m r
iterateM = \f m -> buildSeries (F.iterateM f m)
{-# INLINE iterateM #-}

-- ---------------
-- repeat
-- ---------------

repeat :: a -> Series (Of a) m r
repeat = buildSeries . F.repeat 
{-# INLINE repeat #-}

repeatM :: Monad m => m a -> Series (Of a) m r
repeatM = buildSeries . F.repeatM
{-# INLINE repeatM #-}

-- ---------------
-- filter 
-- ---------------

filter  :: (Monad m) => (a -> Bool) -> Series (Of a) m r -> Series (Of a) m r               
filter pred = buildSeries . F.filter pred . foldSeries
{-# INLINE filter #-}

filterM  :: (Monad m) => (a -> m Bool) -> Series (Of a) m r -> Series (Of a) m r
filterM pred = buildSeries . F.filterM pred . foldSeries
{-# INLINE filterM #-}

-- ---------------
-- drop
-- ---------------

drop :: (Monad m) => Int -> Series (Of a) m r -> Series (Of a) m r
drop n = buildSeries . F.drop n . foldSeries
{-# INLINE drop #-}


-- ---------------
-- take
-- ---------------


take :: (Monad m, Functor f) => Int -> Series f m r -> Series f m ()
take n = buildSeries . F.take n . foldSeries
{-# INLINE take #-}

takeWhile :: Monad m => (a -> Bool) -> Series (Of a) m r -> Series (Of a) m ()
takeWhile pred = buildSeries . F.takeWhile pred . foldSeries 
{-# INLINE takeWhile #-}

-- ---------------
-- map
-- ---------------


map :: Monad m => (a -> b) -> Series (Of a) m r -> Series (Of b) m r
map f = buildSeries . F.map f . foldSeries
{-# INLINE map #-}

mapM :: Monad m => (a -> m b) -> Series (Of a) m r -> Series (Of b) m r
mapM f = buildSeries . F.mapM f . foldSeries
{-# INLINE mapM #-}

span :: Monad m => (a -> Bool) -> Series (Of a) m r 
      -> Series (Of a) m (Series (Of a) m r)
span pred = 
  buildSeries 
  . fmap buildSeries
  . F.span pred
  . foldSeries
{-# INLINE span #-}


break :: Monad m => (a -> Bool) -> Series (Of a) m r 
      -> Series (Of a) m (Series (Of a) m r)
break pred = 
  buildSeries 
  . fmap buildSeries
  . F.span (not . pred)
  . foldSeries
{-# INLINE break #-}
--
splitAt :: (Monad m, Functor f) 
         => Int 
         -> Series f m r 
         -> Series f m (Series f m r)
splitAt n = 
   buildSeries 
   . fmap buildSeries
   . F.splitAt n
   . foldSeries 
{-# INLINE splitAt #-}
--
splitAt_ :: (Monad m) 
         => Int 
         -> Series (Of a) m r 
         -> Series (Of a) m (Series (Of a) m r)
splitAt_ n = 
   buildSeries 
   . fmap buildSeries
   . F.splitAt_ n
   . foldSeries 
{-# INLINE splitAt_ #-}

enumFrom n = buildSeries (Folding (F.lenumFrom n))
enumFromTo n m = buildSeries (Folding (F.lenumFromTo n m))
enumFromToStep n m k = buildSeries (Folding (F.lenumFromToStep n m k))

enumFromStepN :: (Monad m, Num a) => a -> a -> Int -> Series (Of a) m ()
enumFromStepN start step n = buildSeries (Folding (F.lenumFromStepN start step n))
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


