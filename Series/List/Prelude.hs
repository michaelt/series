
{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables #-}
module Series.List.Prelude where
import Series.Types
import Series.Folding.Prelude
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

yield :: a -> [a]
yield = buildList . lyield
{-# INLINE yield #-}

-- -------
-- foldl'
-- -------
foldl' :: forall a b . (b -> a -> b) -> b -> [a] -> b
foldl' op b0 = runIdentity
              . (\(Folding phi) -> foldl_ op b0 phi)
              . foldList
{-# INLINE foldl' #-}

-- -------
-- scanr
-- -------

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr op b = buildList 
           . (\(Folding phi) -> Folding (jscanr op b phi)) 
           . foldList 
{-# INLINE scanr #-}


scanr2 :: (a -> b -> b) -> b -> [a] -> [b]
scanr2 op b = buildList 
              . (\(Folding phi) -> Folding (lscanr_ phi op b)) 
              . foldList 
{-# INLINE scanr2 #-}

-- ---------------
-- sum 
-- ---------------

sum :: (Num a) => [a] -> a
sum  = runIdentity . lsum . foldList 
{-# INLINE sum #-}

sum2 :: (Num n) => [n] -> n
sum2 = foldl' (+) 0 
{-# INLINE sum2 #-}

-- ---------------
-- replicate 
-- ---------------

replicate ::  Int -> a -> [a]
replicate n = take n . repeat
{-# INLINE replicate #-}

-- replicateM :: Int -> m a -> [a]
-- replicateM n a = buildList (lreplicateM n a)
-- {-# INLINE replicateM #-}

-- ---------------
-- iterate
-- ---------------

iterate ::  (a -> a) -> a -> [a]
iterate f  = buildList . literate f 
{-# INLINE iterate #-}

-- iterateM :: Monad m => (a -> m a) -> m a -> [a]
-- iterateM = \f m -> buildList (literateM f m)
-- {-# INLINE iterateM #-}

-- ---------------
-- repeat
-- ---------------

repeat :: a -> [a]
repeat = buildList . (\a -> Folding (repeat_ a))
{-# INLINE repeat #-}

-- repeatM :: Monad m => m a -> [a]
-- repeatM = buildList . lrepeatM
-- {-# INLINE repeatM #-}

-- ---------------
-- filter 
-- ---------------
filter2 :: (a -> Bool) -> [a] -> [a] 
filter2 pred = buildList . (\(Folding phi) -> Folding (filter_ phi pred)) . foldList

filter  ::  (a -> Bool) -> [a] -> [a]               
filter pred = buildList . lfilter pred . foldList
{-# INLINE filter #-}
-- 
-- filterM  :: (Monad m) => (a -> m Bool) -> [a] -> [a]
-- filterM pred = buildList . lfilterM pred . foldList
-- {-# INLINE filterM #-}

-- ---------------
-- drop
-- ---------------

drop :: Int -> [a] -> [a]
drop n = buildList . ldrop n . foldList
{-# INLINE drop #-}


-- ---------------
-- take
-- ---------------


take2 ::  Int -> [a] -> [a]
take2 n = buildList . (\(Folding phi)  -> Folding (jtake_ phi n)) . foldList
{-# INLINE take2 #-}

take :: Int -> [a] -> [a]
take n = buildList . ltake n . foldList
{-# INLINE take #-}

takeWhile ::   (a -> Bool) -> [a] -> [a]
takeWhile pred = buildList . ltakeWhile pred . foldList 
{-# INLINE takeWhile #-}

takeWhile2 ::  (a -> Bool) -> [a] -> [a]
takeWhile2 pred = buildList 
               . (\(Folding fold) -> Folding (jtakeWhile_ fold pred)) 
               . foldList
{-# INLINE takeWhile2#-}

-- ---------------
-- map
-- ---------------

map2 ::  (a -> b) -> [a] -> [b]
map2 f = buildList . (\(Folding phi) -> Folding (map_ phi f)) . foldList
{-# INLINE map2 #-}

map ::  (a -> b) -> [a] -> [b]
map f = buildList . lmap f . foldList
{-# INLINE map #-}

-- mapM :: Monad m => (a -> m b) -> [a] -> [b]
-- mapM f = buildList . lmapM f . foldList
-- {-# INLINE mapM #-}




enumFrom n = buildList (Folding (lenumFrom n))
enumFromTo n m = buildList (Folding (lenumFromTo n m))
enumFromToStep n m k = buildList (Folding (lenumFromToStep n m k))

enumFromStepN :: (Num a) => a -> a -> Int -> [a]
enumFromStepN start step n = buildList (Folding (lenumFromStepN start step n))
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
-- stdinLn :: MonadIO m => Producer String m ()
-- stdinLn = fromHandle IO.stdin
-- {-# INLINABLE stdinLn #-}
-- 
-- -- | 'read' values from 'IO.stdin', ignoring failed parses
-- readLn :: (MonadIO m, Read a) => [a]
-- readLn = map read stdinLn
-- {-# INLINABLE readLn #-}
-- 
-- {-| Read 'String's from a 'IO.Handle' using 'IO.hGetLine'
-- 
--     Terminates on end of input
-- -}
-- fromHandle :: MonadIO m => IO.Handle -> Producer String m ()
-- fromHandle h = go
--   where
--     go = do
--         eof <- liftIO $ IO.hIsEOF h
--         unless eof $ do
--             str <- liftIO $ IO.hGetLine h
--             yield str
--             go
-- {-# INLINABLE fromHandle #-}     
-- 
