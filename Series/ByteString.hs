{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables #-}
module Series.ByteString where
import           Series.Types
import qualified Series.Folding.Prelude as FP
import qualified Series.Folding.ByteString as FB
import           Control.Monad hiding (filterM, mapM)
import           Data.Functor.Identity
import           Control.Monad.Trans
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Internal (foldrChunks, defaultChunkSize)
import           Data.ByteString (ByteString) 
import qualified System.IO as IO
import           Prelude hiding (map, filter, drop, take, sum
                        , iterate, repeat, replicate, splitAt
                        , takeWhile, enumFrom, enumFromTo)

fromLazy  = buildSeries . FB.fromLazy

stdinLn ::  Series (Of ByteString) IO ()
stdinLn = fromHandleLn IO.stdin
{-# INLINE stdinLn #-}

fromHandleLn ::  IO.Handle -> Series (Of ByteString) IO ()
fromHandleLn = buildSeries . FB.fromHandleLn
{-# INLINABLE fromHandleLn #-}

stdin :: Series (Of ByteString) IO ()
stdin = fromHandle IO.stdin
{-# INLINE stdin #-}

fromHandle :: IO.Handle -> Series (Of ByteString) IO ()
fromHandle = hGetSome defaultChunkSize
{-# INLINABLE fromHandle #-}

hGetSome :: Int -> IO.Handle -> Series (Of ByteString) IO ()
hGetSome size = buildSeries . FB.hGetSome size
{-# INLINABLE hGetSome #-}

hGet :: Int -> IO.Handle -> Series (Of ByteString) IO ()
hGet size = buildSeries . FB.hGet size
{-# INLINABLE hGet #-}

