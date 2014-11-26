{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables #-}
module Series.ByteString where
import           Series.Types
import           Series.Folding.Prelude hiding (fromHandle)
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

fromLazy bs = buildSeries (Folding (\construct wrap done -> 
  foldrChunks (kurry construct) (done ()) bs))

stdinLn ::  Series (Of ByteString) IO ()
stdinLn = fromHandleLn IO.stdin
{-# INLINABLE stdinLn #-}

fromHandleLn ::  IO.Handle -> Series (Of ByteString) IO ()
fromHandleLn h = buildSeries $ Folding $ \construct wrap done -> 
  wrap $ let go = do eof <- IO.hIsEOF h
                     if eof then return (done ()) 
                            else do bs <- B.hGetLine h 
                                    return (construct (bs :> wrap go))
         in go
{-# INLINABLE fromHandleLn #-}


stdin :: Series (Of ByteString) IO ()
stdin = fromHandle IO.stdin

fromHandle :: IO.Handle -> Series (Of ByteString) IO ()
fromHandle = hGetSome defaultChunkSize
{-# INLINABLE fromHandle #-}

hGetSome :: Int -> IO.Handle -> Series (Of ByteString) IO ()
hGetSome size h = buildSeries $ Folding $ \construct wrap done -> 
  let go = do bs <- B.hGetSome h size
              if B.null bs then return (done ())
                           else liftM (construct . (bs :>)) go
  in wrap go
{-# INLINABLE hGetSome #-}


hGet :: Int -> IO.Handle -> Series (Of ByteString) IO ()
hGet size h = buildSeries $ Folding $ \construct wrap done -> 
  let go = do bs <- B.hGet h size
              if B.null bs then return (done ())
                           else liftM (construct . (bs :>)) go
  in wrap go
{-# INLINABLE hGet #-}

