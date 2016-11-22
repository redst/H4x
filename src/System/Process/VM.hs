module System.Process.VM 
    -- reexports
    ( Word32, Word64, CPid
    -- read ops
    , readv, memread, chainRead, chainOffset
    -- write ops
    , writev, memwrite, chainWrite
    ) where

import Control.Monad

import Data.Foldable
import Data.Serialize
import Data.Word

import Foreign
import Foreign.C

import System.Posix.Types

type LPTR = Word64
type SPTR = Word32

data IOvec a = IOvec a
    deriving Show

foreign import ccall "memread"
    readv :: CPid -> Word -> Ptr () -> Int -> IO ()

foreign import ccall "memwrite"
    writev :: CPid -> Word -> Ptr () -> Int -> IO () 

memread :: (Integral ptr, Storable a) => CPid -> ptr -> IO a
memread pid addr = do
    retPtr <- malloc :: Storable a => IO (Ptr a)
    readv pid (fromIntegral addr) (castPtr retPtr) (sizeOfPtr retPtr)
    ret <- peek retPtr
    free retPtr
    return ret

chainRead :: (Integral ptr, Storable ptr, Storable a) => CPid -> [ptr] -> IO a
chainRead pid adds = chainOffset pid adds >>= memread pid

chainOffset :: (Storable ptr, Integral ptr) => CPid -> [ptr] -> IO ptr
chainOffset _ [] = return 0
chainOffset pid (add:adds) = fromIntegral <$> foldl step (return add) adds
    where
        step :: (Storable a, Integral a) => IO a -> a -> IO a
        step baseM offset = do
            base <- baseM
            target <- memread pid base
            return (target+offset)

memwrite :: (Integral ptr, Storable a) => CPid -> ptr -> a -> IO ()
memwrite pid addr elem = with elem $ \ptr -> do
    writev pid (fromIntegral addr) (castPtr ptr) (sizeOf elem)    

chainWrite :: (Storable ptr, Integral ptr, Storable a) 
    => CPid -> [ptr] -> a -> IO ()
chainWrite pid adds val = do
    add <- chainOffset pid adds
    when (add/=0) $ memwrite pid add val
        

sizeOfPtr :: Storable a => Ptr a -> Int
sizeOfPtr = sizeOf . (undefined :: Ptr a -> a)
