module H4x where

import Data.Word

import Foreign
import Foreign.C

type LPTR = Word64
type SPTR = Word32
type PTR = LPTR
type Pid = Word32


memread :: Storable a => Pid -> PTR -> IO a
memread pid addr = do
    retPtr <- malloc :: Storable a => IO (Ptr a)
    c_memread pid addr (castPtr retPtr) (sizeOfPtr retPtr)
    ret <- peek retPtr
    free retPtr
    return ret

foreign import ccall "memread"
    c_memread :: Pid -> PTR -> Ptr () -> Int -> IO ()

chainRead32 :: Storable a => Pid -> [SPTR] -> IO a
chainRead32 pid [lst] = memread pid (fromIntegral lst) 
chainRead32 pid (ptr:ptr':ps) = memread pid (fromIntegral ptr) >>=
    \ptr'' -> chainRead32 pid ((ptr''+ptr'):ps)


memwrite :: Storable a => Pid -> PTR -> a -> IO ()
memwrite pid addr elem = with elem $ \ptr -> do
    

foreign import ccall "memwrite"
    c_memwrite :: Pid -> PTR -> Ptr () -> Int -> IO ()

sizeOfPtr :: Storable a => Ptr a -> Int
sizeOfPtr = sizeOf . (undefined :: Ptr a -> a)
