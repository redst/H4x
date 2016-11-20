module H4x where

import Control.Monad

import Data.Word

import Foreign
import Foreign.C

type LPTR = Word64
type SPTR = Word32
type PTR = LPTR
type Pid = Word32


memread :: (Integral ptr, Storable a) => Pid -> ptr -> IO a
memread pid addr = do
    retPtr <- malloc :: Storable a => IO (Ptr a)
    c_memread pid (fromIntegral addr) (castPtr retPtr) (sizeOfPtr retPtr)
    ret <- peek retPtr
    free retPtr
    return ret

foreign import ccall "memread"
    c_memread :: Pid -> PTR -> Ptr () -> Int -> IO ()

chainRead32 :: Storable a => Pid -> [SPTR] -> IO a
chainRead32 pid [lst] = memread pid (fromIntegral lst) 
chainRead32 pid (ptr:ptr':ps) = memread pid (fromIntegral ptr) >>=
    \ptr'' -> chainRead32 pid ((ptr''+ptr'):ps)

chainRead :: Storable a => Pid -> [LPTR] -> IO a
chainRead pid [lst] = memread pid (fromIntegral lst) 
chainRead pid (ptr:ptr':ps) = memread pid (fromIntegral ptr) >>=
    \ptr'' -> chainRead pid ((ptr''+ptr'):ps)

readoff :: Storable a => Pid -> PTR -> PTR -> IO a
readoff pid addr offset = memread pid (addr + offset)

readoff32 :: Storable a => Pid -> SPTR -> SPTR -> IO a
readoff32 pid addr offset = memread pid $ fromIntegral (addr + offset)

chainOffset :: (Storable ptr, Integral ptr) => Pid -> [ptr] -> IO PTR
chainOffset _ [] = return 0
-- chainOffset _ [addr] = return (fromIntegral addr)
chainOffset pid (add:adds) = fromIntegral <$> foldl step (return add) adds
    where
        step :: (Storable a, Integral a) => IO a -> a -> IO a
        step baseM offset = do
            base <- baseM
            target <- memread pid base
            return (target+offset)



memwrite :: (Integral ptr, Storable a) => Pid -> ptr -> a -> IO ()
memwrite pid addr elem = with elem $ \ptr -> do
    c_memwrite pid (fromIntegral addr) (castPtr ptr) (sizeOf elem)    

foreign import ccall "memwrite"
    c_memwrite :: Pid -> PTR -> Ptr () -> Int -> IO ()


writeOff :: Storable a => Pid -> PTR -> PTR -> IO a
writeOff pid addr offset = memread pid (addr + offset)

writeoff32 :: Storable a => Pid -> SPTR -> SPTR -> IO a
writeoff32 pid addr offset = memread pid $ fromIntegral (addr + offset)



sizeOfPtr :: Storable a => Ptr a -> Int
sizeOfPtr = sizeOf . (undefined :: Ptr a -> a)
