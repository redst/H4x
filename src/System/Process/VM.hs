{-# LANGUAGE DoAndIfThenElse, OverloadedLists, TypeFamilies #-}

module System.Process.VM 
    -- reexports
    ( Word32, Word64, CPid,
    -- read ops
    readv, readv0, memread, chainRead, chainOffset, readmalloc0,
    readByteString,
    -- write ops
    writev, memwrite, chainWrite,
    -- VM reading monad
    VM(..), newVM, newSizedVM, freeVM, reloadVM, peekVM, readVM
    ) where

import Control.Monad

import Data.ByteString (ByteString, packCStringLen)
import Data.IORef
import Data.Foldable hiding (toList)
import Data.Serialize
import Data.Word

import Foreign
import Foreign.C

import qualified GHC.Exts as Exts
import GHC.IO

import Numeric

import System.Posix.Types

foreign import ccall "memread"
    readv :: CPid -> Word -> Ptr () -> Int -> IO ()

foreign import ccall "memwrite"
    writev :: CPid -> Word -> Ptr () -> Int -> IO () 

foreign import ccall "memread0"
    c_readv0 :: CPid -> Word -> Ptr () -> Int -> Ptr () -> Int -> IO Int

memread :: (Integral ptr, Storable a) => CPid -> ptr -> IO a
memread pid addr = do
    retPtr <- malloc :: Storable a => IO (Ptr a)
    readv pid (fromIntegral addr) (castPtr retPtr) (sizeOfPtr retPtr)
    ret <- peek retPtr
    free retPtr
    return ret

readv0 :: (Integral ptr, Storable a) => CPid -> ptr -> Ptr a -> Int -> a -> IO Int
readv0 pid addr arr n del = with del $ \del' -> 
    c_readv0 pid (fromIntegral addr) (castPtr arr) (n*(sizeOf del)) (castPtr del') (sizeOf del)

readmalloc0 :: (Integral ptr, Storable a) => CPid -> ptr -> a -> IO (Ptr a, Int)
readmalloc0 pid addr del = with del $ 
    \del' -> (f' nullPtr del' (1024 `quot` (sizeOf del)) 0)
    where
        szo = sizeOf del
        f' :: (Integral ptr, Storable a) => (Ptr a) -> (Ptr a) -> Int -> Int -> IO (Ptr a, Int)
        f' buff del n read = do
            let sz = n*szo
            buff' <- reallocArray buff n
            read' <- c_readv0 pid ((fromIntegral addr)+(fromIntegral read))
                (castPtr buff') sz (castPtr del) szo
            if read' < 0 then
                f' buff' del (n + n `quot` 2) (read-read')
            else do
                buff'' <- reallocArray buff' (read+read')
                return (buff', read+read')

readInto :: (Integral ptr, Storable a) => CPid -> ptr -> ((Ptr a, Int) -> IO b) -> a -> IO b
readInto pid addr f del = do
    (p,sz) <- readmalloc0 pid addr del 
    ret <- f (p,sz)
    free p
    return ret

readByteString :: (Integral ptr) => CPid -> ptr -> IO ByteString
readByteString pid addr = readInto pid addr packCStringLen 0

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

data VM = VM 
    { vm_pid    :: CPid
    , vm_size   :: Int 
    , vm_addr   :: (IORef Word) 
    , vm_mem    :: (Ptr ()) 
    }   

instance Show VM where
    show (VM pid size _ _) = (show pid) ++ "[" ++ (show size) ++ "]"

newVM :: CPid -> IO VM
newVM pid = newSizedVM pid 1024

newSizedVM :: CPid -> Int -> IO VM
newSizedVM pid size = do
    addr <- newIORef 0
    ptr <- mallocBytes size
    return $ VM pid size addr ptr
    
freeVM :: VM -> IO ()
freeVM (VM _ _ _ ptr) = free ptr
    
positionVM :: VM -> Int -> Word -> IO ()
positionVM (VM pid vsz idir ptr) sz addr = do
    dir <- readIORef idir
    if addr < dir || addr+(fromIntegral sz) > dir+(fromIntegral vsz) then do
        readv pid addr ptr vsz
        writeIORef idir addr
    else
        return ()

reloadVM :: VM -> IO ()
reloadVM (VM pid vsz idir ptr) = do
    dir <- readIORef idir
    readv pid dir ptr vsz

peekVM :: Storable a => VM -> Word -> IO a
peekVM = readVM' undefined where
    readVM' :: (Storable a) => a -> VM -> Word -> IO a
    readVM' a vm@(VM _ _ idir ptr) addr = do
        positionVM vm (sizeOf a) addr
        dir <- readIORef idir
        peekByteOff ptr $ fromIntegral (addr-dir)

readVM :: VM -> Word -> Int -> IO (Ptr ())
readVM vm@(VM _ _ idir ptr) addr sz = do
    positionVM vm sz addr
    dir <- readIORef idir
    return (ptr `plusPtr` (fromIntegral $ addr-dir))

        
data Vec a = VEC (ForeignPtr Word8) Int

instance Show (Vec a) where
    show (VEC ptr sz) = ""

instance (Storable a) => Exts.IsList (Vec a) where
    type Item (Vec a) = a
    toList = toList
    fromList = fromList
    fromListN = fromListN

fromList :: Storable a => [a] -> Vec a
fromList xs = fromListN (length xs) xs

fromListN :: Storable a => Int -> [a] -> Vec a
fromListN n xs = unsafePerformIO $ do
    let sz = n*(sizeOf $ head xs)
    ptr <- mallocForeignPtrBytes sz
    return $ VEC ptr n

toList :: Storable a => Vec a -> [a]
toList (VEC ptr sz) = unsafePerformIO $ withForeignPtr 
    (castForeignPtr ptr) $ peekArray sz
    
castVec :: Vec a -> Vec b
castVec (VEC ptr sz) = VEC ptr sz


memcmp :: (Integral a) => Ptr Word8 -> Ptr Word8 -> a -> IO Bool
memcmp pa pb sz = (==0) <$> c_memcmp pa pb (fromIntegral sz)

foreign import ccall unsafe "string.h memcmp"
    c_memcmp :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt

-- utils 

sizeOfPtr :: Storable a => Ptr a -> Int
sizeOfPtr = sizeOf . (undefined :: Ptr a -> a)


