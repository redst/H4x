{-# LANGUAGE DoAndIfThenElse, OverloadedStrings, TypeFamilies #-}

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
import Control.Monad.Except

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
    readv :: CPid -> Word -> Ptr () -> Int -> IO CSize

foreign import ccall "memwrite"
    writev :: CPid -> Word -> Ptr () -> Int -> IO CSize

foreign import ccall "memread0"
    c_readv0 :: CPid -> Word -> Ptr () -> Int -> Ptr () -> Int -> IO Int

memread :: (Integral addr, Storable a) => CPid -> addr -> IO a
memread pid addr = do
    retPtr <- malloc :: Storable a => IO (Ptr a)
    read' <- readv pid (fromIntegral addr) (castPtr retPtr) (sizeOfPtr retPtr)
    if read'<0 then
        fail $ "readv failed: ERRNO="++(show read')
    else do
        ret <- peek retPtr
        free retPtr
        return ret

readv0 :: (Integral addr, Storable a) => CPid -> addr -> Ptr a -> Int -> a -> IO Int
readv0 pid addr arr n del = with del $ \del' -> 
    c_readv0 pid (fromIntegral addr) (castPtr arr) (n*(sizeOf del)) (castPtr del') (sizeOf del)

readmalloc0 :: (Integral addr, Storable a) => CPid -> addr -> a -> IO (Ptr a, Int)
readmalloc0 pid addr del = with del $ 
    \del' -> (f' nullPtr del' (1024 `quot` (sizeOf del)) 0)
    where
        szo = sizeOf del
        f' :: (Integral addr, Storable a) => (Ptr a) -> (Ptr a) -> Int -> Int -> IO (Ptr a, Int)
        f' buff del n read = do
            let sz = n*szo
            buff' <- reallocArray buff n
            read' <- c_readv0 pid ((fromIntegral addr)+(fromIntegral read))
                (castPtr buff') sz (castPtr del) szo
            if read' > sz then
                f' buff' del (n + n `quot` 2) (read-read'+sz)
            else if read' == 0 then
                fail "readv0 failed"
            else do
                buff'' <- reallocArray buff' (read+read')
                return (buff', read+read')

readInto :: (Integral addr, Storable a) => CPid -> addr -> ((Ptr a, Int) -> IO b) -> a -> IO b
readInto pid addr f del = do
    (p,sz) <- readmalloc0 pid addr del 
    ret <- f (p,sz)
    free p
    return ret

readByteString :: (Integral addr) => CPid -> addr -> IO ByteString
readByteString pid addr = 
    catchError 
        (readInto pid addr packCStringLen 0)
        (\_ -> return "")

chainRead :: (Integral addr, Storable addr, Storable a) => CPid -> [addr] -> IO a
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

memwrite :: (Integral addr, Storable a) => CPid -> addr -> a -> IO ()
memwrite pid addr elem = with elem $ \ptr -> do
    ret <- writev pid (fromIntegral addr) (castPtr ptr) (sizeOf elem)    
    if ret<0 then
        fail ("writev failed: ERRNO=" ++ (show ret))
    else
        return ()

chainWrite :: (Storable addr, Integral addr, Storable a) 
    => CPid -> [addr] -> a -> IO ()
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
    ret <- readv pid dir ptr vsz
    if ret<0 then
        fail $ "readv failed: ERRNO="++(show ret)
    else return ()

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

        
-- utils 

sizeOfPtr :: Storable a => Ptr a -> Int
sizeOfPtr = sizeOf . (undefined :: Ptr a -> a)


