-- module <modulename>
    -- reading operations
    ( read, tryRead, read_, read0, readFloat, readDouble, readInt, readWord 
    , readByteString , chainRead, tryChainRead, chainRead_, chainOffset
    , tryChainOffset, chainOffset_, readMalloc0, readMalloc0WithSize, readBytes
    , readWord8, readWord16, readWord32 , readWord64
    , readInt8,  readInt16,  readInt32,   readInt64
    , vmRead, memread
    -- writing operations
    , write, chainWrite, writeFloat, writeDouble, writeInt, writeWord
    , writeWord8, writeWord16, writeWord32, writeWord64
    , writeInt8,  writeInt16,  writeInt32,   writeInt64
    , vmWrite, memwrite
    -- memory region operations
    , module System.Process.VM.Common
    -- max IOV size const
    , maxIOV
    -- type reexports
    , CPid (..) 
    , module Data.Int
    , module Data.Word
    ) where

import Control.Monad
import Control.Exception

import Data.ByteString (ByteString, packCStringLen)
import Data.Int
import Data.Foldable hiding (toList)
import Data.Word

import Foreign

import GHC.IO

import Prelude hiding (read, readFile)

import System.Posix.Types

import System.Process.VM.Common

#ifndef SAFETY
#define SAFETY unsafe
#endif

#define FIMPORT foreign import ccall SAFETY

zeroesPtr :: Ptr a
zeroesPtr = unsafePerformIO (castPtr <$> (callocArray 1024 :: IO (Ptr Word8)))

-- we use the IN_GHCI define to overcome linking problems with CApi and ghci
#ifndef IN_GHCI
foreign import capi "sys/uio.h value UIO_MAXIOV" 
  maxIOV :: Int
#else
maxIOV :: Int
maxIOV = 1024
#endif

#ifndef IN_GHCI
foreign import capi "errno.h value EFAULT"
  eFault :: Int
#else
eFault :: Int
eFault = 14
#endif

#ifndef IN_GHCI
foreing import capi "errno.h value EINVAL"
  eInval :: Int
#else
eInval :: Int
eInval = 22
#endif

#ifndef IN_GHCI
foreign import capi "errno.h value ESRCH"
  eSrch :: Int
#else
eSrch :: Int
eSrch = 3
#endif

-- | Specific size reading functions
FIMPORT "vm_read8"
    readWord8  :: CPid -> Word -> IO Word8

FIMPORT "vm_read16"
    readWord16 :: CPid -> Word -> IO Word16

FIMPORT "vm_read32"
    readWord32 :: CPid -> Word -> IO Word32

FIMPORT "vm_read64"
    readWord64 :: CPid -> Word -> IO Word64

FIMPORT "vm_read8"
    readInt8  :: CPid -> Word -> IO Int8

FIMPORT "vm_read16"
    readInt16 :: CPid -> Word -> IO Int16

FIMPORT "vm_read32"
    readInt32 :: CPid -> Word -> IO Int32

FIMPORT "vm_read64"
    readInt64 :: CPid -> Word -> IO Int64

-- | Simple Type reading functions

FIMPORT
#if WORD_SIZE_IN_BITS == 64
    "vm_read64"
#else
    "vm_read32"
#endif
    readWord :: CPid -> Word -> IO Word

FIMPORT
#if WORD_SIZE_IN_BITS == 64
    "vm_read64"
#else
    "vm_read32"
#endif
    readInt :: CPid -> Word -> IO Int

FIMPORT "vm_readf"
    readFloat  :: CPid -> Word -> IO Float

FIMPORT "vm_readd"
    readDouble :: CPid -> Word -> IO Double

FIMPORT "vm_read"
    vm_read :: CPid -> Word -> Ptr () -> Int -> IO Int

FIMPORT "vm_read0"
    vm_read0 :: CPid -> Word -> Ptr () -> Int -> Ptr () -> Int -> Int -> IO Int

readBytes :: CPid -> Word -> Ptr () -> Int -> IO Int
readBytes = vm_read

read :: Storable a => CPid -> Word -> IO a
{-# INLINE [1] read #-}
read pid addr = do
    retPtr <- malloc :: Storable a => IO (Ptr a)
    read' <- vm_read pid addr (castPtr retPtr) (sizeOfPtr retPtr)
    if read' < 0 then
        fail $ "readv failed: ERRNO=" ++ show read'
    else do
        ret <- peek retPtr
        free retPtr
        return ret
{-# RULES 
"read/CPid -> Word -> IO Word"   read = readWord   :: CPid -> Word -> IO Word
"read/CPid -> Word -> IO Word8"  read = readWord8  :: CPid -> Word -> IO Word8
"read/CPid -> Word -> IO Word16" read = readWord16 :: CPid -> Word -> IO Word16
"read/CPid -> Word -> IO Word32" read = readWord32 :: CPid -> Word -> IO Word32
"read/CPid -> Word -> IO Word64" read = readWord64 :: CPid -> Word -> IO Word64
"read/CPid -> Word -> IO Int"    read = readInt    :: CPid -> Word -> IO Int
"read/CPid -> Word -> IO Int8"   read = readInt8   :: CPid -> Word -> IO Int8
"read/CPid -> Word -> IO Int16"  read = readInt16  :: CPid -> Word -> IO Int16
"read/CPid -> Word -> IO Int32"  read = readInt32  :: CPid -> Word -> IO Int32
"read/CPid -> Word -> IO Int64"  read = readInt64  :: CPid -> Word -> IO Int64
"read/CPid -> Word -> IO Float"  read = readFloat  :: CPid -> Word -> IO Float
"read/CPid -> Word -> IO Double" read = readDouble :: CPid -> Word -> IO Double
  #-}

vmRead :: Storable a => CPid -> Word -> IO a
vmRead = read

memread :: Storable a => CPid -> Word -> IO a
memread = read

tryRead :: Storable a => CPid -> Word -> IO (Either IOException a)
tryRead pid addr = try (read pid addr)

read_ :: Storable a => CPid -> Word -> IO a
read_ pid addr = either (\_ -> peek zeroesPtr) return =<< tryRead pid addr

read0 :: Storable a => CPid -> Word -> Ptr a -> Int -> a -> IO Int
read0 pid addr arr n del = with del $ \del' -> 
    vm_read0 pid addr (castPtr arr) (n * sizeOf del) 
             (castPtr del') (sizeOf del) 1

readMalloc0 :: (Integral a, Storable a) => CPid -> Word -> a -> IO (Ptr a, Int)
readMalloc0 = readMalloc0WithSize (sizeOf (0 :: Word))

readMalloc0WithSize :: (Integral a, Storable a) => Int -> CPid -> Word -> 
                                                   a -> IO (Ptr a, Int)
readMalloc0WithSize = readMalloc0WithSizeInt

readMalloc0WithSizeGeneric :: Storable a => Int -> CPid -> Word -> 
                                            a -> IO (Ptr a, Int)
readMalloc0WithSizeGeneric isz pid addr del = with del $ 
    \delp -> read0step pid addr nullPtr delp True startingn 0
    where startingn = (max delsz isz) `quot` delsz
          delsz = sizeOf del

readMalloc0WithSizeInt :: (Storable a, Integral a) => Int -> CPid -> Word -> 
                                                      a -> IO (Ptr a, Int)
readMalloc0WithSizeInt isz pid addr del
  | delsz <= sizeOf nullPtr = read0step pid addr nullPtr 
                          (wordPtrToPtr $ fromIntegral del) False startingn 0
  | otherwise = readMalloc0WithSizeGeneric isz pid addr del
  where startingn = (max delsz isz) `quot` delsz
        delsz = sizeOf del

read0step :: Storable a => CPid -> Word -> Ptr a -> Ptr a -> 
                           Bool -> Int -> Int -> IO (Ptr a, Int)
read0step pid addr ptr delp memcmp n read' = do
    let sz = n * delsz
    ptr' <- reallocArray ptr n
    read'' <- vm_read0 pid (addr + fromIntegral read')
                           (castPtr ptr')
                           sz 
                           (castPtr delp) 
                           delsz
                           (if memcmp then 1 else 0)
    if read'' >= sz then
        read0step pid addr ptr' delp memcmp nextn (read' - read'' + sz)
    else if read'' <= 0 then
        fail $ "readv0 failed: " ++ show read''
    else do
        ptr'' <- reallocArray ptr' (read' + read'')
        return (ptr'', read' + read'')
    where
        nextn = if n < 8 then 2 * n else 3 * n `quot` 2
        delsz = sizeOfPtr delp

readByteString :: CPid -> Word -> IO ByteString
readByteString pid addr = 
    either (pure "" :: IOException -> ByteString) 
           id <$> try (readMalloc0WithSize 64 pid addr 0 >>= packCStringLen)

chainRead :: (Integral addr, Storable addr, Storable a) => CPid -> [addr] -> IO a
chainRead pid adds = chainOffset pid adds >>= memread pid . fromIntegral

tryChainRead :: (Integral addr, Storable addr, Storable a) => CPid -> [addr] -> 
                  IO (Either IOException a)
tryChainRead pid addrs = fmap join $ tryChainOffset pid addrs 
                            >>= mapM (tryRead pid . fromIntegral)

chainRead_ :: (Integral addr, Storable addr, Storable a) => CPid -> [addr] -> IO a
chainRead_ pid addrs = do
    eith <- tryChainRead pid addrs
    either (\_ -> peek zeroesPtr) return eith where

chainOffset :: (Storable addr, Integral addr) => CPid -> [addr] -> IO addr
chainOffset _ [] = return 0
chainOffset pid (add:adds) = fromIntegral <$> foldl step (return add) adds
    where
        step :: (Storable a, Integral a) => IO a -> a -> IO a
        step baseM offset = do
            base <- baseM
            target <- memread pid (fromIntegral base)
            return (target+offset)

tryChainOffset :: (Storable addr, Integral addr) => CPid -> [addr] -> 
                                                    IO (Either IOException addr)
tryChainOffset pid addrs = try (chainOffset pid addrs)

chainOffset_ :: (Storable addr, Integral addr) => CPid -> [addr] -> IO addr
chainOffset_ pid addrs = either (pure 0) id <$> tryChainOffset pid addrs

-- | Simple Type writing functions
FIMPORT
    vm_write :: CPid -> Word -> Ptr () -> Int -> IO Int

FIMPORT 
#if WORD_SIZE_IN_BITS == 64
  "vm_write64"
#else
  "vm_write32"
#endif
  writeWord :: CPid -> Word -> Word -> IO ()

FIMPORT 
#if WORD_SIZE_IN_BITS == 64
  "vm_write64"
#else
  "vm_write32"
#endif
  writeInt :: CPid -> Word -> Int -> IO ()

FIMPORT "vm_writef"
    writeFloat :: CPid -> Word -> Float -> IO ()

FIMPORT "vm_writed"
    writeDouble :: CPid -> Word -> Double -> IO ()

-- | Specific size writing functions
FIMPORT "vm_write8"
    writeWord8   :: CPid -> Word -> Word8 -> IO ()

FIMPORT "vm_write16"
    writeWord16   :: CPid -> Word -> Word16 -> IO ()

FIMPORT "vm_write32"
    writeWord32   :: CPid -> Word -> Word32 -> IO ()

FIMPORT "vm_write64"
    writeWord64   :: CPid -> Word -> Word64 -> IO ()

FIMPORT "vm_write8"
    writeInt8   :: CPid -> Word -> Int8 -> IO ()

FIMPORT "vm_write16"
    writeInt16   :: CPid -> Word -> Int16 -> IO ()

FIMPORT "vm_write32"
    writeInt32   :: CPid -> Word -> Int32 -> IO ()

FIMPORT "vm_write64"
    writeInt64   :: CPid -> Word -> Int64 -> IO ()

write :: Storable a => CPid -> Word -> a -> IO ()
{-# INLINE [1] write #-}
write pid addr e = with e $ \ptr -> do
    ret <- vm_write pid addr (castPtr ptr) (sizeOf e)    
    when (ret < 0) $ do
        fail ("writev failed: ERRNO=" ++ (show ret))

{-# RULES
"write/CPid -> Word -> Word -> IO ()" write = writeWord
"write/CPid -> Word -> Int  -> IO ()" write = writeInt
 #-}

vmWrite :: Storable a => CPid -> Word -> a -> IO ()
vmWrite = write
memwrite :: Storable a => CPid -> Word -> a -> IO ()
memwrite = write

chainWrite :: (Storable addr, Integral addr, Storable a) 
    => CPid -> [addr] -> a -> IO ()
chainWrite pid adds val = do
    add <- chainOffset pid adds
    when (add/=0) $ memwrite pid (fromIntegral add) val

-- utils 

sizeOfPtr :: Storable a => Ptr a -> Int
sizeOfPtr = sizeOf . (undefined :: Ptr a -> a)

