{-# LANGUAGE DoAndIfThenElse, OverloadedStrings, CPP #-}

module System.Process.VM.UnsafeCall 
    -- reading operations
    ( read, read0, readFloat, readDouble, readInt, readWord , readByteString
    , chainRead, chainOffset, readMalloc0, readMalloc0WithSize
    , readWord8, readWord16, readWord32 , readWord64
    , readInt8,  readInt16,  readInt32,   readInt64
    , vmRead, memread
    -- writing operations
    , write, chainWrite, writeFloat, writeDouble, writeInt, writeWord
    , writeWord8, writeWord16, writeWord32 , writeWord64
    , writeInt8,  writeInt16,  writeInt32,   writeInt64
    , vmWrite, memwrite
    -- memory region operations
    , module System.Process.VM.Common
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
import Foreign.C

import GHC.IO

import Prelude hiding (read, readFile)

import System.Posix.Types

import System.Process.VM.Common

#define SAFETY unsafe

#define FIMPORT foreign import ccall SAFETY

default (Word)

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
    vm_read :: CPid -> Word -> Ptr () -> Int -> IO CSize

FIMPORT "vm_read0"
    vm_read0 :: CPid -> Word -> Ptr () -> Int -> Ptr () -> Int -> IO Int

-- | Simple Type Writing Functions

FIMPORT
    vm_write :: CPid -> Word -> Ptr () -> Int -> IO Int

read :: Storable a => CPid -> Word -> IO a
{-# INLINE [1] read #-}
read pid addr = do
    retPtr <- malloc :: Storable a => IO (Ptr a)
    read' <- vm_read pid addr (castPtr retPtr) (sizeOfPtr retPtr)
    if read'<0 then
        fail $ "readv failed: ERRNO="++(show read')
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

read0 :: Storable a => CPid -> Word -> Ptr a -> Int -> a -> IO Int
read0 pid addr arr n del = with del $ \del' -> 
    vm_read0 pid addr (castPtr arr) (n*(sizeOf del)) (castPtr del') (sizeOf del)

readMalloc0 :: Storable a => CPid -> Word -> a -> IO (Ptr a, Int)
readMalloc0 = readMalloc0WithSize 64

readMalloc0WithSize :: Storable a => Int -> CPid -> Word -> a -> IO (Ptr a, Int)
readMalloc0WithSize isz pid addr del = with del $ 
    \del' -> (step nullPtr del' (startingsz `quot` (sizeOf del)) 0)
    where
        startingsz = max delsz isz
        delsz = sizeOf del
        step ptr del' n read' = do
            let sz = n * delsz
            ptr' <- reallocArray ptr n
            read'' <- vm_read0 pid (addr + (fromIntegral read'))
                                   (castPtr ptr')
                                   sz 
                                   (castPtr del') 
                                   delsz
            if read'' >= sz then
                step ptr' del' (n + n `quot` 2) (read' - read'' + sz)
            else if read'' <= 0 then
                fail "readv0 failed"
            else do
                ptr'' <- reallocArray ptr' (read' + read'')
                return (ptr'', read' + read'')

readInto :: Storable a => CPid -> Word -> ((Ptr a, Int) -> IO b) -> a -> IO b
readInto pid addr f del = do
    (p,sz) <- readMalloc0 pid addr del 
    ret <- f (p,sz)
    free p
    return ret

readByteString :: CPid -> Word -> IO ByteString
readByteString pid addr = 
    either (pure "" :: IOException -> ByteString) 
           id <$> try (readInto pid addr packCStringLen 0)

chainRead :: (Integral addr, Storable addr, Storable a) => CPid -> [addr] -> IO a
chainRead pid adds = chainOffset pid adds >>= memread pid . fromIntegral

chainOffset :: (Storable addr, Integral addr) => CPid -> [addr] -> IO addr
chainOffset _ [] = return 0
chainOffset pid (add:adds) = fromIntegral <$> foldl step (return add) adds
    where
        step :: (Storable a, Integral a) => IO a -> a -> IO a
        step baseM offset = do
            base <- baseM
            target <- memread pid (fromIntegral base)
            return (target+offset)

-- | Simple Type writing functions
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

