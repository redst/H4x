-- module <modulename>
    ( 
    -- ** The shortcomings of 'Storable'
    
    -- $intro

    -- * The 'Remote' class
    Remote(..)
    -- ** Exceptions thrown by VM operations
    , VMException(..) 
    -- ** Peeping functions
    , peepBytes, peepStorable, chainOffset, chainPeep, peep0, peepMalloc0
    , peepMalloc0WithSize, peepByteString, tryPeepBytes, peepBytes_
    , tryPeepStorable, peepStorable_, tryChainOffset, chainOffset_
    , tryChainPeep, chainPeep_, tryPeep0, peep0_, tryPeepMalloc0, peepMalloc0_
    , tryPeepMalloc0WithSize, peepMalloc0WithSize_, tryPeepByteString
    , peepBytestring_
    -- ** Planting functions
    , plantBytes, plantStorable, plantIntegral_, tryWrite, chainWrite
    , tryChainWrite, chainWrite_
    -- ** Synonym functions for 'Remote' class methods
    , tryPeep, peep_, tryPlant, plant_
    -- *** Specific type peeping functions
    , peepFloat, peepDouble, peepInt, peepWord, peepWord8, peepWord16
    , peepWord32, peepWord64, peepInt8, peepInt16, peepInt32, peepInt64
    -- *** Specific type planting functions
    , plantFloat, plantDouble, plantInt, plantWord, plantWord8, plantWord16
    , plantWord32, plantWord64, plantInt8, plantInt16, plantInt32, plantInt64
    , maxIOV
    , module System.Process.VM.Common
    -- type reexports
    , CPid (..) 
    , Storable (..)
    , module Data.Int
    , module Data.Word
    ) where

import Control.Monad
import Control.Exception

import Data.ByteString (ByteString, packCStringLen)
import Data.Int
import Data.Foldable hiding (toList)
import Data.Word

import Foreign hiding (void)
import Foreign.C

import GHC.IO

import System.Posix.Types 
import System.Process.VM.Common

import Text.Printf

#ifndef SAFETY
#define SAFETY unsafe
#endif

#define FIMPORT foreign import ccall SAFETY

zeroesPtr :: Ptr a
zeroesPtr = unsafePerformIO (castPtr <$> (callocArray 1024 :: IO (Ptr Word8)))

-- we use the IN_GHCI define to overcome linking problems with CApi and ghci
-- *** max individual transfer size, as defined in [<sys/uio.h>](/usr/include/sys/uio.h)
#ifndef IN_GHCI
foreign import capi "sys/uio.h value UIO_MAXIOV" 
  maxIOV :: Int
#else
maxIOV :: Int
maxIOV = 1024
#endif

-- $intro
-- The C-side behaviour of @process_vm_readv@ and @process_vm_writev@
-- works on provided buffers and lengths, the most rudimentary aproaches to 
-- this are 'peepBytes' and 'peepStorable', the latter making use of "Foreign"
-- memory allocations. But on small, C-like types this entails a considerable
-- overhead due to Foreign allocations being foreign calls as well

-- | 'Remote' supersedes 'Storable' and allows for the definition 
-- of operations on non-continuous memory addresses, and fast reads on basic
-- types
class Remote a where
    -- | Read a value from the given address in the given pid's address space
    peep        :: CPid -> Word -> IO a
    -- | Same as peep, but returning a null value in case of error, as opposed
    -- to throwing a 'VMException'
    peepUnsafe  :: CPid -> Word -> IO a
    -- | Write a value to the given address at the given pid. Consider 'peep_'
    -- for a short synonym
    plant       :: CPid -> Word -> a -> IO ()
    -- | idem. Returning a null value in case of error. Consider 'plant_' for a
    -- synonym
    plantUnsafe :: CPid -> Word -> a -> IO ()

peep_ :: Remote a => CPid -> Word -> IO a
peep_ = peepUnsafe

plant_ :: Remote a => CPid -> Word -> a -> IO ()
plant_ = plantUnsafe

-- | Automatically derive storable integral-types, this instantiation is
-- [overlappable](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#overlapping-instances) 
instance {-# Overlappable #-} (Integral a, Storable a) => Remote a where
    peep = peepStorable
    peepUnsafe = peepIntegral_
    plant = plantStorable
    plantUnsafe = plantIntegral_

instance Remote Double where
    peep = peepStorable
    peepUnsafe = peepDouble
    plant = plantStorable
    plantUnsafe = plantDouble

instance Remote Float where
    peep = peepStorable
    peepUnsafe = peepFloat
    plant = plantStorable
    plantUnsafe = plantFloat

instance Remote Word where
    peep = peepStorable
    plant = plantStorable
    peepUnsafe = peepWord
    plantUnsafe = plantWord

instance Remote Word8 where
    peep = peepStorable
    plant = plantStorable
    peepUnsafe = peepWord8
    plantUnsafe = plantWord8

instance Remote Word16 where
    peep = peepStorable
    plant = plantStorable
    peepUnsafe = peepWord16
    plantUnsafe = plantWord16

instance Remote Word32 where
    peep = peepStorable
    plant = plantStorable
    peepUnsafe = peepWord32
    plantUnsafe = plantWord32

instance Remote Word64 where
    peep = peepStorable
    plant = plantStorable
    peepUnsafe = peepWord64
    plantUnsafe = plantWord64

instance Remote Int where
    peep = peepStorable
    plant = plantStorable
    peepUnsafe = peepInt
    plantUnsafe = plantInt

instance Remote Int8 where
    peep = peepStorable
    plant = plantStorable
    peepUnsafe = peepInt8
    plantUnsafe = plantInt8

instance Remote Int16 where
    peep = peepStorable
    plant = plantStorable
    peepUnsafe = peepInt16
    plantUnsafe = plantInt16

instance Remote Int32 where
    peep = peepStorable
    plant = plantStorable
    peepUnsafe = peepInt32
    plantUnsafe = plantInt32

instance Remote Int64 where
    peep = peepStorable
    plant = plantStorable
    peepUnsafe = peepInt64
    plantUnsafe = plantInt64

-- | Exceptions thrown by process_vm_readv and process_vm_writev
data VMException 
  = BadAddress String
  | InvalidSize String
  | NoMemory String
  | NotPermited String
  | NoProcess String
  | UnexpectedERRNO String
  deriving (Eq)

instance Show VMException where
    show e = case e of
      BadAddress str      -> "BadAddress " ++ str
      InvalidSize str     -> "InvalidSize " ++ str
      NoMemory str        -> "NoMemory " ++ str
      NotPermited str     -> "NotPermited " ++ str
      NoProcess str       -> "NoProcess " ++ str
      UnexpectedERRNO str -> "UnexpectedERRNO " ++ str
instance Exception VMException

errnoToException :: Errno 
                 -> CPid
                 -> String 
                 -> Word -> Word 
                 -> Int -> Int 
                 -> VMException
errnoToException (err @ (Errno errno)) 
                  pid
                  call
                  la ra
                  ll rl = case () of
  _| err == eFAULT -> BadAddress $ p"local=0x%x remote=0x%x" la ra 
   | err == eINVAL -> InvalidSize $ p"local=%u remote=%u" ll rl
   | err == eNOMEM -> NoMemory $ p""
   | err == ePERM  -> NotPermited $ p"pid=%u" (fromIntegral pid :: Int)
   | err == eSRCH  -> NoProcess $ p"pid=%u" (fromIntegral pid :: Int)
   | errno < 0 -> errnoToException (Errno (- errno)) pid call la ra ll rl
   | otherwise -> UnexpectedERRNO $ p"err=%d" (fromIntegral errno :: Int)
  where
  p :: PrintfType r => String -> r 
  p str = printf (str ++ " in function \"" ++ call ++ "\"")

throwerr :: Int -> CPid -> String -> Ptr a -> Word -> Int -> Int -> b
throwerr err pid call la ra ll rl = throw $
    errnoToException (Errno $ fromIntegral err) pid call 
                      (fromIntegral $ ptrToWordPtr la) ra 
                      ll rl

-- | Specific size peeping functions
FIMPORT "vm_read8"
    peepWord8  :: CPid -> Word -> IO Word8

FIMPORT "vm_read16"
    peepWord16 :: CPid -> Word -> IO Word16

FIMPORT "vm_read32"
    peepWord32 :: CPid -> Word -> IO Word32

FIMPORT "vm_read64"
    peepWord64 :: CPid -> Word -> IO Word64

FIMPORT "vm_read8"
    peepInt8  :: CPid -> Word -> IO Int8

FIMPORT "vm_read16"
    peepInt16 :: CPid -> Word -> IO Int16

FIMPORT "vm_read32"
    peepInt32 :: CPid -> Word -> IO Int32

FIMPORT "vm_read64"
    peepInt64 :: CPid -> Word -> IO Int64

-- | Simple Type peeping functions

FIMPORT
#if WORD_SIZE_IN_BITS == 64
    "vm_read64"
#else
    "vm_read32"
#endif
    peepWord :: CPid -> Word -> IO Word

FIMPORT
#if WORD_SIZE_IN_BITS == 64
    "vm_read64"
#else
    "vm_read32"
#endif
    peepInt :: CPid -> Word -> IO Int

FIMPORT "vm_readf"
    peepFloat  :: CPid -> Word -> IO Float

FIMPORT "vm_readd"
    peepDouble :: CPid -> Word -> IO Double

FIMPORT "vm_read"
    vm_read :: CPid -> Word -> Ptr () -> Int -> IO Int

FIMPORT "vm_read0"
    vm_read0 :: CPid -> Word -> Ptr () -> Int -> Ptr () -> Int -> Int -> IO Int

peepBytes :: CPid -> Word -> Ptr () -> Int -> IO Int
peepBytes pid addr ptr sz = do
    cons <- vm_read pid addr ptr sz
    when (cons < 0) $ do
        throwerr cons pid "peepBytes" ptr addr sz sz
    return cons

tryPeepBytes :: CPid -> Word -> Ptr () -> Int -> IO (Either VMException Int)
tryPeepBytes p a p' s = try (peepBytes p a p' s)

peepBytes_ :: CPid -> Word -> Ptr () -> Int -> IO ()
peepBytes_ p a p' s = void $ tryPeepBytes p a p' s

peepStorable :: Storable a => CPid -> Word -> IO a
peepStorable pid addr = do
#ifdef DEBUG
    printf "performing 'peepStorable' on 0x%x\n" addr
#endif
    retPtr <- malloc :: Storable a => IO (Ptr a)
    peep' <- vm_read pid addr (castPtr retPtr) (sizeOfPtr retPtr)
    if peep' < 0 then
        throwerr (- peep') pid "peepStorable"
                           retPtr addr 
                          (sizeOfPtr retPtr) (sizeOfPtr retPtr)

    else do
        ret <- peek retPtr
        free retPtr
        return ret

tryPeep :: Remote a => CPid -> Word -> IO (Either VMException a)
tryPeep pid addr = try (peep pid addr)

peepStorable_ :: Storable a => CPid -> Word -> IO a
{-# INLINE [1] peepStorable_ #-}
peepStorable_ pid addr = do
#ifdef DEBUG
    printf "generic 'peepStorable_' on 0x%x\n" addr
#endif
    eith <- try (peepStorable pid addr)
    either ((\_ -> peek zeroesPtr) :: Storable a => VMException -> IO a) 
           return eith 
{-# RULES 
"peepStorable_" peepStorable_ = peepWord   :: CPid -> Word -> IO Word
"peepStorable_" peepStorable_ = peepWord8  :: CPid -> Word -> IO Word8
"peepStorable_" peepStorable_ = peepWord16 :: CPid -> Word -> IO Word16
"peepStorable_" peepStorable_ = peepWord32 :: CPid -> Word -> IO Word32
"peepStorable_" peepStorable_ = peepWord64 :: CPid -> Word -> IO Word64
"peepStorable_" peepStorable_ = peepInt    :: CPid -> Word -> IO Int
"peepStorable_" peepStorable_ = peepInt8   :: CPid -> Word -> IO Int8
"peepStorable_" peepStorable_ = peepInt16  :: CPid -> Word -> IO Int16
"peepStorable_" peepStorable_ = peepInt32  :: CPid -> Word -> IO Int32
"peepStorable_" peepStorable_ = peepInt64  :: CPid -> Word -> IO Int64
"peepStorable_" peepStorable_ = peepFloat  :: CPid -> Word -> IO Float
"peepStorable_" peepStorable_ = peepDouble :: CPid -> Word -> IO Double
  #-}

peepIntegral_ :: (Integral a, Storable a) => CPid -> Word -> IO a
peepIntegral_ = peep' undefined where
    peep' :: (Integral a, Storable a) => a -> CPid -> Word -> IO a
    peep' x 
      | sizeOf x == 1 = chain peepWord8
      | sizeOf x == 2 = chain peepWord8
      | sizeOf x == 4 = chain peepWord8
      | sizeOf x == 8 = chain peepWord8
      | otherwise = peepStorable_
    chain f = \p a -> do
#ifdef DEBUG
        printf "performing 'peepIntegral_' on 0x%x\n" a :: IO ()
#endif
        fromIntegral <$> f p a

peep0 :: Storable a => CPid -> Word -> Ptr a -> Int -> a -> IO Int
peep0 pid addr arr n del = with del $ \del' -> 
    vm_read0 pid addr (castPtr arr) (n * sizeOf del) 
             (castPtr del') (sizeOf del) 1

peepMalloc0 :: (Integral a, Storable a) => CPid -> Word -> a -> IO (Ptr a, Int)
peepMalloc0 = peepMalloc0WithSize (sizeOf (0 :: Word))

peepMalloc0WithSize :: (Integral a, Storable a) => Int -> CPid -> Word -> 
                                                   a -> IO (Ptr a, Int)
peepMalloc0WithSize = peepMalloc0WithSizeInt

peepMalloc0WithSizeGeneric :: Storable a => Int -> CPid -> Word -> 
                                            a -> IO (Ptr a, Int)
peepMalloc0WithSizeGeneric isz pid addr del = with del $ 
    \delp -> peep0step pid addr nullPtr delp True startingn 0
    where startingn = (max delsz isz) `quot` delsz
          delsz = sizeOf del

peepMalloc0WithSizeInt :: (Storable a, Integral a) => Int -> CPid -> Word -> 
                                                      a -> IO (Ptr a, Int)
peepMalloc0WithSizeInt isz pid addr del
  | delsz <= sizeOf nullPtr = peep0step pid addr nullPtr 
                          (wordPtrToPtr $ fromIntegral del) False startingn 0
  | otherwise = peepMalloc0WithSizeGeneric isz pid addr del
  where startingn = (max delsz isz) `quot` delsz
        delsz = sizeOf del

peep0step :: Storable a => CPid -> Word -> Ptr a -> Ptr a -> 
                           Bool -> Int -> Int -> IO (Ptr a, Int)
peep0step pid addr ptr delp memcmp n peep' = do
    let sz = n * delsz
    ptr' <- reallocArray ptr n
    peep'' <- vm_read0 pid (addr + fromIntegral peep')
                           (castPtr ptr')
                           sz 
                           (castPtr delp) 
                           delsz
                           (if memcmp then 1 else 0)
    if peep'' >= sz then
        peep0step pid addr ptr' delp memcmp nextn (peep' - peep'' + sz)
    else if peep'' <= 0 then
        throwerr peep'' pid "peep0Malloc"
                 ptr addr sz sz
    else do
        ptr'' <- reallocArray ptr' (peep' + peep'')
        return (ptr'', peep' + peep'')
    where
        nextn = if n < 8 then 2 * n else 3 * n `quot` 2
        delsz = sizeOfPtr delp

peepByteString :: CPid -> Word -> IO ByteString
peepByteString pid addr = peepMalloc0WithSize 64 pid addr 0 >>= packCStringLen

tryPeepByteString :: CPid -> Word -> IO (Either VMException ByteString)
tryPeepByteString p a = try (peepByteString p a)

peepByteString_ :: CPid -> Word -> IO ByteString
peepByteString_ p a = either (pure "") id <$> tryPeepByteString p a

chainPeep :: (Integral addr, Remote addr, Remote a) 
        => CPid -> [addr] -> IO a
chainPeep pid adds = chainOffset pid adds >>= peep pid . fromIntegral

tryChainPeep :: (Integral addr, Remote addr, Remote a) => CPid -> [addr] -> 
                  IO (Either VMException a)
tryChainPeep pid addrs = fmap join $ tryChainOffset pid addrs 
                            >>= mapM (tryPeep pid . fromIntegral)

chainPeep_ :: (Integral addr, Remote addr, Remote a) 
        => CPid -> [addr] -> IO a
chainPeep_ pid addrs = chainOffset_ pid addrs >>= peep_ pid . fromIntegral

chainOffset :: (Remote addr, Integral addr) => CPid -> [addr] -> IO addr
chainOffset = chainOffsetWith peep

tryChainOffset :: (Remote addr, Integral addr) 
        => CPid -> [addr] -> IO (Either VMException addr)
tryChainOffset pid addrs = try (chainOffset pid addrs)

chainOffset_ :: (Remote addr, Integral addr) => CPid -> [addr] -> IO addr
chainOffset_ pid addrs = do
#ifdef DEBUG
    putStrLn "generic chainOffset_"
#endif
    chainOffsetWith peep_ pid addrs

-- | Simple Type writing functions
FIMPORT
    vm_write :: CPid -> Word -> Ptr () -> Int -> IO Int

chainOffsetWith :: Integral addr
        => (CPid -> Word -> IO addr) -> CPid -> [addr] -> IO addr
chainOffsetWith _ _ [] = return 0
chainOffsetWith peeper pid (add:adds) = do
    foldl step (return add) adds
    where step baseM offset = do
            base <- baseM
            liftM (+ offset) (peeper pid (fromIntegral base))

FIMPORT 
#if WORD_SIZE_IN_BITS == 64
  "vm_write64"
#else
  "vm_write32"
#endif
  plantWord :: CPid -> Word -> Word -> IO ()

FIMPORT 
#if WORD_SIZE_IN_BITS == 64
  "vm_write64"
#else
  "vm_write32"
#endif
  plantInt :: CPid -> Word -> Int -> IO ()

FIMPORT "vm_writef"
    plantFloat :: CPid -> Word -> Float -> IO ()

FIMPORT "vm_writed"
    plantDouble :: CPid -> Word -> Double -> IO ()

-- | Specific size writing functions
FIMPORT "vm_write8"
    plantWord8   :: CPid -> Word -> Word8 -> IO ()

FIMPORT "vm_write16"
    plantWord16   :: CPid -> Word -> Word16 -> IO ()

FIMPORT "vm_write32"
    plantWord32   :: CPid -> Word -> Word32 -> IO ()

FIMPORT "vm_write64"
    plantWord64   :: CPid -> Word -> Word64 -> IO ()

FIMPORT "vm_write8"
    plantInt8   :: CPid -> Word -> Int8 -> IO ()

FIMPORT "vm_write16"
    plantInt16   :: CPid -> Word -> Int16 -> IO ()

FIMPORT "vm_write32"
    plantInt32   :: CPid -> Word -> Int32 -> IO ()

FIMPORT "vm_write64"
    plantInt64   :: CPid -> Word -> Int64 -> IO ()

plantBytes :: CPid -> Word -> Ptr () -> Int -> IO Int
plantBytes pid addr ptr sz = do
    written <- vm_write pid addr ptr sz
    when (written < 0) $ do
        throwerr (-written) pid "plantBytes" ptr addr sz sz
    return written

plantStorable :: Storable a => CPid -> Word -> a -> IO ()
plantStorable pid addr e = with e $ \ptr -> do
    ret <- vm_write pid addr (castPtr ptr) (sizeOf e)    
    when (ret < 0) $ do
        throwerr ret pid "plant" ptr addr (sizeOf e) (sizeOf e) 

tryWrite :: Remote a => CPid -> Word -> a -> IO (Maybe VMException)
tryWrite pid addr e = either Just (pure Nothing) <$> try (plant pid addr e)

plantStorable_ :: Storable a => CPid -> Word -> a -> IO ()
{-# INLINE [1] plantStorable_ #-}
plantStorable_ pid addr e = do
#ifdef DEBUG
    printf "generic 'plant_' on 0x%x\n" addr
#endif
    void $ try' $ plantStorable pid addr e 
    where try' = try :: IO a -> IO (Either VMException a)
{-# RULES
"plantStorable_" plantStorable_ = plantWord
"plantStorable_" plantStorable_ = plantWord8
"plantStorable_" plantStorable_ = plantWord16
"plantStorable_" plantStorable_ = plantWord32
"plantStorable_" plantStorable_ = plantWord64
"plantStorable_" plantStorable_ = plantInt
"plantStorable_" plantStorable_ = plantInt8
"plantStorable_" plantStorable_ = plantInt16
"plantStorable_" plantStorable_ = plantInt32
"plantStorable_" plantStorable_ = plantInt64
"plantStorable_" plantStorable_ = plantFloat
"plantStorable_" plantStorable_ = plantDouble
 #-}

plantIntegral_ :: (Storable a, Integral a) => CPid -> Word -> a -> IO ()
plantIntegral_ = plant' 0 where
    plant' :: (Integral a, Storable a) => a -> CPid -> Word -> a -> IO ()
    plant' x
      | sizeOf x == 1 = chain plantWord8
      | sizeOf x == 2 = chain plantWord16
      | sizeOf x == 4 = chain plantWord32
      | sizeOf x == 8 = chain plantWord64
      | otherwise = plantStorable_
    chain f = \p a b -> do
#ifdef DEBUG
        printf "performing 'plantIntegral_' on 0x%x\n" a :: IO ()
#endif
        f p a (fromIntegral b)

chainWrite :: (Remote addr, Integral addr, Remote a) 
        => CPid -> [addr] -> a -> IO ()
chainWrite pid adds val = do
    add <- chainOffset pid adds
    when (add/=0) $ plant pid (fromIntegral add) val

tryChainWrite :: (Remote addr, Integral addr, Remote a) 
        => CPid -> [addr] -> a -> IO (Maybe VMException)
tryChainWrite pid adds val = try' $ do
    add <- chainOffset pid adds
    when (add/=0) $ plant pid (fromIntegral add) val
    where try' = fmap (either Just (pure Nothing)) . try

chainWrite_ :: (Remote addr, Integral addr, Remote a) 
        => CPid -> [addr] -> a -> IO ()
{-# INLINE [2] chainWrite_ #-}
chainWrite_ pid addrs e = chainOffset_ pid addrs 
                          >>= (\a -> plant_ pid a e) . fromIntegral
-- utils 

sizeOfPtr :: Storable a => Ptr a -> Int
sizeOfPtr = sizeOf . (undefined :: Ptr a -> a)

