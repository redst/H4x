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

import Data.ByteString (ByteString, packCStringLen, readFile)
import qualified Data.ByteString.Char8 as B
import Data.IORef
import Data.Foldable hiding (toList)
import Data.Serialize
import Data.Word

import Foreign
import Foreign.C

import qualified GHC.Exts as Exts
import GHC.IO

import Numeric

import Prelude hiding (readFile)

import System.Posix.Types

import Text.Parsec
import Text.ParserCombinators.Parsec
import Text.Printf

foreign import ccall safe "memread"
    readv :: CPid -> Word -> Ptr () -> Int -> IO CSize

foreign import ccall safe "memwrite"
    writev :: CPid -> Word -> Ptr () -> Int -> IO CSize

foreign import ccall safe "memread0"
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

data Region = Region
  { r_start :: Word
  , r_end   :: Word
  , r_name  :: ByteString
  , r_read  :: Bool
  , r_write :: Bool
  , r_shared:: Bool
  } deriving (Eq)

instance Show Region where
    show r = printf "%s %c%c: 0x%x-0x%x (0x%o)" 
                name rea wri start end (end - start) 
      where
        name = if B.null (r_name r) then "<anon>" else B.unpack (r_name r)
        start = r_start r
        end = r_end r
        rea = if r_read r then 'r' else '-'
        wri = if r_write r then 'w' else '-'

anon :: Region -> Bool
anon = B.null . r_name
-- instance Read Region where
--     readPrec = do
--         p <- readPrec
--         return (Region p 0 "")

parseRegion :: Parsec ByteString () Region
parseRegion = do
    st <- (fst . head . readHex) <$> manyTill hexDigit (char '-')
    end <- (fst . head . readHex) <$> manyTill hexDigit (char ' ')
    r <- option False (char 'r' >> return True)
    w <- option False (char 'w' >> return True)
    anyChar
    s <- option False (char 's' >> return True)
    count 4 $ manyTill anyChar (char ' ')
    name <- option "" $ do
        many (char ' ')
        manyTill anyChar (char '\n')
    return (Region st end (B.pack name) r w s)

getRegions :: CPid -> IO [Region]
getRegions pid = do
    cont <- readFile ("/proc/" ++ show pid ++ "/maps")
    return $ either (pure []) id $ parse (manyTill parseRegion eof) "getRegions" cont
        
-- utils 

sizeOfPtr :: Storable a => Ptr a -> Int
sizeOfPtr = sizeOf . (undefined :: Ptr a -> a)

test pid = do
    lins <- readFile ("/proc/" ++ show pid ++ "/maps")
    return $ parse (manyTill parseRegion eof) "<>" lins

