
module Wow where

import Control.Monad

import Data.Foldable
import Data.Serialize
import Data.Word

import Foreign
import Foreign.C

import System.Posix.Types

type LPTR = Word64
type SPTR = Word32
type PTR = LPTR

data Point = Point
    { p_x   :: Maybe Float
    , p_y   :: Maybe Float
    , p_z   :: Maybe Float
    , p_th  :: Maybe Float }

instance Show Point where
    show (Point x y z th) = 
        "(" ++ unmaybe x ++ ", " ++ unmaybe y ++ ", " ++ unmaybe z ++ ") " 
        ++ unmaybe th ++ " rad"
        where
            unmaybe = maybe "_" show 

instance Storable Point where
    sizeOf _ = 20
    alignment _ = 8
    peek ptr = do
        [x,y,z,r] <- fmap Just <$> traverse (peekByteOff ptr) [0,4,8,16]
        return $ Point x y z r
    poke ptr (Point x y z r) = do
        traverse_ (pokeByteOff ptr 0) x
        traverse_ (pokeByteOff ptr 4) y
        traverse_ (pokeByteOff ptr 8) z
        traverse_ (pokeByteOff ptr 16) r

instance Serialize Point where
    get = liftM4 Point get get get get
    put (Point x y z r) = put x >> put y >> put z >> put r


memread :: (Integral ptr, Storable a) => CPid -> ptr -> IO a
memread pid addr = do
    retPtr <- malloc :: Storable a => IO (Ptr a)
    c_memread pid (fromIntegral addr) (castPtr retPtr) (sizeOfPtr retPtr)
    ret <- peek retPtr
    free retPtr
    return ret

foreign import ccall "memread"
    c_memread :: CPid -> PTR -> Ptr () -> Int -> IO ()

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
    c_memwrite pid (fromIntegral addr) (castPtr ptr) (sizeOf elem)    

foreign import ccall "memwrite"
    c_memwrite :: CPid -> PTR -> Ptr () -> Int -> IO ()



sizeOfPtr :: Storable a => Ptr a -> Int
sizeOfPtr = sizeOf . (undefined :: Ptr a -> a)
