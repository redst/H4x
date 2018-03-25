{-# LANGUAGE GHCForeignImportPrim,
                 MagicHash,
                 UnboxedTuples,
                 UnliftedFFITypes #-}

module System.Process.VM.PrimCall where

import System.Posix.Types

import GHC.Base
import GHC.Int
import GHC.Word

foreign import prim "prim_vm_read32"
  vm_read32# :: Int# -> Word# -> State# RealWorld -> (# State# RealWorld, Word# #)

vmReadWord32 :: CPid -> Word -> IO Word32
vmReadWord32 (CPid (I32# pid)) (W# addr)= IO $ \s -> 
    case (vm_read32# pid addr s) of
        (# s', w #) -> (# s', W32# w #)

foreign import prim "prim_vm_read64"
  vm_read64# :: Int# -> Word# -> State# RealWorld -> (# State# RealWorld, Word# #)

vmReadWord64 :: CPid -> Word -> IO Word64
vmReadWord64 (CPid (I32# pid)) (W# addr)= IO $ \s -> 
    case (vm_read64# pid addr s) of
        (# s', w #) -> (# s', W64# w #)

