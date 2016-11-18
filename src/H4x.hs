module H4x where

import Data.Word

import Foreign

type HANDLE = Word64

type PROCESS = Word32

foreign import ccall "OpenProcess" 
    openProcess :: Word32 -> Bool -> Word32 -> IO HANDLE

foreign import ccall "CloseHandle"
    closeHandle :: HANDLE -> IO Bool

foreign import ccall "K32EnumProcesses"
    c_EnumProcesses :: Ptr PROCESS -> Word32 -> Ptr Word32 -> IO Bool
enumProcesses :: IO [PROCESS]
enumProcesses = alloca $ \c -> allocaArray 1024 $ \arr -> do   
    ok <- c_EnumProcesses arr 1024 c
    if ok then
        peekArray 1024 arr
    else
        return []
