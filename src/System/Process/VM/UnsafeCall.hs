-----------------------------------------------------------------------------
-- |
-- Module: System.Process.VM.UnsafeCall
-- Maintainer: Alejandro Castelló <a.castello.pas@gmail.com>
-- Stability: experimental
-- Portability: not portable (linux-specific)
--
-- Implementation of [process_vm_readv and process_vm_writev]
-- (http://man7.org/linux/man-pages/man2/process_vm_readv.2.html)
-- using [unsafe foreign calls]
-- (https://wiki.haskell.org/Foreign_Function_Interface#Unsafe_calls) 
--
-----------------------------------------------------------------------------

#define MODULENAME System.Process.VM.UnsafeCall
#define SAFETY unsafe
#include "Undefined.hs"
