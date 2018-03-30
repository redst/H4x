-----------------------------------------------------------------------------
-- |
-- Module: System.Process.VM.SafeCall
-- Maintainer: Alejandro Castelló <a.castello.pas@gmail.com>
-- Stability: experimental
-- Portability: not portable (linux-specific)
--
-- Implementation of [process_vm_readv and process_vm_writev]
-- (http://man7.org/linux/man-pages/man2/process_vm_readv.2.html)
-- using [foreign calls]
-- (https://wiki.haskell.org/Foreign_Function_Interface)
--
-----------------------------------------------------------------------------

module System.Process.VM.SafeCall 
#define SAFETY safe
#include "Undefined.hs"
