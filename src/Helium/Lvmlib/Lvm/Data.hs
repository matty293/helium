--------------------------------------------------------------------------------
-- Copyright 2001-2012, Daan Leijen, Bastiaan Heeren, Jurriaan Hage. This file 
-- is distributed under the terms of the BSD3 License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
--------------------------------------------------------------------------------
--  $Id: Data.hs 291 2012-11-08 11:27:33Z heere112 $

module Helium.Lvmlib.Lvm.Data
   ( module Helium.Lvmlib.Lvm.Core.Module, LvmModule, LvmDecl
     -- constants
   , recHeader,recFooter           
   ) where

import Helium.Lvmlib.Lvm.Core.Module
import Helium.Lvmlib.Lvm.Instr.Data   ( Instr )

{--------------------------------------------------------------
  An LVM module
---------------------------------------------------------------}
type LvmModule  = Module [Instr]
type LvmDecl    = Decl [Instr]

{---------------------------------------------------------------
  Constants
---------------------------------------------------------------}
recHeader,recFooter :: Int
recHeader     = 0x1F4C564D
recFooter     = 0x1E4C564D
