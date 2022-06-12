{-# LANGUAGE Safe, MagicHash #-}

{- |
    Module      :  SDP.ByteList.STUblist
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.ByteList.STUblist" provides 'STUblist' - mutable unboxed strict
    unrolled linked list.
-}
module SDP.ByteList.STUblist
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.Unboxed,
  module SDP.SortM,
  
  -- * STUblist and UblistST
  STUblist, UblistST
)
where

import SDP.Templates.AnyChunks
import SDP.Templates.AnyVar

import SDP.Prim.SBytes
import SDP.IndexedM
import SDP.Unboxed
import SDP.SortM

import Control.Monad.ST

default ()

--------------------------------------------------------------------------------

-- | This 'STUblist' is mutable version of 'SDP.ByteList.Ublist.Ublist'.
type STUblist s = AnyChunks (STBytes# s)

-- | This 'UblistST' is mutable version of 'SDP.ByteList.Ublist.Ublist'.
type UblistST s = AnyVar (ST s) (STUblist s)


