{-# LANGUAGE Safe, MagicHash #-}

{- |
    Module      :  SDP.ByteList.IOUblist
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    "SDP.ByteList.IOUblist" provides mutable strict unboxed unrolled
    linked lists.
-}
module SDP.ByteList.IOUblist
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.Unboxed,
  module SDP.SortM,
  
  -- * MonadIO and IO Ublists
  -- ** With immutable bounds
  MIOUblist, IOUblist,
  
  -- ** With mutable bounds
  UblistMIO, UblistIO
)
where

import SDP.Templates.AnyChunks
import SDP.Templates.AnyVar

import SDP.Prim.SBytes
import SDP.IndexedM
import SDP.Unboxed
import SDP.SortM

default ()

--------------------------------------------------------------------------------

-- | 'MIOUblist' is mutable version of 'SDP.ByteList.Ublist.Ublist'.
type MIOUblist io = AnyChunks (MIOBytes# io)

-- | 'IOUblist' is mutable version of 'SDP.ByteList.Ublist.Ublist'.
type IOUblist = AnyChunks IOBytes#

-- | 'UblistMIO' is mutable version of 'SDP.ByteList.Ublist.Ublist'.
type UblistMIO io = AnyVar io (MIOUblist io)

-- | 'UblistIO' is mutable version of 'SDP.ByteList.Ublist.Ublist'.
type UblistIO = AnyVar IO IOUblist




