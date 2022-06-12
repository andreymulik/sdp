{-# LANGUAGE Safe #-}

{- |
    Module      :  SDP.ByteList.IO
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.ByteList.IO" provides 'IOByteList' - mutable strict unboxed bordered
    unrolled linked list type.
-}
module SDP.ByteList.IO
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.Unboxed,
  module SDP.SortM,
  
  -- * MonadIO and IO ByteLists
  -- ** With immutable bounds
  MIOByteList, IOByteList,
  
  -- ** With mutable bounds
  ByteListMIO, ByteListIO
)
where

import SDP.Templates.AnyBorder
import SDP.Templates.AnyVar

import SDP.ByteList.IOUblist
import SDP.IndexedM
import SDP.Unboxed
import SDP.SortM

default ()

--------------------------------------------------------------------------------

-- | 'MIOByteList' is mutable version of 'SDP.ByteList.ByteList'.
type MIOByteList io = AnyBorder (MIOUblist io)

-- | 'IOByteList' is mutable version of 'SDP.ByteList.ByteList'.
type IOByteList = AnyBorder IOUblist

-- | 'ByteListMIO' is mutable version of 'SDP.ByteList.ByteList'.
type ByteListMIO io i = AnyVar io (MIOByteList io i)

-- | 'ByteListIO' is mutable version of 'SDP.ByteList.ByteList'.
type ByteListIO i = AnyVar IO (IOByteList i)




