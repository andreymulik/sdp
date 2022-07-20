{-# LANGUAGE Safe, MagicHash #-}

{- |
    Module      :  SDP.Bytes.IO
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Bytes.IO" provides mutable strict unboxed array types.
-}
module SDP.Bytes.IO
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.Unboxed,
  module SDP.SortM,
  
  -- * MonadIO and IO Bytes
  -- ** With immutable bounds
  MIOBytes, IOBytes,
  
  -- ** With mutable bounds
  BytesMIO, BytesIO
)
where

import SDP.Templates.AnyBorder
import SDP.Templates.AnyVar
import SDP.Prim.SBytes
import SDP.IndexedM
import SDP.Unboxed
import SDP.SortM

default ()

--------------------------------------------------------------------------------

-- | 'MIOBytes' is mutable version of 'SDP.Bytes.Bytes'.
type MIOBytes io = AnyBorder (MIOBytes# io)

-- | 'IOBytes' is mutable version of 'SDP.Bytes.Bytes'.
type IOBytes = AnyBorder IOBytes#

-- | 'BytesMIO' is mutable version of 'SDP.Bytes.Bytes'.
type BytesMIO io i = AnyVar io (MIOBytes io i)

-- | 'BytesIO' is mutable version of 'SDP.Bytes.Bytes'.
type BytesIO i = AnyVar IO (IOBytes i)

