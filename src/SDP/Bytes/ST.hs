{-# LANGUAGE Safe, MagicHash #-}

{- |
    Module      :  SDP.Bytes.ST
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Bytes.ST" provides 'STBytes' - mutable strict unboxed array type.
-}
module SDP.Bytes.ST
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * STBytes and BytesST
  STBytes, BytesST
)
where

import SDP.Templates.AnyBorder
import SDP.Templates.AnyVar
import SDP.Prim.SBytes
import SDP.IndexedM
import SDP.SortM

import Control.Monad.ST

default ()

--------------------------------------------------------------------------------

-- | 'STBytes' is mutable version of 'SDP.Bytes.Bytes'.
type STBytes s = AnyBorder (STBytes# s)

-- | 'BytesST' is mutable version of 'SDP.Array.Array'.
type BytesST s i = AnyVar (ST s) (STBytes s i)

