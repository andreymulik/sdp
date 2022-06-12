{-# LANGUAGE Safe #-}

{- |
    Module      :  SDP.ByteList.ST
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.ByteList.ST" provides 'STByteList' - mutable unboxed strict bordered
    unrolled linked list.
-}
module SDP.ByteList.ST
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.Unboxed,
  module SDP.SortM,
  
  -- * STByteList and ByteListST
  STByteList, ByteListST
)
where

import SDP.Templates.AnyBorder
import SDP.Templates.AnyVar

import SDP.ByteList.STUblist
import SDP.IndexedM
import SDP.Unboxed
import SDP.SortM

import Control.Monad.ST

default ()

--------------------------------------------------------------------------------

-- | 'STByteList' is mutable version of 'SDP.ByteList.ByteList'.
type STByteList s = AnyBorder (STUblist s)

-- | 'ByteListST' is mutable version of 'SDP.ByteList.ByteList'.
type ByteListST s i = AnyVar (ST s) (STByteList s i)


