{-# LANGUAGE Safe, MagicHash #-}

{- |
    Module      :  SDP.Unboxed
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Unboxed" provide service class 'Unboxed', that needed for
    "SDP.Prim.SBytes"-based structures.
-}
module SDP.Unboxed
(
  -- * Unboxed
  Unboxed (..), cloneUnboxed#, cloneUnboxedM#, thawUnboxed#, freezeUnboxed#,
  bytewiseEqUnboxed#, radixSortUnboxed#, asProxy#, toProxy#, fromProxy#,
  sizeof, offsetof, chunkof,
  
  copyUnboxed#, copyUnboxedM#,
  
  -- ** Kind @(Type -> Type)@ proxies
  fromProxy, psizeof#, psizeof, pchunkof, pchunkof#, poffsetof#, poffsetof,
  pnewUnboxed, pcopyUnboxed, pcopyUnboxedM, pcloneUnboxed, pcloneUnboxedM,
  pthawUnboxed, pfreezeUnboxed, cloneUnboxed1#, peqUnboxed,
  
  -- ** Kind @(Type -> Type -> Type)@ proxies
  fromProxy1, pnewUnboxed1, pcloneUnboxed1, pcopyUnboxed1, pcopyUnboxedM1,
  pcloneUnboxedM1,
  
  -- * Wrap helpers
  Wrap (..), lzero#, single#, fromList#, fromFoldable#, fromListN#, calloc#,
  newLinear#, newLinearN#, fromFoldableM#, concat#, pconcat,
  
  -- * Byte order
  ByteOrder (..), targetByteOrder
)
where

import SDP.Unboxed.Class

default ()



