{-# LANGUAGE Safe #-}

{- |
    Module      :  SDP.Unrolled.IO
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Unrolled.IO" provides mutable lazy boxed bordered unrolled
    linked lists.
-}
module SDP.Unrolled.IO
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * MonadIO and IO Unrolleds
  -- ** With immutable bounds
  MIOUnrolled, IOUnrolled,
  
  -- ** With mutable bounds
  UnrolledMIO, UnrolledIO
)
where

import SDP.Templates.AnyBorder
import SDP.Templates.AnyVar

import SDP.Unrolled.IOUnlist
import SDP.IndexedM
import SDP.SortM

default ()

--------------------------------------------------------------------------------

-- | 'MIOUnrolled' is mutable version of 'SDP.Unrolled.Unrolled'.
type MIOUnrolled io = AnyBorder (MIOUnlist io)

-- | 'IOUnrolled' is mutable version of 'SDP.Unrolled.Unrolled'.
type IOUnrolled = AnyBorder IOUnlist

-- | 'UnrolledMIO' is mutable version of 'SDP.Unrolled.Unrolled'.
type UnrolledMIO io i = AnyVar io (MIOUnrolled io i)

-- | 'UnrolledIO' is mutable version of 'SDP.Unrolled.Unrolled'.
type UnrolledIO i = AnyVar IO (IOUnrolled i)

