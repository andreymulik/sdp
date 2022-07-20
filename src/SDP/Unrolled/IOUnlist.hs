{-# LANGUAGE Safe, MagicHash #-}

{- |
    Module      :  SDP.Unrolled.IOUnlist
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Unrolled.IOUnlist" provides mutable lazy boxed unrolled
    linked lists.
-}
module SDP.Unrolled.IOUnlist
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * MonadIO and IO Unlists
  -- ** With immutable bounds
  MIOUnlist, IOUnlist,
  
  -- ** With mutable bounds
  UnlistMIO, UnlistIO
)
where

import SDP.Templates.AnyChunks
import SDP.Templates.AnyVar

import SDP.Prim.SArray
import SDP.IndexedM
import SDP.SortM

default ()

--------------------------------------------------------------------------------

-- | 'MIOUnlist' is mutable version of 'SDP.Unrolled.Unlist.Unlist'.
type MIOUnlist io = AnyChunks (MIOArray# io)

-- | 'IOUnlist' is mutable version of 'SDP.Unrolled.Unlist.Unlist'.
type IOUnlist = AnyChunks IOArray#

-- | 'UnlistMIO' is mutable version of 'SDP.Unrolled.Unlist.Unlist'.
type UnlistMIO io = AnyVar io (MIOUnlist io)

-- | 'UnlistIO' is mutable version of 'SDP.Unrolled.Unlist.Unlist'.
type UnlistIO = AnyVar IO IOUnlist

