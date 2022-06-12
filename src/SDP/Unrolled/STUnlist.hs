{-# LANGUAGE Safe, MagicHash #-}

{- |
    Module      :  SDP.Unrolled.STUnlist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Unrolled.STUnlist" provides 'STUnlist' - mutable boxed lazy bordered
    unrolled linked list.
-}
module SDP.Unrolled.STUnlist
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * STUnlist and UnlistST
  STUnlist, UnlistST
)
where

import SDP.Templates.AnyChunks
import SDP.Templates.AnyVar

import SDP.Prim.SArray
import SDP.IndexedM
import SDP.SortM

import Control.Monad.ST

default ()

--------------------------------------------------------------------------------

-- | 'STUnlist' is mutable version of 'SDP.Unrolled.Unlist.Unlist'.
type STUnlist s = AnyChunks (STArray# s)

-- | This 'UnlistST' is mutable version of 'SDP.Unrolled.Unlist.Unlist'.
type UnlistST s = AnyVar (ST s) (STUnlist s)




