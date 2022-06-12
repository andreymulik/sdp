{-# LANGUAGE Safe #-}

{- |
    Module      :  SDP.Unrolled.ST
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Unrolled.ST" provides 'STUnrolled' - mutable boxed lazy bordered
    unrolled linked list.
-}
module SDP.Unrolled.ST
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * STUnrolled
  STUnrolled, UnrolledST
)
where

import SDP.Templates.AnyBorder
import SDP.Templates.AnyVar

import SDP.Unrolled.STUnlist
import SDP.IndexedM
import SDP.SortM

import Control.Monad.ST

default ()

--------------------------------------------------------------------------------

-- | 'STUnrolled' is mutable version 'SDP.Unrolled.Unrolled'.
type STUnrolled s = AnyBorder (STUnlist s)

-- | 'UnrolledST' is mutable version of 'SDP.Unrolled.Unrolled'.
type UnrolledST s i = AnyVar (ST s) (STUnrolled s i)




