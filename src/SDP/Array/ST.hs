{-# LANGUAGE Safe, MagicHash #-}

{- |
    Module      :  SDP.Array.ST
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Array.ST" provides 'STArray' - mutable lazy boxed array type.
-}
module SDP.Array.ST
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * STArray and ArrayST
  STArray, ArrayST
)
where

import SDP.Templates.AnyBorder
import SDP.Templates.AnyVar
import SDP.Prim.SArray
import SDP.IndexedM
import SDP.SortM

import Control.Monad.ST

default ()

--------------------------------------------------------------------------------

-- | 'STArray' is mutable version of 'SDP.Array.Array'.
type STArray s = AnyBorder (STArray# s)

-- | 'ArrayST' is mutable version of 'SDP.Array.Array'.
type ArrayST s i = AnyVar (ST s) (STArray s i)

