{-# LANGUAGE Safe, MagicHash #-}

{- |
    Module      :  SDP.Array.ST
    Copyright   :  (c) Andrey Mulik 2019-2025
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Array.ST" provides mutable lazy boxed array types.
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
import SDP.Prim.SArray.ST
import SDP.IndexedM
import SDP.SortM

default ()

--------------------------------------------------------------------------------

-- | 'STArray' is mutable version of 'SDP.Array.Array'.
type STArray s = AnyBorder (STArray# s)

-- | 'ArrayST' is mutable version of 'SDP.Array.Array'.
type ArrayST s i = AnyVar (ST s) (STArray s i)



