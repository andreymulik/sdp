{-# LANGUAGE Safe, MagicHash #-}

{- |
    Module      :  SDP.Array.IO
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Array.IO" provides 'IOArray' - mutable lazy boxed array type.
-}
module SDP.Array.IO
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * MonadIO and IO Array
  -- ** With immutable bounds
  MIOArray, IOArray,
  
  -- ** With mutable bounds
  ArrayMIO, ArrayIO
)
where

import SDP.Templates.AnyBorder
import SDP.Templates.AnyVar
import SDP.Prim.SArray
import SDP.IndexedM
import SDP.SortM

default ()

--------------------------------------------------------------------------------

-- | 'MIOArray' is mutable version of 'SDP.Array.Array'.
type MIOArray io = AnyBorder (MIOArray# io)

-- | 'IOArray' is mutable version of 'SDP.Array.Array'.
type IOArray = AnyBorder IOArray#

-- | 'ArrayMIO' is mutable version of 'SDP.Array.Array'.
type ArrayMIO io i = AnyVar io (MIOArray io i)

-- | 'ArrayIO' is mutable version of 'SDP.Array.Array'.
type ArrayIO i = AnyVar IO (IOArray i)



