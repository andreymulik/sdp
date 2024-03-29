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
  STUnrolled
)
where

import SDP.Templates.AnyBorder
import SDP.Unrolled.STUnlist
import SDP.IndexedM
import SDP.SortM

default ()

--------------------------------------------------------------------------------

-- | 'STUnrolled' is mutable version 'SDP.Unrolled.Unrolled'.
type STUnrolled s = AnyBorder (STUnlist s)

