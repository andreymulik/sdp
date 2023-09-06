{-# LANGUAGE Safe, MagicHash #-}

{- |
    Module      :  SDP.Array
    Copyright   :  (c) Andrey Mulik 2019-2021
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Array" provides 'Array' - immutable lazy boxed array type.
-}
module SDP.Array
(
  -- * Exports
  module SDP.Indexed,
  module SDP.Sort,
  module SDP.Scan,
  module SDP.Set,
  
  -- * Array
  Array
)
where

import SDP.Indexed
import SDP.Sort
import SDP.Scan
import SDP.Set

import SDP.Templates.AnyBorder
import SDP.Prim.SArray

default ()

--------------------------------------------------------------------------------

-- | 'Array' - lazy boxed array.
type Array = AnyBorder SArray#


