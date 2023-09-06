{-# LANGUAGE Safe, MagicHash, MultiParamTypeClasses, FlexibleInstances #-}

{- |
    Module      :  SDP.ByteList.Ublist
    Copyright   :  (c) Andrey Mulik 2019-2023
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.ByteList.Ublist" provides 'Ublist' - strict boxed unrolled linked list.
-}
module SDP.ByteList.Ublist
(
  -- * Exports
  module SDP.Indexed,
  module SDP.Unboxed,
  module SDP.Sort,
  module SDP.Scan,
  module SDP.Set,
  
  -- * Ublist
  Ublist
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Indexed
import SDP.Unboxed
import SDP.Sort
import SDP.Scan
import SDP.Set

import SDP.Templates.AnyChunks
import SDP.Prim.SBytes
import SDP.SortM.Tim

default ()

--------------------------------------------------------------------------------

-- | 'Ublist' is unrolled linked list of unboxed values.
type Ublist = AnyChunks SBytes#

--------------------------------------------------------------------------------

instance Unboxed e => Sort (Ublist e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'
    
    sortedBy f = go . toChunks
      where
        go (x1 : x2 : xs) = sortedBy f x1 && last x1 `f` head x2 && go (x2 : xs)
        go      [x1]      = sortedBy f x1
        go       []       = True

--------------------------------------------------------------------------------

{-# INLINE done #-}
done :: Unboxed e => STBytes# s e -> ST s (Ublist e)
done =  unsafeFreeze




