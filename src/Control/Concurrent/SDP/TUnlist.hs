{-# LANGUAGE Safe, MagicHash #-}

{- |
    Module      :  Control.Concurrent.SDP.TUnlist
    Copyright   :  (c) Andrey Mulik 2020-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "Control.Concurrent.SDP.TUnlist" provides lazy boxed unrolled linked list of
    @stm@ 'TVar's.
-}
module Control.Concurrent.SDP.TUnlist
(
  -- * TUnlist
  TUnlist, STM, TVar
)
where

import SDP.Templates.AnyChunks
import SDP.Prim.TArray

default ()

--------------------------------------------------------------------------------

-- | Lazy boxed unrolled linked list.
type TUnlist = AnyChunks TArray#


