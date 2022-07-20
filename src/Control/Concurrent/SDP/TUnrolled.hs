{-# LANGUAGE Safe #-}

{- |
    Module      :  Control.Concurrent.SDP.TUnrolled
    Copyright   :  (c) Andrey Mulik 2020-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "Control.Concurrent.SDP.TUnrolled" provides lazy boxed unrolled linked list
    of @stm@ 'TVar's.
-}
module Control.Concurrent.SDP.TUnrolled
(
  -- * TUnrolled
  TUnrolled, STM, TVar
)
where

import Control.Concurrent.SDP.TUnlist
import SDP.Templates.AnyBorder

default ()

--------------------------------------------------------------------------------

-- | Lazy boxed unrolled libked list.
type TUnrolled = AnyBorder TUnlist


