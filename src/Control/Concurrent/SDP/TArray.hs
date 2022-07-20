{-# LANGUAGE Safe, MagicHash #-}

{- |
    Module      :  Control.Concurrent.SDP.TArray
    Copyright   :  (c) Andrey Mulik 2020-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "Control.Concurrent.SDP.TArray" provides lazy boxed array of @stm@ 'TVar's.
-}
module Control.Concurrent.SDP.TArray
(
  -- * TArray
  TArray, STM, TVar
)
where

import SDP.Templates.AnyBorder
import SDP.Prim.TArray

default ()

--------------------------------------------------------------------------------

-- | Lazy boxed array.
type TArray = AnyBorder TArray#



