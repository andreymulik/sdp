{-# LANGUAGE Safe, MagicHash #-}

{- |
    Module      :  Control.Concurrent.SDP.TArray
    Copyright   :  (c) Andrey Mulik 2020-2026
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "Control.Concurrent.SDP.TArray" provides lazy boxed array of @stm@ 'TVar's.
-}
module Control.Concurrent.SDP.TArray
(
  -- * Exports
  module SDP.Prim.TArray,
  module SDP.Var,
  
  -- * TArray
  TArray
)
where

import SDP.Templates.AnyBorder
import SDP.Prim.TArray
import SDP.Var

default ()

--------------------------------------------------------------------------------

-- | Lazy boxed array.
type TArray = AnyBorder TArray#



