{-# LANGUAGE Safe, MultiParamTypeClasses, FlexibleInstances #-}

{- |
    Module      :  SDP.Scan
    Copyright   :  (c) Andrey Mulik 2019-2021
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (requires non-portable modules)
    
    "SDP.Scan" provides 'Scan' - class for overloaded scans. 'Scan' needed for
    generalization and not so useful is practice as other @sdp@ classes.
-}
module SDP.Scan ( Scan (..) ) where

import Prelude ()
import SDP.SafePrelude
import SDP.Linear

import qualified Data.List as L ( scanl, scanr, scanl', scanl1, scanr1 )

default ()

--------------------------------------------------------------------------------

-- | Scan is class of scans.
class Sequence s e => Scan s e
  where
    scanl,  scanl' :: (b -> e -> b) -> b -> s -> [b]
    scanr,  scanr' :: (e -> b -> b) -> b -> s -> [b]
    scanl1, scanr1 :: (e -> e -> e) -> s -> [e]
    
    scanl  f base = L.scanl  f base . listL
    scanr  f base = L.scanr  f base . listL
    scanl' f base = L.scanl' f base . listL
    scanr' f base = L.scanr  f base . listL
    
    scanl1 f = L.scanl1 f . listL
    scanr1 f = L.scanr1 f . listL

--------------------------------------------------------------------------------

instance Scan [a] a
  where
    scanl  = L.scanl
    scanr  = L.scanr
    scanl' = L.scanl'
    scanr' = L.scanr
    scanl1 = L.scanl1
    scanr1 = L.scanr1

