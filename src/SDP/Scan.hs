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
class (Linear s a) => Scan s a
  where
    scanl,  scanl' :: (b -> a -> b) -> b -> s -> [b]
    scanr,  scanr' :: (a -> b -> b) -> b -> s -> [b]
    scanl1, scanr1 :: (a -> a -> a) -> s -> [a]
    
    scanl  f base = scanl  f base . listL
    scanr  f base = scanr  f base . listL
    scanl' f base = scanl' f base . listL
    scanr' f base = scanr' f base . listL
    
    scanl1 f = scanl1 f . listL
    scanr1 f = scanr1 f . listL

--------------------------------------------------------------------------------

instance Scan [a] a
  where
    scanl  = L.scanl
    scanr  = L.scanr
    scanl' = L.scanl'
    scanr' = L.scanr
    scanl1 = L.scanl1
    scanr1 = L.scanr1

