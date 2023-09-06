{-# LANGUAGE Safe, ViewPatterns, PatternSynonyms #-}

{- |
    Module      :  SDP.Ratio
    Copyright   :  (c) Andrey Mulik 2020-2021
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Ratio" provides @(':%')@ pattern for 'Ratio'.
-}
module SDP.Ratio
(
  -- * Export
  module Data.Ratio, pattern (:%)
)
where

import Data.Ratio

infixl 7 :%

default ()

--------------------------------------------------------------------------------

{-# COMPLETE (:%) #-}

-- | Crutch pattern: real (':%') is closed 'Ratio' constructor.
pattern (:%) :: (Integral a) => a -> a -> Ratio a
pattern n :% d <- ((\ r -> (numerator r, denominator r)) -> (n, d)) where (:%) = (%)




