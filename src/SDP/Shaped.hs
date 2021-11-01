{-# LANGUAGE Safe, MultiParamTypeClasses, TypeOperators #-}

{- |
    Module      :  SDP.Shaped
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Shaped" provides advanced operations on 'SDP.Indexed.Indexed'
    structures, generalized by 'Index' type.
-}
module SDP.Shaped
(
  -- * Shaped
  Shaped (..)
)
where

import SDP.Linear

default ()

--------------------------------------------------------------------------------

{- |
  Service class for structures with arbitrary bounds. Allows you to extract
  subsequences corresponding to index subranges.
-}
class Shaped s e
  where
    {-# MINIMAL reshape, (!!), slices, unslice #-}
    
    -- | 'rebound' with 'defaultBounds'.
    defaultRebound :: (Index i, Index j, Bordered2 s i e) => s i e -> s j e
    defaultRebound es = es `reshape` defaultBounds (sizeOf es)
    
    -- | Set new bounds, may shrink.
    reshape :: (Index i, Index j) => s i e -> (j, j) -> s j e
    
    -- | @es !! ij@ returns subshape @ij@ of @es@.
    (!!) :: (SubIndex i j) => s i e -> i :|: j -> s j e
    
    -- | Returns list of @es@ subshapes.
    slices :: (SubIndex i j) => s i e -> [s j e]
    
    -- | Unslice subshapes.
    unslice :: (Foldable f, SubIndex i j) => f (s j e) -> s i e


