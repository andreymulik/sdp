{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Safe, CPP, ConstraintKinds #-}

#ifdef SDP_QUALIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.Bordered
    Copyright   :  (c) Andrey Mulik 2021-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Bordered" is a module that provides 'Bordered' - class of structures
    with immutable bounds.
    
    @since 0.3
-}
module SDP.Bordered
(
  -- * Bordered
  Bordered (..), Bordered1, Bordered2,
  
#ifdef SDP_QUALIFIED_CONSTRAINTS
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  Bordered', Bordered''
#endif
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Index

import qualified Data.List as L

default ()

--------------------------------------------------------------------------------

-- | Class of bordered data structures.
class (Index i, Estimate b) => Bordered b i | b -> i
  where
    {-# MINIMAL (bounds|(lower, upper)), rebound #-}
    
    {-# INLINE bounds #-}
    {- |
      Returns the exact 'upper' and 'lower' bounds of given structure. If the
      structure doesn't have explicitly defined boundaries (list, for example),
      use the @'defaultBounds' . 'sizeOf'@.
    -}
    bounds :: b -> (i, i)
    bounds es = (lower es, upper es)
    
    {-# INLINE lower #-}
    -- | Returns lower bound of structure
    lower :: b -> i
    lower =  fst . bounds
    
    {-# INLINE upper #-}
    -- | Returns upper bound of structure
    upper :: b -> i
    upper =  snd . bounds
    
    {-# INLINE sizeOf #-}
    -- | Returns actual size of structure.
    sizeOf :: b -> Int
    sizeOf =  size . bounds
    
    -- | Returns actual sizes of structure.
    sizesOf :: b -> [Int]
    sizesOf =  sizes . bounds
    
    {-# INLINE indexIn #-}
    -- | Checks if an index falls within the boundaries of the structure.
    indexIn :: b -> i -> Bool
    indexIn =  inRange . bounds
    
    {-# INLINE indices #-}
    -- | Returns index range list.
    indices :: b -> [i]
    indices =  range . bounds
    
    {-# INLINE indexOf #-}
    -- | Returns index by offset in structure.
    indexOf :: b -> Int -> i
    indexOf =  index . bounds
    
    {-# INLINE offsetOf #-}
    -- | Returns index offset in structure bounds.
    offsetOf :: b -> i -> Int
    offsetOf =  offset . bounds
    
    {- |
      @since 0.3
      
      @'rebound' es bnds@ changes structure bounds, if possible - in place.
      
      * If given bounds is empty, returns an empty structure (with *any* empty
      bounds, e.g. @defaultBounds 0@).
      * If the new range is lesser than the current size of the structure,
      bounds of a suitable size must be set
      * If the new range is larger than the current size of the structure, an
      'UnacceptableExpansion' exception occurs
      * If the transferred boundaries cannot be set for other reasons,
      boundaries of the same size should be set.
      
      You can calculate new boundaries if given cannot be set in any way. Unless
      otherwise stated, @'defaultBounds' ('size' bnds)@ is implied.
    -}
    rebound :: (i, i) -> b -> b

--------------------------------------------------------------------------------

instance Index i => Bordered (i, i) i
  where
    bounds = id
    lower  = fst
    upper  = snd
    
    indices = range
    indexIn = inRange
    rebound = const
    
    sizeOf   = size
    indexOf  = index
    offsetOf = offset

instance Bordered [e] Int
  where
    sizeOf  = length
    lower   = const 0
    rebound = L.take . size
    
    upper es = length es - 1

--------------------------------------------------------------------------------

-- | 'Bordered' contraint for @(Type -> Type)@-kind types.
type Bordered1 l i e = Bordered (l e) i

-- | 'Bordered' contraint for @(Type -> Type -> Type)@-kind types.
type Bordered2 l i e = Bordered (l i e) i

#ifdef SDP_QUALIFIED_CONSTRAINTS
-- | 'Bordered' contraint for @(Type -> Type)@-kind types.
type Bordered' l i = forall e . Bordered (l e) i

-- | 'Bordered' contraint for @(Type -> Type -> Type)@-kind types.
type Bordered'' l = forall i e . Bordered (l i e) i
#endif


