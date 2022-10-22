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
    
    "SDP.Bordered" is a module that provides 'Bordered' and 'BorderedM' classes
    of structures with bounds.
    
    @since 0.3
-}
module SDP.Bordered
(
  -- * Bordered
  Bordered (..), Bordered1, Bordered2,
  
#ifdef SDP_QUALIFIED_CONSTRAINTS
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  Bordered', Bordered'',
#endif
  
  -- * Monadic Bordered
  BorderedM (..), BorderedM1, BorderedM2,
  
#ifdef SDP_QUALIFIED_CONSTRAINTS
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  BorderedM', BorderedM''
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

-- | 'BorderedM' is 'Bordered' version for mutable data structures.
class (Monad m, Index i, EstimateM m b) => BorderedM m b i | b -> i
  where
    {-# MINIMAL (getBounds|getLower, getUpper), rebounded' #-}
    
    -- | 'getBounds' returns 'bounds' of mutable data structure.
    getBounds :: b -> m (i, i)
    getBounds es = liftA2 (,) (getLower es) (getUpper es)
    
    -- | 'getLower' returns 'lower' bound of mutable data structure.
    getLower :: b -> m i
    getLower =  fsts . getBounds
    
    -- | 'getUpper' returns 'upper' bound of mutable data structure.
    getUpper :: b -> m i
    getUpper =  snds . getBounds
    
    -- | 'getSizesOf' returns 'sizes' of mutable data structure.
    getSizesOf :: b -> m [Int]
    getSizesOf =  fmap sizes . getBounds
    
    -- | 'nowIndexIn' is 'indexIn' version for mutable structures.
    nowIndexIn :: b -> i -> m Bool
    nowIndexIn es i = flip inRange i <$> getBounds es
    
    -- | 'getOffsetOf' is 'offsetOf' version for mutable structures.
    getOffsetOf :: b -> i -> m Int
    getOffsetOf es i = flip offset i <$> getBounds es
    
    -- | 'getIndexOf' is 'indexOf' version for mutable structures.
    getIndexOf :: b -> Int -> m i
    getIndexOf es i = flip index i <$> getBounds es
    
    -- | 'getIndices' returns 'indices' of mutable data structure.
    getIndices :: b -> m [i]
    getIndices =  fmap range . getBounds
    
    {- |
      @since 0.3
      
      @'rebounded' es bnds@ changes structure bounds, if possible - in place.
      
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
    rebounded :: (i, i) -> b -> m b
    rebounded =  rebounded'
    
    {- |
      @since 0.3
      
      Same as 'rebounded', but always create new structure.
    -}
    rebounded' :: (i, i) -> b -> m b

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

-- | 'BorderedM' contraint for @(Type -> Type)@-kind types.
type BorderedM1 m l i e = BorderedM m (l e) i

-- | 'BorderedM' contraint for @(Type -> Type -> Type)@-kind types.
type BorderedM2 m l i e = BorderedM m (l i e) i

#ifdef SDP_QUALIFIED_CONSTRAINTS
-- | 'BorderedM' contraint for @(Type -> Type)@-kind types.
type BorderedM' m l i = forall e . BorderedM m (l e) i

-- | 'BorderedM' contraint for @(Type -> Type -> Type)@-kind types.
type BorderedM'' m l = forall i e . BorderedM m (l i e) i
#endif

--------------------------------------------------------------------------------

instance Index i => Bordered (i, i) i
  where
    bounds = id
    lower  = fst
    upper  = snd
    
    indices = range
    indexIn = inRange
    rebound = const
    
    indexOf  = index
    offsetOf = offset

instance Bordered [e] Int
  where
    lower   = const 0
    rebound = L.take . size
    
    upper es = length es - 1

instance (Monad m, Index i) => BorderedM m (i, i) i
  where
    getBounds  = return
    getLower   = return . fst
    getUpper   = return . snd
    getIndices = return . range
    rebounded' = return ... const
    
    getIndexOf  = return ... index
    nowIndexIn  = return ... inRange
    getOffsetOf = return ... offset

instance Monad m => BorderedM m [e] Int
  where
    getLower  _ = return 0
    getUpper es = return (length es - 1)
    rebounded'  = return ... L.take . size


