{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE Safe, CPP, ConstraintKinds #-}

#ifdef SDP_QUALIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.BorderedM
    Copyright   :  (c) Andrey Mulik 2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.BorderedM" is a module that provides 'BorderedM' - class of structures
    with mutable bounds.
    
    @since 0.3
-}
module SDP.BorderedM
(
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

default ()

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

