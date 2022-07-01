{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE Safe, CPP, ConstraintKinds #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.BorderedM
    Copyright   :  (c) Andrey Mulik 2021
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
  
#if __GLASGOW_HASKELL__ >= 806
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
    {-# MINIMAL (getBounds|getLower, getUpper) #-}
    
    -- | 'getBounds' returns 'bounds' of mutable data structure.
    getBounds :: b -> m (i, i)
    getBounds es = liftA2 (,) (getLower es) (getUpper es)
    
    -- | 'getLower' returns 'lower' bound of mutable data structure.
    getLower :: b -> m i
    getLower =  fsts . getBounds
    
    -- | 'getUpper' returns 'upper' bound of mutable data structure.
    getUpper :: b -> m i
    getUpper =  snds . getBounds
    
    -- | 'getSizeOf' returns 'size' of mutable data structure.
    getSizeOf :: b -> m Int
    getSizeOf =  fmap size . getBounds
    
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

--------------------------------------------------------------------------------

-- | 'BorderedM' contraint for @(Type -> Type)@-kind types.
type BorderedM1 m l i e = BorderedM m (l e) i

-- | 'BorderedM' contraint for @(Type -> Type -> Type)@-kind types.
type BorderedM2 m l i e = BorderedM m (l i e) i

#if __GLASGOW_HASKELL__ >= 806
-- | 'BorderedM' contraint for @(Type -> Type)@-kind types.
type BorderedM' m l i = forall e . BorderedM m (l e) i

-- | 'BorderedM' contraint for @(Type -> Type -> Type)@-kind types.
type BorderedM'' m l = forall i e . BorderedM m (l i e) i
#endif




