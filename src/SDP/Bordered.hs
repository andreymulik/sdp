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
  -- $terminology
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

{- $terminology
  Here and below, the following terms are used.
  
  /Real size/ - the number of actually stored elements in the structure
  
  /Virtual size/ or just /size/ - the declared number of elements in a
  particular data structure. Depending on the implementation of the structure
  can be:
  
  * strictly equal to real size (e.g., list);
  * be less than or equal to the actual size (e.g., Vector, ByteString, all
  standard SDP structures);
  * be larger than real size (e.g., sparse matrixes).
  
  All SDP-provided classes use only the /virtual size/.
  
  /Bounds/ - the 'lower' and 'upper' 'bounds' of the structure, stored or
  computed. 'size' of /bounds/ must be equal to 'sizeOf' structure.
  
  /Range/ - the area between the lower and upper /bounds/ of the structure.
  
  /View/ - an ordered set of elements, stored or calculated, each of which
  corresponds to a unique index from the specified range. 'sizeOf' view must be
  equal to 'size' of its /bounds/. In general, any structure is a view of itself.
  
  /Slice/ is a structure represented by a reference to a part of another
  structure contents. The /virtual size/ of /slice/ is different from the
  /virtual size/ of the original. For example, a sparse matrix is ​​usually not
  considered a slice of a real structure.
-}

--------------------------------------------------------------------------------

{- |
  'Bordered' is a class of structures that have a constant size and borders
  (represented by the smallest and largest indices).
  
  Note that the 'Bordered' class isn't designed for structures with unordered or
  missing keys. Its main purpose is to determine the geometry ('sizesOf'), the
  order of the keys ('indexOf', 'offsetOf') and ensure that the keys are
  correctly mapped to the content.
  
  'Bordered' doesn't affect the contents of the structure, but implies that each
  index within the specified bounds /uniquely/ corresponds to some stored or
  computed element. Boundary operations don't affect the order of the elements,
  but may make some of them unreachable (if the size of the structure is
  reduced) or change the corresponding indices.
  
  All 'Bordered' operations is safe. Only 'viewOf' can 'throw' exceptions.
-}
class (Index i, Estimate b) => Bordered b i | b -> i
  where
    {-# MINIMAL (bounds|(lower, upper)), viewOf #-}
    
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
    
    -- | Returns the virtual geometry of givel structure.
    sizesOf :: b -> [Int]
    sizesOf =  sizes . bounds
    
    {-# INLINE indexIn #-}
    -- | Checks if an index within the boundaries of the structure.
    indexIn :: b -> i -> Bool
    indexIn =  inRange . bounds
    
    {-# INLINE indices #-}
    -- | Returns list of indices, also see 'range'.
    indices :: b -> [i]
    indices =  range . bounds
    
    {-# INLINE indexOf #-}
    -- | Returns 'index' by 'offset'.
    indexOf :: b -> Int -> i
    indexOf =  index . bounds
    
    {-# INLINE offsetOf #-}
    -- | Returns 'offset' by 'index'.
    offsetOf :: b -> i -> Int
    offsetOf =  offset . bounds
    
    {- |
      @since 0.3
      
      @'viewOf' bnds es@ returns a /view/ of the @es@ with @bnds@ bounds.
      
      If the size of the expected view is larger than the size of the original
      structure, the behavior of the function is undefined.
    -}
    viewOf :: (i, i) -> b -> b

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  'BorderedM' is 'Bordered' version for mutable data structures.
-}
class (Monad m, Index i, EstimateM m b) => BorderedM m b i | b -> i
  where
    {-# MINIMAL (getBounds|getLower, getUpper), getViewOf #-}
    
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
    
    -- | 'getIndices' returns 'indices' of mutable data structure.
    getIndices :: b -> m [i]
    getIndices =  fmap range . getBounds
    
    -- | 'getIndexOf' is 'indexOf' version for mutable structures.
    getIndexOf :: b -> Int -> m i
    getIndexOf es i = flip index i <$> getBounds es
    
    -- | 'getOffsetOf' is 'offsetOf' version for mutable structures.
    getOffsetOf :: b -> i -> m Int
    getOffsetOf es i = flip offset i <$> getBounds es
    
    {- |
      @since 0.3
      
      @'getViewOf' bnds es@ returns a /view/ of the @es@ with @bnds@ bounds.
      Also see 'viewOf'.
    -}
    getViewOf :: (i, i) -> b -> m b

--------------------------------------------------------------------------------

-- | 'Bordered' contraint for @(Type -> Type)@-kind types.
type Bordered1 l i e = Bordered (l e) i

-- | 'Bordered' contraint for @(Type -> Type -> Type)@-kind types.
type Bordered2 l i e = Bordered (l i e) i

-- | 'BorderedM' contraint for @(Type -> Type)@-kind types.
type BorderedM1 m l i e = BorderedM m (l e) i

-- | 'BorderedM' contraint for @(Type -> Type -> Type)@-kind types.
type BorderedM2 m l i e = BorderedM m (l i e) i

--------------------------------------------------------------------------------

#ifdef SDP_QUALIFIED_CONSTRAINTS
-- | 'Bordered' contraint for @(Type -> Type)@-kind types.
type Bordered' l i = forall e . Bordered (l e) i

-- | 'Bordered' contraint for @(Type -> Type -> Type)@-kind types.
type Bordered'' l = forall i e . Bordered (l i e) i

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
    
    viewOf   = const
    indexOf  = index
    offsetOf = offset

instance Bordered [e] Int
  where
    lower  _ = 0
    viewOf   = L.take . size
    upper es = length es - 1

instance (Monad m, Index i) => BorderedM m (i, i) i
  where
    getBounds  = return
    getLower   = return . fst
    getUpper   = return . snd
    getIndices = return . range
    getViewOf  = return ... const
    
    getIndexOf  = return ... index
    nowIndexIn  = return ... inRange
    getOffsetOf = return ... offset

instance Monad m => BorderedM m [e] Int
  where
    getLower  _ = return 0
    getViewOf   = return ... viewOf
    getUpper es = return (length es - 1)


