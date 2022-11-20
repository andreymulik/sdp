{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DefaultSignatures #-}
{-# LANGUAGE Safe, CPP, ConstraintKinds #-}

#ifdef SDP_QUALIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.IndexedM
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.IndexedM" provides 'IndexedM' and 'Thaw' classes.
-}
module SDP.IndexedM
(
  -- * Exports
  module SDP.LinearM,
  module SDP.Indexed,
  module SDP.MapM,
  
  -- * IndexedM
  IndexedM (..), IndexedM1, IndexedM2,
  
  -- * Thaw
  Thaw (..), Thaw1, Thaw2,
  
#ifdef SDP_QUALIFIED_CONSTRAINTS
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  IndexedM', IndexedM'', Thaw', Thaw''
#endif
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.LinearM
import SDP.Indexed
import SDP.MapM

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

-- | Class for work with mutable indexed structures.
class (LinearM m v e, BorderedM m v i, MapM m v i e) => IndexedM m v i e
  where
    {-# MINIMAL fromIndexed', fromIndexedM #-}
    
    {- |
      @fromAssocs bnds ascs@ creates new structure from list of associations,
      without default element. Note that @bnds@ is @ascs@ bounds and may not
      match with the result bounds (not always possible).
    -}
    fromAssocs :: (i, i) -> [(i, e)] -> m v
    fromAssocs =
      let err = throw $ UndefinedValue "in SDP.IndexedM.fromAssocs {default}"
      in  flip fromAssocs' err
    
    {- |
      @fromAssocs' bnds defvalue ascs@ creates new structure from list of
      associations, with default element. Note that @bnds@ is @ascs@ bounds and
      may not match with the result bounds (not always possible).
    -}
    fromAssocs' :: (i, i) -> e -> [(i, e)] -> m v
    fromAssocs' bnds defvalue = newMap' defvalue . filter (inRange bnds . fst)
    
    -- | Just swap two elements.
    swapM' :: v -> i -> i -> m ()
    swapM' es i j = do ei <- es >! i; writeM' es i =<< es >! j; writeM' es j ei
    
    -- | fromIndexed' is overloaded version of thaw.
    fromIndexed' :: Indexed v' j e => v' -> m v
    
    -- | fromIndexed converts one mutable structure to other.
    fromIndexedM :: IndexedM m v' j e => v' -> m v
    
    -- | reshaped creates new view of given stricture with reshaping function.
    reshaped :: IndexedM m v' j e => (i, i) -> v' -> (i -> j) -> m v
    reshaped bnds es f = do
      es' <- fromAssocs bnds []
      es' <$ mupdate es' (\ i _ -> es !> f i)
    
    {- |
      @'fromAccum' f es ies@ create a new structure from @es@ elements
      selectively updated by function @f@ and @ies@ associations list.
    -}
    fromAccum :: (e -> e' -> e) -> v -> [(i, e')] -> m v
    fromAccum f es ascs = getBounds es >>=<< ies $ fromAssocs
      where
        ies = sequence [ do e <- es !> i; return (i, f e e') | (i, e') <- ascs ]

--------------------------------------------------------------------------------

-- | Service class of immutable to mutable conversions.
class Monad m => Thaw m v v' | v' -> m
  where
    {- |
      @thaw@ is safe way to convert a immutable structure to a mutable. @thaw@
      should copy the old structure or ensure that it will not be used after the
      procedure calling.
    -}
    thaw :: v -> m v'
    
    {- |
      @unsafeThaw@ is unsafe version of 'thaw'. @unsafeThaw@ doesn't guarantee
      that the structure will be copied or locked. It only guarantees that if
      the old structure isn't used, no error will occur.
    -}
    unsafeThaw :: v -> m v'
    unsafeThaw =  thaw

--------------------------------------------------------------------------------

-- | 'IndexedM' contraint for @(Type -> Type)@-kind types.
type IndexedM1 m v i e = IndexedM m (v e) i e

-- | 'IndexedM' contraint for @(Type -> Type -> Type)@-kind types.
type IndexedM2 m v i e = IndexedM m (v i e) i e

-- | 'Thaw' contraint for @(Type -> Type)@-kind types.
type Thaw1 m v v' e = Thaw m (v e) (v' e)

-- | 'Thaw' contraint for @(Type -> Type -> Type)@-kind types.
type Thaw2 m v v' i e = Thaw m (v i e) (v' i e)

--------------------------------------------------------------------------------

#ifdef SDP_QUALIFIED_CONSTRAINTS
-- | 'IndexedM' contraint for @(Type -> Type)@-kind types.
type IndexedM' m v i = forall e . IndexedM m (v e) i e

-- | 'IndexedM' contraint for @(Type -> Type -> Type)@-kind types.
type IndexedM'' m v = forall i e . IndexedM m (v i e) i e

-- | 'Thaw' contraint for @(Type -> Type)@-kind types.
type Thaw' m v v' = forall e . Thaw m (v e) (v' e)

-- | 'Thaw' contraint for @(Type -> Type -> Type)@-kind types.
type Thaw'' m v v' = forall i e . Thaw m (v i e) (v' i e)
#endif




