{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE Safe, DeriveDataTypeable, DeriveGeneric, FlexibleContexts #-}

{- |
    Module      :  SDP.Templates.AnyVar
    Copyright   :  (c) Andrey Mulik 2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Templates.AnyVar" provides 'AnyVar' - template of generalized by
    index type structure, based on 'Int'-indexed primitive.
-}
module SDP.Templates.AnyVar
(
  -- * Export
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * Border template
  AnyVar ( AnyVar ), withAnyVar
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM
import SDP.SortM

import Data.Typeable
import Data.Field

import GHC.Generics

default ()

--------------------------------------------------------------------------------

{- |
  'AnyVar' is template, that appends arbitrary bounds to any structure.
  
  * 'Eq', 'Ord', 'Eq1' and 'Ord1' instances ingores bounds.
  * 'Thaw' and 'Freeze' instances for @'AnyVar' rep e@ inherit @rep e@ behavior.
-}
newtype AnyVar m rep e = AnyVar {fromAnyVar :: Var m (rep e)}
  deriving ( Typeable, Generic )

--------------------------------------------------------------------------------

{- Eq instance. -}

instance (MonadVar m, Eq (Var m (rep e))) => Eq (AnyVar m rep e)
  where
    AnyVar x == AnyVar y = x == y

--------------------------------------------------------------------------------

{- Nullable, NullableM and Estimate instances. -}

instance (MonadVar m, NullableM1 m rep e) => NullableM m (AnyVar m rep e)
  where
    newNull = AnyVar <$> (var =<< newNull)
    nowNull = nowNull <=< get this.fromAnyVar

--------------------------------------------------------------------------------

{- EstimateM and BorderedM instances. -}

instance (MonadVar m, EstimateM1 m rep e) => EstimateM m (AnyVar m rep e)
  where
    lestimateM xs n = do xs' <- get this (fromAnyVar xs); lestimateM xs' n
    
    estimateM = on (join ... liftA2 estimateM) (get this.fromAnyVar)
    
    getSizeOf = getSizeOf  <=< get this.fromAnyVar

instance (Index i, MonadVar m, BorderedM1 m rep i e)
      => BorderedM m (AnyVar m rep e) i
  where
    getIndices = getIndices <=< get this.fromAnyVar
    getBounds  = getBounds  <=< get this.fromAnyVar
    getLower   = getLower   <=< get this.fromAnyVar
    getUpper   = getUpper   <=< get this.fromAnyVar
    
    rebounded' bnds es = es <$ modifyRecordM this' (fromAnyVar es) (rebounded' bnds)

--------------------------------------------------------------------------------

{- Copyable and LinearM instances. -}

instance (MonadVar m, Copyable1 m rep e) => Copyable m (AnyVar m rep e)
  where
    copied = pack <=< copied <=< get this.fromAnyVar

instance (Index i, MonadVar m, BorderedM1 m rep i e, LinearM1 m rep e)
      => LinearM m (AnyVar m rep e) e
  where
    getHead = getHead <=< get this.fromAnyVar
    getLast = getLast <=< get this.fromAnyVar
    
    prepend e xs@(AnyVar es) = xs <$ (setRecord this es =<< prepend     e =<< get this es)
    append  xs@(AnyVar es) e = xs <$ (setRecord this es =<< flip append e =<< get this es)
    
    getLeft  = getLeft  <=< get this.fromAnyVar
    getRight = getRight <=< get this.fromAnyVar
    
    newLinear = pack <=< newLinear
    filled  n = pack <=< filled  n
    
    AnyVar es !#> i = do es' <- get this es; es' !#> i
    
    writeM (AnyVar es) i e = do es' <- get this es; writeM es' i e
    
    copied' (AnyVar es) l n = do es' <- get this es; pack =<< copied' es' l n
    
    reversed xs@(AnyVar es) = xs <$ (setRecord this es =<< reversed =<< get this es)
    
    copyTo (AnyVar src) os (AnyVar trg) ot n = do
      src' <- get this src
      trg' <- get this trg
      
      copyTo src' os trg' ot n
    
    miterate n = pack <=<< miterate n
    iterateM n = pack <=<< iterateM n
    
    foldrM f e = foldrM f e <=< get this.fromAnyVar
    foldlM f e = foldlM f e <=< get this.fromAnyVar
    
    ofoldrM f e = ofoldrM f e <=< get this.fromAnyVar
    ofoldlM f e = ofoldlM f e <=< get this.fromAnyVar
    
    takeM n = pack <=< takeM n <=< get this.fromAnyVar
    dropM n = pack <=< dropM n <=< get this.fromAnyVar
    keepM n = pack <=< keepM n <=< get this.fromAnyVar
    sansM n = pack <=< sansM n <=< get this.fromAnyVar
    
    splitM  n = uncurry (on (liftA2 (,)) pack) <=< splitM  n <=< get this.fromAnyVar
    divideM n = uncurry (on (liftA2 (,)) pack) <=< divideM n <=< get this.fromAnyVar
    
    prefixM p = prefixM p <=< get this.fromAnyVar
    suffixM p = suffixM p <=< get this.fromAnyVar
    mprefix p = mprefix p <=< get this.fromAnyVar
    msuffix p = msuffix p <=< get this.fromAnyVar

--------------------------------------------------------------------------------

{- MapM and IndexedM instances. -}

instance (MonadVar m, BorderedM1 m rep key e, LinearM1 m rep e, MapM1 m rep key e)
      => MapM m (AnyVar m rep e) key e
  where
    newMap' = pack <=<< newMap'
    newMap  = pack <=<  newMap
    
    writeM' (AnyVar es) i e = do es' <- get this es; writeM' es' i e
    
    AnyVar es >! i = do es' <- get this es; es' >! i
    
    overwrite xs@(AnyVar es) ascs = xs <$ (flip overwrite ascs =<< get this es)
    
    kfoldrM f base = kfoldrM f base <=< get this.fromAnyVar
    kfoldlM f base = kfoldlM f base <=< get this.fromAnyVar

instance (MonadVar m, IndexedM1 m rep key e)
      => IndexedM m (AnyVar m rep e) key e
  where
    fromAssocs  (l, u)   = pack <=< fromAssocs  (l, u)
    fromAssocs' (l, u) e = pack <=< fromAssocs' (l, u) e
    
    fromIndexed' = pack <=< fromIndexed'
    fromIndexedM = pack <=< fromIndexedM

--------------------------------------------------------------------------------

{- SortM instances. -}

instance (MonadVar m, SortM1 m rep e)
      => SortM m (AnyVar m rep e) e
  where
    sortedMBy f = sortedMBy f <=< get this.fromAnyVar
    sortMBy   f = sortMBy   f <=< get this.fromAnyVar

--------------------------------------------------------------------------------

{- Freeze and Thaw instances. -}

instance (MonadVar m, Thaw m imm (mut e))
      => Thaw m imm (AnyVar m mut e)
  where
    unsafeThaw = pack <=< unsafeThaw
    thaw       = pack <=< thaw

instance (MonadVar m, Freeze m (imm e) mut)
      => Freeze m (AnyVar m imm e) mut
  where
    unsafeFreeze = get this.fromAnyVar >=> unsafeFreeze
    freeze       = get this.fromAnyVar >=> freeze

--------------------------------------------------------------------------------

{-# INLINE pack #-}
pack :: MonadVar m => rep e -> m (AnyVar m rep e)
pack =  fmap AnyVar . var

{-# INLINE withAnyVar #-}
withAnyVar :: (MonadVar m, Typeable m, Typeable rep, Typeable (Var m), Typeable e)
           => (rep e -> m (rep e)) -> AnyVar m rep e -> m ()
withAnyVar f ps = () <$ modifyRecordM this' (fromAnyVar ps) f

