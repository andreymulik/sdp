{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, GADTs #-}
{-# LANGUAGE Safe, DeriveDataTypeable, DeriveGeneric, FlexibleContexts, DataKinds #-}

{- |
    Module      :  SDP.Templates.AnyVar
    Copyright   :  (c) Andrey Mulik 2022-2023
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

default ()

--------------------------------------------------------------------------------

{- |
  'AnyVar' is template, that appends arbitrary bounds to any structure.

  * 'Eq', 'Ord', 'Eq1' and 'Ord1' instances ingores bounds.
  * 'Thaw' and 'Freeze' instances for @'AnyVar' m var rep@ inherit @rep@ behavior.
-}
data AnyVar m var rep
    where
      AnyVar :: IsMVar m var => {fromAnyVar :: var rep} -> AnyVar m var rep
  deriving ( Typeable )

--------------------------------------------------------------------------------

instance (IsMVar m var, Eq (var rep)) => Eq (AnyVar m var rep)
  where
    AnyVar x == AnyVar y = x == y

{- Eq Nullable, NullableM and Estimate instances. -}

instance (IsMVar m var, NullableM m rep) => NullableM m (AnyVar m var rep)
  where
    newNull = pack    =<< newNull
    nowNull = nowNull <=< unpack

--------------------------------------------------------------------------------

{- EstimateM and BorderedM instances. -}

instance (IsMVar m var, EstimateM m rep) => EstimateM m (AnyVar m var rep)
  where
    lestimateM xs n = (`lestimateM` n) =<< unpack xs

    estimateM = (join ... liftA2 estimateM) `on` unpack

    getSizeOf = getSizeOf <=< unpack

instance (Index i, IsMVar m var, BorderedM m rep i) => BorderedM m (AnyVar m var rep) i
  where
    getIndices = getIndices <=< unpack
    getBounds  = getBounds  <=< unpack
    getLower   = getLower   <=< unpack
    getUpper   = getUpper   <=< unpack

    getViewOf bnds (AnyVar es) = pack =<< getViewOf bnds =<< fromMRef es

--------------------------------------------------------------------------------

{- ForceableM and LinearM instances. -}

instance (IsMVar m var, ForceableM m rep) => ForceableM m (AnyVar m var rep)
  where
    copied = pack <=< copied <=< unpack

instance
    (
      Attribute "set" "" m (var rep) rep, IsMVar m var,
      Index i, BorderedM m rep i, LinearM m rep e
    ) => SequenceM m (AnyVar m var rep) e
  where
    getHead = getHead <=< unpack
    getLast = getLast <=< unpack

    getTail = pack <=< getTail <=< unpack
    getInit = pack <=< getInit <=< unpack

    e += xs = xs <$ withAnyVar (e +=) xs
    xs =+ e = xs <$ withAnyVar (=+ e) xs

    getLeft  = getLeft  <=< unpack
    getRight = getRight <=< unpack

    newLinear = pack <=< newLinear

    reversed xs = xs <$ reversed' xs

    es !#> i = (!#> i) =<< unpack es

    writeM es i e = do es' <- unpack es; writeM es' i e

    foldrM f e = foldrM f e <=< unpack
    foldlM f e = foldlM f e <=< unpack

    ofoldrM f e = ofoldrM f e <=< unpack
    ofoldlM f e = ofoldlM f e <=< unpack

    prefixM p = prefixM p <=< unpack
    suffixM p = suffixM p <=< unpack
    mprefix p = mprefix p <=< unpack
    msuffix p = msuffix p <=< unpack

instance
    (
      Attribute "set" "" m (var rep) rep, IsMVar m var,
      Index i, BorderedM m rep i, LinearM m rep e
    ) => LinearM m (AnyVar m var rep) e
  where
    filled n = pack <=< filled n

    copied' es l n = do es' <- unpack es; pack =<< copied' es' l n

    copyTo src os trg ot n = do
      src' <- unpack src
      trg' <- unpack trg
      copyTo src' os trg' ot n

    miterate n = pack <=<< miterate n
    iterateM n = pack <=<< iterateM n

    reversed' = reversed' <=< unpack

    takeM n es = es <$ withAnyVar (takeM n) es
    dropM n es = es <$ withAnyVar (dropM n) es
    keepM n es = es <$ withAnyVar (keepM n) es
    sansM n es = es <$ withAnyVar (sansM n) es

    splitM  n = uncurry (liftA2 (,) `on` pack) <=< splitM  n <=< unpack
    divideM n = uncurry (liftA2 (,) `on` pack) <=< divideM n <=< unpack

--------------------------------------------------------------------------------

{- MapM and IndexedM instances. -}

instance
    (
      NullableM m rep, BorderedM m rep key, LinearM m rep e, MapM m rep key e,
      Attribute "set" "" m (var rep) rep, IsMVar m var
    ) => MapM m (AnyVar m var rep) key e
  where
    newMap' = pack <=<< newMap'
    newMap  = pack <=<  newMap

    insertM' es key e = do es' <- unpack es; insertM' es' key e
    deleteM' es key   = do es' <- unpack es; deleteM' es' key

    writeM' es i e = do es' <- unpack es; writeM' es' i e

    es >! i = (>! i) =<< unpack es

    overwrite es ascs = flip overwrite ascs =<< unpack es

    kfoldrM f base = kfoldrM f base <=< unpack
    kfoldlM f base = kfoldlM f base <=< unpack

instance
    (
      Attribute "set" "" m (var rep) rep, IsMVar m var,
      IndexedM m rep key e, NullableM m rep
    ) => IndexedM m (AnyVar m var rep) key e
  where
    fromAssocs  (l, u)   = pack <=< fromAssocs  (l, u)
    fromAssocs' (l, u) e = pack <=< fromAssocs' (l, u) e

    fromIndexed' = pack <=< fromIndexed'
    fromIndexedM = pack <=< fromIndexedM

--------------------------------------------------------------------------------

{- SortM instances. -}

instance (IsMVar m var, SortM m rep e) => SortM m (AnyVar m var rep) e
  where
    sortedMBy f = sortedMBy f <=< unpack
    sortMBy   f = sortMBy   f <=< unpack

--------------------------------------------------------------------------------

{- Freeze and Thaw instances. -}

instance (IsMVar m var, Thaw m imm mut) => Thaw m imm (AnyVar m var mut)
  where
    unsafeThaw = pack <=< unsafeThaw
    thaw       = pack <=< thaw

instance (IsMVar m var, Freeze m imm mut) => Freeze m (AnyVar m var imm) mut
  where
    unsafeFreeze = unpack >=> unsafeFreeze
    freeze       = unpack >=> freeze

--------------------------------------------------------------------------------

{-# INLINE pack #-}
pack :: IsMVar m var => rep -> m (AnyVar m var rep)
pack =  fmap AnyVar . var

unpack :: IsMVar m var => AnyVar m var rep -> m rep
unpack =  fromMRef . fromAnyVar

{-# INLINE withAnyVar #-}
-- | Perform @(var e)@ action on 'AnyVar' and update it.
withAnyVar :: (Attribute "set" "" m (var rep) rep, IsMVar m var)
           => (rep -> m rep) -> AnyVar m var rep -> m ()
withAnyVar f (AnyVar es) = accessSet attribute es =<< f =<< fromMRef es

