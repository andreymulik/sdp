{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE Safe, DeriveGeneric, FlexibleContexts #-}

{- |
    Module      :  SDP.Templates.AnyVar
    Copyright   :  (c) Andrey Mulik 2022-2026
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
  AnyVar ( AnyVar )
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM
import SDP.SortM
import SDP.Var

import GHC.Generics

default ()

--------------------------------------------------------------------------------

{- |
  'AnyVar' is template, that appends arbitrary bounds to any structure.
  
  * 'Eq', 'Ord', 'Eq1' and 'Ord1' instances ingores bounds.
  * 'Thaw' and 'Freeze' instances for @'AnyVar' rep e@ inherit @rep e@ behavior.
-}
newtype AnyVar m rep e = AnyVar {fromAnyVar :: Var m (rep e)}
  deriving ( Generic )

--------------------------------------------------------------------------------

{- Eq instance. -}

instance Eq (Var m (rep e)) => Eq (AnyVar m rep e)
  where
    (==) = on (==) fromAnyVar

--------------------------------------------------------------------------------

{- NullableM, EstimateM and BorderedM instances. -}

instance (MonadVar m, NullableM1 m rep e) => NullableM m (AnyVar m rep e)
  where
    isNullM = isNullM <=< unpack
    newNull = pack =<< newNull

instance (MonadVar m, EstimateM1 m rep e) => EstimateM m (AnyVar m rep e)
  where
    getSizeHint = getSizeHint <=< unpack
    getSizeOf   = getSizeOf <=< unpack
    
    (<<=>>) = onAnyVar (<<=>>)
    
    estimateMEQ = onAnyVar estimateMEQ
    estimateMNE = onAnyVar estimateMNE
    estimateMLE = onAnyVar estimateMLE
    estimateMGE = onAnyVar estimateMGE
    estimateMLT = onAnyVar estimateMLT
    estimateMGT = onAnyVar estimateMGT
    
    es <<=> n = unpack es >>= (<<=> n)
    
    hasLengthM     es n = unpack es >>= (`hasLengthM`     n)
    otherLengthM   es n = unpack es >>= (`otherLengthM`   n)
    noLongerThanM  es n = unpack es >>= (`noLongerThanM`  n)
    noShorterThanM es n = unpack es >>= (`noShorterThanM` n)
    shorterThanM   es n = unpack es >>= (`shorterThanM`   n)
    longerThanM    es n = unpack es >>= (`longerThanM`    n)

instance (MonadVar m, BorderedM1 m rep i e) => BorderedM m (AnyVar m rep e) i
  where
    getIndices = getIndices <=< unpack
    getSizesOf = getSizesOf <=< unpack
    getBounds  = getBounds  <=< unpack
    getLower   = getLower   <=< unpack
    getUpper   = getUpper   <=< unpack
    
    getOffsetOf es i = unpack es >>= (`getOffsetOf` i)
    getIndexOf  es i = unpack es >>=  (`getIndexOf` i)
    nowIndexIn  es i = unpack es >>=  (`nowIndexIn` i)
    
    getEitherViewOf bnds = mapM pack <=< getEitherViewOf bnds <=< unpack

--------------------------------------------------------------------------------

{- ForceableM, ConcatM and SequenceM instances. -}

instance (MonadVar m, ForceableM1 m rep e) => ForceableM m (AnyVar m rep e)
  where
    copied = pack <=< copied <=< unpack

instance (MonadVar m, ConcatM m (rep e)) => ConcatM m (AnyVar m rep e)
  where
    (<~>)      = pack <=<< onAnyVar (<~>)
    concatM    = pack <=<  concatMapM unpack
    concatMapM = pack <=<< concatMapM . (unpack <=<)

instance (MonadVar m, SequenceM1 m rep e) => SequenceM m (AnyVar m rep e) e
  where
    ofoldrM f e = ofoldrM f e <=< unpack
    ofoldlM f e = ofoldlM f e <=< unpack
    
    foldrM f e = foldrM f e <=< unpack
    foldlM f e = foldlM f e <=< unpack
    
    foldrM1 f = foldrM1 f <=< unpack
    foldlM1 f = foldlM1 f <=< unpack
    
    getLeft  = getLeft  <=< unpack
    getRight = getRight <=< unpack
    
    mprefix p = mprefix p <=< unpack
    msuffix p = msuffix p <=< unpack

--------------------------------------------------------------------------------

{- LinearM instance. -}

instance (MonadVar m, LinearM1 m rep e) => LinearM m (AnyVar m rep e) e
  where
    unsafeReadByOff es i   = do xs <- unpack es; unsafeReadByOff xs i
    unsafeWriteM    es i e = do xs <- unpack es; unsafeWriteM    xs i e
    
    prepend e es = pack =<<   prepend e  =<< unpack es
    append  es e = pack =<< (`append` e) =<< unpack es
    
    unconsM' = unpack >=> unconsM' >=> mapM (\ (x, xs) -> do xs' <- pack xs; pure (x, xs'))
    unsnocM' = unpack >=> unsnocM' >=> mapM (\ (xs, x) -> do xs' <- pack xs; pure (xs', x))
    
    takeM n = pack <=< takeM n <=< unpack
    dropM n = pack <=< dropM n <=< unpack
    keepM n = pack <=< keepM n <=< unpack
    sansM n = pack <=< sansM n <=< unpack
    
    singleM = pack <=< singleM
    
    newLinear     = pack <=< newLinear
    newLinearN  n = pack <=< newLinearN n
    replicateM  n = pack <=< replicateM n
    fromFoldableM = pack <=< fromFoldableM
    
    reverseM = pack <=< reverseM <=< unpack
    reversed = reversed <=< unpack
    
    (!*) = unsafeReadByOff
    
    writeM = unsafeWriteM
    
    unsafeCopyM es l n = do
      xs   <- unpack es
      copy <- unsafeCopyM xs l n
      pack copy
    
    unsafeCopyTo xs xo ys yo n = do
      xs' <- unpack xs
      ys' <- unpack ys
      
      unsafeCopyTo xs' xo ys' yo n

--------------------------------------------------------------------------------

{- MapM and IndexedM instances. -}

instance (MonadVar m, MapM1 m rep key e) => MapM m (AnyVar m rep e) key e
  where
    newMap' = pack <=<< newMap'
    newMap  = pack <=<  newMap
    
    getKeys   = getKeys <=< unpack
    getAssocs = getAssocs <=< unpack
    
    writeM'  es key e = do xs <- unpack es; writeM'  xs key e
    insertM' es key e = do xs <- unpack es; insertM' xs key e
    deleteM' es key   = do xs <- unpack es; deleteM' xs key
    memberM' es key   = do xs <- unpack es; memberM' xs key
    
    overwrite es ascs = do xs <- unpack es; overwrite xs ascs
    
    unionM''        f = pack <=<< onAnyVar (unionM''        f)
    differenceM''   f = pack <=<< onAnyVar (differenceM''   f)
    intersectionM'' f = pack <=<< onAnyVar (intersectionM'' f)
    
    kfoldrM  f base = kfoldrM  f base <=< unpack
    kfoldlM  f base = kfoldlM  f base <=< unpack
    kfoldrM' f base = kfoldrM' f base <=< unpack
    kfoldlM' f base = kfoldlM' f base <=< unpack
    
    unsafeReadMByKey es key = do xs <- unpack es; unsafeReadMByKey xs key
    eitherReadMByKey es key = do xs <- unpack es; eitherReadMByKey xs key

instance (MonadVar m, IndexedM1 m rep key e) => IndexedM m (AnyVar m rep e) key e
  where
    fromAssocs  bnds = pack <=<  fromAssocs  bnds
    fromAssocs' bnds = pack <=<< fromAssocs' bnds
    
    fromIndexed' = pack <=< fromIndexed'
    fromIndexedM = pack <=< fromIndexedM

--------------------------------------------------------------------------------

{- SortM instance. -}

instance (MonadVar m, SequenceM1 m (AnyVar m rep) e, SortM1 m rep e)
      => SortM m (AnyVar m rep e) e
  where
    sortedMBy f = sortedMBy f <=< unpack
    sortMBy   f = sortMBy   f <=< unpack

--------------------------------------------------------------------------------

{- Freeze and Thaw instances. -}

instance (MonadVar m, Thaw m imm (mut e)) => Thaw m imm (AnyVar m mut e)
  where
    unsafeThaw = pack <=< unsafeThaw
    thaw       = pack <=< thaw

instance (MonadVar m, Freeze m (imm e) mut) => Freeze m (AnyVar m imm e) mut
  where
    unsafeFreeze = unpack >=> unsafeFreeze
    freeze       = unpack >=> freeze

--------------------------------------------------------------------------------

{-# INLINE onAnyVar #-}
onAnyVar :: MonadVar m => (rep e -> rep e -> m a) -> AnyVar m rep e -> AnyVar m rep e -> m a
onAnyVar go =  on (join ... liftA2 go) unpack

{-# INLINE pack #-}
pack :: MonadVar m => rep e -> m (AnyVar m rep e)
pack =  fmap AnyVar . newVar

{-# INLINE unpack #-}
unpack :: MonadVar m => AnyVar m rep e -> m (rep e)
unpack =  readVar . fromAnyVar




