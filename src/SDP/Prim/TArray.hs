{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, GADTs #-}
{-# LANGUAGE Trustworthy, MagicHash, PatternSynonyms, UndecidableInstances #-}

{- |
    Module      :  SDP.Prim.TArray
    Copyright   :  (c) Andrey Mulik 2020-2026
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable
    
    "SDP.Prim.TArray" provides lazy boxed array of @stm@ 'TVar's.
    Note that 'TArray#' stores each element in 'TVar'.
-}
module SDP.Prim.TArray
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.Var,
  
  -- * MArray
  MArray# (.., TArray#), TArray#
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Prim.SArray
import SDP.IndexedM
import SDP.Var

import Data.Functor

import GHC.Conc

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

-- | 'SArray#' of @stm@ 'TVar's.
type TArray# = MArray# STM

{-# COMPLETE TArray# #-}
-- | Legacy pattern synonym for @sdp-0.2@ compatibility.
pattern TArray# :: SArray# (Var STM e) -> TArray# e
pattern TArray# es = MArray# es

--------------------------------------------------------------------------------

-- | Generalized array of variables.
data MArray# m e
  where
    MArray# :: MonadVar m => {-# UNPACK #-} !(SArray# (Var m e)) -> MArray# m e

--------------------------------------------------------------------------------

{- Eq instance. -}

instance Eq (Var m e) => Eq (MArray# m e)
  where
    MArray# xs == MArray# ys = xs == ys

--------------------------------------------------------------------------------

{- Nullable and NullableM instances. -}

instance MonadVar m => Nullable (MArray# m e)
  where
    isNull = \ (MArray# es) -> isNull es
    lzero  = MArray# lzero

instance MonadVar m => NullableM m (MArray# m e)
  where
    newNull = pure lzero
    isNullM = pure . isNull

--------------------------------------------------------------------------------

{- Estimate and EstimateM instances. -}

instance Estimate (MArray# m e)
  where
    sizeOf = sizeOf . unpack
    
    (<==>) = on (<=>) sizeOf
    (.<=.) = on (<=)  sizeOf
    (.>=.) = on (>=)  sizeOf
    (.>.)  = on (>)   sizeOf
    (.<.)  = on (<)   sizeOf
    
    (<.=>) = (<=>) . sizeOf
    (.>=)  = (>=)  . sizeOf
    (.<=)  = (<=)  . sizeOf
    (.>)   = (>)   . sizeOf
    (.<)   = (<)   . sizeOf

instance Monad m => EstimateM m (MArray# m e)
  where
    getSizeHint = pure . sizeHint
    getSizeOf   = pure . sizeOf
    
    estimateMGE = pure ... (.>=.)
    estimateMLE = pure ... (.<=.)
    estimateMGT = pure ... (.>.)
    estimateMLT = pure ... (.<.)
    estimateMNE = pure ... (./=.)
    estimateMEQ = pure ... (.==.)
    
    noShorterThanM = pure ... (.>=)
    noLongerThanM  = pure ... (.<=)
    longerThanM    = pure ... (.>)
    shorterThanM   = pure ... (.<)
    otherLengthM   = pure ... (./=)
    hasLengthM     = pure ... (.==)
    
    (<<=>>) = pure ... (<==>)
    (<<=>)  = pure ... (<.=>)

--------------------------------------------------------------------------------

{- Bordered and BorderedM instances. -}

instance Bordered (MArray# m e) Int
  where
    lower _ = 0
    upper   = upper . unpack
    indexIn = \ es i -> i >= 0 && i < sizeOf (unpack es)
    
    bounds   (MArray# es) = (0, upper es)
    indices  (MArray# es) = [0 .. upper es]
    indexOf  (MArray# es) = index (0, upper es)
    offsetOf (MArray# es) = offset (0, upper es)
    
    eitherViewOf bnds (MArray# es) = MArray# <$> eitherViewOf bnds es

instance MonadVar m => BorderedM m (MArray# m e) Int
  where
    getIndexOf = pure ... indexOf
    getIndices = pure . indices
    getBounds  = pure . bounds
    getUpper   = pure . upper
    getLower _ = pure 0
    
    getEitherViewOf bnds (MArray# es) = fmap MArray# <$> getEitherViewOf bnds es

--------------------------------------------------------------------------------

{- ForceableM and LinearM instances. -}

instance MonadVar m => ForceableM m (MArray# m e)
  where
    copied (MArray# es) = MArray# <$> traverse (newVar <=< readVar) es

instance MonadVar m => ConcatM m (MArray# m e)
  where
    MArray# xs <~> MArray# ys = pure $ MArray# (xs ++ ys)

instance MonadVar m => SequenceM m (MArray# m e) e
  where
    ofoldlM f base (MArray# es) = ofoldl (\ i xs -> join . liftA2 (f i) xs . readVar) (pure base) es
    ofoldrM f base (MArray# es) = ofoldr (\ i -> join ... liftA2 (f i) . readVar) (pure base) es
    
    foldlM f base (MArray# es) = foldl (\ xs -> join . liftA2 f xs . readVar) (pure base) es
    foldrM f base (MArray# es) = foldr (join ... liftA2 f . readVar) (pure base) es
    
    getLeft  (MArray# es) = mapM readVar $ listL es
    getRight (MArray# es) = mapM readVar $ listR es

instance MonadVar m => LinearM m (MArray# m e) e
  where
    prepend e (MArray# es) = MArray# . (:> es) <$> newVar e
    append  (MArray# es) e = MArray# . (es :<) <$> newVar e
    
    unconsM' (MArray# es) = case uncons' es of
      Just (v, t) -> readVar v <&> \ h -> Just (h, MArray# t)
      Nothing     -> pure Nothing
    
    unsnocM' (MArray# es) = case unsnoc' es of
      Just (i, v) -> readVar v <&> \ l -> Just (MArray# i, l)
      Nothing     -> pure Nothing
    
    takeM n = pure . MArray# . take n . unpack
    dropM n = pure . MArray# . drop n . unpack
    keepM n = pure . MArray# . keep n . unpack
    sansM n = pure . MArray# . sans n . unpack
    
    singleM e = MArray# . single <$> newVar e
    
    newLinear     = fmap (MArray# . fromList) . mapM newVar
    newLinearN  n = fmap (MArray# . fromListN n) . mapM newVar
    fromFoldableM = fmap (MArray# . fromList) . foldr (liftA2 (:) . newVar) (pure [])
    
    reverseM = pure . MArray# . reverse . unpack
    
    reversed (MArray# es) = [1 .. n `quot` 2] `forM_` \ i -> do
        let ei' = es !! i; j = n - i; ej' = es !! j
        
        ei <- readVar ei'
        ej <- readVar ej'
        
        writeVar ej' ei
        writeVar ei' ej
      where
        n = sizeOf es
    
    unsafeReadByOff (MArray# es) o = readVar  (es !! o)
    unsafeWriteM    (MArray# es) o = writeVar (es !! o)
    
    (!*) = unsafeReadByOff
    
    writeM = unsafeWriteM
    
    unsafeCopyM (MArray# es) l n = pure . MArray# . take n $ drop l es
    
    unsafeCopyTo src so trg to n = when (n > 0) $ do
        when      (so < 0 || to < 0)      $ underEx "unsafeCopyTo"
        when (so + n > n1 || to + n > n2) $ overEx  "unsafeCopyTo"
        go so to n
      where
        go _ _ 0 = pure ()
        go i j c = do
          unsafeWriteM trg j =<< unsafeReadByOff src i
          go (i + 1) (j + 1) (c - 1)
        
        n1 = sizeOf src
        n2 = sizeOf trg

--------------------------------------------------------------------------------

{- MapM instance. -}

instance MonadVar m => MapM m (MArray# m e) Int e
  where
    newMap' def ascs =
      let bnds = rangeBounds (fsts ascs)
      in  fromAssocs' def bnds ascs
    
    {-# INLINE writeM' #-}
    writeM' = unsafeWriteM
    
    {-# INLINE unsafeReadMByKey #-}
    unsafeReadMByKey = unsafeReadByOff
    
    overwrite es ascs = uncurry (unsafeWriteM es) `mapM_` filter (indexIn es . fst) ascs
    
    kfoldrM = ofoldrM
    kfoldlM = ofoldlM

--------------------------------------------------------------------------------

{- IndexedM instance. -}

instance MonadVar m => IndexedM m (MArray# m e) Int e
  where
    fromAssocs' def bnds ascs = do
      es <- mreplicate (size bnds) def
      es <$ overwrite es ascs
    
    fromIndexed' es = do
      copy <- mreplicate (sizeOf es) (unreachEx "fromIndexed'")
      copy <$ ofoldr (\ i e go -> do unsafeWriteM copy i e; go) (pure ()) es
    
    fromIndexedM es = do
      copy <- flip mreplicate (unreachEx "fromIndexedM") =<< getSizeOf es
      copy <$ ofoldrM (\ i e _ -> unsafeWriteM copy i e) () es

--------------------------------------------------------------------------------

{- Freeze and Thaw instances. -}

instance MonadVar m => Thaw m (SArray# e) (MArray# m e)
  where
    thaw es = MArray# <$> mapM newVar es

instance Monad m => Freeze m (MArray# m e) (SArray# e)
  where
    freeze (MArray# es) = mapM readVar es

--------------------------------------------------------------------------------

unpack :: MArray# m e -> SArray# (Var m e)
unpack =  \ (MArray# es) -> es

overEx :: String -> a
overEx =  throw . IndexOverflow . showString "in SDP.Prim.TArray."

underEx :: String -> a
underEx =  throw . IndexUnderflow . showString "in SDP.Prim.TArray."

unreachEx :: String -> a
unreachEx =  throw . UnreachableException . showString "in SDP.Prim.TArray."



