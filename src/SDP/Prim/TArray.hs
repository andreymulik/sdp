{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, GADTs #-}
{-# LANGUAGE Trustworthy, MagicHash, PatternSynonyms, UndecidableInstances #-}

{- |
    Module      :  SDP.Prim.TArray
    Copyright   :  (c) Andrey Mulik 2020-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable
    
    "SDP.Prim.TArray" provides lazy boxed array of @stm@ 'TVar's.
    Note that 'TArray#' stores each element in 'TVar'.
-}
module SDP.Prim.TArray
(
  -- TArray
  MArray# (.., TArray#), TArray#, STM, TVar
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Prim.SArray hiding ( set )

import Data.Field

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
    lzero  = MArray# Z

instance MonadVar m => NullableM m (MArray# m e)
  where
    newNull = return Z
    nowNull = return . isNull

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
    getSizeOf = return . sizeOf
    
    estimateMLT = return ... (.<.)
    estimateMGT = return ... (.>.)
    estimateMLE = return ... (.<=.)
    estimateMGE = return ... (.>=.)
    estimateM   = return ... (<==>)
    
    lestimateMLT = return ... (.<)
    lestimateMGT = return ... (.>)
    lestimateMLE = return ... (.<=)
    lestimateMGE = return ... (.>=)
    lestimateM   = return ... (<.=>)

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
    
    viewOf bnds (MArray# es) = MArray# (viewOf bnds es)

instance MonadVar m => BorderedM m (MArray# m e) Int
  where
    getIndexOf = return ... indexOf
    getIndices = return . indices
    getBounds  = return . bounds
    getUpper   = return . upper
    getLower _ = return 0
    
    getViewOf = takeM . size

--------------------------------------------------------------------------------

{- ForceableM and LinearM instances. -}

instance MonadVar m => ForceableM m (MArray# m e)
  where
    copied (MArray# arr) = MArray# <$> otraverse (const $ var <=< get this') arr

instance MonadVar m => LinearM m (MArray# m e) e
  where
    getHead = get this . head . unpack
    getLast = get this . last . unpack
    singleM = fmap (MArray# . single) . var
    
    prepend e es = MArray# . (:> unpack es) <$> var e
    append  es e = MArray# . (unpack es :<) <$> var e
    
    newLinear     = fmap (MArray# . fromList) . mapM var
    newLinearN  n = fmap (MArray# . fromListN n) . mapM var
    fromFoldableM = fmap (MArray# . fromList) . foldr (liftA2 (:) . var) (return [])
    
    (!#>)  = get this ... (!^) . unpack
    writeM = writeM'
    
    getLeft  = mapM (get this) . listL . unpack
    getRight = mapM (get this) . listR . unpack
    merged   = return . MArray# . concatMap unpack
    reversed = return . MArray# . reverse . unpack
    filled n = fmap (MArray# . fromList) . replicateM n . var
    
    copyTo src so trg to n = when (n > 0) $ do
        when      (so < 0 || to < 0)      $ underEx "copyTo"
        when (so + n > n1 || to + n > n2) $ overEx  "copyTo"
        go so to n
      where
        go _ _ 0 = return ()
        go i j c = do e <- src !#> i; writeM trg j e; go (i + 1) (j + 1) (c - 1)
        
        n1 = sizeOf src
        n2 = sizeOf trg
    
    ofoldlM f base = ofoldl (\ i es -> ($ f i) . (es >>=<<) . get this) (return base) . unpack
    ofoldrM f base = ofoldr (\ i -> ($ f i) ... (>>=<<) . get this) (return base) . unpack
    
    foldlM f base = foldl (\ es -> ($ f) . (es >>=<<) . get this) (return base) . unpack
    foldrM f base = foldr (($ f) ... (>>=<<) . get this) (return base) . unpack
    
    takeM n = return . MArray# . take n . unpack
    dropM n = return . MArray# . drop n . unpack
    keepM n = return . MArray# . keep n . unpack
    sansM n = return . MArray# . sans n . unpack
    
    prefixM p es =
      let
          go i = i >= c ? return c $ do e <- es !#> i; p e ? go (succ 1) $ return i
          c = sizeOf es
      in  go 0
    
    suffixM p es =
      let
          go i = i < 0 ? return c $ do e <- es !#> i; p e ? go (pred i) $ return (c - i - 1)
          c = sizeOf es
      in  go (c - 1)
    
    mprefix p es =
      let
          go i = i >= c ? return c $ do e <- es !#> i; p e ?^ go (succ 1) $ return i
          c = sizeOf es
      in  go 0
    
    msuffix p es =
      let
          go i = i < 0 ? return c $ do e <- es !#> i; p e ?^ go (pred i) $ return (c - i - 1)
          c = sizeOf es
      in  go (c - 1)

--------------------------------------------------------------------------------

{- MapM instance. -}

instance MonadVar m => MapM m (MArray# m e) Int e
  where
    newMap' defvalue ascs =
      let bnds = rangeBounds (fsts ascs)
      in  fromAssocs' bnds defvalue ascs
    
    {-# INLINE writeM' #-}
    writeM' = setField this ... (!^) . unpack
    
    (>!) = (!#>)
    
    overwrite es ascs = uncurry (writeM es) `mapM_` (filter (indexIn es . fst) ascs)
    
    kfoldrM = ofoldrM
    kfoldlM = ofoldlM

--------------------------------------------------------------------------------

{- IndexedM instance. -}

instance MonadVar m => IndexedM m (MArray# m e) Int e
  where
    fromAssocs' bnds defvalue ascs = do
      es <- filled (size bnds) defvalue
      overwrite es ascs
      return es
    
    fromIndexed' es = do
      copy <- filled (sizeOf es) (unreachEx "fromIndexed'")
      copy <$ ofoldr (\ i e go -> do writeM copy i e; go) (return ()) es
    
    fromIndexedM es = do
      copy <- flip filled (unreachEx "fromIndexedM") =<< getSizeOf es
      copy <$ ofoldrM (\ i e _ -> writeM copy i e) () es

--------------------------------------------------------------------------------

{- Freeze and Thaw instances. -}

instance MonadVar m => Thaw m (SArray# e) (MArray# m e)
  where
    thaw es = MArray# <$> mapM var es

instance Monad m => Freeze m (MArray# m e) (SArray# e)
  where
    freeze (MArray# es) = get this `mapM` es

--------------------------------------------------------------------------------

unpack :: MArray# m e -> SArray# (Var m e)
unpack =  \ (MArray# es) -> es

overEx :: String -> a
overEx =  throw . IndexOverflow . showString "in SDP.Prim.TArray."

underEx :: String -> a
underEx =  throw . IndexUnderflow . showString "in SDP.Prim.TArray."

unreachEx :: String -> a
unreachEx =  throw . UnreachableException . showString "in SDP.Prim.TArray."

