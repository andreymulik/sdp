{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, GADTs #-}
{-# LANGUAGE Safe, MagicHash, PatternSynonyms, UndecidableInstances, DataKinds #-}

{- |
    Module      :  SDP.Prim.TArray
    Copyright   :  (c) Andrey Mulik 2020-2023
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

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

-- | 'SArray#' of @stm@ 'TVar's.
type TArray# = MArray# STM TVar

{-# COMPLETE TArray# #-}
-- | Legacy pattern synonym for @sdp-0.2@ compatibility.
pattern TArray# :: SArray# (TVar e) -> TArray# e
pattern TArray# es = MArray# es

--------------------------------------------------------------------------------

-- | Generalized array of variables.
data MArray# m var e
  where
    MArray# :: IsMVar m var => {-# UNPACK #-} !(SArray# (var e)) -> MArray# m var e

--------------------------------------------------------------------------------

{- Eq instance. -}

instance Eq (var e) => Eq (MArray# m var e)
  where
    MArray# xs == MArray# ys = xs == ys

--------------------------------------------------------------------------------

{- Nullable and NullableM instances. -}

instance IsMVar m var => Nullable (MArray# m var e)
  where
    isNull = \ (MArray# es) -> isNull es
    lzero  = MArray# Z

instance IsMVar m var => NullableM m (MArray# m var e)
  where
    newNull = return Z
    nowNull = return . isNull

--------------------------------------------------------------------------------

{- Estimate and EstimateM instances. -}

instance Estimate (MArray# m var e)
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

instance Monad m => EstimateM m (MArray# m var e)
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

instance Bordered (MArray# m var e) Int
  where
    lower _ = 0
    upper   = upper . unpack
    indexIn = \ es i -> i >= 0 && i < sizeOf (unpack es)
    
    bounds   (MArray# es) = (0, upper es)
    indices  (MArray# es) = [0 .. upper es]
    indexOf  (MArray# es) = index (0, upper es)
    offsetOf (MArray# es) = offset (0, upper es)
    
    viewOf bnds (MArray# es) = MArray# (viewOf bnds es)

instance (Attribute "set" "" m (var e) e, IsMVar m var)
      => BorderedM m (MArray# m var e) Int
  where
    getIndexOf = return ... indexOf
    getIndices = return . indices
    getBounds  = return . bounds
    getUpper   = return . upper
    getLower _ = return 0
    
    getViewOf = takeM . size

--------------------------------------------------------------------------------

{- ForceableM and LinearM instances. -}

instance IsMVar m var => ForceableM m (MArray# m var e)
  where
    copied (MArray# arr) = MArray# <$> otraverse (const $ var <=< fromMRef) arr

instance (Attribute "set" "" m (var e) e, IsMVar m var)
      => LinearM m (MArray# m var e) e
  where
    getHead = fromMRef . head . unpack
    getLast = fromMRef . last . unpack
    singleM = fmap (MArray# . single) . var
    
    prepend e es = MArray# . (:> unpack es) <$> var e
    append  es e = MArray# . (unpack es :<) <$> var e
    
    newLinear     = fmap (MArray# . fromList) . mapM var
    newLinearN  n = fmap (MArray# . fromListN n) . mapM var
    fromFoldableM = fmap (MArray# . fromList) . foldr (liftA2 (:) . var) (return [])
    
    (!#>)  = fromMRef ... (!^) . unpack
    writeM = writeM'
    
    getLeft  = mapM fromMRef . listL . unpack
    getRight = mapM fromMRef . listR . unpack
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
    
    ofoldlM f base = ofoldl (\ i es -> ($ f i) . (es >>=<<) . fromMRef) (return base) . unpack
    ofoldrM f base = ofoldr (\ i -> ($ f i) ... (>>=<<) . fromMRef) (return base) . unpack
    
    foldlM f base = foldl (\ es -> ($ f) . (es >>=<<) . fromMRef) (return base) . unpack
    foldrM f base = foldr (($ f) ... (>>=<<) . fromMRef) (return base) . unpack
    
    takeM n = return . MArray# . take n . unpack
    dropM n = return . MArray# . drop n . unpack
    keepM n = return . MArray# . keep n . unpack
    sansM n = return . MArray# . sans n . unpack
    
    prefixM p es =
      let go i = i >= c ? return c $ do e <- es !#> i; p e ? go (succ 1) $ return i
          c = sizeOf es
      in  go 0
    
    suffixM p es =
      let go i = i < 0 ? return c $ do e <- es !#> i; p e ? go (pred i) $ return (c - i - 1)
          c = sizeOf es
      in  go (c - 1)
    
    mprefix p es =
      let go i = i >= c ? return c $ do e <- es !#> i; p e ?^ go (succ 1) $ return i
          c = sizeOf es
      in  go 0
    
    msuffix p es =
      let go i = i < 0 ? return c $ do e <- es !#> i; p e ?^ go (pred i) $ return (c - i - 1)
          c = sizeOf es
      in  go (c - 1)

--------------------------------------------------------------------------------

{- MapM instance. -}

instance (Attribute "set" "" m (var e) e, IsMVar m var)
      => MapM m (MArray# m var e) Int e
  where
    newMap' defvalue ascs =
      let bnds = rangeBounds (fsts ascs)
      in  fromAssocs' bnds defvalue ascs
    
    {-# INLINE writeM' #-}
    writeM' (MArray# es) key e = accessSet attribute (es !^ key) e
    
    (>!) = (!#>)
    
    overwrite es ascs = uncurry (writeM es) `mapM_` (filter (indexIn es . fst) ascs)
    
    kfoldrM = ofoldrM
    kfoldlM = ofoldlM

--------------------------------------------------------------------------------

{- IndexedM instance. -}

instance (Attribute "set" "" m (var e) e, IsMVar m var)
      => IndexedM m (MArray# m var e) Int e
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

instance IsMVar m var => Thaw m (SArray# e) (MArray# m var e)
  where
    thaw es = MArray# <$> mapM var es

instance Monad m => Freeze m (MArray# m var e) (SArray# e)
  where
    freeze (MArray# es) = fromMRef `mapM` es

--------------------------------------------------------------------------------

unpack :: MArray# m var e -> SArray# (var e)
unpack =  \ (MArray# es) -> es

--------------------------------------------------------------------------------

{-# NOINLINE overEx #-}
overEx :: String -> a
overEx =  throw . IndexOverflow . showString "in SDP.Prim.TArray."

{-# NOINLINE underEx #-}
underEx :: String -> a
underEx =  throw . IndexUnderflow . showString "in SDP.Prim.TArray."

{-# NOINLINE unreachEx #-}
unreachEx :: String -> a
unreachEx =  throw . UnreachableException . showString "in SDP.Prim.TArray."



