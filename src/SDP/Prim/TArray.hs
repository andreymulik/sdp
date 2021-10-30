{-# LANGUAGE Trustworthy, MagicHash, MultiParamTypeClasses, FlexibleInstances #-}

{- |
    Module      :  SDP.Prim.TArray
    Copyright   :  (c) Andrey Mulik 2020-2021
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable
    
    "SDP.Prim.TArray" provides lazy boxed array of @stm@ 'TVar's.
    Note that 'TArray#' stores each element in 'TVar'.
-}
module SDP.Prim.TArray
(
  -- TArray
  TArray# (..), STM, TVar
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Prim.SArray

import GHC.Conc

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

-- | 'SArray#' of @stm@ 'TVar's.
newtype TArray# e = TArray# (SArray# (TVar e)) deriving ( Eq )

--------------------------------------------------------------------------------

{- Nullable, Estimate and Bordered instances. -}

instance Nullable (TArray# e)
  where
    isNull = \ (TArray# es) -> isNull es
    lzero  = TArray# Z

instance NullableM STM (TArray# e)
  where
    newNull = return (TArray# Z)
    nowNull = return . isNull . unpack

instance Estimate (TArray# e)
  where
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

instance Bordered (TArray# e) Int
  where
    lower _ = 0
    
    upper    (TArray# arr) = upper arr
    sizeOf   (TArray# arr) = sizeOf arr
    bounds   (TArray# arr) = (0, upper arr)
    indices  (TArray# arr) = [0 .. upper arr]
    indexOf  (TArray# arr) = index (0, upper arr)
    offsetOf (TArray# arr) = offset (0, upper arr)
    indexIn  (TArray# arr) = \ i -> i >= 0 && i < sizeOf arr

--------------------------------------------------------------------------------

{- BorderedM and LinearM instances. -}

instance BorderedM STM (TArray# e) Int
  where
    getIndexOf = return ... indexOf
    getIndices = return . indices
    getBounds  = return . bounds
    getSizeOf  = return . sizeOf
    getUpper   = return . upper
    getLower _ = return 0

instance LinearM STM (TArray# e) e
  where
    getHead = readTVar . head . unpack
    getLast = readTVar . last . unpack
    singleM = fmap (TArray# . single) . newTVar
    
    prepend e es = TArray# . (:> unpack es) <$> newTVar e
    append  es e = TArray# . (unpack es :<) <$> newTVar e
    
    newLinear     = fmap (TArray# . fromList) . mapM newTVar
    newLinearN  n = fmap (TArray# . fromListN n) . mapM newTVar
    fromFoldableM = fmap (TArray# . fromList) . foldr (liftA2 (:) . newTVar) (return [])
    
    (!#>)  = readTVar ... (!^) . unpack
    writeM = writeM'
    
    getLeft  = mapM readTVar . listL . unpack
    getRight = mapM readTVar . listR . unpack
    merged   = return . TArray# . concatMap unpack
    reversed = return . TArray# . reverse . unpack
    filled n = fmap (TArray# . fromList) . replicateM n . newTVar
    
    copyTo src so trg to n = when (n > 0) $ do
        when      (so < 0 || to < 0)      $ underEx "copyTo"
        when (so + n > n1 || to + n > n2) $ overEx  "copyTo"
        go so to n
      where
        go _ _ 0 = return ()
        go i j c = do e <- src !#> i; writeM trg j e; go (i + 1) (j + 1) (c - 1)
        
        n1 = sizeOf src
        n2 = sizeOf trg
    
    ofoldlM f base = ofoldl (\ i es -> ($ f i) . (es >>=<<) . readTVar) (return base) . unpack
    ofoldrM f base = ofoldr (\ i -> ($ f i) ... (>>=<<) . readTVar) (return base) . unpack
    
    foldlM f base = foldl (\ es -> ($ f) . (es >>=<<) . readTVar) (return base) . unpack
    foldrM f base = foldr (($ f) ... (>>=<<) . readTVar) (return base) . unpack
    
    takeM n = return . TArray# . take n . unpack
    dropM n = return . TArray# . drop n . unpack
    keepM n = return . TArray# . keep n . unpack
    sansM n = return . TArray# . sans n . unpack
    
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

{- MapM and IndexedM instances. -}

instance MapM STM (TArray# e) Int e
  where
    newMap' defvalue ascs = fromAssocs' (ascsBounds ascs) defvalue ascs
    
    {-# INLINE writeM' #-}
    writeM' = writeTVar ... (!^) . unpack
    
    (>!) = (!#>)
    
    overwrite es ascs = do
      mapM_ (uncurry $ writeM es) (filter (indexIn es . fst) ascs)
      return es
    
    kfoldrM = ofoldrM
    kfoldlM = ofoldlM

instance IndexedM STM (TArray# e) Int e
  where
    fromAssocs' bnds defvalue ascs = size bnds `filled` defvalue >>= (`overwrite` ascs)
    
    fromIndexed' es = do
      let n = sizeOf es
      copy <- filled n (unreachEx "fromIndexed'")
      forM_ [0 .. n - 1] $ \ i -> writeM copy i (es !^ i)
      return copy
    
    fromIndexedM es = do
      n    <- getSizeOf es
      copy <- filled n (unreachEx "fromIndexedM")
      forM_ [0 .. n - 1] $ \ i -> es !#> i >>= writeM copy i
      return copy

--------------------------------------------------------------------------------

{- Freeze and Thaw instances. -}

instance Thaw STM (SArray# e) (TArray# e) where thaw = fmap TArray# . mapM newTVar

instance Freeze STM (TArray# e) (SArray# e) where freeze = mapM readTVar . unpack

--------------------------------------------------------------------------------

ascsBounds :: (Ord a) => [(a, b)] -> (a, a)
ascsBounds =  \ ((x, _) : xs) -> foldr (\ (e, _) (mn, mx) -> (min mn e, max mx e)) (x, x) xs

unpack :: TArray# e -> SArray# (TVar e)
unpack =  \ (TArray# arr) -> arr

overEx :: String -> a
overEx =  throw . IndexOverflow . showString "in SDP.Prim.TArray."

underEx :: String -> a
underEx =  throw . IndexUnderflow . showString "in SDP.Prim.TArray."

unreachEx :: String -> a
unreachEx =  throw . UnreachableException . showString "in SDP.Prim.TArray."

