{-# LANGUAGE Trustworthy, CPP, MagicHash, UnboxedTuples, BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE RoleAnnotations, GADTs, TypeFamilies #-}

{- |
    Module      :  SDP.Prim.SBytes
    Copyright   :  (c) Andrey Mulik 2019-2025
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Prim.SBytes" provides strict unboxed pseudo-primitive sized arrays.
-}
module SDP.Prim.SBytes
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.Unboxed,
  module SDP.SortM,
  module SDP.Sort,
  
  -- * Preudo-primitive types
  SBytes#, fromSBytes#, unpackSBytes#, offsetSBytes#, packSBytes#,
  
  -- ** Extra functions
  mapSBytes#, fmapSBytes#, unsafeCoerceSBytes#, hashSBytesWith#
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Prim.SBytes.ST
import SDP.IndexedM
import SDP.Unboxed
import SDP.SortM
import SDP.Sort
import SDP.Scan

import SDP.SortM.Tim

import qualified GHC.Exts as E
import GHC.Exts ( ByteArray#, (+#), (-#), unsafeFreezeByteArray# )

import GHC.Types
import GHC.ST ( ST (..) )

import Data.String

import Text.Read

import Unsafe.Coerce

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

{- |
  'SBytes#' is immutable pseudo-primitive 'Int'-indexed strict unboxed array
  type.
  
  'SBytes#' isn't real Haskell primitive (like "GHC.Exts" types) but for
  reliability and stability, I made it inaccessible to direct work.
-}
data SBytes# e
  where
    SBytes# :: Unboxed e
            => {-# UNPACK #-} !Int -- ^ Element count (not a real size)
            -> {-# UNPACK #-} !Int -- ^ Offset (in elements)
            -> !ByteArray#         -- ^ Real primitive byte array
            -> SBytes# e

type role SBytes# nominal

--------------------------------------------------------------------------------

{- Eq instance. -}

instance Eq (SBytes# e)
  where
    xs@(SBytes# c1@(I# c#) (I# o1#) xs#) == (SBytes# c2 (I# o2#) ys#) =
      c1 == c2 && peqUnboxed xs xs# o1# ys# o2# c#

--------------------------------------------------------------------------------

{- Ord and Ord1 instances. -}

instance Ord e => Ord (SBytes# e)
  where
    compare xs@(SBytes# c1 _ _) ys@(SBytes# c2 _ _) = cmp' 0
      where
        cmp' i = i == c ? c1 <=> c2 $ (xs!!i <=> ys!!i) <> cmp' (i + 1)
        c = min c1 c2

--------------------------------------------------------------------------------

{- Show and Read instances. -}

instance Show e => Show (SBytes# e)
  where
    showsPrec p = showsPrec p . toList

instance (Unboxed e, Read e) => Read (SBytes# e)
  where
    readPrec = fromList <$> readPrec

--------------------------------------------------------------------------------

{- Overloaded Lists and Strings support. -}

instance IsString (SBytes# Char) where fromString = fromList

instance Unboxed e => E.IsList (SBytes# e)
  where
    type Item (SBytes# e) = e
    
    fromListN = fromListN
    fromList  = fromList
    toList    = toList

--------------------------------------------------------------------------------

{- Semigroup and Monoid instances. -}

instance Semigroup (SBytes# e)
  where
    xs@(SBytes# (I# n1#) (I# o1#) arr1#) <> SBytes# (I# n2#) (I# o2#) arr2# =
      runST $ ST $ \ s1# -> case pconcat xs arr1# n1# o1# arr2# n2# o2# s1# of
        (# s2#, n#, marr# #) -> case unsafeFreezeByteArray# marr# s2# of
          (# s3#, arr# #) -> (# s3#, SBytes# (I# n#) 0 arr# #)

instance Monoid (SBytes# e)
  where
    mempty  = lzero
    mappend = (<>)

--------------------------------------------------------------------------------

instance Foldable SBytes#
  where
    foldr f base = \ arr@(SBytes# c _ _) ->
      let go i = c == i ? base $ f (arr !! i) (go (i + 1))
      in  go 0
    
    foldl f base = \ arr@(SBytes# c _ _) ->
      let go i = -1 == i ? base $ f (go (i - 1)) (arr !! i)
      in  go (c - 1)
    
    foldr' f base = \ arr@(SBytes# c _ _) ->
      let go i = c == i ? base $ f (arr !! i) (go (i + 1))
      in  go 0
    
    foldl' f base = \ arr@(SBytes# c _ _) ->
      let go i = -1 == i ? base $ f (go (i - 1)) (arr !! i)
      in  go (c - 1)
    
    null   = isNull
    length = sizeOf
    toList = foldr (:) []

--------------------------------------------------------------------------------

{- Nullable and NullableM instances. -}

instance Nullable (SBytes# e)
  where
    isNull es = case es of {SBytes# 0 _ _ -> True; _ -> False}
    lzero     = unsafeCoerce (SBytes# 0 0 (unwrap lzero#) :: SBytes# Word)

instance Monad m => NullableM m (SBytes# e)
  where
    newNull = pure lzero
    isNullM = pure . isNull

--------------------------------------------------------------------------------

{- Estimate and EstimateM instances. -}

instance Estimate (SBytes# e)
  where
    sizeHint (SBytes# c _ _) = Just (SizeHintEQ c)
    sizeOf   (SBytes# c _ _) = c
    
    (<==>) = on (<=>) sizeOf
    (<.=>) = (<=>) . sizeOf
    
    (.==.) = on (==) sizeOf
    (./=.) = on (/=) sizeOf
    (.<=.) = on (<=) sizeOf
    (.>=.) = on (>=) sizeOf
    (.>.)  = on (>)  sizeOf
    (.<.)  = on (<)  sizeOf
    
    (.==) = (==) . sizeOf
    (./=) = (/=) . sizeOf
    (.>=) = (>=) . sizeOf
    (.<=) = (<=) . sizeOf
    (.>)  = (>)  . sizeOf
    (.<)  = (<)  . sizeOf

instance Monad m => EstimateM m (SBytes# e)
  where
    getSizeHint (SBytes# c _ _) = pure $ Just (SizeHintEQ c)
    getSizeOf   (SBytes# c _ _) = pure c
    
    estimateMGE = pure ... (.>=.)
    estimateMLE = pure ... (.<=.)
    estimateMGT = pure ... (.>.)
    estimateMLT = pure ... (.<.)
    estimateMNE = pure ... (./=.)
    estimateMEQ = pure ... (.==.)
    
    notShorterThanM = pure ... (.>=)
    noLongerThanM   = pure ... (.<=)
    longerThanM     = pure ... (.>)
    shorterThanM    = pure ... (.<)
    otherLengthM    = pure ... (./=)
    hasLengthM      = pure ... (.==)
    
    (<<=>>) = pure ... (<==>)
    (<=>>)  = pure ... (<.=>)

--------------------------------------------------------------------------------

{- Bordered and BorderedM instances. -}

instance Bordered (SBytes# e) Int
  where
    lower                  _ = 0
    upper    (SBytes# c _ _) = c - 1
    bounds   (SBytes# c _ _) = (0, c - 1)
    indices  (SBytes# c _ _) = [0 .. c - 1]
    indexOf  (SBytes# c _ _) = index (0, c - 1)
    offsetOf (SBytes# c _ _) = offset (0, c - 1)
    indexIn  (SBytes# c _ _) = \ i -> i >= 0 && i < c
    
    eitherViewOf bnds@(l, _) es@(SBytes# c _ _)
        | isEmpty bnds = Right Z
        |    l /= 0    = Left  inapplicableEx
        |    n > c     = Left  expandEx
        |     True     = Right (take n es)
      where
        inapplicableEx = InapplicableBoundaries
                       . showString "in SDP.Bordered.eitherViewOf: lower border "
                       $ shows l " of list should be 0"
        
        expandEx = UnacceptableExpansion
                 . showString "in SDP.Bordered.eitherViewOf: new borders "
                 $ shows bnds " can't be wider than range of list values"
        
        n = size bnds

instance Monad m => BorderedM m (SBytes# e) Int
  where
    nowIndexIn (SBytes# c _ _) = pure . inRange (0, c - 1)
    getIndices (SBytes# c _ _) = pure [0 .. c - 1]
    getBounds  (SBytes# c _ _) = pure (0, c - 1)
    getUpper   (SBytes# c _ _) = pure (c - 1)
    getLower                 _ = pure 0
    
    getEitherViewOf = pure ... eitherViewOf

--------------------------------------------------------------------------------

{- Forceable instance. -}

instance Forceable (SBytes# e)
  where
    force es@(SBytes# n@(I# n#) (I# o#) bytes#) =
      SBytes# n 0 (cloneUnboxed1# es bytes# o# n#)

--------------------------------------------------------------------------------

{- Concat instance. -}
-- TODO: create buffer type and implement
instance Concat (SBytes# e)

--------------------------------------------------------------------------------

{- Forceable and Linear instances. -}

instance Sequence (SBytes# e) e
  where
    sfoldr = foldr
    sfoldl = foldl
    
    ofoldr f base = \ arr@(SBytes# c _ _) ->
      let go i = c == i ? base $ f i (arr !! i) (go $ i + 1)
      in  go 0
    
    ofoldl f base = \ arr@(SBytes# c _ _) ->
      let go i = -1 == i ? base $ f i (go $ i - 1) (arr !! i)
      in  go (c - 1)
    
    listL = toList
    listR = flip (:) `foldl` []

instance Unboxed e => Linear (SBytes# e) e
  where
    toHead e (SBytes# (I# c#) (I# o#) arr#) = let n# = c# +# 1# in runST $ ST $
      \ s1# -> case newUnboxed' e n# s1# of
        (# s2#, marr# #) -> case copyUnboxed# e arr# o# marr# 1# c# s2# of
          s3# -> case unsafeFreezeByteArray# marr# s3# of
            (# s4#, res# #) -> (# s4#, SBytes# (I# n#) 0 res# #)
    
    toLast (SBytes# (I# c#) (I# o#) arr#) e = let n# = c# +# 1# in runST $ ST $
      \ s1# -> case newUnboxed' e n# s1# of
        (# s2#, marr# #) -> case copyUnboxed# e arr# o# marr# 0# c# s2# of
          s3# -> case unsafeFreezeByteArray# marr# s3# of
            (# s4#, res# #) -> (# s4#, SBytes# (I# n#) 0 res# #)
    
    uncons'    (SBytes# 0 _    _) = Nothing
    uncons' es@(SBytes# n o arr#) = Just (es !! 0, SBytes# (n - 1) (o + 1) arr#)
    
    unsnoc'    (SBytes# 0 _    _) = Nothing
    unsnoc' es@(SBytes# n o arr#) = Just (SBytes# (n - 1) o arr#, es !! (n - 1))
    
    head es = null es ? undEx "head" $ es !! 0
    last es = null es ? undEx "last" $ es !! (sizeOf es - 1)
    
    init (SBytes# c o arr#) = c < 1 ? undEx "init" $ SBytes# (c - 1) o arr#
    tail (SBytes# c o arr#) = c < 1 ? undEx "tail" $ SBytes# (c - 1) (o + 1) arr#
    
    -- | O(1) 'take', O(1) memory.
    take n es@(SBytes# c o arr#)
      | n <= 0 = Z
      | n >= c = es
      |  True  = SBytes# n o arr#
    
    -- | O(1) 'drop', O(1) memory.
    drop n es@(SBytes# c o arr#)
      | n <= 0 = es
      | n >= c = Z
      |  True  = SBytes# (c - n) (o + n) arr#
    
    -- | O(1) 'split', O(1) memory.
    split n es@(SBytes# c o arr#)
      | n <= 0 = (Z, es)
      | n >= c = (es, Z)
      |  True  = (SBytes# n o arr#, SBytes# (c - n) (o + n) arr#)
    
    -- | O(1) 'keep', O(1) memory.
    keep n es@(SBytes# c o arr#)
      | n <= 0 = Z
      | n >= c = es
      |  True  = SBytes# n (o + c - n) arr#
    
    -- | O(1) 'sans', O(1) memory.
    sans n es@(SBytes# c o arr#)
      | n <= 0 = es
      | n >= c = Z
      |  True  = SBytes# (c - n) o arr#
    
    -- | O(1) 'divide', O(1) memory.
    divide n es@(SBytes# c o arr#)
      | n <= 0 = (es, Z)
      | n >= c = (Z, es)
      |  True  = (SBytes# (c - n) o arr#, SBytes# n (o + c - n) arr#)
    
    single = replicate 1
    
    fromList     es = runST $ newLinear     es >>= done
    fromListN  n es = runST $ newLinearN  n es >>= done
    fromFoldable es = runST $ fromFoldableM es >>= done
    replicate  n  e = runST $ mreplicate  n  e >>= done
    
    reverse es = runST $ do es' <- thaw es; reversed es'; done es'
    
    splitsBy f es = dropWhileEnd f <$> f *$ es `parts` es
    
    isPrefixOf sub line = sub == take (sizeOf sub) line
    isSuffixOf sub line = sub == keep (sizeOf sub) line
    
    pad (Left n@(I# n#)) e es@(SBytes# c@(I# c#) (I# o#) src#) = case c <=> n of
      EQ -> es
      GT -> take n es
      LT -> runST $ ST $ \ s1# -> case newUnboxed' e n# s1# of
        (# s2#, mbytes# #) -> case copyUnboxed# e src# o# mbytes# 0# c# s2# of
          s3# -> case unsafeFreezeByteArray# mbytes# s3# of
            (# s4#, bytes# #) -> (# s4#, SBytes# n 0 bytes# #)
    
    pad (Right n@(I# n#)) e es@(SBytes# c@(I# c#) (I# o#) src#) = case c <=> n of
      EQ -> es
      GT -> take n es
      LT -> runST $ ST $ \ s1# -> case newUnboxed' e n# s1# of
        (# s2#, mbytes# #) -> case copyUnboxed# e src# o# mbytes# (n# -# c#) c# s2# of
          s3# -> case unsafeFreezeByteArray# mbytes# s3# of
            (# s4#, bytes# #) -> (# s4#, SBytes# n 0 bytes# #)
    
    {-# INLINE (!!) #-}
    (!!) (SBytes# _ (I# o#) arr#) = \ (I# i#) -> arr# !# (i# +# o#)
    
    write es n e = not (indexIn es n) ? es $ runST $ do
      es' <- thaw es
      unsafeWriteM es' n e
      done es'
    
    remove n@(I# n#) es@(SBytes# c@(I# c#) (I# o#) arr#) = n < 0 || n >= c ? es $
      runST $ ST $ \ s1# -> case pnewUnboxed es (c# -# 1#) s1# of
        (# s2#, marr# #) -> case pcopyUnboxed es arr# o# marr# 0# n# s2# of
          s3# -> case pcopyUnboxed es arr# (o# +# n# +# 1#) marr# n# (c# -# n# -# 1#) s3# of
            s4# -> case unsafeFreezeByteArray# marr# s4# of
              (# s5#, res# #) -> (# s5#, SBytes# (c - 1) 0 res# #)

--------------------------------------------------------------------------------

{- Set and SetWith instances. -}

instance Ord e => Set (SBytes# e) e

instance SetWith (SBytes# e) e
  where
    setWith f es@(SBytes# _ _ _) = nubSorted f (sortBy f es)
    subsets   es@(SBytes# _ _ _) = subsequences (setWith compare es)
    
    groupSetWith cmp f es@(SBytes# _ _ _) = fromList $ groupSetWith cmp f (listL es)
    
    insertWith f e es@(SBytes# c@(I# c#) (I# o#) bytes#) = case g .$ es of
        Nothing -> es :< e
        
        Just n@(I# n#)
          | exists n -> es
          |  n >= c  -> es :< e
          |  n <= 0  -> e :> es
          |   True   -> runST $ ST $ \ s1# -> case newUnboxed e (c# +# 1#) s1# of
            (# s2#, mbytes# #) -> case copyUnboxed# e bytes# o# mbytes# 0# n# s2# of
              s3# -> case copyUnboxed# e bytes# (o# +# n#) mbytes# (n# +# 1#) (c# -# n#) s3# of
                s4# -> case unsafeFreezeByteArray# mbytes# s4# of
                  (# s5#, res# #) -> (# s5#, SBytes# (c + 1) 0 res# #)
      where
        exists n = e `f` (es!!n) == EQ
        
        g x = x `f` e /= LT
    
    deleteWith f e es@(SBytes# _ _ _) = memberWith f e es ?
      except (\ x -> f e x == EQ) es $ es
    
    {-# INLINE intersectionWith #-}
    intersectionWith f xs@(SBytes# n1 _ _) ys@(SBytes# n2 _ _) = fromList $ go 0 0
      where
        go i j = i == n1 || j == n2 ? [] $ case x `f` y of
            EQ -> x : go (i + 1) (j + 1)
            LT -> go (i + 1) j
            GT -> go i (j + 1)
          where
            x = xs !! i
            y = ys !! j
    
    {-# INLINE differenceWith #-}
    differenceWith f xs@(SBytes# n1 _ _) ys@(SBytes# n2 _ _) = fromList $ go 0 0
      where
        go i j
            | i == n1 = []
            | j == n2 = (xs !!) <$> [i .. n1 - 1]
            |  True   = case x `f` y of
              EQ -> go (i + 1) (j + 1)
              LT -> x : go (i + 1) j
              GT -> go i (j + 1)
          where
            x = xs !! i
            y = ys !! j
    
    {-# INLINE unionWith #-}
    unionWith f xs@(SBytes# n1 _ _) ys@(SBytes# n2 _ _) = fromList $ go 0 0
      where
        go i j
            | i == n1 = (ys !!) <$> [j .. n2 - 1]
            | j == n2 = (xs !!) <$> [i .. n1 - 1]
            |  True   = case x `f` y of
              EQ -> x : go (i + 1) (j + 1)
              LT -> x : go (i + 1) j
              GT -> y : go i (j + 1)
          where
            x = xs !! i
            y = ys !! j
    
    {-# INLINE symdiffWith #-}
    symdiffWith f xs@(SBytes# n1 _ _) ys@(SBytes# n2 _ _) = fromList $ symdiff' 0 0
      where
        symdiff' i j
            | i == n1 = (ys !!) <$> [j .. n2 - 1]
            | j == n2 = (xs !!) <$> [i .. n1 - 1]
            |  True   = case x `f` y of
              EQ -> symdiff' (i + 1) (j + 1)
              LT -> x : symdiff' (i + 1) j
              GT -> y : symdiff' i (j + 1)
          where
            x = xs !! i
            y = ys !! j
    
    memberWith _ _  Z = False
    memberWith f e es@(SBytes# _ _ _)
        | x :> _ <- es,
          LT <- f e x
        = False
        
        | _ :< x <- es,
          GT <- f e x
        = False
        
        | True
        = contain 0 (sizeOf es - 1)
      where
        contain l u = not (l > u) && case f e (es !! j) of
            LT -> contain l (j - 1)
            GT -> contain (j + 1) u
            EQ -> True
          where
            j = u - l `div` 2 + l
    
    lookupLTWith f o es@(SBytes# _ _ _)
        |     isNull es     = Nothing
        | GT <- o `f` last' = Just last'
        | GT <- o `f` head' = look' head' 0 (sizeOf es - 1)
        |       True        = Nothing
      where
        head' = unsafeReadByKey es 0
        last' = unsafeReadByKey es u'
        u' = upper es
        
        look' r l u = l > u ? Just r $ case o `f` e of
            LT -> look' r l (j - 1)
            EQ -> Just $ j < 1 ? r $ es !! (j - 1)
            GT -> look' e (j + 1) u
          where
            j = l + (u - l) `div` 2
            e = es !! j
    
    lookupLEWith f o es@(SBytes# _ _ _)
        |     isNull es     = Nothing
        | GT <- o `f` last' = Just last'
        | LT <- o `f` head' = Nothing
        |       True        = look' head' 0 (sizeOf es - 1)
      where
        head' = unsafeReadByKey es 0
        last' = unsafeReadByKey es u'
        u' = upper es
        
        look' r l u = l > u ? Just r $ case o `f` e of
            LT -> look' r l (j - 1)
            _  -> look' e (j + 1) u
          where
            j = l + (u - l) `div` 2
            e = es !! j
    
    lookupGTWith f o es@(SBytes# _ _ _)
        |     isNull es     = Nothing
        | LT <- o `f` head' = Just head'
        | LT <- o `f` last' = look' last' 0 (sizeOf es - 1)
        |       True        = Nothing
      where
        head' = unsafeReadByKey es 0
        last' = unsafeReadByKey es u'
        u' = upper es
        
        look' r l u = l > u ? Just r $ case o `f` e of
            LT -> look' e l (j - 1)
            EQ -> j >= (sizeOf es - 1) ? Nothing $ Just (es !! (j + 1))
            GT -> look' r (j + 1) u
          where
            j = l + (u - l) `div` 2
            e = es !! j
    
    lookupGEWith f o es@(SBytes# _ _ _)
        |     isNull es     = Nothing
        | GT <- o `f` last' = Nothing
        | GT <- o `f` head' = look' last' 0 (sizeOf es - 1)
        |       True        = Just head'
      where
        head' = unsafeReadByKey es 0
        last' = unsafeReadByKey es u'
        u' = upper es
        
        look' r l u = l > u ? Just r $ case o `f` e of
            LT -> look' e l (j - 1)
            EQ -> Just e
            GT -> look' r (j + 1) u
          where
            j = l + (u - l) `div` 2
            e = es !! j
    
    isSubsetWith f xs ys@(SBytes# _ _ _) = sfoldr
      (\ x b -> b && memberWith f x ys) True xs

--------------------------------------------------------------------------------

{- Scan and Sort instances. -}

instance Unboxed e => Scan (SBytes# e) e

instance Sort (SBytes# e) e
  where
    sortBy cmp es@(SBytes# _ _ _) = runST $ do
      es' <- thaw es
      timSortBy cmp es'
      done es'
    
    sortedBy f es@(SBytes# n _ _) =
      let go i = let i1 = i + 1 in i1 == n || (f (es !! i) (es !! i1) && go i1)
      in  n < 2 || go 0

--------------------------------------------------------------------------------

{- Map and Indexed instances. -}

instance Unboxed e => Map (SBytes# e) Int e
  where
    toMap ascs = runST $ newMap ascs >>= done
    
    Z  // ascs = toMap ascs
    es // ascs = runST $ do
      es' <- thaw es
      overwrite es' ascs
      done es'
    
    (*$) p = ofoldr (\ i e is -> p e ? (i : is) $ is) []
    
    unsafeReadByKey = (!!)
    
    kfoldr = ofoldr
    kfoldl = ofoldl

instance Unboxed e => Indexed (SBytes# e) Int e
  where
    assoc' e bnds ascs = runST $ fromAssocs' bnds e ascs >>= done
    
    fromIndexed es = runST $ do
      let n = sizeOf es
      copy <- mreplicate n filler
      updateM copy (\ i _ -> es!!i)
      done copy

--------------------------------------------------------------------------------

{- Freeze and Thaw instances. -}

instance Thaw (ST s) (SBytes# e) (STBytes# s e)
  where
    thaw es@(SBytes# c@(I# c#) (I# o#) bytes#) = ST $
      \ s1# -> case pthawUnboxed es bytes# c# o# s1# of
        (# s2#, mbytes# #) -> (# s2#, packSTBytes# c 0 mbytes# #)
    
#if MIN_VERSION_ghc-prim(0,12,0)
    unsafeThaw es@(SBytes# c o bytes#) = ST $
      \ s1# -> case unsafeThawByteArray# bytes# of
        (# s2#, mbytes# #) -> (# s2#, STBytes# c o mbytes# #)
#else
    unsafeThaw = thaw
#endif

instance Unboxed e => Freeze (ST s) (STBytes# s e) (SBytes# e)
  where
    freeze es = ST $ \ s1# -> case pcloneUnboxedM es (unpackSTBytes# es) o# n# s1# of
        (# s2#, mbytes# #) -> case unsafeFreezeUnboxed# mbytes# s2# of
          (# s3#, bytes# #) -> (# s3#, SBytes# n (I# o#) bytes# #)
      where
        !n@(I# n#) = sizeOf es
        
        o# = offsetSTBytes# es
    
    unsafeFreeze es = ST $ \ s1# -> case unsafeFreezeUnboxed# (unpackSTBytes# es) s1# of
        (# s2#, bytes# #) -> (# s2#, SBytes# n (I# o#) bytes# #)
      where
        o# = offsetSTBytes# es
        n  = sizeOf es

--------------------------------------------------------------------------------

-- | 'unpackSBytes#' returns 'ByteArray#' field of 'SBytes#'.
unpackSBytes# :: SBytes# e -> ByteArray#
unpackSBytes# = \ (SBytes# _ _ marr#) -> marr#

-- | 'offsetSBytes#' returns 'SBytes#' offset in elements.
offsetSBytes# :: SBytes# e -> Int
offsetSBytes# =  \ (SBytes# _ o _) -> o

-- | 'packSBytes#' creates new 'SBytes#' from sized 'ByteArray#'.
packSBytes# :: Unboxed e => Int -> ByteArray# -> SBytes# e
packSBytes# n marr# = SBytes# (max 0 n) 0 marr#

-- | 'fromSBytes#' returns new 'ByteArray#'.
fromSBytes# :: SBytes# e -> ByteArray#
fromSBytes# es@(SBytes# c@(I# c#) o@(I# o#) src#) = unpackSBytes# res
  where
    res = runST $ ST $ \ s1# -> case pnewUnboxed es c# s1# of
      (# s2#, mcopy# #) -> case pcopyUnboxed es src# o# mcopy# 0# c# s2# of
        s3# -> case unsafeFreezeByteArray# mcopy# s3# of
          (# s4#, copy# #) -> (# s4#, SBytes# c o copy# `asTypeOf` es #)

{- |
  'unsafeCoerceSBytes#' is unsafe low-lowel coerce of an array with recounting
  the number of elements and offset (with possible rounding).
-}
unsafeCoerceSBytes# :: Unboxed b => SBytes# a -> SBytes# b
unsafeCoerceSBytes# pa@(SBytes# n o arr#) = pb
  where
    pb = SBytes# ((n * s1) `div` s2) ((o * s1) `div` s2) arr#
    s1 = psizeof pa n; s2 = psizeof pb n

-- | Calculate hash 'SBytes#' using 'hashUnboxedWith'.
hashSBytesWith# :: Int -> SBytes# e -> Int
hashSBytesWith# (I# salt#) es@(SBytes# (I# c#) (I# o#) bytes#) =
  I# (hashUnboxedWith (toProxy## (fromProxy es)) c# o# bytes# salt#)

{- |
  @since 0.3
  
  'fmap' for 'SBytes#'.
-}
fmapSBytes# :: Unboxed e' => (e -> e') -> SBytes# e -> SBytes# e'
fmapSBytes# f es@(SBytes# n@(I# nx#) (I# o#) xs#) = runST $ ST $
  \ s1# -> case pnewUnboxed es nx# s1# of
    (# s2#, marr# #) ->
      let
          go# _  0# = \ s# -> s#
          go# i# n# =
            \ s# -> case writeUnboxed# marr# i# (f (xs# !# i#)) s# of
              s'# -> go# (i# +# 1#) (n# -# 1#) s'#
      in  case go# o# nx# s2# of
            s3# -> case unsafeFreezeByteArray# marr# s3# of
              (# s4#, arr# #) -> (# s4#, SBytes# n 0 arr# #)

{- |
  @since 0.3
  
  See 'fmapSBytes#'.
-}
mapSBytes# :: (e -> e) -> SBytes# e -> SBytes# e
mapSBytes# f es@(SBytes# _ _ _) = fmapSBytes# f es

--------------------------------------------------------------------------------

{-# INLINE done #-}
done :: Unboxed e => STBytes# s e -> ST s (SBytes# e)
done =  unsafeFreeze

--------------------------------------------------------------------------------

{-# INLINE nubSorted #-}
nubSorted :: Compare e -> SBytes# e -> SBytes# e
nubSorted f es@(SBytes# _ _ _) = case unsnoc' es of
  Just (xs, x) -> fromList . toList $ sfoldr (\ e (l :| ls) ->
      f e l == EQ ? l :| ls $ e :| l : ls
    ) (x :| []) xs
  _            -> es

--------------------------------------------------------------------------------

undEx :: String -> a
undEx =  throw . UndefinedValue . showString "in SDP.Prim.SBytes."




