{-# LANGUAGE Trustworthy, MagicHash, UnboxedTuples, BangPatterns, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, RoleAnnotations #-}

{- |
    Module      :  SDP.Prim.SBytes
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Prim.SBytes" provides strict unboxed pseudo-primitive arrays.
-}
module SDP.Prim.SBytes
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.Unboxed,
  module SDP.SortM,
  module SDP.Sort,
  
  -- * Preudo-primitive types
  MIOBytes# (..), IOBytes#, STBytes#, SBytes#,
  
  -- ** Unpack unboxed arrays
  fromSBytes#,  packSBytes#,  unpackSBytes#,  offsetSBytes#,
  fromSTBytes#, packSTBytes#, unpackSTBytes#, offsetSTBytes#,
  
  -- ** Coerce unboxed arrays
  unsafeCoerceSBytes#, unsafeCoerceSTBytes#, unsafeCoerceMIOBytes#,
  
  -- ** Unsafe pointer conversions
  unsafeSBytesToPtr#, unsafePtrToSBytes#,
  
  -- ** Hash
  hashSBytesWith#
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM
import SDP.Unboxed
import SDP.SortM
import SDP.Sort
import SDP.Scan

import SDP.SortM.Tim

import qualified GHC.Exts as E
import GHC.Exts
  (
    ByteArray#, MutableByteArray#, State#, Int#, (+#), (-#),
    newByteArray#, unsafeFreezeByteArray#, sameMutableByteArray#
  )

import GHC.Types
import GHC.ST ( ST (..) )

import Data.Default.Class
import Data.Typeable
import Data.Coerce
import Data.String
import Text.Read

import Foreign
  (
    Ptr, Storable, peekByteOff, peekElemOff, pokeByteOff, pokeElemOff,
    mallocBytes, callocArray
  )

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

{- |
  'SBytes#' is immutable pseudo-primitive 'Int'-indexed strict unboxed array
  type.
  
  'SBytes#' isn't real Haskell primitive (like "GHC.Exts" types) but for
  reliability and stability, I made it inaccessible to direct work.
-}
data SBytes# e = SBytes#
          {-# UNPACK #-} !Int -- ^ Element count (not a real size)
          {-# UNPACK #-} !Int -- ^ Offset (in elements)
          !ByteArray#         -- ^ Real primitive byte array
  deriving ( Typeable )

type role SBytes# representational

--------------------------------------------------------------------------------

{- Eq instance. -}

instance Unboxed e => Eq (SBytes# e)
  where
    xs@(SBytes# c1@(I# c#) (I# o1#) xs#) == (SBytes# c2 (I# o2#) ys#) =
      c1 == c2 && peqUnboxed xs xs# o1# ys# o2# c#

--------------------------------------------------------------------------------

{- Ord and Ord1 instances. -}

instance (Unboxed e, Ord e) => Ord (SBytes# e)
  where
    compare xs@(SBytes# c1 _ _) ys@(SBytes# c2 _ _) = cmp' 0
      where
        cmp' i = i == c ? c1 <=> c2 $ (xs!^i <=> ys!^i) <> cmp' (i + 1)
        c = min c1 c2

--------------------------------------------------------------------------------

{- Show and Read instances. -}

instance (Unboxed e, Show e) => Show (SBytes# e)
  where
    showsPrec p = showsPrec p . listL

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
    toList    = listL

--------------------------------------------------------------------------------

{- Semigroup, Monoid and Default instances. -}

instance Default (SBytes# e) where def = SBytes# 0 0 (unwrap lzero#)

instance Unboxed e => Monoid    (SBytes# e) where mempty = Z; mappend = (<>)
instance Unboxed e => Semigroup (SBytes# e)
  where
    xs@(SBytes# (I# n1#) (I# o1#) arr1#) <> SBytes# (I# n2#) (I# o2#) arr2# =
      runST $ ST $ \ s1# -> case pconcat xs arr1# n1# o1# arr2# n2# o2# s1# of
        (# s2#, n#, marr# #) -> case unsafeFreezeByteArray# marr# s2# of
          (# s3#, arr# #) -> (# s3#, SBytes# (I# n#) 0 arr# #)

--------------------------------------------------------------------------------

{- Estimate and EstimateM instances. -}

instance Estimate (SBytes# e)
  where
    sizeOf (SBytes# c _ _) = c
    
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

instance Monad m => EstimateM m (SBytes# e)
  where
    getSizeOf (SBytes# c _ _) = return c
    
    estimateMLT = return ... (.<.)
    estimateMGT = return ... (.>.)
    estimateMLE = return ... (.<=.)
    estimateMGE = return ... (.>=.)
    estimateM   = return ... (<==>)
    
    lestimateMLT' = return ... (.<)
    lestimateMGT' = return ... (.>)
    lestimateMLE' = return ... (.<=)
    lestimateMGE' = return ... (.>=)
    lestimateM'   = return ... (<.=>)

--------------------------------------------------------------------------------

{- Nullable and NullableM instances. -}

instance Nullable (SBytes# e)
  where
    isNull es = case es of {(SBytes# 0 _ _) -> True; _ -> False}
    lzero     = def

instance Monad m => NullableM m (SBytes# e)
  where
    newNull = return lzero
    nowNull = return . isNull

--------------------------------------------------------------------------------

{- Bordered and BorderedM instances. -}

instance Bordered (SBytes# e) Int
  where
    lower = const 0
    
    rebound bnds es@(SBytes# c o bytes#)
        | n < 0 = Z
        | n >= c = es
        | True = SBytes# n o bytes#
      where
        n = size bnds
    
    upper    (SBytes# c _ _) = c - 1
    bounds   (SBytes# c _ _) = (0, c - 1)
    indices  (SBytes# c _ _) = [0 .. c - 1]
    indexOf  (SBytes# c _ _) = index (0, c - 1)
    offsetOf (SBytes# c _ _) = offset (0, c - 1)
    indexIn  (SBytes# c _ _) = \ i -> i >= 0 && i < c

instance Monad m => BorderedM m (SBytes# e) Int
  where
    nowIndexIn (SBytes# c _ _) = return . inRange (0, c - 1)
    getIndices (SBytes# c _ _) = return [0 .. c - 1]
    getBounds  (SBytes# c _ _) = return (0, c - 1)
    getUpper   (SBytes# c _ _) = return (c - 1)
    
    getLower _ = return 0
    rebounded' = return ... rebound

--------------------------------------------------------------------------------

{- Forceable and Linear instances. -}

instance Unboxed e => Forceable (SBytes# e)
  where
    force es@(SBytes# n@(I# n#) (I# o#) bytes#) =
      SBytes# n 0 (cloneUnboxed1# es bytes# n# o#)

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
    
    head es = es !^ 0
    last es@(SBytes# c _ _) = es !^ (c - 1)
    
    tail (SBytes# c o arr#) = SBytes# (max 1 c - 1) (o + 1) arr#
    init (SBytes# c o arr#) = SBytes# (max 1 c - 1) o arr#
    
    fromList = fromFoldable
    
    fromListN  n es = runST $ newLinearN  n es >>= done
    fromFoldable es = runST $ fromFoldableM es >>= done
    
    single e = SBytes# 1 0 (single# e)
    
    {-# INLINE (!^) #-}
    (!^) (SBytes# _ (I# o#) arr#) = \ (I# i#) -> arr# !# (i# +# o#)
    
    write es n e = not (indexIn es n) ? es $ runST $ do
      es' <- thaw es
      writeM es' n e
      done es'
    
    replicate n e = runST $ filled n e >>= done
    
    listL = sfoldr (:) []
    listR = flip (:) `sfoldl` []
    
    concat ess = runST $ do
      let n = foldr' ((+) . sizeOf) 0 ess
      marr@(STBytes# _ _ marr#) <- alloc n
      
      let
        writeBlock# (SBytes# c@(I# c#) (I# o#) arr#) i@(I# i#) = ST $
          \ s2# -> case pcopyUnboxed1 ess arr# o# marr# i# c# s2# of
            s3# -> (# s3#, i + c #)
      
      foldl (flip $ (=<<) . writeBlock#) (return 0) ess >> done marr
    
    reverse es = runST $ do es' <- fromIndexed' es; reversed' es'; done es'
    
    before es@(SBytes# c@(I# c#) (I# o#) arr#) n@(I# n#) e
      | n >= c = es :< e
      | n <= 0 = e :> es
      |  True  = runST $ ST $ \ s1# -> case newUnboxed' e (c# +# 1#) s1# of
        (# s2#, marr# #) -> case copyUnboxed# e arr# o# marr# 0# n# s2# of
          s3# -> case copyUnboxed# e arr# (o# +# n#) marr# (n# +# 1#) (c# -# n#) s3# of
            s4# -> case unsafeFreezeByteArray# marr# s4# of
              (# s5#, res# #) -> (# s5#, SBytes# (c + 1) 0 res# #)
    
    remove n@(I# n#) es@(SBytes# c@(I# c#) (I# o#) arr#) = n < 0 || n >= c ? es $
      runST $ ST $ \ s1# -> case pnewUnboxed es (c# -# 1#) s1# of
        (# s2#, marr# #) -> case pcopyUnboxed es arr# o# marr# 0# n# s2# of
          s3# -> case pcopyUnboxed es arr# (o# +# n# +# 1#) marr# n# (c# -# n# -# 1#) s3# of
            s4# -> case unsafeFreezeByteArray# marr# s4# of
              (# s5#, res# #) -> (# s5#, SBytes# (c - 1) 0 res# #)
    
    select  f = sfoldr (\ o es -> case f o of {Just e -> e : es; _ -> es}) []
    extract f =
      let g o = second (o :) `maybe` (first . (:)) $ f o
      in  second fromList . sfoldr g ([], [])
    
    selects fs = second fromList . selects fs . listL
    
    ofoldr f base = \ arr@(SBytes# c _ _) ->
      let go i = c == i ? base $ f i (arr !^ i) (go $ i + 1)
      in  go 0
    
    ofoldl f base = \ arr@(SBytes# c _ _) ->
      let go i = -1 == i ? base $ f i (go $ i - 1) (arr !^ i)
      in  go (c - 1)
    
    sfoldr f base = \ arr@(SBytes# c _ _) ->
      let go i = c == i ? base $ f (arr !^ i) (go $ i + 1)
      in  go 0
    
    sfoldl f base = \ arr@(SBytes# c _ _) ->
      let go i = -1 == i ? base $ f (go $ i - 1) (arr !^ i)
      in  go (c - 1)
    
    -- | O(1) 'take', O(1) memory.
    take n es@(SBytes# c o arr#)
      | n <= 0 = Z
      | n >= c = es
      | True = SBytes# n o arr#
    
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
      | n <= 0 = (Z, es)
      | n >= c = (es, Z)
      |  True  = (SBytes# n (o + c - n) arr#, SBytes# (c - n) o arr#)
    
    splitsBy f es = dropWhile f <$> f *$ es `parts` es
    
    combo _                  Z = 0
    combo f es@(SBytes# n _ _) =
      let go e i = let e' = es !^ i in i == n || not (f e e') ? i $ go e' (i + 1)
      in  go (head es) 1
    
    justifyL n@(I# n#) e es@(SBytes# c@(I# c#) (I# o#) src#) = case c <=> n of
      EQ -> es
      GT -> take n es
      LT -> runST $ ST $ \ s1# -> case newUnboxed' e n# s1# of
        (# s2#, mbytes# #) -> case copyUnboxed# e src# o# mbytes# 0# c# s2# of
          s3# -> case unsafeFreezeByteArray# mbytes# s3# of
            (# s4#, bytes# #) -> (# s4#, SBytes# n 0 bytes# #)
    
    justifyR n@(I# n#) e es@(SBytes# c@(I# c#) (I# o#) src#) = case c <=> n of
      EQ -> es
      GT -> take n es
      LT -> runST $ ST $ \ s1# -> case newUnboxed' e n# s1# of
        (# s2#, mbytes# #) -> case copyUnboxed# e src# o# mbytes# (n# -# c#) c# s2# of
          s3# -> case unsafeFreezeByteArray# mbytes# s3# of
            (# s4#, bytes# #) -> (# s4#, SBytes# n 0 bytes# #)
    
    each n es@(SBytes# c _ _) =
      let go i = i < c ? es!^i : go (i + n) $ []
      in  case n <=> 1 of {LT -> Z; EQ -> es; GT -> fromList $ go (n - 1)}
    
    isPrefixOf xs@(SBytes# c1 _ _) ys@(SBytes# c2 _ _) =
      let eq i = i == c1 || (xs !^ i) == (ys !^ i) && eq (i + 1)
      in  c1 <= c2 && eq 0
    
    isSuffixOf xs@(SBytes# c1 _ _) ys@(SBytes# c2 _ _) =
      let eq i j = i == c1 || (xs !^ i) == (ys !^ j) && eq (i + 1) (j + 1)
      in  c1 <= c2 && eq 0 (c2 - c1)
    
    selectWhile f es@(SBytes# c _ _) =
      let go i = i == c ? [] $ maybe [] (: go (i + 1)) $ f (es !^ i)
      in  go 0
    
    selectEnd g xs@(SBytes# c _ _) =
      let go i es = i == 0 ? [] $ maybe [] (: go (i - 1) es) $ g (es !^ i)
      in  reverse $ go (c - 1) xs

--------------------------------------------------------------------------------

{- Set and SetWith instances. -}

instance (Unboxed e, Ord e) => Set (SBytes# e) e

instance Unboxed e => SetWith (SBytes# e) e
  where
    setWith f = nubSorted f . sortBy f
    
    insertWith f e es = case (\ x -> x `f` e /= LT) .$ es of
      Just i -> e `f` (es!^i) == EQ ? es $ before es i e
      _      -> es :< e
    
    deleteWith f e es = memberWith f e es ? except (\ x -> f e x == EQ) es $ es
    
    {-# INLINE intersectionWith #-}
    intersectionWith f xs@(SBytes# n1 _ _) ys@(SBytes# n2 _ _) = fromList $ go 0 0
      where
        go i j = i == n1 || j == n2 ? [] $ case x `f` y of
            EQ -> x : go (i + 1) (j + 1)
            LT -> go (i + 1) j
            GT -> go i (j + 1)
          where
            x = xs !^ i
            y = ys !^ j
    
    {-# INLINE unionWith #-}
    unionWith f xs@(SBytes# n1 _ _) ys@(SBytes# n2 _ _) = fromList $ go 0 0
      where
        go i j
            | i == n1 = (ys !^) <$> [j .. n2 - 1]
            | j == n2 = (xs !^) <$> [i .. n1 - 1]
            |  True   = case x `f` y of
              EQ -> x : go (i + 1) (j + 1)
              LT -> x : go (i + 1) j
              GT -> y : go i (j + 1)
          where
            x = xs !^ i
            y = ys !^ j
    
    {-# INLINE differenceWith #-}
    differenceWith f xs@(SBytes# n1 _ _) ys@(SBytes# n2 _ _) = fromList $ go 0 0
      where
        go i j
            | i == n1 = []
            | j == n2 = (xs !^) <$> [i .. n1 - 1]
            |  True   = case x `f` y of
              EQ -> go (i + 1) (j + 1)
              LT -> x : go (i + 1) j
              GT -> go i (j + 1)
          where
            x = xs !^ i
            y = ys !^ j
    
    {-# INLINE symdiffWith #-}
    symdiffWith f xs@(SBytes# n1 _ _) ys@(SBytes# n2 _ _) = fromList $ symdiff' 0 0
      where
        symdiff' i j
            | i == n1 = (ys !^) <$> [j .. n2 - 1]
            | j == n2 = (xs !^) <$> [i .. n1 - 1]
            |  True   = case x `f` y of
              EQ -> symdiff' (i + 1) (j + 1)
              LT -> x : symdiff' (i + 1) j
              GT -> y : symdiff' i (j + 1)
          where
            x = xs !^ i
            y = ys !^ j
    
    memberWith = memberSorted
    
    lookupLTWith _ _ Z  = Nothing
    lookupLTWith f o es
        | GT <- o `f` last' = Just last'
        | GT <- o `f` head' = look' head' 0 (sizeOf es - 1)
        |       True        = Nothing
      where
        head' = es .! lower es
        last' = es .! upper es
        
        look' r l u = l > u ? Just r $ case o `f` e of
            LT -> look' r l (j - 1)
            EQ -> Just $ j < 1 ? r $ es !^ (j - 1)
            GT -> look' e (j + 1) u
          where
            j = l + (u - l) `div` 2
            e = es !^ j
    
    lookupLEWith _ _ Z  = Nothing
    lookupLEWith f o es
        | GT <- o `f` last' = Just last'
        | LT <- o `f` head' = Nothing
        |       True        = look' head' 0 (sizeOf es - 1)
      where
        head' = es .! lower es
        last' = es .! upper es
        
        look' r l u = l > u ? Just r $ case o `f` e of
            LT -> look' r l (j - 1)
            _  -> look' e (j + 1) u
          where
            j = l + (u - l) `div` 2
            e = es !^ j
    
    lookupGTWith _ _ Z  = Nothing
    lookupGTWith f o es
        | LT <- o `f` head' = Just head'
        | LT <- o `f` last' = look' last' 0 (sizeOf es - 1)
        |       True        = Nothing
      where
        head' = es .! lower es
        last' = es .! upper es
        
        look' r l u = l > u ? Just r $ case o `f` e of
            LT -> look' e l (j - 1)
            EQ -> j >= (sizeOf es - 1) ? Nothing $ Just (es !^ (j + 1))
            GT -> look' r (j + 1) u
          where
            j = l + (u - l) `div` 2
            e = es !^ j
    
    lookupGEWith _ _ Z  = Nothing
    lookupGEWith f o es
        | GT <- o `f` last' = Nothing
        | GT <- o `f` head' = look' last' 0 (sizeOf es - 1)
        |       True        = Just head'
      where
        head' = es .! lower es
        last' = es .! upper es
        
        look' r l u = l > u ? Just r $ case o `f` e of
            LT -> look' e l (j - 1)
            EQ -> Just e
            GT -> look' r (j + 1) u
          where
            j = l + (u - l) `div` 2
            e = es !^ j
    
    isSubsetWith f xs ys = sfoldr (\ x b -> b && memberWith f x ys) True xs

--------------------------------------------------------------------------------

{- Scan and Sort instances. -}

instance Unboxed e => Scan (SBytes# e) e

instance Unboxed e => Sort (SBytes# e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'
    
    sortedBy f es@(SBytes# n _ _) =
      let go i = let i1 = i + 1 in i1 == n || (f (es !^ i) (es !^ i1) && go i1)
      in  n < 2 || go 0

--------------------------------------------------------------------------------

{- Map and Indexed instances. -}

instance Unboxed e => Map (SBytes# e) Int e
  where
    toMap ascs =
      let bnds = rangeBounds (fsts ascs)
      in  isNull ascs ? Z $ assoc bnds ascs
    
    toMap' defvalue ascs =
      let bnds = rangeBounds (fsts ascs)
      in  isNull ascs ? Z $ assoc' bnds defvalue ascs
    
    Z  // ascs = toMap ascs
    es // ascs = runST $ thaw es >>= flip overwrite ascs >>= done
    
    (*$) p = ofoldr (\ i e is -> p e ? (i : is) $ is) []
    (.!)   = (!^)
    
    kfoldr = ofoldr
    kfoldl = ofoldl

instance Unboxed e => Indexed (SBytes# e) Int e
  where
    assoc  bnds ascs = runST $ fromAssocs bnds ascs >>= done
    assoc' bnds defvalue ascs = runST $ fromAssocs' bnds defvalue ascs >>= done
    
    fromIndexed es = runST $ do
      copy <- alloc (sizeOf es)
      ofoldr (\ i e go -> do writeM copy i e; go) (done copy) es

--------------------------------------------------------------------------------

{- Freeze and Thaw instances. -}

instance Unboxed e => Thaw (ST s) (SBytes# e) (STBytes# s e)
  where
    thaw es@(SBytes# c@(I# c#) (I# o#) arr#) = do
      marr@(STBytes# _ _ marr#) <- alloc c
      ST $ \ s1# -> (# pcopyUnboxed es arr# o# marr# 0# c# s1#, () #)
      return marr

instance Unboxed e => Freeze (ST s) (STBytes# s e) (SBytes# e)
  where
    freeze       = cloneSTBytes# >=> done
    unsafeFreeze = done

--------------------------------------------------------------------------------

-- | 'STBytes#' is mutable pseudo-primitive 'Int'-indexed strict unboxed array type.
data STBytes# s e = STBytes#
              {-# UNPACK #-} !Int    -- ^ Element count (not a real size)
              {-# UNPACK #-} !Int    -- ^ Offset (in elements)
              !(MutableByteArray# s) -- ^ Real primitive byte array
  deriving ( Typeable )

type role STBytes# nominal representational

--------------------------------------------------------------------------------

instance Eq (STBytes# s e)
  where
    (STBytes# c1 o1 marr1#) == (STBytes# c2 o2 marr2#) =
      let same = isTrue# (sameMutableByteArray# marr1# marr2#)
      in  c1 == c2 && (c1 == 0 || o1 == o2 && same)

--------------------------------------------------------------------------------

{- NullableM instance. -}

instance NullableM (ST s) (STBytes# s e)
  where
    nowNull (STBytes# n _ _) = return (n < 1)
    
    newNull = ST $ \ s1# -> case newByteArray# 0# s1# of
      (# s2#, marr# #) -> (# s2#, STBytes# 0 0 marr# #)

--------------------------------------------------------------------------------

{- Estimate and EstimateM instances. -}

instance Estimate (STBytes# s e)
  where
    sizeOf (STBytes# c _ _) = c
    
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

instance EstimateM (ST s) (STBytes# s e)
  where
    getSizeOf (STBytes# c _ _) = return c
    
    estimateMLT = return ... (.<.)
    estimateMGT = return ... (.>.)
    estimateMLE = return ... (.<=.)
    estimateMGE = return ... (.>=.)
    estimateM   = return ... (<==>)
    
    lestimateMLT' = return ... (.<)
    lestimateMGT' = return ... (.>)
    lestimateMLE' = return ... (.<=)
    lestimateMGE' = return ... (.>=)
    lestimateM'   = return ... (<.=>)

--------------------------------------------------------------------------------

{- Bordered and Bordered instances. -}

instance Bordered (STBytes# s e) Int
  where
    bounds (STBytes# c _ _) = (0, c - 1)
    
    rebound bnds es@(STBytes# c o bytes#)
        | n < 0 = STBytes# 0 0 bytes#
        | n < c = STBytes# n o bytes#
        | True  = es
      where
        n = size bnds

instance Unboxed e => BorderedM (ST s) (STBytes# s e) Int
  where
    getLower _ = return 0
    rebounded' = takeM . size
    
    nowIndexIn (STBytes# c _ _) = return . inRange (0, c - 1)
    getIndices (STBytes# c _ _) = return [0 .. c - 1]
    getBounds  (STBytes# c _ _) = return (0, c - 1)
    getUpper   (STBytes# c _ _) = return (c - 1)

--------------------------------------------------------------------------------

{- Copyable and LinearM instances. -}

instance Unboxed e => Copyable (ST s) (STBytes# s e)
  where
    copied es@(STBytes# n@(I# n#) (I# o#) marr#) = ST $
      \ s1# -> case pcloneUnboxedM es marr# o# n# s1# of
        (# s2#, copy# #) -> (# s2#, STBytes# n 0 copy# #)

instance Unboxed e => LinearM (ST s) (STBytes# s e) e
  where
    getHead es = es >! 0
    getLast es = es >! upper es
    newLinear  = fromFoldableM
    
    newLinearN c es = let !n@(I# n#) = max 0 c in ST $
      \ s1# -> case newLinearN# n# es s1# of
        (# s2#, marr# #) -> (# s2#, STBytes# n 0 marr# #)
    
    fromFoldableM es = ST $ \ s1# -> case fromFoldableM# es s1# of
      (# s2#, n, marr# #) -> (# s2#, STBytes# n 0 marr# #)
    
    getLeft  es@(STBytes# n _ _) = (es !#>) `mapM` [0 .. n - 1]
    getRight es@(STBytes# n _ _) = (es !#>) `mapM` [n - 1, n - 2 .. 0]
    
    {-# INLINE (!#>) #-}
    STBytes# _ (I# o#) marr# !#> I# i# = ST $ readUnboxed# marr# (o# +# i#)
    
    writeM = writeM'
    
    copied' es@(STBytes# c (I# o#) marr#) l@(I# l#) n@(I# n#)
      | l >= 0 && n >= 0 && c >= l = ST $
        \ s1# -> case pcloneUnboxedM es marr# (o# +# l#) n# s1# of
          (# s2#, copy# #) -> (# s2#, STBytes# n 0 copy# #)
      | True = unreachEx "copied'"
    
    reversed es = do es' <- copied es; reversed' es'; return es'
    
    reversed' es@(STBytes# n _ _) =
      let go i j = when (i < j) $ go (i + 1) (j - 1) >> swapM es i j
      in  go 0 (n - 1)
    
    filled n e = ST $ \ s1# -> case newUnboxed' e n# s1# of
        (# s2#, marr# #) -> (# s2#, STBytes# n' 0 marr# #)
      where
        !n'@(I# n#) = max 0 n
    
    copyTo src sc trg tc n@(I# n#) = when (n > 0) $ do
        when      (sc < 0 || tc < 0)      $ underEx "copyTo"
        when (sc + n > n1 || tc + n > n2) $ overEx  "copyTo"
        ST $ \ s1# -> case pcopyUnboxedM src src# so# trg# to# n# s1# of
          s2# -> (# s2#, () #)
      where
        !(STBytes# n1 o1 src#) = src; !(I# so#) = o1 + sc
        !(STBytes# n2 o2 trg#) = trg; !(I# to#) = o2 + tc
    
    merged ess = do
        marr <- alloc n
        let writer arr@(STBytes# c _ _) i = (i + c) <$ copyTo arr 0 marr i c
        marr <$ foldr ((=<<) . writer) (return 0) ess
      where
        n = foldr' ((+) . sizeOf) 0 ess
    
    ofoldrM f base = \ arr@(STBytes# n _ _) ->
      let go i =  n == i ? return base $ (arr !#> i) >>=<< go (i + 1) $ f i
      in  go 0
    
    ofoldlM f base = \ arr@(STBytes# n _ _) ->
      let go i = -1 == i ? return base $ go (i - 1) >>=<< (arr !#> i) $ (f i)
      in  go (n - 1)
    
    foldrM f base = \ arr@(STBytes# n _ _) ->
      let go i = n == i ? return base $ (arr !#> i) >>=<< go (i + 1) $ f
      in  go 0
    
    foldlM f base = \ arr@(STBytes# n _ _) ->
      let go i = -1 == i ? return base $ go (i - 1) >>=<< (arr !#> i) $ f
      in  go (n - 1)
    
    miterate n@(I# n#) f e = do
      es <- filled n e
      
      let
        iterate' 0# _ _ = return ()
        iterate' c# i x = do writeM es i x; iterate' (c# -# 1#) (i + 1) (f x)
      
      es <$ iterate' n# 0 e
    
    iterateM n@(I# n#) go e = do
      es <- filled n e
      
      let
        iterate' 0# _ _ = return ()
        iterate' c# i x = do writeM es i x; iterate' (c# -# 1#) (i + 1) =<< go x
      
      es <$ iterate' n# 0 e
    
    takeM n es@(STBytes# c o marr#)
      | n <= 0 = newNull
      | n >= c = return es
      |  True  = return (STBytes# n o marr#)
    
    dropM n es@(STBytes# c o marr#)
      | n >= c = newNull
      | n <= 0 = return es
      |  True  = return (STBytes# (c - n) (o + n) marr#)
    
    keepM n es@(STBytes# c o marr#)
      | n <= 0 = newNull
      | n >= c = return es
      |  True  = return (STBytes# n (c - n + o) marr#)
    
    sansM n es@(STBytes# c o marr#)
      | n >= c = newNull
      | n <= 0 = return es
      |  True  = return (STBytes# (c - n) o marr#)
    
    splitM n es@(STBytes# c o marr#)
      | n <= 0 = do e' <- newNull; return (e', es)
      | n >= c = do e' <- newNull; return (es, e')
      |  True  = return (STBytes# n o marr#, STBytes# (c - n) (o + n) marr#)
    
    divideM n es@(STBytes# c o marr#)
      | n <= 0 = do e' <- newNull; return (es, e')
      | n >= c = do e' <- newNull; return (e', es)
      |  True  = return (STBytes# n (c - n + o) marr#, STBytes# (c - n) o marr#)
    
    prefixM p es@(STBytes# c _ _) =
      let go i = i >= c ? return c $ do e <- es !#> i; p e ? go (succ i) $ return i
      in  go 0
    
    suffixM p es@(STBytes# c _ _) =
      let go i = i < 0 ? return c $ do e <- es !#> i; p e ? go (pred i) $ return (c - i - 1)
      in  go (max 0 (c - 1))
    
    mprefix p es@(STBytes# c _ _) =
      let go i = i >= c ? return c $ do e <- es !#> i; p e ?^ go (succ 1) $ return i
      in  go 0
    
    msuffix p es@(STBytes# c _ _) =
      let go i = i < 0 ? return c $ do e <- es !#> i; p e ?^ go (pred i) $ return (c - i - 1)
      in  go (max 0 (c - 1))

--------------------------------------------------------------------------------

{- MapM and IndexedM instances. -}

instance Unboxed e => MapM (ST s) (STBytes# s e) Int e
  where
    newMap ascs =
      let bnds = rangeBounds (fsts ascs)
      in  fromAssocs bnds ascs
    
    newMap' defvalue ascs =
      let bnds = rangeBounds (fsts ascs)
      in  fromAssocs' bnds defvalue ascs
    
    {-# INLINE writeM' #-}
    writeM' (STBytes# _ (I# o#) marr#) = \ (I# i#) e -> ST $
      \ s1# -> case writeUnboxed# marr# (o# +# i#) e s1# of
        s2# -> (# s2#, () #)
    
    (>!) = (!#>)
    
    overwrite es@(STBytes# c _ _) ascs =
      let ies = filter (inRange (0, c - 1) . fst) ascs
      in  mapM_ (uncurry $ writeM es) ies >> return es
    
    kfoldrM = ofoldrM
    kfoldlM = ofoldlM

instance Unboxed e => IndexedM (ST s) (STBytes# s e) Int e
  where
    fromAssocs  bnds ascs = alloc (size bnds) >>= flip overwrite ascs
    fromAssocs' bnds defvalue ascs = size bnds `filled` defvalue >>= (`overwrite` ascs)
    
    fromIndexed' es = do
      copy <- alloc (sizeOf es)
      copy <$ ofoldr (\ i e go -> do writeM copy i e; go) (return ()) es
    
    fromIndexedM es = do
      copy <- alloc =<< getSizeOf es
      copy <$ ofoldrM (\ i e _ -> writeM copy i e) () es

--------------------------------------------------------------------------------

{- SortM instance. -}

instance Unboxed e => SortM (ST s) (STBytes# s e) e
  where
    sortedMBy f es@(STBytes# n _ _) =
      let go i e1 = i == n ? return True $ do e2 <- es !#> i; e1 `f` e2 ? go (i + 1) e2 $ return False
      in  n < 2 ? return True $ go 1 =<< getHead es
    
    sortMBy = timSortBy

--------------------------------------------------------------------------------

-- | 'MIOBytes#' is mutable pseudo-primitive 'Int'-indexed strict unboxed array.
newtype MIOBytes# (io :: Type -> Type) e = MIOBytes# (STBytes# RealWorld e)
  deriving ( Eq )

-- | 'IOBytes#' is mutable pseudo-primitive 'Int'-indexed strict unboxed array.
type IOBytes# = MIOBytes# IO

{-# INLINE unpack #-}
unpack :: MIOBytes# io e -> STBytes# RealWorld e
unpack =  coerce

{-# INLINE pack #-}
pack :: (MonadIO io) => ST RealWorld (STBytes# RealWorld e) -> io (MIOBytes# io e)
pack =  stToMIO . coerce

--------------------------------------------------------------------------------

{- NullableM instance. -}

instance MonadIO io => NullableM io (MIOBytes# io e)
  where
    newNull = pack newNull
    nowNull = stToMIO . nowNull . unpack

--------------------------------------------------------------------------------

{- Estimate and EstimateM instances. -}

instance Estimate (MIOBytes# io e)
  where
    sizeOf (MIOBytes# (STBytes# c _ _)) = c
    
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

instance MonadIO io => EstimateM io (MIOBytes# io e)
  where
    getSizeOf = return . sizeOf . unpack
    
    estimateMLT = return ... (.<.)
    estimateMGT = return ... (.>.)
    estimateMLE = return ... (.<=.)
    estimateMGE = return ... (.>=.)
    estimateM   = return ... (<==>)
    
    lestimateMLT' = return ... (.<)
    lestimateMGT' = return ... (.>)
    lestimateMLE' = return ... (.<=)
    lestimateMGE' = return ... (.>=)
    lestimateM'   = return ... (<.=>)

--------------------------------------------------------------------------------

{- Bordered and BorderedM instances. -}

instance Bordered (MIOBytes# io e) Int
  where
    lower = const 0
    
    rebound bnds (MIOBytes# es) = MIOBytes# (rebound bnds es)
    
    upper    (MIOBytes# (STBytes# c _ _)) = c - 1
    bounds   (MIOBytes# (STBytes# c _ _)) = (0, c - 1)
    indices  (MIOBytes# (STBytes# c _ _)) = [0 .. c - 1]
    indexOf  (MIOBytes# (STBytes# c _ _)) = index (0, c - 1)
    offsetOf (MIOBytes# (STBytes# c _ _)) = offset (0, c - 1)
    indexIn  (MIOBytes# (STBytes# c _ _)) = \ i -> i >= 0 && i < c

instance (MonadIO io, Unboxed e) => BorderedM io (MIOBytes# io e) Int
  where
    getIndexOf = return ... indexOf . unpack
    getIndices = return . indices . unpack
    getBounds  = return . bounds . unpack
    getUpper   = return . upper . unpack
    
    rebounded' = takeM . size
    getLower _ = return 0

--------------------------------------------------------------------------------

{- Copyable and LinearM instances. -}

instance (MonadIO io, Unboxed e) => Copyable io (MIOBytes# io e)
  where
    copied = pack . copied . unpack

instance (MonadIO io, Unboxed e) => LinearM io (MIOBytes# io e) e
  where
    singleM = pack . singleM
    getHead = stToMIO . getHead . unpack
    getLast = stToMIO . getLast . unpack
    
    prepend e = pack . prepend e . unpack
    append es = pack . append (unpack es)
    
    newLinear     = pack . newLinear
    newLinearN    = pack ... newLinearN
    fromFoldableM = pack . fromFoldableM
    
    (!#>) = stToMIO ... (!#>) . unpack
    
    writeM es = stToMIO ... writeM (unpack es)
    
    reversed = pack  . reversed . unpack
    getLeft  = stToMIO . getLeft  . unpack
    getRight = stToMIO . getRight . unpack
    
    copied' es = pack ... copied' (unpack es)
    
    filled = pack ... filled
    merged = pack  .  merged . foldr ((:) . unpack) []
    
    copyTo src so trg to = stToMIO . copyTo (unpack src) so (unpack trg) to
    
    ofoldrM f base = \ arr@(MIOBytes# (STBytes# n _ _)) ->
      let go i =  n == i ? return base $ (arr !#> i) >>=<< go (i + 1) $ f i
      in  go 0
    
    ofoldlM f base = \ arr@(MIOBytes# (STBytes# n _ _)) ->
      let go i = -1 == i ? return base $ go (i - 1) >>=<< (arr !#> i) $ (f i)
      in  go (n - 1)
    
    foldrM f base arr =
      let go i = sizeOf arr == i ? return base $ (arr !#> i) >>=<< go (i + 1) $ f
      in  go 0
    
    foldlM f base arr =
      let go i = -1 == i ? return base $ go (i - 1) >>=<< (arr !#> i) $ f
      in  go (sizeOf arr - 1)
    
    takeM n = pack . takeM n . unpack
    dropM n = pack . dropM n . unpack
    keepM n = pack . keepM n . unpack
    sansM n = pack . sansM n . unpack
    
    prefixM f = stToMIO . prefixM f . unpack
    suffixM f = stToMIO . suffixM f . unpack
    
    mprefix p es@(MIOBytes# (STBytes# c _ _)) =
      let go i = i >= c ? return c $ do e <- es !#> i; p e ?^ go (succ 1) $ return i
      in  go 0
    
    msuffix p es@(MIOBytes# (STBytes# c _ _)) =
      let go i = i < 0 ? return c $ do e <- es !#> i; p e ?^ go (pred i) $ return (c - i - 1)
      in  go (max 0 (c - 1))

--------------------------------------------------------------------------------

{- MapM and IndexedM instances. -}

instance (MonadIO io, Unboxed e) => MapM io (MIOBytes# io e) Int e
  where
    newMap' = pack ... newMap'
    newMap  = pack  .  newMap
    
    (>!) = (!#>)
    
    writeM' es = stToMIO ... writeM' (unpack es)
    overwrite  = pack ... overwrite . unpack
    
    kfoldrM = ofoldrM
    kfoldlM = ofoldlM

instance (MonadIO io, Unboxed e) => IndexedM io (MIOBytes# io e) Int e
  where
    fromAssocs  bnds = pack  .  fromAssocs  bnds
    fromAssocs' bnds = pack ... fromAssocs' bnds
    
    fromIndexed' = pack . fromIndexed'
    fromIndexedM = \ es -> do
      copy <- flip filled (unreachEx "fromIndexedM") =<< getSizeOf es
      copy <$ ofoldrM (\ i e _ -> writeM copy i e) () es

--------------------------------------------------------------------------------

{- SortM instance. -}

instance (MonadIO io, Unboxed e) => SortM io (MIOBytes# io e) e
  where
    sortedMBy f = stToMIO . sortedMBy f . unpack
    sortMBy     = timSortBy

--------------------------------------------------------------------------------

{- Freeze and Thaw instances. -}

instance (MonadIO io, Unboxed e) => Thaw io (SBytes# e) (MIOBytes# io e)
  where
    unsafeThaw = pack . unsafeThaw
    thaw       = pack . thaw

instance (MonadIO io, Unboxed e) => Freeze io (MIOBytes# io e) (SBytes# e)
  where
    unsafeFreeze = stToMIO . unsafeFreeze . unpack
    freeze       = stToMIO . freeze . unpack

instance (Storable e, Unboxed e) => Freeze IO (Int, Ptr e) (SBytes# e)
  where
    freeze (c, ptr) = do
      let
        err = unreachEx "freeze {(Int, Ptr e) => SBytes# e}"
        
        filled' :: Unboxed e => proxy e -> e -> IO (MIOBytes# IO e)
        filled' =  const $ filled (max 0 c)
      
      es <- filled' ptr err
      freeze =<< mupdate es (\ i _ -> peekElemOff ptr i)

instance (Storable e, Unboxed e) => Thaw IO (SBytes# e) (Int, Ptr e)
  where
    thaw arr@(SBytes# n _ _) = do
      ptr <- callocArray n
      (n, ptr) <$ ofoldr (\ i e go -> do pokeElemOff ptr i e; go) (return ()) arr

--------------------------------------------------------------------------------

-- | 'unpackSBytes#' returns 'ByteArray#' field of 'SBytes#'.
unpackSBytes# :: Unboxed e => SBytes# e -> ByteArray#
unpackSBytes# = \ (SBytes# _ _ marr#) -> marr#

-- | 'offsetSBytes#' returns 'SBytes#' offset in elements.
offsetSBytes# :: Unboxed e => SBytes# e -> Int
offsetSBytes# =  \ (SBytes# _ o _) -> o

-- | 'packSBytes#' creates new 'SBytes#' from sized 'ByteArray#'.
packSBytes# :: Unboxed e => Int -> ByteArray# -> SBytes# e
packSBytes# n marr# = SBytes# (max 0 n) 0 marr#

-- | 'fromSBytes#' returns new 'ByteArray#'.
fromSBytes# :: Unboxed e => SBytes# e -> ByteArray#
fromSBytes# es@(SBytes# c@(I# c#) o@(I# o#) src#) =
  let
    !(SBytes# _ _ res#) = runST $ ST $ \ s1# -> case pnewUnboxed es c# s1# of
      (# s2#, mcopy# #) -> case pcopyUnboxed es src# o# mcopy# 0# c# s2# of
        s3# -> case unsafeFreezeByteArray# mcopy# s3# of
          (# s4#, copy# #) -> (# s4#, SBytes# c o copy# #)
  in  res#

{- |
  'unsafeCoerceSBytes#' is unsafe low-lowel coerce of an array with recounting
  the number of elements and offset (with possible rounding).
-}
unsafeCoerceSBytes# :: (Unboxed a, Unboxed b) => SBytes# a -> SBytes# b
unsafeCoerceSBytes# pa@(SBytes# n o arr#) = pb
  where
    pb = SBytes# ((n * s1) `div` s2) ((o * s1) `div` s2) arr#
    s1 = psizeof pa n; s2 = psizeof pb n

-- | 'unpackSTBytes#' returns 'MutableByteArray#' field of 'STBytes#'.
unpackSTBytes# :: Unboxed e => STBytes# s e -> MutableByteArray# s
unpackSTBytes# =  \ (STBytes# _ _ marr#) -> marr#

-- | 'offsetSTBytes#' returns 'STBytes#' offset in bytes.
offsetSTBytes# :: Unboxed e => STBytes# s e -> Int#
offsetSTBytes# =  \ (STBytes# _ (I# o#) _) -> o#

-- | 'packSTBytes#' creates new 'STBytes#' from sized 'MutableByteArray#'.
packSTBytes# :: Unboxed e => Int -> MutableByteArray# s -> STBytes# s e
packSTBytes# n marr# = STBytes# (max 0 n) 0 marr#

-- | 'fromSTBytes#' returns new 'MutableByteArray#'.
fromSTBytes# :: Unboxed e => STBytes# s e -> State# s -> (# State# s, MutableByteArray# s #)
fromSTBytes# es = \ s1# -> case cloneSTBytes# es of
  ST rep -> case rep s1# of (# s2#, (STBytes# _ _ marr#) #) -> (# s2#, marr# #)

{- |
  'unsafeCoerceSTBytes#' is unsafe low-lowel coerce of an mutable array with
  recounting the number of elements and offset (with possible rounding).
-}
unsafeCoerceSTBytes# :: (Unboxed a, Unboxed b) => STBytes# s a -> STBytes# s b
unsafeCoerceSTBytes# pa@(STBytes# n o arr#) = pb
  where
    pb = STBytes# (n * s1 `div` s2) (o * s1 `div` s2) arr#
    s1 = psizeof pa 8; s2 = psizeof pb 8

{- |
  'unsafeCoerceMIOBytes#' is unsafe low-lowel coerce of an mutable array with
  recounting the number of elements and offset (with possible rounding).
-}
unsafeCoerceMIOBytes# :: (MonadIO io, Unboxed a, Unboxed b)
                      => MIOBytes# io a -> MIOBytes# io b
unsafeCoerceMIOBytes# (MIOBytes# es) = MIOBytes# (unsafeCoerceSTBytes# es)

{- |
  @'unsafeSBytesToPtr#' es@ byte-wise stores 'SBytes#' content to 'Ptr'. Returns
  the number of overwritten elements and a pointer to @psizeof es (sizeOf es)@
  bytes of allocated memory.
-}
unsafeSBytesToPtr# :: Unboxed e => SBytes# e -> IO (Int, Ptr e)
unsafeSBytesToPtr# es = do
  let
    es' = unsafeCoerceSBytes# es :: SBytes# Word8
    n   = sizeOf es'
  
  ptr <- mallocBytes n
  ofoldr (\ i e go -> do pokeByteOff ptr i e; go) (return (n, ptr)) es'

{- |
  @'unsafePtrToSBytes#' n ptr@ byte-wise stores @n@ elements of 'Ptr' @ptr@ to
  'SBytes#'.
-}
unsafePtrToSBytes# :: Unboxed e => (Int, Ptr e) -> IO (SBytes# e)
unsafePtrToSBytes# (c, ptr) = do
  let
    filled' :: Unboxed e => proxy e -> e -> IO (IOBytes# e)
    filled' =  const $ filled (max 0 c)
  
  es <- filled' ptr (unreachEx "unsafePtrToSBytes#")
  
  let es' = unsafeCoerceMIOBytes# es :: IOBytes# Word8
  void $ mupdate es' (\ i _ -> peekByteOff ptr i)
  done' es

-- | Calculate hash 'SBytes#' using 'hashUnboxedWith'.
hashSBytesWith# :: Unboxed e => Int -> SBytes# e -> Int
hashSBytesWith# (I# salt#) es@(SBytes# (I# c#) (I# o#) bytes#) =
  I# (hashUnboxedWith (fromProxy es) c# o# bytes# salt#)

--------------------------------------------------------------------------------

{-# INLINE done #-}
done :: STBytes# s e -> ST s (SBytes# e)
done (STBytes# n o marr#) = ST $ \ s1# -> case unsafeFreezeByteArray# marr# s1# of
  (# s2#, arr# #) -> (# s2#, SBytes# n o arr# #)

{-# INLINE done' #-}
done' :: MonadIO io => MIOBytes# io e -> io (SBytes# e)
done' =  \ (MIOBytes# arr) -> stToMIO (done arr)

-- | Create 'filled' by default value pseudo-primitive.
alloc :: Unboxed e => Int -> ST s (STBytes# s e)
alloc c@(I# c#) =
  let res = ST $ \ s1# -> case pnewUnboxed1 res c# s1# of
        (# s2#, marr# #) -> (# s2#, STBytes# c 0 marr# #)
  in  res

cloneSTBytes# :: Unboxed e => STBytes# s e -> ST s (STBytes# s e)
cloneSTBytes# es@(STBytes# c@(I# c#) (I# o#) marr#) = do
  copy@(STBytes# _ _ copy#) <- alloc c
  ST $ \ s1# -> (# pcopyUnboxedM es marr# o# copy# 0# c# s1#, () #)
  return copy

{-# INLINE nubSorted #-}
nubSorted :: Unboxed e => Compare e -> SBytes# e -> SBytes# e
nubSorted _ Z  = Z
nubSorted f es =
  let fun = \ e ls -> e `f` head ls == EQ ? ls $ e : ls
  in  fromList $ foldr fun [last es] ((es !^) <$> [0 .. sizeOf es - 2])

--------------------------------------------------------------------------------

overEx :: String -> a
overEx =  throw . IndexOverflow . showString "in SDP.Prim.SBytes."

underEx :: String -> a
underEx =  throw . IndexUnderflow . showString "in SDP.Prim.SBytes."

unreachEx :: String -> a
unreachEx =  throw . UnreachableException . showString "in SDP.Prim.SBytes."




