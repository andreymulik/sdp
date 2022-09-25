{-# LANGUAGE Trustworthy, MagicHash, UnboxedTuples, BangPatterns, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, RoleAnnotations #-}

{- |
    Module      :  SDP.Prim.SArray
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Prim.SArray" provides lazy boxed pseudo-primitive safe arrays.
-}
module SDP.Prim.SArray
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  module SDP.Sort,
  
  -- * Pseudo-primitive types
  MIOArray# (..), IOArray#, STArray#, SArray#,
  
  -- ** Safe (copy) unpack
  fromSArray#, fromSTArray#,
  
  -- ** Unsafe unpack
  unpackSArray#, offsetSArray#, unpackSTArray#, offsetSTArray#,
  
  -- ** Unsafe pack
  packSArray#, packSTArray#,
  
  -- ** Coerce
  coerceSArray#, coerceSTArray#
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM
import SDP.SortM
import SDP.Sort
import SDP.Scan

import SDP.SortM.Tim

import qualified GHC.Exts as E
import GHC.Exts
  (
    Array#, MutableArray#, State#, Int#,
    
    newArray#, indexArray#, readArray#, writeArray#, sameMutableArray#,
    thawArray#, unsafeThawArray#, freezeArray#, unsafeFreezeArray#,
    copyArray#, copyMutableArray#, cloneArray#, cloneMutableArray#,
    (+#), (-#), (==#)
  )

import GHC.Types
import GHC.ST ( ST (..), STRep )

import Data.Default.Class
import Data.Typeable
import Data.Coerce
import Data.String

import Text.Read

import Foreign ( Ptr, Storable, callocArray, peekElemOff, pokeElemOff )

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

{- |
  'SArray#' is immutable pseudo-primitive 'Int'-indexed lazy boxed array type.
  
  'SArray#' isn't real Haskell primitive (like "GHC.Exts" types) but for
  reliability and stability, I made it inaccessible to direct work.
-}
data SArray# e = SArray#
          {-# UNPACK #-} !Int -- ^ Element count (not a real size)
          {-# UNPACK #-} !Int -- ^ Offset (is elements)
          !(Array# e)         -- ^ Real primitive array
  deriving ( Typeable )

type role SArray# representational

--------------------------------------------------------------------------------

{- Eq and Eq1 instances. -}

instance Eq e => Eq (SArray# e) where (==) = eq1

instance Eq1 SArray#
  where
    liftEq eq xs@(SArray# c1 _ _) ys@(SArray# c2 _ _) =
      let eq' i = i == c1 || eq (xs !^ i) (ys !^ i) && eq' (i + 1)
      in  c1 == c2 && eq' 0

--------------------------------------------------------------------------------

{- Ord and Ord1 instances. -}

instance Ord e => Ord (SArray# e) where compare = compare1

instance Ord1 SArray#
  where
    liftCompare f xs@(SArray# c1 _ _) ys@(SArray# c2 _ _) =
      let f' i = i == (c1`min`c2) ? c1 <=> c2 $ (xs!^i) `f` (ys!^i) <> f' (i+1)
      in  f' 0

--------------------------------------------------------------------------------

{- Show and Read instances. -}

instance Show e => Show (SArray# e) where showsPrec p = showsPrec p . listL

instance Read e => Read (SArray# e) where readPrec = fromList <$> readPrec

--------------------------------------------------------------------------------

{- Overloaded Lists and Strings. -}

instance IsString (SArray# Char) where fromString = fromList

instance E.IsList (SArray# e)
  where
    type Item (SArray# e) = e
    
    fromListN = fromListN
    fromList  = fromList
    toList    = toList

--------------------------------------------------------------------------------

{- Semigroup, Monoid and Default instances. -}

instance Default (SArray# e) where def = Z
instance Monoid  (SArray# e) where mempty = Z; mappend = (<>)

instance Semigroup (SArray# e)
  where
    -- [internal]: always return new array, even if (at least) one is empty
    SArray# (I# n1#) (I# o1#) arr1# <> SArray# (I# n2#) (I# o2#) arr2# =
      runST $ ST $ \ s1# -> case newArray# n# (unreachEx "(++)") s1# of
        (#s2#, marr# #) -> case copyArray# arr1# o1# marr# 0# n1# s2# of
          s3# -> case copyArray# arr2# o2# marr# n1# n2# s3# of
            s4# -> case unsafeFreezeArray# marr# s4# of
              (# s5#, arr# #) -> (# s5#, SArray# (I# n#) 0 arr# #)
      where
        n# = n1# +# n2#

--------------------------------------------------------------------------------

{- Nullable and NullableM instances. -}

instance Nullable (SArray# e)
  where
    lzero  = runST $ filled 0 (unreachEx "lzero") >>= done
    isNull = \ (SArray# c _ _) -> c == 0

instance Monad m => NullableM m (SArray# e)
  where
    newNull = return lzero
    nowNull = return . isNull

--------------------------------------------------------------------------------

{- Estimate and EstimateM instances. -}

instance Estimate (SArray# e)
  where
    sizeOf (SArray# c _ _) = c
    
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

instance Monad m => EstimateM m (SArray# e)
  where
    getSizeOf (SArray# c _ _) = return c
    
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

instance Bordered (SArray# e) Int
  where
    lower _ = 0
    rebound = take . size
    
    upper    (SArray# c _ _) = c - 1
    bounds   (SArray# c _ _) = (0, c - 1)
    indices  (SArray# c _ _) = [0 .. c - 1]
    indexOf  (SArray# c _ _) = index (0, c - 1)
    offsetOf (SArray# c _ _) = offset (0, c - 1)
    indexIn  (SArray# c _ _) = \ i -> i >= 0 && i < c

instance Monad m => BorderedM m (SArray# e) Int
  where
    nowIndexIn (SArray# c _ _) = return . inRange (0, c - 1)
    getIndices (SArray# c _ _) = return [0 .. c - 1]
    getBounds  (SArray# c _ _) = return (0, c - 1)
    getUpper   (SArray# c _ _) = return (c - 1)
    
    getLower _ = return 0
    rebounded' = return ... rebound

--------------------------------------------------------------------------------

{- Functor, Zip and Applicative instances. -}

instance Functor SArray#
  where
    fmap f arr@(SArray# n@(I# n#) _ _) = runST $ ST $ \ s1# ->
      case newArray# n# (unreachEx "fmap") s1# of
        (# s2#, marr# #) ->
          let go i@(I# i#) s3# = if i == n
              then case unsafeFreezeArray# marr# s3# of (# s4#, arr# #) -> (# s4#, SArray# n 0 arr# #)
              else case writeArray# marr# i# (f $ arr ! i) s3# of s5# -> go (i + 1) s5#
          in go 0 s2#

instance Zip SArray#
  where
    all2 f as bs = go (sizeOf as <?=> bs)
      where
        apply i = f (as!^i) (bs!^i)
        
        go 0 = True
        go i = let i' = i - 1 in apply i' && go i'
    
    all3 f as bs cs = go (sizeOf as <?=> bs <?=> cs)
      where
        apply i = f (as!^i) (bs!^i) (cs!^i)
        
        go 0 = True
        go i = let i' = i - 1 in apply i' && go i'
    
    all4 f as bs cs ds = go (sizeOf as <?=> bs <?=> cs <?=> ds)
      where
        apply i = f (as!^i) (bs!^i) (cs!^i) (ds!^i)

        go 0 = True
        go i = let i' = i - 1 in apply i' && go i'
    
    all5 f as bs cs ds es = go (sizeOf as <?=> bs <?=> cs <?=> ds <?=> es)
      where
        apply i = f (as!^i) (bs!^i) (cs!^i) (ds!^i) (es!^i)
        
        go 0 = True
        go i = let i' = i - 1 in apply i' && go i'
    
    all6 f as bs cs ds es fs = go (sizeOf as <?=> bs <?=> cs <?=> ds <?=> es <?=> fs)
      where
        apply i = f (as!^i) (bs!^i) (cs!^i) (ds!^i) (es!^i) (fs!^i)
        
        go 0 = True
        go i = let i' = i - 1 in apply i' && go i'
    
    any2 f as bs = go (sizeOf as <?=> bs)
      where
        apply i = f (as!^i) (bs!^i)
        
        go 0 = False
        go i = let i' = i - 1 in apply i' || go i'
    
    any3 f as bs cs = go (sizeOf as <?=> bs <?=> cs)
      where
        apply i = f (as!^i) (bs!^i) (cs!^i)
        
        go 0 = False
        go i = let i' = i - 1 in apply i' || go i'
    
    any4 f as bs cs ds = go (sizeOf as <?=> bs <?=> cs <?=> ds)
      where
        apply i = f (as!^i) (bs!^i) (cs!^i) (ds!^i)
        
        go 0 = False
        go i = let i' = i - 1 in apply i' || go i'
    
    any5 f as bs cs ds es = go (sizeOf as <?=> bs <?=> cs <?=> ds <?=> es)
      where
        apply i = f (as!^i) (bs!^i) (cs!^i) (ds!^i) (es!^i)
        
        go 0 = False
        go i = let i' = i - 1 in apply i' || go i'
    
    any6 f as bs cs ds es fs = go (sizeOf as <?=> bs <?=> cs <?=> ds <?=> es <?=> fs)
      where
        apply i = f (as!^i) (bs!^i) (cs!^i) (ds!^i) (es!^i) (fs!^i)
        
        go 0 = False
        go i = let i' = i - 1 in apply i' || go i'
    
    zipWith f as bs = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !^ i) (bs !^ i)
        sz = minimum [sizeOf as, sizeOf bs]
    
    zipWith3 f as bs cs = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !^ i) (bs !^ i) (cs !^ i)
        sz = minimum [sizeOf as, sizeOf bs, sizeOf cs]
    
    zipWith4 f as bs cs ds = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !^ i) (bs !^ i) (cs !^ i) (ds !^ i)
        sz = minimum [sizeOf as, sizeOf bs, sizeOf cs, sizeOf ds]
    
    zipWith5 f as bs cs ds es = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !^ i) (bs !^ i) (cs !^ i) (ds !^ i) (es !^ i)
        sz = minimum [sizeOf as, sizeOf bs, sizeOf cs, sizeOf ds, sizeOf es]
    
    zipWith6 f as bs cs ds es fs = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !^ i) (bs !^ i) (cs !^ i) (ds !^ i) (es !^ i) (fs !^ i)
        sz = minimum [sizeOf as, sizeOf bs, sizeOf cs, sizeOf ds, sizeOf es, sizeOf fs]

instance Applicative SArray#
  where
    pure = single
    
    fs@(SArray# fn _ _) <*> es@(SArray# en _ _) = runST $ do
      xs <- filled (fn * en) $ unreachEx "in SDP.Prim.SArray.(<*>) :: SArray# e"
      
      let
        go (-1)  _  _ = return ()
        go   i (-1) k = go (i - 1) (en - 1) k
        go   i   j  k = writeM xs k (fs!^i $ es!^j) >> go i (j - 1) (k - 1)
      
      go (fn - 1) (en - 1) (upper xs)
      done xs

--------------------------------------------------------------------------------

{- Foldable and Traversable instances. -}

instance Foldable SArray#
  where
    foldr  f base = \ arr ->
      let go i = arr .== i ? base $ f (arr !^ i) (go $ i + 1)
      in  go 0
    
    foldl  f base = \ arr ->
      let go i = -1 == i ? base $ f (go $ i - 1) (arr !^ i)
      in  go (sizeOf arr - 1)
    
    foldr' f base = \ arr ->
      let go i !a = -1 == i ? a $ go (i - 1) (f (arr !^ i) a)
      in  go (sizeOf arr - 1) base
    
    foldl' f base = \ arr ->
      let go i !a = arr .== i ? a $ go (i + 1) (f a $ arr !^ i)
      in  go 0 base
    
    foldr1 f = \ arr ->
      let go i = arr .== (i + 1) ? e $ f e (go $ i + 1) where e = arr !^ i
      in  null arr ? pfailEx "foldr1" $ go 0
    
    foldl1 f = \ arr ->
      let go i = 0 == i ? e $ f (go $ i - 1) e where e = arr !^ i
      in  null arr ? pfailEx "foldl1" $ go (sizeOf arr - 1)
    
    length = sizeOf
    null   = isNull

instance Traversable SArray#
  where
    traverse f es = fromListN (sizeOf es) <$> foldr (liftA2 (:) . f) (pure Z) es

--------------------------------------------------------------------------------

{- Forceable and Linear instances. -}

instance Forceable (SArray# e)
  where
    force (SArray# n@(I# n#) (I# o#) arr#) = runST $ ST $
      \ s1# -> case newArray# n# (unreachEx "force") s1# of
        (# s2#, marr# #) -> case copyArray# arr# o# marr# 0# n# s2# of
          s3# -> case unsafeFreezeArray# marr# s3# of
            (# s4#, copy# #) -> (# s4#, SArray# n 0 copy# #)

instance Linear (SArray# e) e
  where
    replicate n e = runST $ filled n e >>= done
    single      e = runST $ filled 1 e >>= done
    
    toHead e (SArray# (I# c#) (I# o#) arr#) = let n# = c# +# 1# in runST $ ST $
      \ s1# -> case newArray# n# e s1# of
        (# s2#, marr# #) -> case copyArray# arr# o# marr# 1# c# s2# of
          s3# -> case unsafeFreezeArray# marr# s3# of
            (# s4#, res# #) -> (# s4#, SArray# (I# n#) 0 res# #)
    
    toLast (SArray# (I# c#) (I# o#) arr#) e = let n# = c# +# 1# in runST $ ST $
      \ s1# -> case newArray# n# e s1# of
        (# s2#, marr# #) -> case copyArray# arr# o# marr# 0# c# s2# of
          s3# -> case unsafeFreezeArray# marr# s3# of
            (# s4#, res# #) -> (# s4#, SArray# (I# n#) 0 res# #)
    
    head es = es !^ 0
    last es@(SArray# c _ _) = es !^ (c - 1)
    init (SArray# c o arr#) = SArray# (max 1 c - 1) o arr#
    tail (SArray# c o arr#) = SArray# (max 1 c - 1) (o + 1) arr#
    
    fromList = fromFoldable
    
    fromListN  n es = runST $ newLinearN  n es >>= done
    fromFoldable es = runST $ fromFoldableM es >>= done
    
    listL = toList
    listR = flip (:) `foldl` []
    
    (!^) (SArray# _ (I# o#) arr#) = \ (I# i#) ->
      case indexArray# arr# (i# +# o#) of (# e #) -> e
    
    write es n e = not (indexIn es n) ? es $ runST $ do
      es' <- thaw es
      writeM es' n e
      done es'
    
    reverse es = runST $ do es' <- fromIndexed' es; reversed' es'; done es'
    
    -- [internal]: always return new array, even if only one is nonempty
    concat ess = runST $ do
      let n = foldr' ((+) . sizeOf) 0 ess
      marr@(STArray# _ _ marr#) <- filled n (unreachEx "concat")
      
      let
        writeBlock# (SArray# (I# c#) (I# o#) arr#) (I# i#) = ST $
          \ s1# -> (# copyArray# arr# o# marr# i# c# s1#, I# (i# +# c#) #)
      
      void $ foldl (\ b a -> writeBlock# a =<< b) (return 0) ess
      done marr
    
    before es@(SArray# c@(I# c#) (I# o#) arr#) n@(I# n#) e
      | n >= c = es :< e
      | n <= 0 = e :> es
      |  True  = runST $ ST $ \ s1# -> case newArray# (c# +# 1#) e s1# of
        (# s2#, marr# #) -> case copyArray# arr# o# marr# 0# n# s2# of
          s3# -> case copyArray# arr# (o# +# n#) marr# (n# +# 1#) (c# -# n#) s3# of
            s4# -> case unsafeFreezeArray# marr# s4# of
              (# s5#, res# #) -> (# s5#, SArray# (c + 1) 0 res# #)
    
    remove n@(I# n#) es@(SArray# c@(I# c#) (I# o#) arr#) = n < 0 || n >= c ? es $
      runST $ ST $ \ s1# -> case newArray# (c# -# 1#) (unreachEx "remove") s1# of
        (# s2#, marr# #) -> case copyArray# arr# o# marr# 0# n# s2# of
          s3# -> case copyArray# arr# (o# +# n# +# 1#) marr# n# (c# -# n# -# 1#) s3# of
            s4# -> case unsafeFreezeArray# marr# s4# of
              (# s5#, res# #) -> (# s5#, SArray# (c - 1) 0 res# #)
    
    ofoldr f base = \ arr@(SArray# c _ _) ->
      let go i = c == i ? base $ f i (arr !^ i) (go $ i + 1)
      in  go 0
    
    ofoldl f base = \ arr@(SArray# c _ _) ->
      let go i = -1 == i ? base $ f i (go $ i - 1) (arr !^ i)
      in  go (c - 1)
    
    sfoldr = foldr
    sfoldl = foldl
    
    -- | O(1) 'take', O(1) memory.
    take n es@(SArray# c o arr#)
      | n <= 0 = Z
      | n >= c = es
      |  True  = SArray# n o arr#
    
    -- | O(1) 'drop', O(1) memory.
    drop n es@(SArray# c o arr#)
      | n <= 0 = es
      | n >= c = Z
      |  True  = SArray# (c - n) (o + n) arr#
    
    -- | O(1) 'split', O(1) memory.
    split n es@(SArray# c o arr#)
      | n <= 0 = (Z, es)
      | n >= c = (es, Z)
      |  True  = (SArray# n o arr#, SArray# (c - n) (o + n) arr#)
    
    -- | O(1) 'keep', O(1) memory.
    keep n es@(SArray# c o arr#)
      | n <= 0 = Z
      | n >= c = es
      |  True  = SArray# n (o + c - n) arr#
    
    -- | O(1) 'sans', O(1) memory.
    sans n es@(SArray# c o arr#)
      | n <= 0 = es
      | n >= c = Z
      |  True  = SArray# (c - n) o arr#
    
    -- | O(1) 'divide', O(1) memory.
    divide n es@(SArray# c o arr#)
      | n <= 0 = (Z, es)
      | n >= c = (es, Z)
      |  True  = (SArray# n (o + c - n) arr#, SArray# (c - n) o arr#)
    
    splitsBy f es = dropWhileEnd f <$> f *$ es `parts` es
    
    padL n@(I# n#) e es@(SArray# c@(I# c#) (I# o#) src#) = case c <=> n of
      EQ -> es
      GT -> take n es
      LT -> runST $ ST $ \ s1# -> case newArray# n# e s1# of
        (# s2#, marr# #) -> case copyArray# src# o# marr# 0# c# s2# of
          s3# -> case unsafeFreezeArray# marr# s3# of
            (# s4#, arr# #) -> (# s4#, SArray# n 0 arr# #)
    
    padR n@(I# n#) e es@(SArray# c@(I# c#) (I# o#) src#) = case c <=> n of
      EQ -> es
      GT -> take n es
      LT -> runST $ ST $ \ s1# -> case newArray# n# e s1# of
        (# s2#, marr# #) -> case copyArray# src# o# marr# (n# -# c#) c# s2# of
          s3# -> case unsafeFreezeArray# marr# s3# of
            (# s4#, arr# #) -> (# s4#, SArray# n 0 arr# #)
    
    isPrefixOf xs@(SArray# c1 _ _) ys@(SArray# c2 _ _) =
      let eq i = i == c1 || (xs !^ i) == (ys !^ i) && eq (i + 1)
      in  c1 <= c2 && eq 0
    
    isSuffixOf xs@(SArray# c1 _ _) ys@(SArray# c2 _ _) =
      let eq i j = i == c1 || (xs !^ i) == (ys !^ j) && eq (i + 1) (j + 1)
      in  c1 <= c2 && eq 0 (c2 - c1)
    
    selectWhile f es@(SArray# c _ _) =
      let go i = i == c ? [] $ maybe [] (: go (i + 1)) $ f (es !^ i)
      in  go 0
    
    selectEnd g xs@(SArray# c _ _) =
      let go i es = i == 0 ? [] $ maybe [] (: go (i - 1) es) $ g (es !^ i)
      in  reverse $ go (c - 1) xs

--------------------------------------------------------------------------------

{- Set and SetWith instances. -}

instance Ord e => Set (SArray# e) e

instance SetWith (SArray# e) e
  where
    setWith f = nubSorted f . sortBy f
    
    insertWith f e es = case (\ x -> x `f` e /= LT) .$ es of
      Just i -> e `f` (es!^i) == EQ ? es $ before es i e
      _      -> es :< e
    
    deleteWith f e es = memberWith f e es ? except (\ x -> f e x == EQ) es $ es
    
    {-# INLINE intersectionWith #-}
    intersectionWith f xs@(SArray# n1 _ _) ys@(SArray# n2 _ _) = fromList $ go 0 0
      where
        go i j = i == n1 || j == n2 ? [] $ case x `f` y of
            EQ -> x : go (i + 1) (j + 1)
            LT -> go (i + 1) j
            GT -> go i (j + 1)
          where
            x = xs !^ i
            y = ys !^ j
    
    {-# INLINE unionWith #-}
    unionWith f xs@(SArray# n1 _ _) ys@(SArray# n2 _ _) = fromList $ go 0 0
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
    differenceWith f xs@(SArray# n1 _ _) ys@(SArray# n2 _ _) = fromList $ go 0 0
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
    symdiffWith f xs@(SArray# n1 _ _) ys@(SArray# n2 _ _) = fromList $ symdiff' 0 0
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
    
    lookupLTWith _ _ Z = Nothing
    lookupLTWith f o es
        | GT <- o `f` last' = Just last'
        | GT <- o `f` head' = look' head' 0 u'
        |       True        = Nothing
      where
        head' = es .! 0
        last' = es .! u'
        u' = upper es
        
        look' r l u = l > u ? Just r $ case o `f` e of
            EQ -> Just $ j < 1 ? r $ es !^ (j - 1)
            LT -> look' r l (j - 1)
            GT -> look' e (j + 1) u
          where
            j = l + (u - l) `div` 2
            e = es !^ j
    
    lookupLEWith _ _ Z = Nothing
    lookupLEWith f o es
        | GT <- o `f` last' = Just last'
        | LT <- o `f` head' = Nothing
        |       True        = look' head' 0 u'
      where
        head' = es .! 0
        last' = es .! u'
        u' = upper es
        
        look' r l u = l > u ? Just r $ case o `f` e of
            LT -> look' r l (j - 1)
            _  -> look' e (j + 1) u
          where
            j = l + (u - l) `div` 2
            e = es !^ j
    
    lookupGTWith _ _ Z = Nothing
    lookupGTWith f o es
        | LT <- o `f` head' = Just head'
        | LT <- o `f` last' = look' last' 0 u'
        |       True        = Nothing
      where
        head' = es .! 0
        last' = es .! u'
        u'    = upper es
        
        look' r l u = l > u ? Just r $ case o `f` e of
            LT -> look' e l (j - 1)
            EQ -> j >= u' ? Nothing $ Just (es !^ (j + 1))
            GT -> look' r (j + 1) u
          where
            j = l + (u - l) `div` 2
            e = es !^ j
    
    lookupGEWith _ _ Z = Nothing
    lookupGEWith f o es
        | GT <- o `f` last' = Nothing
        | GT <- o `f` head' = look' last' 0 u'
        |       True        = Just head'
      where
        head' = es .! 0
        last' = es .! u'
        u'    = upper es
        
        look' r l u = l > u ? Just r $ case o `f` e of
            LT -> look' e l (j - 1)
            EQ -> Just e
            GT -> look' r (j + 1) u
          where
            j = l + (u - l) `div` 2
            e = es !^ j

--------------------------------------------------------------------------------

{- Scan and Sort instances. -}

instance Scan (SArray# e) e

instance Sort (SArray# e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'
    sortedBy f es = all2 f es (tail es)

--------------------------------------------------------------------------------

{- Map and Indexed instances. -}

instance Map (SArray# e) Int e
  where
    toMap' e ascs =
      let bnds = rangeBounds (fsts ascs)
      in  isNull ascs ? Z $ assoc' bnds e ascs
    
    Z  // ascs = toMap ascs
    es // ascs = runST $ fromFoldableM es >>= (`overwrite` ascs) >>= done
    
    (*$) p = ofoldr (\ i e is -> p e ? (i : is) $ is) []
    (.!)   = (!^)
    
    kfoldr = ofoldr
    kfoldl = ofoldl

instance Indexed (SArray# e) Int e
  where
    assoc' bnds e ascs = runST $ fromAssocs' bnds e ascs >>= done
    
    fromIndexed es = runST $ do
      let n = sizeOf es
      copy <- filled n (unreachEx "fromIndexed")
      updateM copy (\ i _ -> es!^i) >>= done

--------------------------------------------------------------------------------

{- Thaw and Freeze instances. -}

instance Thaw (ST s) (SArray# e) (STArray# s e)
  where
    thaw (SArray# c@(I# c#) (I# o#) arr#) = ST $
      \ s1# -> case thawArray# arr# o# c# s1# of
        (# s2#, marr# #) -> (# s2#, STArray# c 0 marr# #)
    
    unsafeThaw (SArray# c o arr#) = ST $
      \ s1# -> case unsafeThawArray# arr# s1# of
        (# s2#, marr# #) -> (# s2#, STArray# c o marr# #)

instance Freeze (ST s) (STArray# s e) (SArray# e)
  where
    freeze (STArray# c@(I# c#) (I# o#) marr#) = ST $
      \ s1# -> case freezeArray# marr# o# c# s1# of
        (# s2#, arr# #) -> (# s2#, SArray# c 0 arr# #)
    
    unsafeFreeze = done

--------------------------------------------------------------------------------

-- | 'STArray#' is mutable preudo-primitive 'Int'-indexed lazy boxed array type.
data STArray# s e = STArray#
              {-# UNPACK #-} !Int  -- ^ Element count (not a real size)
              {-# UNPACK #-} !Int  -- ^ Offset (in elements)
              !(MutableArray# s e) -- ^ Real primitive array
  deriving ( Typeable )

type role STArray# nominal representational

--------------------------------------------------------------------------------

{- Eq instance. -}

instance Eq (STArray# s e)
  where
    STArray# c1 o1 marr1# == STArray# c2 o2 marr2# =
      let same = isTrue# (sameMutableArray# marr1# marr2#)
      in  c1 == c2 && (c1 == 0 || o1 == o2 && same)

--------------------------------------------------------------------------------

{- NullableM instance. -}

instance NullableM (ST s) (STArray# s e)
  where
    newNull = ST $ \ s1# -> case newArray# 0# (unreachEx "newNull") s1# of
      (# s2#, marr# #) -> (# s2#, STArray# 0 0 marr# #)
    
    nowNull (STArray# n _ _) = return (n < 1)

--------------------------------------------------------------------------------

{- Estimate and EstimateM instances. -}

instance Estimate (STArray# s e)
  where
    sizeOf (STArray# c _ _) = c
    
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

instance EstimateM (ST s) (STArray# s e)
  where
    getSizeOf (STArray# c _ _) = return c
    
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

instance Bordered (STArray# s e) Int
  where
    lower _ = 0
    
    upper    (STArray# c _ _) = c - 1
    bounds   (STArray# c _ _) = (0, c - 1)
    indices  (STArray# c _ _) = [0 .. c - 1]
    indexOf  (STArray# c _ _) = index (0, c - 1)
    offsetOf (STArray# c _ _) = offset (0, c - 1)
    indexIn  (STArray# c _ _) = \ i -> i >= 0 && i < c
    
    rebound bnds es@(STArray# c o arr#)
        | n < 0 = STArray# 0 0 arr#
        | n < c = STArray# n o arr#
        | True  = es
      where
        n = size bnds

instance BorderedM (ST s) (STArray# s e) Int
  where
    nowIndexIn (STArray# c _ _) = return . inRange (0, c - 1)
    getIndices (STArray# c _ _) = return [0 .. c - 1]
    getBounds  (STArray# c _ _) = return (0, c - 1)
    getUpper   (STArray# c _ _) = return (c - 1)
    
    getLower _ = return 0
    rebounded' = takeM . size

--------------------------------------------------------------------------------

{- Copyable and LinearM instances. -}

instance Copyable (ST s) (STArray# s e)
  where
    copied (STArray# n@(I# n#) (I# o#) marr#) = ST $
      \ s1# -> case cloneMutableArray# marr# o# n# s1# of
        (# s2#, copy# #) -> (# s2#, STArray# n 0 copy# #)

instance LinearM (ST s) (STArray# s e) e
  where
    getHead es = es >! 0
    getLast es = es >! upper es
    
    newLinear = fromFoldableM
    
    newLinearN c es = ST $ \ s1# -> case newArray# n# err s1# of
      (# s2#, marr# #) ->
        let go y r = \ i# s3# -> case writeArray# marr# i# y s3# of
              s4# -> if isTrue# (i# ==# n# -# 1#) then s4# else r (i# +# 1#) s4#
        in done' n marr# ( if n == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2# )
      where
        err = undEx "newLinearN"
        !n@(I# n#) = max 0 c
    
    fromFoldableM es = ST $ \ s1# -> case newArray# n# err s1# of
      (# s2#, marr# #) ->
        let go y r = \ i# s3# -> case writeArray# marr# i# y s3# of
              s4# -> if isTrue# (i# ==# n# -# 1#) then s4# else r (i# +# 1#) s4#
        in done' n marr# ( if n == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2# )
      where
        err = unreachEx "fromFoldableM"
        !n@(I# n#) = length es
    
    getLeft  es@(STArray# n _ _) = (es !#>) `mapM` [0 .. n - 1]
    getRight es@(STArray# n _ _) = (es !#>) `mapM` [n - 1, n - 2 .. 0]
    
    {-# INLINE (!#>) #-}
    (!#>) (STArray# _ (I# o#) marr#) = \ (I# i#) -> ST $ readArray# marr# (o# +# i#)
    
    writeM = writeM'
    
    copied' (STArray# c (I# o#) marr#) l@(I# l#) n@(I# n#)
      | l >= 0 && n >= 0 && c >= l = ST $
        \ s1# -> case cloneMutableArray# marr# (o# +# l#) n# s1# of
          (# s2#, copy# #) -> (# s2#, STArray# n 0 copy# #)
      | True = undEx "copied'"
    
    reversed  es = do es' <- copied es; reversed' es'; return es'
    reversed' es =
      let go i j = when (i < j) $ go (i + 1) (j - 1) >> swapM es i j
      in  go 0 (sizeOf es - 1)
    
    filled n e = let !n'@(I# n#) = max 0 n in ST $
      \ s1# -> case newArray# n# e s1# of
        (# s2#, marr# #) -> (# s2#, STArray# n' 0 marr# #)
    
    copyTo src sc trg tc n@(I# n#) = when (n > 0) $ do
        when      (sc < 0 || tc < 0)      $ underEx "copyTo"
        when (sc + n > n1 || tc + n > n2) $ overEx  "copyTo"
        ST $ \ s1# -> case copyMutableArray# src# so# trg# to# n# s1# of
          s2# -> (# s2#, () #)
      where
        !(STArray# n1 o1 src#) = src; !(I# so#) = o1 + sc
        !(STArray# n2 o2 trg#) = trg; !(I# to#) = o2 + tc
    
    merged ess = do
        marr <- filled n (unreachEx "merged")
        let writer arr@(STArray# c _ _) i = (i + c) <$ copyTo arr 0 marr i c
        
        void $ foldr ((=<<) . writer) (return 0) ess
        return marr
      where
        n = foldr' ((+) . sizeOf) 0 ess
    
    ofoldrM f base = \ arr@(STArray# n _ _) ->
      let go i =  n == i ? return base $ (arr !#> i) >>=<< go (i + 1) $ f i
      in  go 0
    
    ofoldlM f base = \ arr@(STArray# n _ _) ->
      let go i = -1 == i ? return base $ go (i - 1) >>=<< (arr !#> i) $ f i
      in  go (n - 1)
    
    foldrM f base = \ arr@(STArray# n _ _) ->
      let go i = n == i ? return base $ (arr !#> i) >>=<< go (i + 1) $ f
      in  go 0
    
    foldlM f base = \ arr@(STArray# n _ _) ->
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
    
    takeM n es@(STArray# c o marr#)
      | n <= 0 = newNull
      | n >= c = return es
      |  True  = return (STArray# n o marr#)
    
    dropM n es@(STArray# c o marr#)
      | n >= c = newNull
      | n <= 0 = return es
      |  True  = return (STArray# (c - n) (o + n) marr#)
    
    keepM n es@(STArray# c o marr#)
      | n <= 0 = newNull
      | n >= c = return es
      |  True  = return (STArray# n (c - n + o) marr#)
    
    sansM n es@(STArray# c o marr#)
      | n >= c = newNull
      | n <= 0 = return es
      |  True  = return (STArray# (c - n) o marr#)
    
    splitM n es@(STArray# c o marr#)
      | n <= 0 = do e' <- newNull; return (e', es)
      | n >= c = do e' <- newNull; return (es, e')
      |  True  = return (STArray# n o marr#, STArray# (c - n) (o + n) marr#)
    
    divideM n es@(STArray# c o marr#)
      | n <= 0 = do e' <- newNull; return (es, e')
      | n >= c = do e' <- newNull; return (e', es)
      |  True  = return (STArray# n (c - n + o) marr#, STArray# (c - n) o marr#)
    
    prefixM p es@(STArray# c _ _) =
      let go i = i >= c ? return c $ do e <- es !#> i; p e ? go (succ i) $ return i
      in  go 0
    
    suffixM p es@(STArray# c _ _) =
      let go i = i < 0 ? return c $ do e <- es !#> i; p e ? go (pred i) $ return (c - i - 1)
      in  go (max 0 (c - 1))
    
    mprefix p es@(STArray# c _ _) =
      let go i = i >= c ? return c $ do e <- es !#> i; p e ?^ go (succ 1) $ return i
      in  go 0
    
    msuffix p es@(STArray# c _ _) =
      let go i = i < 0 ? return c $ do e <- es !#> i; p e ?^ go (pred i) $ return (c - i - 1)
      in  go (max 0 (c - 1))

--------------------------------------------------------------------------------

{- MapM and IndexedM instances. -}

instance MapM (ST s) (STArray# s e) Int e
  where
    newMap' e ascs =
      let bnds = rangeBounds (fsts ascs)
      in  isNull ascs ? newNull $ fromAssocs' bnds e ascs
    
    {-# INLINE writeM' #-}
    writeM' (STArray# _ (I# o#) marr#) = \ (I# i#) e -> ST $
      \ s1# -> case writeArray# marr# (o# +# i#) e s1# of s2# -> (# s2#, () #)
    
    (>!) = (!#>)
    
    overwrite es@(STArray# c _ _) ascs =
      let ies = filter (inRange (0, c - 1) . fst) ascs
      in  mapM_ (uncurry $ writeM es) ies >> return es
    
    kfoldrM = ofoldrM
    kfoldlM = ofoldlM

instance IndexedM (ST s) (STArray# s e) Int e
  where
    fromAssocs' bnds e ascs = size bnds `filled` e >>= (`overwrite` ascs)
    
    fromIndexed' es = do
      copy <- filled (sizeOf es) (unreachEx "fromIndexed'")
      copy <$ ofoldr (\ i e go -> do writeM copy i e; go) (return ()) es
    
    fromIndexedM es = do
      copy <- flip filled (unreachEx "fromIndexedM") =<< getSizeOf es
      copy <$ ofoldrM (\ i e _ -> writeM copy i e) () es

--------------------------------------------------------------------------------

{- SortM instance. -}

instance SortM (ST s) (STArray# s e) e
  where
    sortedMBy f es@(STArray# n _ _) =
      let go i e1 = i == n ? return True $ do e2 <- es !#> i; e1 `f` e2 ? go (i + 1) e2 $ return False
      in  n < 2 ? return True $ go 1 =<< getHead es
    
    sortMBy = timSortBy

--------------------------------------------------------------------------------

-- | 'MIOArray#' is mutable preudo-primitive 'Int'-indexed lazy boxed array.
newtype MIOArray# (io :: Type -> Type) e = MIOArray# (STArray# RealWorld e)
  deriving ( Eq )

-- | 'IOArray#' is mutable preudo-primitive 'Int'-indexed lazy boxed array.
type IOArray# = MIOArray# IO

--------------------------------------------------------------------------------

{- NullableM instance. -}

instance MonadIO io => NullableM io (MIOArray# io e)
  where
    newNull = pack newNull
    nowNull = stToMIO . nowNull . unpack

--------------------------------------------------------------------------------

{- Estimate and EstimateM instances. -}

instance Estimate (MIOArray# io e)
  where
    sizeOf (MIOArray# (STArray# c _ _)) = c
    
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

instance MonadIO io => EstimateM io (MIOArray# io e)
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

instance Bordered (MIOArray# io e) Int
  where
    lower _ = 0
    rebound bnds (MIOArray# es) = MIOArray# (rebound bnds es)
    
    upper    (MIOArray# (STArray# c _ _)) = c - 1
    bounds   (MIOArray# (STArray# c _ _)) = (0, c - 1)
    indices  (MIOArray# (STArray# c _ _)) = [0 .. c - 1]
    indexOf  (MIOArray# (STArray# c _ _)) = index (0, c - 1)
    offsetOf (MIOArray# (STArray# c _ _)) = offset (0, c - 1)
    indexIn  (MIOArray# (STArray# c _ _)) = \ i -> i >= 0 && i < c

instance MonadIO io => BorderedM io (MIOArray# io e) Int
  where
    getIndexOf = return ... indexOf . unpack
    getIndices = return . indices . unpack
    getBounds  = return . bounds . unpack
    getUpper   = return . upper . unpack
    
    rebounded' = takeM . size
    getLower _ = return 0

--------------------------------------------------------------------------------

{- Copyable and LinearM instances. -}

instance MonadIO io => Copyable io (MIOArray# io e)
  where
    copied = pack . copied . unpack

instance MonadIO io => LinearM io (MIOArray# io e) e
  where
    singleM = pack . singleM
    getHead = stToMIO . getHead . unpack
    getLast = stToMIO . getLast . unpack
    
    prepend e = pack . prepend e . unpack
    append es = pack . append (unpack es)
    
    newLinear     = pack . newLinear
    newLinearN    = pack ... newLinearN
    fromFoldableM = pack . fromFoldableM
    
    writeM = writeM'
    (!#>)  = stToMIO ... (!#>) . unpack
    
    reversed  = pack . reversed . unpack
    getLeft   = stToMIO . getLeft  . unpack
    getRight  = stToMIO . getRight . unpack
    reversed' = stToMIO . reversed' . unpack
    
    copied' es = pack ... copied' (unpack es)
    
    filled = pack ... filled
    merged = pack . merged . foldr ((:) . unpack) []
    
    copyTo src so trg to = stToMIO . copyTo (unpack src) so (unpack trg) to
    
    ofoldrM f base = \ arr@(MIOArray# (STArray# n _ _)) ->
      let go i =  n == i ? return base $ (arr !#> i) >>=<< go (i + 1) $ f i
      in  go 0
    
    ofoldlM f base = \ arr@(MIOArray# (STArray# n _ _)) ->
      let go i = -1 == i ? return base $ go (i - 1) >>=<< (arr !#> i) $ f i
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
    
    mprefix p es@(MIOArray# (STArray# c _ _)) =
      let go i = i >= c ? return c $ do e <- es !#> i; p e ?^ go (succ 1) $ return i
      in  go 0
    
    msuffix p es@(MIOArray# (STArray# c _ _)) =
      let go i = i < 0 ? return c $ do e <- es !#> i; p e ?^ go (pred i) $ return (c - i - 1)
      in  go (max 0 (c - 1))

--------------------------------------------------------------------------------

{- MapM and IndexedM instances. -}

instance MonadIO io => MapM io (MIOArray# io e) Int e
  where
    newMap' e ascs =
      let bnds = rangeBounds (fsts ascs)
      in  isNull ascs ? newNull $ fromAssocs' bnds e ascs
    
    writeM' es = stToMIO ... writeM' (unpack es)
    
    (>!) = (!#>)
    
    overwrite = pack ... overwrite . unpack
    kfoldrM   = ofoldrM
    kfoldlM   = ofoldlM

instance MonadIO io => IndexedM io (MIOArray# io e) Int e
  where
    fromAssocs  bnds = pack  .  fromAssocs  bnds
    fromAssocs' bnds = pack ... fromAssocs' bnds
    
    fromIndexed' = pack . fromIndexed'
    
    fromIndexedM es = do
      copy <- flip filled (unreachEx "fromIndexedM") =<< getSizeOf es
      copy <$ ofoldrM (\ i e _ -> writeM copy i e) () es

--------------------------------------------------------------------------------

{- SortM instance. -}

instance MonadIO io => SortM io (MIOArray# io e) e
  where
    sortedMBy f = stToMIO . sortedMBy f . unpack
    sortMBy     = timSortBy

--------------------------------------------------------------------------------

{- Thaw and Freeze instances. -}

instance MonadIO io => Thaw io (SArray# e) (MIOArray# io e)
  where
    unsafeThaw = pack . unsafeThaw
    thaw       = pack . thaw

instance MonadIO io => Freeze io (MIOArray# io e) (SArray# e)
  where
    unsafeFreeze = stToMIO . unsafeFreeze . unpack
    freeze       = stToMIO . freeze . unpack

--------------------------------------------------------------------------------

{- Thaw and Freeze instances. -}

instance Storable e => Thaw IO (SArray# e) (Int, Ptr e)
  where
    thaw es = do
      let n = sizeOf es
      ptr <- callocArray n
      (n, ptr) <$ ofoldr (\ i e go -> do pokeElemOff ptr i e; go) (return ()) es

instance Storable e => Freeze IO (Int, Ptr e) (SArray# e)
  where
    freeze (c, ptr) = do
      let
        err = unreachEx "freeze {(Int, Ptr e) => SArray# e}"
        
        filled' :: proxy e -> e -> IO (MIOArray# IO e)
        filled' =  const $ filled (max 0 c)
      
      es <- filled' ptr err
      freeze =<< mupdate es (\ i _ -> peekElemOff ptr i)

--------------------------------------------------------------------------------

{- Primitive operations on SArray# and and STArray#. -}

-- | 'unpackSArray#' returns 'MutableArray#' field of 'SArray#'.
unpackSArray# :: SArray# e -> Array# e
unpackSArray# =  \ (SArray# _ _ arr#) -> arr#

-- | 'offsetSArray#' returns 'SArray#' offset in elements.
offsetSArray# :: SArray# e -> Int#
offsetSArray# =  \ (SArray# _ (I# o#) _) -> o#

-- | 'packSArray#' creates new 'SArray#' from sized 'Array#'.
packSArray# :: Int -> Array# e -> SArray# e
packSArray# n arr# = SArray# (max 0 n) 0 arr#

-- | 'fromSArray#' returns new 'Array#' (uses 'cloneArray#').
fromSArray# :: SArray# e -> Array# e
fromSArray# (SArray# (I# c#) (I# o#) arr#) = cloneArray# arr# o# c#

-- | 'coerceSArray#' is 'coerce' alias.
coerceSArray# :: (Coercible a b) => SArray# a -> SArray# b
coerceSArray# =  coerce

-- | 'unpackSTArray#' returns 'MutableArray#' field of 'STArray#' or fails.
unpackSTArray# :: STArray# s e -> MutableArray# s e
unpackSTArray# =  \ (STArray# _ _ marr#) -> marr#

-- | 'offsetSTArray#' returns 'STArray#' offset in elements.
offsetSTArray# :: STArray# s e -> Int#
offsetSTArray# =  \ (STArray# _ (I# o#) _) -> o#

-- | 'packSTArray#' creates new 'STArray#' from sized 'MutableArray#'.
packSTArray# :: Int -> MutableArray# s e -> STArray# s e
packSTArray# n marr# = STArray# (max 0 n) 0 marr#

-- | 'fromSTArray#' returns new 'MutableArray#'.
fromSTArray# :: STArray# s e -> State# s -> (# State# s, MutableArray# s e #)
fromSTArray# (STArray# (I# c#) (I# o#) marr#) = cloneMutableArray# marr# o# c#

-- | 'coerceSTArray#' is 'coerce' alias.
coerceSTArray# :: Coercible a b => STArray# s a -> STArray# s b
coerceSTArray# =  coerce

--------------------------------------------------------------------------------

{-# INLINE unpack #-}
unpack :: MIOArray# io e -> STArray# RealWorld e
unpack =  coerce

{-# INLINE pack #-}
pack :: (MonadIO io) => ST RealWorld (STArray# RealWorld e) -> io (MIOArray# io e)
pack =  stToMIO . coerce

{-# INLINE done #-}
done :: STArray# s e -> ST s (SArray# e)
done (STArray# n o marr#) = ST $ \ s1# -> case unsafeFreezeArray# marr# s1# of
  (# s2#, arr# #) -> (# s2#, SArray# n o arr# #)

{-# INLINE done' #-}
done' :: Int -> MutableArray# s e -> STRep s (STArray# s e)
done' n marr# = \ s1# -> (# s1#, STArray# n 0 marr# #)

{-# INLINE nubSorted #-}
nubSorted :: Compare e -> SArray# e -> SArray# e
nubSorted _ Z  = Z
nubSorted f es = fromList $ foldr fun [last es] ((es !^) <$> [0 .. sizeOf es - 2])
  where
    fun = \ e ls -> e `f` head ls == EQ ? ls $ e : ls

(<?=>) :: Bordered b i => Int -> b -> Int
(<?=>) =  (. sizeOf) . min

--------------------------------------------------------------------------------

undEx :: String -> a
undEx =  throw . UndefinedValue . showString "in SDP.Prim.SArray."

overEx :: String -> a
overEx =  throw . IndexOverflow . showString "in SDP.Prim.SArray."

underEx :: String -> a
underEx =  throw . IndexUnderflow . showString "in SDP.Prim.SArray."

pfailEx :: String -> a
pfailEx =  throw . PatternMatchFail . showString "in SDP.Prim.SArray."

unreachEx :: String -> a
unreachEx =  throw . UnreachableException . showString "in SDP.Prim.SArray."




