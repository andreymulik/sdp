{-# LANGUAGE Trustworthy, MagicHash, UnboxedTuples, BangPatterns, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, RoleAnnotations #-}

{- |
    Module      :  SDP.Prim.SArray
    Copyright   :  (c) Andrey Mulik 2019-2025
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Prim.SArray" provides boxed pseudo-primitive sized arrays.
-}
module SDP.Prim.SArray
(
  -- * Exports
  module SDP.Indexed,
  module SDP.Sort,
  
  -- * Pseudo-primitive types
  SArray#, fromSArray#, unpackSArray#, offsetSArray#, packSArray#, coerceSArray#
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Prim.SArray.ST
import SDP.Indexed
import SDP.Sort
import SDP.Scan

import qualified GHC.Exts as E
import GHC.Exts
  (
    Array#, Int#, newArray#, indexArray#, writeArray#, copyArray#, cloneArray#,
    freezeArray#, unsafeFreezeArray#, thawArray#, unsafeThawArray#,
    andI#, (+#), (-#), (>=#), (<#)
  )

import GHC.Types
import GHC.ST ( ST (..) )

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

type role SArray# representational

--------------------------------------------------------------------------------

{- Eq and Eq1 instances. -}

instance Eq e => Eq (SArray# e) where (==) = eq1

instance Eq1 SArray#
  where
    liftEq eq xs@(SArray# c1 _ _) ys@(SArray# c2 _ _) =
      let eq' i = i == c1 || eq (xs !! i) (ys !! i) && eq' (i + 1)
      in  c1 == c2 && eq' 0

--------------------------------------------------------------------------------

{- Ord and Ord1 instances. -}

instance Ord e => Ord (SArray# e) where compare = compare1

instance Ord1 SArray#
  where
    liftCompare f xs@(SArray# c1 _ _) ys@(SArray# c2 _ _) =
      let f' i = i == (c1`min`c2) ? c1 <=> c2 $ (xs!!i) `f` (ys!!i) <> f' (i+1)
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

{- Semigroup and Monoid instances. -}

instance Monoid (SArray# e) where mempty = Z; mappend = (<>)

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
    lzero  = runST $ newNull >>= done
    isNull = \ (SArray# c _ _) -> c == 0

instance Monad m => NullableM m (SArray# e)
  where
    newNull = return lzero
    isNullM = return . isNull

--------------------------------------------------------------------------------

{- Estimate and EstimateM instances. -}

instance Estimate (SArray# e)
  where
    sizeHint (SArray# c _ _) = Just (SizeHintEQ c)
    sizeOf   (SArray# c _ _) = c
    
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

instance Monad m => EstimateM m (SArray# e)
  where
    getSizeHint (SArray# c _ _) = return $ Just (SizeHintEQ c)
    getSizeOf   (SArray# c _ _) = return c
    
    estimateMGE = return ... (.>=.)
    estimateMLE = return ... (.<=.)
    estimateMGT = return ... (.>.)
    estimateMLT = return ... (.<.)
    estimateMNE = return ... (./=.)
    estimateMEQ = return ... (.==.)
    
    notShorterThanM = return ... (.>=)
    noLongerThanM   = return ... (.<=)
    longerThanM     = return ... (.>)
    shorterThanM    = return ... (.<)
    otherLengthM    = return ... (./=)
    hasLengthM      = return ... (.==)
    
    (<<=>>) = return ... (<==>)
    (<=>>)  = return ... (<.=>)

--------------------------------------------------------------------------------

{- Bordered and BorderedM instances. -}

instance Bordered (SArray# e) Int
  where
    lower                  _ = 0
    upper    (SArray# c _ _) = c - 1
    bounds   (SArray# c _ _) = (0, c - 1)
    indices  (SArray# c _ _) = [0 .. c - 1]
    indexOf  (SArray# c _ _) = index (0, c - 1)
    offsetOf (SArray# c _ _) = offset (0, c - 1)
    indexIn  (SArray# c _ _) = \ i -> i >= 0 && i < c
    
    eitherViewOf bnds@(l, _) es
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
        
        c = sizeOf es
        n = size bnds

instance Monad m => BorderedM m (SArray# e) Int
  where
    nowIndexIn (SArray# c _ _) = return . inRange (0, c - 1)
    getIndices (SArray# c _ _) = return [0 .. c - 1]
    getBounds  (SArray# c _ _) = return (0, c - 1)
    getUpper   (SArray# c _ _) = return (c - 1)
    getLower                 _ = return 0
    
    getEitherViewOf = pure ... eitherViewOf

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
        apply i = f (as!!i) (bs!!i)
        
        go 0 = True
        go i = let i' = i - 1 in apply i' && go i'
    
    all3 f as bs cs = go (sizeOf as <?=> bs <?=> cs)
      where
        apply i = f (as!!i) (bs!!i) (cs!!i)
        
        go 0 = True
        go i = let i' = i - 1 in apply i' && go i'
    
    all4 f as bs cs ds = go (sizeOf as <?=> bs <?=> cs <?=> ds)
      where
        apply i = f (as!!i) (bs!!i) (cs!!i) (ds!!i)

        go 0 = True
        go i = let i' = i - 1 in apply i' && go i'
    
    all5 f as bs cs ds es = go (sizeOf as <?=> bs <?=> cs <?=> ds <?=> es)
      where
        apply i = f (as!!i) (bs!!i) (cs!!i) (ds!!i) (es!!i)
        
        go 0 = True
        go i = let i' = i - 1 in apply i' && go i'
    
    all6 f as bs cs ds es fs = go (sizeOf as <?=> bs <?=> cs <?=> ds <?=> es <?=> fs)
      where
        apply i = f (as!!i) (bs!!i) (cs!!i) (ds!!i) (es!!i) (fs!!i)
        
        go 0 = True
        go i = let i' = i - 1 in apply i' && go i'
    
    any2 f as bs = go (sizeOf as <?=> bs)
      where
        apply i = f (as!!i) (bs!!i)
        
        go 0 = False
        go i = let i' = i - 1 in apply i' || go i'
    
    any3 f as bs cs = go (sizeOf as <?=> bs <?=> cs)
      where
        apply i = f (as!!i) (bs!!i) (cs!!i)
        
        go 0 = False
        go i = let i' = i - 1 in apply i' || go i'
    
    any4 f as bs cs ds = go (sizeOf as <?=> bs <?=> cs <?=> ds)
      where
        apply i = f (as!!i) (bs!!i) (cs!!i) (ds!!i)
        
        go 0 = False
        go i = let i' = i - 1 in apply i' || go i'
    
    any5 f as bs cs ds es = go (sizeOf as <?=> bs <?=> cs <?=> ds <?=> es)
      where
        apply i = f (as!!i) (bs!!i) (cs!!i) (ds!!i) (es!!i)
        
        go 0 = False
        go i = let i' = i - 1 in apply i' || go i'
    
    any6 f as bs cs ds es fs = go (sizeOf as <?=> bs <?=> cs <?=> ds <?=> es <?=> fs)
      where
        apply i = f (as!!i) (bs!!i) (cs!!i) (ds!!i) (es!!i) (fs!!i)
        
        go 0 = False
        go i = let i' = i - 1 in apply i' || go i'
    
    zipWith f as bs = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !! i) (bs !! i)
        sz = minimum [sizeOf as, sizeOf bs]
    
    zipWith3 f as bs cs = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !! i) (bs !! i) (cs !! i)
        sz = minimum [sizeOf as, sizeOf bs, sizeOf cs]
    
    zipWith4 f as bs cs ds = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !! i) (bs !! i) (cs !! i) (ds !! i)
        sz = minimum [sizeOf as, sizeOf bs, sizeOf cs, sizeOf ds]
    
    zipWith5 f as bs cs ds es = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !! i) (bs !! i) (cs !! i) (ds !! i) (es !! i)
        sz = minimum [sizeOf as, sizeOf bs, sizeOf cs, sizeOf ds, sizeOf es]
    
    zipWith6 f as bs cs ds es fs = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !! i) (bs !! i) (cs !! i) (ds !! i) (es !! i) (fs !! i)
        sz = minimum [sizeOf as, sizeOf bs, sizeOf cs, sizeOf ds, sizeOf es, sizeOf fs]

instance Applicative SArray#
  where
    pure = single
    
    fs@(SArray# fn _ _) <*> es@(SArray# en _ _) = runST $ do
      xs <- mreplicate (fn * en) $ unreachEx "in SDP.Prim.SArray.(<*>) :: SArray# e"
      
      let
        go (-1)  _  _ = return ()
        go   i (-1) k = go (i - 1) (en - 1) k
        go   i   j  k = unsafeWriteM xs k (fs!!i $ es!!j) >> go i (j - 1) (k - 1)
      
      go (fn - 1) (en - 1) (upper xs)
      done xs

--------------------------------------------------------------------------------

{- Foldable and Traversable instances. -}

instance Foldable SArray#
  where
    foldr  f base = \ arr ->
      let go i = arr .== i ? base $ f (arr !! i) (go $ i + 1)
      in  go 0
    
    foldl  f base = \ arr ->
      let go i = -1 == i ? base $ f (go $ i - 1) (arr !! i)
      in  go (sizeOf arr - 1)
    
    foldr' f base = \ arr ->
      let go i !a = -1 == i ? a $ go (i - 1) (f (arr !! i) a)
      in  go (sizeOf arr - 1) base
    
    foldl' f base = \ arr ->
      let go i !a = arr .== i ? a $ go (i + 1) (f a $ arr !! i)
      in  go 0 base
    
    foldr1 f = \ arr ->
      let go i = arr .== (i + 1) ? e $ f e (go $ i + 1) where e = arr !! i
      in  null arr ? pfailEx "foldr1" $ go 0
    
    foldl1 f = \ arr ->
      let go i = 0 == i ? e $ f (go $ i - 1) e where e = arr !! i
      in  null arr ? pfailEx "foldl1" $ go (sizeOf arr - 1)
    
    length = sizeOf
    null   = isNull

instance Traversable SArray#
  where
    traverse f es = fromListN (sizeOf es) <$> foldr (liftA2 (:) . f) (pure Z) es

--------------------------------------------------------------------------------

{- Forceable instance. -}

instance Forceable (SArray# e)
  where
    force (SArray# n@(I# n#) (I# o#) arr#) = runST $ ST $
      \ s1# -> case newArray# n# (unreachEx "force") s1# of
        (# s2#, marr# #) -> case copyArray# arr# o# marr# 0# n# s2# of
          s3# -> case unsafeFreezeArray# marr# s3# of
            (# s4#, copy# #) -> (# s4#, SArray# n 0 copy# #)

--------------------------------------------------------------------------------

{- Concat instance. -}
-- TODO: create buffer type and implement
instance Concat (SArray# e)

--------------------------------------------------------------------------------

{- Sequence and Linear instances. -}

instance Sequence (SArray# e) e
  where
    ofoldr f base = \ arr@(SArray# c _ _) ->
      let go i = c == i ? base $ f i (arr !! i) (go $ i + 1)
      in  go 0
    
    ofoldl f base = \ arr@(SArray# c _ _) ->
      let go i = -1 == i ? base $ f i (go $ i - 1) (arr !! i)
      in  go (c - 1)
    
    sfoldr  = foldr
    sfoldl  = foldl
    sfoldr' = foldr'
    sfoldl' = foldl'
    
    listL = E.toList
    listR = flip (:) `foldl` []

instance Linear (SArray# e) e
  where
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
    
    uncons'    (SArray# 0 _    _) = Nothing
    uncons' es@(SArray# n o arr#) = Just (es !! 0, SArray# (n - 1) (o + 1) arr#)
    
    unsnoc'    (SArray# 0 _    _) = Nothing
    unsnoc' es@(SArray# n o arr#) = Just (SArray# (n - 1) o arr#, es !! (n - 1))
    
    head es = null es ? undEx "head" $ es !! 0
    last es = null es ? undEx "last" $ es !! (sizeOf es - 1)
    
    init (SArray# c o arr#) = c < 1 ? undEx "init" $ SArray# (c - 1) o arr#
    tail (SArray# c o arr#) = c < 1 ? undEx "tail" $ SArray# (c - 1) (o + 1) arr#
    
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
      | n <= 0 = (es, Z)
      | n >= c = (Z, es)
      |  True  = (SArray# (c - n) o arr#, SArray# n (o + c - n) arr#)
    
    single = replicate 1
    
    fromList     es = runST $ newLinear     es >>= done
    fromListN  n es = runST $ newLinearN  n es >>= done
    fromFoldable es = runST $ fromFoldableM es >>= done
    replicate  n  e = runST $ mreplicate  n  e >>= done
    
    reverse es = runST $ do es' <- thaw es; reversed es'; done es'
    
    splitsBy f es = dropWhileEnd f <$> f *$ es `parts` es
    
    isPrefixOf sub line = sub == take (sizeOf sub) line
    isSuffixOf sub line = sub == keep (sizeOf sub) line
    
    pad (Left n@(I# n#)) e es@(SArray# c@(I# c#) (I# o#) src#) = case c <=> n of
      EQ -> es
      GT -> take n es
      LT -> runST $ ST $ \ s1# -> case newArray# n# e s1# of
        (# s2#, marr# #) -> case copyArray# src# o# marr# 0# c# s2# of
          s3# -> case unsafeFreezeArray# marr# s3# of
            (# s4#, arr# #) -> (# s4#, SArray# n 0 arr# #)
    
    pad (Right n@(I# n#)) e es@(SArray# c@(I# c#) (I# o#) src#) = case c <=> n of
      EQ -> es
      GT -> take n es
      LT -> runST $ ST $ \ s1# -> case newArray# n# e s1# of
        (# s2#, marr# #) -> case copyArray# src# o# marr# (n# -# c#) c# s2# of
          s3# -> case unsafeFreezeArray# marr# s3# of
            (# s4#, arr# #) -> (# s4#, SArray# n 0 arr# #)
    
    SArray# (I# n#) (I# o#) arr# !! I# i# = case andI# (i# >=# 0#) (i# <# n#) of
      1# -> case indexArray# arr# (i# +# o#) of (# e #) -> e
      _  -> undEx "!!"
    
    write es n e = not (indexIn es n) ? es $ runST $ do
      es' <- thaw es
      unsafeWriteM es' n e
      done es'
    
    remove n@(I# n#) es@(SArray# c@(I# c#) (I# o#) arr#) = n < 0 || n >= c ? es $
      runST $ ST $ \ s1# -> case newArray# (c# -# 1#) (unreachEx "remove") s1# of
        (# s2#, marr# #) -> case copyArray# arr# o# marr# 0# n# s2# of
          s3# -> case copyArray# arr# (o# +# n# +# 1#) marr# n# (c# -# n# -# 1#) s3# of
            s4# -> case unsafeFreezeArray# marr# s4# of
              (# s5#, res# #) -> (# s5#, SArray# (c - 1) 0 res# #)

--------------------------------------------------------------------------------

{- Set and SetWith instances. -}

instance Ord e => Set (SArray# e) e

instance SetWith (SArray# e) e
  where
    setWith f es = case sortBy f es of
        (se :< s) -> fromList $ toList $ foldr fun (s :| []) se
        _         -> Z
      where
        fun e ls@(l :| _) = f e l == EQ ? ls $ e <| ls
    
    insertWith f e es@(SArray# c@(I# c#) (I# o#) arr#) = case g .$ es of
        Nothing -> es :< e
        
        Just n@(I# n#)
          | exists n -> es
          |  n >= c  -> es :< e
          |  n <= 0  -> e :> es
          |   True   -> runST $ ST $ \ s1# -> case newArray# (c# +# 1#) e s1# of
            (# s2#, marr# #) -> case copyArray# arr# o# marr# 0# n# s2# of
              s3# -> case copyArray# arr# (o# +# n#) marr# (n# +# 1#) (c# -# n#) s3# of
                s4# -> case unsafeFreezeArray# marr# s4# of
                  (# s5#, res# #) -> (# s5#, SArray# (c + 1) 0 res# #)
      where
        exists n = e `f` (es!!n) == EQ
        
        g x = x `f` e /= LT
    
    deleteWith f e es = memberWith f e es ? except (\ x -> f e x == EQ) es $ es
    
    {-# INLINE intersectionWith #-}
    intersectionWith f xs@(SArray# n1 _ _) ys@(SArray# n2 _ _) = fromList $ go 0 0
      where
        go i j = i == n1 || j == n2 ? [] $ case x `f` y of
            EQ -> x : go (i + 1) (j + 1)
            LT -> go (i + 1) j
            GT -> go i (j + 1)
          where
            x = xs !! i
            y = ys !! j
    
    {-# INLINE unionWith #-}
    unionWith f xs@(SArray# n1 _ _) ys@(SArray# n2 _ _) = fromList $ go 0 0
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
    
    {-# INLINE differenceWith #-}
    differenceWith f xs@(SArray# n1 _ _) ys@(SArray# n2 _ _) = fromList $ go 0 0
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
    
    {-# INLINE symdiffWith #-}
    symdiffWith f xs@(SArray# n1 _ _) ys@(SArray# n2 _ _) = fromList $ symdiff' 0 0
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
    memberWith f e es
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
    
    lookupLTWith _ _ Z = Nothing
    lookupLTWith f o es
        | GT <- o `f` last' = Just last'
        | GT <- o `f` head' = look' head' 0 u'
        |       True        = Nothing
      where
        head' = unsafeReadByKey es 0
        last' = unsafeReadByKey es u'
        u' = upper es
        
        look' r l u = l > u ? Just r $ case o `f` e of
            EQ -> Just $ j < 1 ? r $ es !! (j - 1)
            LT -> look' r l (j - 1)
            GT -> look' e (j + 1) u
          where
            j = l + (u - l) `div` 2
            e = es !! j
    
    lookupLEWith _ _ Z = Nothing
    lookupLEWith f o es
        | GT <- o `f` last' = Just last'
        | LT <- o `f` head' = Nothing
        |       True        = look' head' 0 u'
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
    
    lookupGTWith _ _ Z = Nothing
    lookupGTWith f o es
        | LT <- o `f` head' = Just head'
        | LT <- o `f` last' = look' last' 0 u'
        |       True        = Nothing
      where
        head' = unsafeReadByKey es 0
        last' = unsafeReadByKey es u'
        
        u' = upper es
        
        look' r l u = l > u ? Just r $ case o `f` e of
            LT -> look' e l (j - 1)
            EQ -> j >= u' ? Nothing $ Just (es !! (j + 1))
            GT -> look' r (j + 1) u
          where
            j = l + (u - l) `div` 2
            e = es !! j
    
    lookupGEWith _ _ Z = Nothing
    lookupGEWith f o es
        | GT <- o `f` last' = Nothing
        | GT <- o `f` head' = look' last' 0 u'
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

--------------------------------------------------------------------------------

{- Scan and Sort instances. -}

instance Scan (SArray# e) e

instance Sort (SArray# e) e
  where
    sortedBy f es = case uncons' es of {Just (_, ts) -> all2 f es ts; _ -> True}
    sortBy cmp es = runST $ do es' <- thaw es; sortMBy cmp es'; done es'

--------------------------------------------------------------------------------

{- Map and Indexed instances. -}

instance Map (SArray# e) Int e
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

instance Indexed (SArray# e) Int e
  where
    assoc' e bnds ascs = runST $ fromAssocs' bnds e ascs >>= done
    
    fromIndexed es = runST $ do
      let n = sizeOf es
      copy <- mreplicate n (unreachEx "fromIndexed")
      updateM copy (\ i _ -> es!!i)
      done copy

--------------------------------------------------------------------------------

{- Thaw and Freeze instances. -}

instance Thaw (ST s) (SArray# e) (STArray# s e)
  where
    thaw (SArray# c@(I# c#) (I# o#) arr#) = ST $
      \ s1# -> case thawArray# arr# o# c# s1# of
        (# s2#, marr# #) -> (# s2#, packSTArray# c 0 marr# #)
    
    unsafeThaw (SArray# c o arr#) = ST $
      \ s1# -> case unsafeThawArray# arr# s1# of
        (# s2#, marr# #) -> (# s2#, packSTArray# c o marr# #)

instance Freeze (ST s) (STArray# s e) (SArray# e)
  where
    freeze es = ST $ \ s1# -> case freezeArray# marr# o# c# s1# of
        (# s2#, arr# #) -> (# s2#, packSArray# (I# c#) 0 arr# #)
      where
        marr# = unpackSTArray# es
        o#    = offsetSTArray# es
        
        !(I# c#) = sizeOf es
    
    unsafeFreeze es = ST $ \ s1# -> case unsafeFreezeArray# marr# s1# of
        (# s2#, arr# #) -> (# s2#, SArray# (sizeOf es) (I# o#) arr# #)
      where
        marr# = unpackSTArray# es
        o#    = offsetSTArray# es

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
    freeze (n, ptr) = do
        es <- mreplicate' ptr err
        forM_ [1 .. n] $ \ i -> stToMIO . unsafeWriteM es i =<< peekElemOff ptr i
        stToMIO (freeze es)
      where
        mreplicate' :: proxy e -> e -> IO (STArray# RealWorld e)
        mreplicate' =  const $ stToMIO . mreplicate n
        
        err = unreachEx "freeze {(Int, Ptr e) => SArray# e}"

--------------------------------------------------------------------------------

{- Primitive operations on SArray#. -}

-- | 'unpackSArray#' returns 'MutableArray#' field of 'SArray#'.
unpackSArray# :: SArray# e -> Array# e
unpackSArray# =  \ (SArray# _ _ arr#) -> arr#

-- | 'offsetSArray#' returns 'SArray#' offset in elements.
offsetSArray# :: SArray# e -> Int#
offsetSArray# =  \ (SArray# _ (I# o#) _) -> o#

-- | 'packSArray#' creates new 'SArray#' from sized 'Array#'.
packSArray# :: Int -> Int -> Array# e -> SArray# e
packSArray# n o arr# = SArray# (max 0 n) (max 0 o) arr#

-- | 'fromSArray#' returns new 'Array#' (uses 'cloneArray#').
fromSArray# :: SArray# e -> Array# e
fromSArray# (SArray# (I# c#) (I# o#) arr#) = cloneArray# arr# o# c#

-- | 'coerceSArray#' is 'coerce' alias.
coerceSArray# :: Coercible a b => SArray# a -> SArray# b
coerceSArray# =  coerce

--------------------------------------------------------------------------------

{-# INLINE done #-}
done :: STArray# s e -> ST s (SArray# e)
done =  unsafeFreeze

(<?=>) :: Bordered b i => Int -> b -> Int
(<?=>) =  (. sizeOf) . min

--------------------------------------------------------------------------------

undEx :: String -> a
undEx =  throw . UndefinedValue . showString "in SDP.Prim.SArray."

pfailEx :: String -> a
pfailEx =  throw . PatternMatchFail . showString "in SDP.Prim.SArray."

unreachEx :: String -> a
unreachEx =  throw . UnreachableException . showString "in SDP.Prim.SArray."


