{-# LANGUAGE Trustworthy, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GADTs, TypeFamilies #-}

{- |
    Module      :  SDP.Prim.SBytes.ST
    Copyright   :  (c) Andrey Mulik 2025
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Prim.SBytes.ST" provides boxed pseudo-primitive safe array.
-}
module SDP.Prim.SBytes.ST
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * Pseudo-primitive types
  STBytes#, fromSTBytes#, unpackSTBytes#, offsetSTBytes#,
  packSTBytes#, coerceSTBytes#
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM
import SDP.Unboxed

import SDP.SortM.Tim
import SDP.SortM

import GHC.Exts
  (
    MutableByteArray#, State#, Int#, (+#), (<=#),
    newByteArray#, sameMutableByteArray#, resizeMutableByteArray#
  )

import GHC.Types
import GHC.ST ( ST (..) )

import Data.STRef

import Unsafe.Coerce

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

-- | 'STBytes#' is mutable pseudo-primitive 'Int'-indexed strict unboxed array type.
data STBytes# s e
  where
    STBytes# :: Unboxed e
             => {-# UNPACK #-} !Int    -- ^ Element count (not a real size)
             -> {-# UNPACK #-} !Int    -- ^ Offset (in elements)
             -> !(MutableByteArray# s) -- ^ Real primitive byte array
             -> STBytes# s e

type role STBytes# nominal nominal

--------------------------------------------------------------------------------

{- Eq instance. -}

instance Eq (STBytes# s e)
  where
    (STBytes# c1 o1 marr1#) == (STBytes# c2 o2 marr2#) =
      let same = isTrue# (sameMutableByteArray# marr1# marr2#)
      in  c1 == c2 && (c1 == 0 || o1 == o2 && same)

--------------------------------------------------------------------------------

{- NullableM instance. -}

instance NullableM (ST s) (STBytes# s e)
  where
    isNullM (STBytes# n _ _) = pure (n < 1)
    
    newNull = ST $ \ s1# -> case newByteArray# 0# s1# of
        (# s2#, marr# #) -> (# s2#, coerce' (STBytes# 0 0 marr#) #)
      where
        coerce' :: STBytes# s Word -> STBytes# s e
        coerce' =  unsafeCoerce

--------------------------------------------------------------------------------

{- Estimate and EstimateM instances. -}

instance Estimate (STBytes# s e)
  where
    sizeHint (STBytes# c _ _) = Just (SizeHintEQ c)
    sizeOf   (STBytes# c _ _) = c
    
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

instance Monad m => EstimateM m (STBytes# s e)
  where
    getSizeHint (STBytes# c _ _) = pure $ Just (SizeHintEQ c)
    getSizeOf   (STBytes# c _ _) = pure c
    
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

instance Bordered (STBytes# s e) Int
  where
    lower                   _ = 0
    upper    (STBytes# c _ _) = c - 1
    bounds   (STBytes# c _ _) = (0, c - 1)
    indices  (STBytes# c _ _) = [0 .. c - 1]
    indexOf  (STBytes# c _ _) = index (0, c - 1)
    offsetOf (STBytes# c _ _) = offset (0, c - 1)
    indexIn  (STBytes# c _ _) = \ i -> i >= 0 && i < c
    
    eitherViewOf bnds@(l, _) es@(STBytes# _ _ arr#)
        | isEmpty bnds = Right (packSTBytes# 0 0 arr#)
        |    l /= 0    = Left  inapplicableEx
        |    n > c     = Left  expandEx
        |     True     = Right (packSTBytes# n 0 arr#)
      where
        inapplicableEx = InapplicableBoundaries
                       . showString "in SDP.Bordered.eitherViewOf: lower border "
                       $ shows l " of list should be 0"
        
        expandEx = UnacceptableExpansion
                 . showString "in SDP.Bordered.eitherViewOf: new borders "
                 $ shows bnds " can't be wider than range of list values"
        
        c = sizeOf es
        n = size bnds

instance BorderedM (ST s) (STBytes# s e) Int
  where
    nowIndexIn (STBytes# c _ _) = pure . inRange (0, c - 1)
    getIndices (STBytes# c _ _) = pure [0 .. c - 1]
    getBounds  (STBytes# c _ _) = pure (0, c - 1)
    getUpper   (STBytes# c _ _) = pure (c - 1)
    getLower                  _ = pure 0
    
    getEitherViewOf = pure ... eitherViewOf

--------------------------------------------------------------------------------

{- ForceableM instance. -}

instance ForceableM (ST s) (STBytes# s e)
  where
    copied es@(STBytes# n@(I# n#) (I# o#) marr#) = ST $
      \ s1# -> case pcloneUnboxedM es marr# o# n# s1# of
        (# s2#, copy# #) -> (# s2#, STBytes# n 0 copy# #)

--------------------------------------------------------------------------------

{- ConcatM instance. -}

instance Unboxed e => ConcatM (ST s) (STBytes# s e)
  where
    xs@(STBytes# _ _ _) <~> ys = do
      let xn = sizeOf xs; yn = sizeOf ys; n = xn + yn
      marr <- mreplicate n filler
      
      unsafeCopyTo xs 0 marr 0  xn
      unsafeCopyTo ys 0 marr xn yn
      
      pure marr
    
    concatM = concatMapM pure
    
    concatMapM f ess = do
      -- create empty buffer
      buff <- newNull
      
      -- write all structures to buffer
      foldr (\ es go -> do appendBufferM buff =<< f es; go) (pure ()) ess
      
      -- Since we don't use the buffer any further, we can turn it into STBytes#
      unsafeFromBufferM buff

--------------------------------------------------------------------------------

{- SequenceM and LinearM instances. -}

instance SequenceM (ST s) (STBytes# s e) e
  where
    foldrM f base es@(STBytes# c _ _) = go 0 c
      where
        go i n = i >= n ? pure base $ do
          e   <- unsafeReadByOff es i
          acc <- go (i + 1) n
          f e acc
    
    foldlM f base es@(STBytes# c _ _) = go c 0
      where
        go i n = i < n ? pure base $ do
          e   <- unsafeReadByOff es i
          acc <- go (i - 1) n
          f acc e
    
    ofoldrM f base es@(STBytes# c _ _) = go 0 c
      where
        go i n = i >= n ? pure base $ do
          e   <- unsafeReadByOff es i
          acc <- go (i + 1) n
          f i e acc
    
    ofoldlM f base es@(STBytes# c _ _) = go c 0
      where
        go i n = i < n ? pure base $ do
          e   <- unsafeReadByOff es i
          acc <- go (i - 1) n
          f i acc e
    
    getLeft  = foldrM (pure ... (:)) []
    getRight = foldlM (flip $ pure ... (:)) []

instance Unboxed e => LinearM (ST s) (STBytes# s e) e
  where
    prepend e es@(STBytes# (I# c#) (I# o#) arr#) = let n# = c# +# 1# in ST $
      \ s1# -> case newUnboxed' e n# s1# of
        (# s2#, marr# #) -> case pcopyUnboxedM es arr# o# marr# 1# c# s2# of
          s3# -> (# s3#, STBytes# (I# c#) 0 marr# #)
    
    append es@(STBytes# (I# c#) (I# o#) arr#) e = let n# = c# +# 1# in ST $
      \ s1# -> case newUnboxed' e n# s1# of
        (# s2#, marr# #) -> case pcopyUnboxedM es arr# o# marr# 0# c# s2# of
          s3# -> (# s3#, STBytes# (I# c#) 0 marr# #)
    
    unconsM'    (STBytes# 0 _    _) = pure Nothing
    unconsM' es@(STBytes# n o arr#) = do
      h <- unsafeReadMByKey es 0
      pure $ Just (h, STBytes# (n - 1) (o + 1) arr#)
    
    unsnocM'    (STBytes# 0 _    _) = pure Nothing
    unsnocM' es@(STBytes# n o arr#) = do
      l <- unsafeReadMByKey es (n - 1)
      pure $ Just (STBytes# (n - 1) o arr#, l)
    
    takeM n es@(STBytes# c o marr#)
      | n <= 0 = newNull
      | n >= c = pure es
      |  True  = pure (STBytes# n o marr#)
    
    dropM n es@(STBytes# c o marr#)
      | n >= c = newNull
      | n <= 0 = pure es
      |  True  = pure (STBytes# (c - n) (o + n) marr#)
    
    keepM n es@(STBytes# c o marr#)
      | n <= 0 = newNull
      | n >= c = pure es
      |  True  = pure (STBytes# n (c - n + o) marr#)
    
    sansM n es@(STBytes# c o marr#)
      | n >= c = newNull
      | n <= 0 = pure es
      |  True  = pure (STBytes# (c - n) o marr#)
    
    splitM n es@(STBytes# c o marr#)
      | n <= 0 = do e' <- newNull; pure (e', es)
      | n >= c = do e' <- newNull; pure (es, e')
      |  True  = pure (STBytes# n o marr#, STBytes# (c - n) (o + n) marr#)
    
    divideM n es@(STBytes# c o marr#)
      | n <= 0 = do e' <- newNull; pure (es, e')
      | n >= c = do e' <- newNull; pure (e', es)
      |  True  = pure (STBytes# n (c - n + o) marr#, STBytes# (c - n) o marr#)
    
    newLinear = fromFoldableM
    
    newLinearN c es = let !n@(I# n#) = max 0 c in ST $
      \ s1# -> case newLinearN# n# es s1# of
        (# s2#, marr# #) -> (# s2#, STBytes# n 0 marr# #)
    
    fromFoldableM es = ST $ \ s1# -> case fromFoldableM# es s1# of
      (# s2#, n, marr# #) -> (# s2#, STBytes# n 0 marr# #)
    
    reverseM es = do es' <- copied es; es' <$ reversed es'
    
    reversed es =
      let go i j = when (i < j) $ do go (i + 1) (j - 1); unsafeSwapM es i j
      in  go 0 (sizeOf es - 1)
    
    (!*) = unsafeReadByOff
    
    {-# INLINE unsafeReadByOff #-}
    unsafeReadByOff (STBytes# _ (I# o#) marr#) (I# i#) = ST $ readUnboxed# marr# (o# +# i#)
    
    {-# INLINE writeM #-}
    writeM = unsafeWriteM
    
    unsafeWriteM (STBytes# _ (I# o#) marr#) = \ (I# i#) e -> ST $
      \ s1# -> case writeUnboxed# marr# (o# +# i#) e s1# of
        s2# -> (# s2#, () #)
    
    unsafeCopyM es l n = do
      copy <- mreplicate n filler
      copy <$ unsafeCopyTo es l copy 0 n
    
    unsafeCopyTo src sc trg tc n@(I# n#) = when (n > 0) $ do
        when      (sc < 0 || tc < 0)      $ underEx "copyTo"
        when (sc + n > n1 || tc + n > n2) $ overEx  "copyTo"
        ST $ \ s1# -> (# pcopyUnboxedM src src# so# trg# to# n# s1#, () #)
      where
        !(STBytes# n1 o1 src#) = src; !(I# so#) = o1 + sc
        !(STBytes# n2 o2 trg#) = trg; !(I# to#) = o2 + tc

--------------------------------------------------------------------------------

{- MapM and IndexedM instances. -}

instance Unboxed e => MapM (ST s) (STBytes# s e) Int e
  where
    {-# INLINE writeM' #-}
    writeM' (STBytes# _ (I# o#) marr#) = \ (I# i#) e -> ST $
      \ s1# -> case writeUnboxed# marr# (o# +# i#) e s1# of
        s2# -> (# s2#, () #)
    
    newMap = newMap' filler
    
    newMap' e ascs =
      let bnds = rangeBounds (fsts ascs)
      in  fromAssocs' bnds e ascs
    
    unsafeReadMByKey = unsafeReadByOff
    
    overwrite es@(STBytes# c _ _) ascs = uncurry (unsafeWriteM es) `mapM_`
      filter (inRange (0, c - 1) . fst) ascs
    
    kfoldrM = ofoldrM
    kfoldlM = ofoldlM

instance Unboxed e => IndexedM (ST s) (STBytes# s e) Int e
  where
    fromAssocs' bnds e ascs = do
      es <- mreplicate (size bnds) e
      es <$ overwrite es ascs
    
    fromIndexed' es = do
      copy <- mreplicate (sizeOf es) filler
      copy <$ ofoldr (\ i e go -> do unsafeWriteM copy i e; go) (pure ()) es
    
    fromIndexedM es = do
      copy <- flip mreplicate filler =<< getSizeOf es
      copy <$ ofoldrM (\ i e _ -> unsafeWriteM copy i e) () es

--------------------------------------------------------------------------------

{- SortM instance. -}

instance Unboxed e => SortM (ST s) (STBytes# s e) e
  where
    sortedMBy f es = n < 2 ? pure True $ fmap and $ forM [0 .. n - 2] $ \ i -> g i (i + 1)
      where
        g = liftA2 f `on` unsafeReadByOff es
        n = sizeOf es
    
    sortMBy = timSortBy

--------------------------------------------------------------------------------

-- | 'unpackSTBytes#' returns 'MutableByteArray#' field of 'STBytes#'.
unpackSTBytes# :: STBytes# s e -> MutableByteArray# s
unpackSTBytes# =  \ (STBytes# _ _ marr#) -> marr#

-- | 'offsetSTBytes#' returns 'STBytes#' offset in bytes.
offsetSTBytes# :: STBytes# s e -> Int#
offsetSTBytes# =  \ (STBytes# _ (I# o#) _) -> o#

-- | 'packSTBytes#' creates new 'STBytes#' from sized 'MutableByteArray#'.
packSTBytes# :: Unboxed e => Int -> Int -> MutableByteArray# s -> STBytes# s e
packSTBytes# n o marr# = STBytes# (max 0 n) (max 0 o) marr#

-- | 'fromSTBytes#' returns new 'MutableByteArray#'.
fromSTBytes# :: STBytes# s e -> State# s -> (# State# s, MutableByteArray# s #)
fromSTBytes# es@(STBytes# (I# c#) (I# o#) marr#) = \ s1# -> case pnewUnboxed es c# s1# of
  (# s2#, copy# #) -> case pcopyUnboxedM es marr# o# copy# 0# c# s2# of
    s3# -> (# s3#, marr# #)

{- |
  'coerceSTBytes#' is unsafe low-lowel coerce of an mutable array with
  recounting the number of elements and offset (with possible rounding).
-}
coerceSTBytes# :: Unboxed b => STBytes# s a -> STBytes# s b
coerceSTBytes# pa@(STBytes# n o bytes#) = pb
  where
    pb = STBytes# (n * s1 `div` s2) (o * s1 `div` s2) bytes#
    s1 = psizeof pa 8; s2 = psizeof pb 8

--------------------------------------------------------------------------------

data STBufferRep# s e
  where
    STBufferRep# :: Unboxed e => Int# -> MutableByteArray# s -> STBufferRep# s e

newtype STBuffer# s e = STBuffer# (STRef s (STBufferRep# s e))

instance Unboxed e => NullableM (ST s) (STBuffer# s e)
  where
    isNullM (STBuffer# ref) = do
      STBufferRep# c# _ <- readSTRef ref
      return (isTrue# (c# <=# 0#))
    
    newNull = do
      rep <- ST $ \ s1# -> case newByteArray# 0# s1# of
        (# s2#, marr# #) -> (# s2#, STBufferRep# 0# marr# #)
      
      STBuffer# <$> newSTRef rep

instance Unboxed e => BufferM (ST s) (STBytes# s e)
  where
    type BufferForM (ST s) (STBytes# s e) = STBuffer# s e
    
    unsafeFromBufferM (STBuffer# ref) = do
      STBufferRep# c# marr# <- readSTRef ref
      return (STBytes# (I# c#) 0 marr#)
    
    fromBufferM buff = do
      es@(STBytes# (I# c#) (I# o#) marr#) <- unsafeFromBufferM buff
      
      ST $ \ s1# -> case pcloneUnboxedM es marr# c# o# s1# of
        (# s2#, copy# #) -> (# s2#, STBytes# (I# c#) (I# o#) copy# #)
    
    appendBufferM (STBuffer# ref) es@(STBytes# (I# c#) (I# o#) marr#) = do
      STBufferRep# n# buff# <- readSTRef ref
      
      rep <- ST $ \ s1# -> case resizeMutableByteArray# buff# (psizeof# es (n# +# c#)) s1# of
        (# s2#, new_buff# #) -> case pcopyUnboxedM es marr# o# new_buff# n# c# s2# of
          s3# -> (# s3#, STBufferRep# (n# +# c#) new_buff# #)
      
      writeSTRef ref rep

--------------------------------------------------------------------------------

overEx :: String -> a
overEx =  throw . IndexOverflow . showString "in SDP.Prim.SBytes."

underEx :: String -> a
underEx =  throw . IndexUnderflow . showString "in SDP.Prim.SBytes."



