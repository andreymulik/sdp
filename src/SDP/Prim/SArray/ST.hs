{-# LANGUAGE Trustworthy, MagicHash, UnboxedTuples, BangPatterns, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, RoleAnnotations #-}

{- |
    Module      :  SDP.Prim.SArray.ST
    Copyright   :  (c) Andrey Mulik 2025
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Prim.SArray.ST" provides boxed pseudo-primitive safe array.
-}
module SDP.Prim.SArray.ST
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * Pseudo-primitive types
  STArray#, fromSTArray#, unpackSTArray#, offsetSTArray#,
  packSTArray#, coerceSTArray#
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM

import SDP.SortM.Tim
import SDP.SortM

import GHC.Exts
  (
    MutableArray#, State#, Int#,
    
    newArray#, readArray#, writeArray#, sameMutableArray#,
    copyMutableArray#, cloneMutableArray#, (+#), (-#), (==#)
  )

import GHC.Types
import GHC.ST ( ST (..), STRep )

import Data.Coerce

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

-- | 'STArray#' is mutable preudo-primitive 'Int'-indexed lazy boxed array type.
data STArray# s e = STArray#
              {-# UNPACK #-} !Int  -- ^ Element count (not a real size)
              {-# UNPACK #-} !Int  -- ^ Offset (in elements)
              !(MutableArray# s e) -- ^ Real primitive array

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
    
    isNullM (STArray# n _ _) = pure (n < 1)

--------------------------------------------------------------------------------

{- Estimate and EstimateM instances. -}

instance Estimate (STArray# s e)
  where
    sizeHint (STArray# c _ _) = Just (SizeHintEQ c)
    sizeOf   (STArray# c _ _) = c
    
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
    
    shrinkTo n (STArray# c o marr#) = STArray# (c `min` n `max` 0) o marr#

instance Monad m => EstimateM m (STArray# s e)
  where
    getSizeHint (STArray# c _ _) = pure $ Just (SizeHintEQ c)
    getSizeOf   (STArray# c _ _) = pure c
    
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

instance Bordered (STArray# s e) Int
  where
    lower                   _ = 0
    upper    (STArray# c _ _) = c - 1
    bounds   (STArray# c _ _) = (0, c - 1)
    indices  (STArray# c _ _) = [0 .. c - 1]
    indexOf  (STArray# c _ _) = index (0, c - 1)
    offsetOf (STArray# c _ _) = offset (0, c - 1)
    indexIn  (STArray# c _ _) = \ i -> i >= 0 && i < c
    
    eitherViewOf bnds@(l, _) es@(STArray# _ _ arr#)
        | isEmpty bnds = Right (packSTArray# 0 0 arr#)
        |    l /= 0    = Left  inapplicableEx
        |    n > c     = Left  expandEx
        |     True     = Right (packSTArray# n 0 arr#)
      where
        inapplicableEx = InapplicableBoundaries
                       . showString "in SDP.Bordered.eitherViewOf: lower border "
                       $ shows l " of list should be 0"
        
        expandEx = UnacceptableExpansion
                 . showString "in SDP.Bordered.eitherViewOf: new borders "
                 $ shows bnds " can't be wider than range of list values"
        
        c = sizeOf es
        n = size bnds

instance BorderedM (ST s) (STArray# s e) Int
  where
    nowIndexIn (STArray# c _ _) = pure . inRange (0, c - 1)
    getIndices (STArray# c _ _) = pure [0 .. c - 1]
    getBounds  (STArray# c _ _) = pure (0, c - 1)
    getUpper   (STArray# c _ _) = pure (c - 1)
    getLower                  _ = pure 0
    
    getEitherViewOf = pure ... eitherViewOf

--------------------------------------------------------------------------------

{- ForceableM instance. -}

instance ForceableM (ST s) (STArray# s e)
  where
    copied (STArray# n@(I# n#) (I# o#) marr#) = ST $
      \ s1# -> case cloneMutableArray# marr# o# n# s1# of
        (# s2#, copy# #) -> (# s2#, STArray# n 0 copy# #)

--------------------------------------------------------------------------------

{- ConcatM instance. -}

instance ConcatM (ST s) (STArray# s e)
  where
    xs <~> ys = do
      let xn = sizeOf xs; yn = sizeOf ys; n = xn + yn
      marr <- mreplicate n (unreachEx "<~>")
      
      unsafeCopyTo xs 0 marr 0  xn
      unsafeCopyTo ys 0 marr xn yn
      
      pure marr
    
    concatM ess = do
      let n = foldr' ((+) . sizeOf) 0 ess
      marr <- mreplicate n (unreachEx "merged")
      marr <$ foldr (\ arr@(STArray# c _ _) o' -> do
          o <- o'
          unsafeCopyTo arr 0 marr o c
          pure (o + c)
        ) (pure 0) ess
    
    concatMapM f = concatM <=< mapM f . toList

--------------------------------------------------------------------------------

{- SequenceM and LinearM instances. -}

instance SequenceM (ST s) (STArray# s e) e
  where
    foldrM f base es = go 0 (sizeOf es)
      where
        go i n = i >= n ? pure base $ do
          e   <- unsafeReadByOff es i
          acc <- go (i + 1) n
          f e acc
    
    foldlM f base es = go (sizeOf es) 0
      where
        go i n = i < n ? pure base $ do
          e   <- unsafeReadByOff es i
          acc <- go (i - 1) n
          f acc e
    
    ofoldrM f base es = go 0 (sizeOf es)
      where
        go i n = i >= n ? pure base $ do
          e   <- unsafeReadByOff es i
          acc <- go (i + 1) n
          f i e acc
    
    ofoldlM f base es = go (sizeOf es) 0
      where
        go i n = i < n ? pure base $ do
          e   <- unsafeReadByOff es i
          acc <- go (i - 1) n
          f i acc e
    
    getLeft  = foldrM (pure ... (:)) []
    getRight = foldlM (flip $ pure ... (:)) []

instance LinearM (ST s) (STArray# s e) e
  where
    prepend e (STArray# (I# c#) (I# o#) arr#) = let n# = c# +# 1# in ST $
      \ s1# -> case newArray# n# e s1# of
        (# s2#, marr# #) -> case copyMutableArray# arr# o# marr# 1# c# s2# of
          s3# -> (# s3#, STArray# (I# c#) 0 marr# #)
    
    append (STArray# (I# c#) (I# o#) arr#) e = let n# = c# +# 1# in ST $
      \ s1# -> case newArray# n# e s1# of
        (# s2#, marr# #) -> case copyMutableArray# arr# o# marr# 0# c# s2# of
          s3# -> (# s3#, STArray# (I# c#) 0 marr# #)
    
    unconsM'    (STArray# 0 _    _) = pure Nothing
    unconsM' es@(STArray# n o arr#) = do
      h <- unsafeReadMByKey es 0
      pure $ Just (h, STArray# (n - 1) (o + 1) arr#)
    
    unsnocM'    (STArray# 0 _    _) = pure Nothing
    unsnocM' es@(STArray# n o arr#) = do
      l <- unsafeReadMByKey es (n - 1)
      pure $ Just (STArray# (n - 1) o arr#, l)
    
    takeM n es@(STArray# c o marr#)
      | n <= 0 = newNull
      | n >= c = pure es
      |  True  = pure (STArray# n o marr#)
    
    dropM n es@(STArray# c o marr#)
      | n >= c = newNull
      | n <= 0 = pure es
      |  True  = pure (STArray# (c - n) (o + n) marr#)
    
    keepM n es@(STArray# c o marr#)
      | n <= 0 = newNull
      | n >= c = pure es
      |  True  = pure (STArray# n (c - n + o) marr#)
    
    sansM n es@(STArray# c o marr#)
      | n >= c = newNull
      | n <= 0 = pure es
      |  True  = pure (STArray# (c - n) o marr#)
    
    splitM n es@(STArray# c o marr#)
      | n <= 0 = do e' <- newNull; pure (e', es)
      | n >= c = do e' <- newNull; pure (es, e')
      |  True  = pure (STArray# n o marr#, STArray# (c - n) (o + n) marr#)
    
    divideM n es@(STArray# c o marr#)
      | n <= 0 = do e' <- newNull; pure (es, e')
      | n >= c = do e' <- newNull; pure (e', es)
      |  True  = pure (STArray# n (c - n + o) marr#, STArray# (c - n) o marr#)
    
    newLinear = fromFoldableM
    
    newLinearN c es = ST $ \ s1# -> case newArray# n# err s1# of
      (# s2#, marr# #) ->
        let go y r = \ i# s3# -> case writeArray# marr# i# y s3# of
              s4# -> if isTrue# (i# ==# n# -# 1#) then s4# else r (i# +# 1#) s4#
        in done n marr# ( if n == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2# )
      where
        err = undEx "newLinearN"
        !n@(I# n#) = max 0 c
    
    fromFoldableM es = ST $ \ s1# -> case newArray# n# err s1# of
      (# s2#, marr# #) ->
        let go y r = \ i# s3# -> case writeArray# marr# i# y s3# of
              s4# -> if isTrue# (i# ==# n# -# 1#) then s4# else r (i# +# 1#) s4#
        in done n marr# ( if n == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2# )
      where
        err = unreachEx "fromFoldableM"
        !n@(I# n#) = length es
    
    reverseM es = do es' <- copied es; es' <$ reversed es'
    
    reversed es =
      let go i j = when (i < j) $ do go (i + 1) (j - 1); unsafeSwapM es i j
      in  go 0 (sizeOf es - 1)
    
    (!*) = unsafeReadByOff
    
    {-# INLINE unsafeReadByOff #-}
    unsafeReadByOff (STArray# _ (I# o#) marr#) (I# i#) = ST $ readArray# marr# (o# +# i#)
    
    writeM       = writeM'
    unsafeWriteM = writeM'
    
    unsafeCopyTo src sc trg tc n@(I# n#) = when (n > 0) $ do
        when      (sc < 0 || tc < 0)      $ underEx "copyTo"
        when (sc + n > n1 || tc + n > n2) $ overEx  "copyTo"
        ST $ \ s1# -> case copyMutableArray# src# so# trg# to# n# s1# of
          s2# -> (# s2#, () #)
      where
        !(STArray# n1 o1 src#) = src; !(I# so#) = o1 + sc
        !(STArray# n2 o2 trg#) = trg; !(I# to#) = o2 + tc

--------------------------------------------------------------------------------

{- MapM and IndexedM instances. -}

instance MapM (ST s) (STArray# s e) Int e
  where
    {-# INLINE writeM' #-}
    writeM' (STArray# _ (I# o#) marr#) (I# i#) e = ST $ \ s1# ->
      case writeArray# marr# (o# +# i#) e s1# of s2# -> (# s2#, () #)
    
    newMap' e ascs =
      let bnds = rangeBounds (fsts ascs)
      in  fromAssocs' bnds e ascs
    
    unsafeReadMByKey = unsafeReadByOff
    
    overwrite es@(STArray# c _ _) ascs = uncurry (unsafeWriteM es) `mapM_`
      filter (inRange (0, c - 1) . fst) ascs
    
    kfoldrM = ofoldrM
    kfoldlM = ofoldlM

instance IndexedM (ST s) (STArray# s e) Int e
  where
    fromAssocs' bnds e ascs = do
      es <- mreplicate (size bnds) e
      es <$ overwrite es ascs
    
    fromIndexed' es = do
      copy <- mreplicate (sizeOf es) (unreachEx "fromIndexed'")
      copy <$ ofoldr (\ i e go -> do unsafeWriteM copy i e; go) (pure ()) es
    
    fromIndexedM es = do
      copy <- flip mreplicate (unreachEx "fromIndexedM") =<< getSizeOf es
      copy <$ ofoldrM (\ i e _ -> unsafeWriteM copy i e) () es

--------------------------------------------------------------------------------

{- SortM instance. -}

instance SortM (ST s) (STArray# s e) e
  where
    sortedMBy f es = n < 2 ? pure True $ fmap and $ forM [0 .. n - 2] $ \ i -> g i (i + 1)
      where
        g = liftA2 f `on` unsafeReadByOff es
        n = sizeOf es
    
    sortMBy = timSortBy

--------------------------------------------------------------------------------

-- | 'unpackSTArray#' returns 'MutableArray#' field of 'STArray#' or fails.
unpackSTArray# :: STArray# s e -> MutableArray# s e
unpackSTArray# =  \ (STArray# _ _ marr#) -> marr#

-- | 'offsetSTArray#' returns 'STArray#' offset in elements.
offsetSTArray# :: STArray# s e -> Int#
offsetSTArray# =  \ (STArray# _ (I# o#) _) -> o#

-- | 'packSTArray#' creates new 'STArray#' from sized 'MutableArray#'.
packSTArray# :: Int -> Int -> MutableArray# s e -> STArray# s e
packSTArray# n o marr# = STArray# (max 0 n) (max 0 o) marr#

-- | 'fromSTArray#' returns new 'MutableArray#'.
fromSTArray# :: STArray# s e -> State# s -> (# State# s, MutableArray# s e #)
fromSTArray# (STArray# (I# c#) (I# o#) marr#) = cloneMutableArray# marr# o# c#

-- | 'coerceSTArray#' is 'coerce' alias.
coerceSTArray# :: Coercible a b => STArray# s a -> STArray# s b
coerceSTArray# =  coerce

--------------------------------------------------------------------------------

{-# INLINE done #-}
done :: Int -> MutableArray# s e -> STRep s (STArray# s e)
done n marr# = \ s1# -> (# s1#, STArray# n 0 marr# #)

undEx :: String -> a
undEx =  throw . UndefinedValue . showString "in SDP.Prim.SArray.ST."

overEx :: String -> a
overEx =  throw . IndexOverflow . showString "in SDP.Prim.SArray.ST."

underEx :: String -> a
underEx =  throw . IndexUnderflow . showString "in SDP.Prim.SArray.ST."

unreachEx :: String -> a
unreachEx =  throw . UnreachableException . showString "in SDP.Prim.SArray.ST."




