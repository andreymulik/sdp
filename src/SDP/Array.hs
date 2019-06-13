{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

{- |
    Module      :  SDP.Array
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    SDP.Array provides immutable lazy boxed array type.
    This implementation of array no much different from Data.Array (array),
    but incopatible with it.
    The main difference is the Index class instead of Ix.
-}

module SDP.Array
(
  module SDP.Indexed,
  module SDP.Scan,
  module SDP.Set,
  
  Array (..)
)
where

import Prelude ()
import SDP.SafePrelude

import Test.QuickCheck

import GHC.Base
  (
    MutableArray#, Array#, Int (..),
    
    newArray#, unsafeFreezeArray#, writeArray#, indexArray#,
    
    isTrue#, (+#), (-#), (==#)
  )
import GHC.Show ( appPrec )
import GHC.ST   ( ST (..), STRep, runST )

import Text.Read
import Text.Read.Lex ( expect )

import SDP.Internal.MutableArrays ( STArray (..), fill )
import SDP.Indexed
import SDP.Scan
import SDP.Set

import SDP.Simple

--------------------------------------------------------------------------------

{- |
  This Array type definition is no different from the standard GHC.Arr,
  but I have to redefine it because of the limitation of the Ix class.
-}
data Array i e = Array !i !i {-# UNPACK #-} !Int (Array# e)

type role Array nominal representational

--------------------------------------------------------------------------------

{- Eq and Eq1 instances. -}

instance (Index i, Eq e) => Eq (Array i e) where (==) = eq1

instance (Index i) => Eq1 (Array i)
  where
    liftEq eq xs ys = n1 == n2 && (n1 == 0 || l1 == l2 && u1 == u2 && elemsEq)
      where
        elemsEq = and [ (xs !# i) `eq` (ys !# i) | i <- [0 .. n1 - 1] ]
        
        (Array l1 u1 n1 _) = xs
        (Array l2 u2 n2 _) = ys

--------------------------------------------------------------------------------

{- Ord and Ord1 instances. -}

instance (Index i, Ord e) => Ord (Array i e) where compare = compare1

instance (Index i) => Ord1 (Array i)
  where
    liftCompare cmp xs ys = liftCompare cmp' (assocs xs) (assocs ys)
      where
        cmp' (ix, x) (iy, y) = (ix <=> iy) <> (cmp x y)

--------------------------------------------------------------------------------

{- Show and Read instances. -}

instance (Index i, Show i, Show e) => Show (Array i e)
  where
    showsPrec p arr@(Array l u _ _) = showParen (p > appPrec) shows'
      where
        shows' = showString "array " . shows (l, u) . showChar ' ' . shows (assocs arr)

instance (Index i, Read i, Read e) => Read (Array i e)
  where
    readList = readListDefault
    readPrec = parens $ prec appPrec (lift . expect $ Ident "array") >> liftA2 assoc (step readPrec) (step readPrec)

--------------------------------------------------------------------------------

{- Functor, Zip and Applicative instances. -}

instance (Index i) => Functor (Array i)
  where
    fmap f arr@(Array l u n@(I# n#) _) = runST $ ST $ \ s1# ->
      case newArray# n# (undEx "fmap") s1# of
        (# s2#, marr# #) ->
          let go i s# = if i == n
              then done (l, u) n marr# s#
              else fill marr# (i, f $ arr !# i) (go $ i + 1) s#
          in go 0 s2#

instance (Index i) => Zip (Array i)
  where
    zipWith f as bs              = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !# i) (bs !# i)
        sz      = eminimum [EL as, EL bs]
    
    zipWith3 f as bs cs          = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !# i) (bs !# i) (cs !# i)
        sz      = eminimum [EL as, EL bs, EL cs]
    
    zipWith4 f as bs cs ds       = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !# i) (bs !# i) (cs !# i) (ds !# i)
        sz      = eminimum [EL as, EL bs, EL cs, EL ds]
    
    zipWith5 f as bs cs ds es    = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !# i) (bs !# i) (cs !# i) (ds !# i) (es !# i)
        sz      = eminimum [EL as, EL bs, EL cs, EL ds, EL es]
    
    zipWith6 f as bs cs ds es fs = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !# i) (bs !# i) (cs !# i) (ds !# i) (es !# i) (fs !# i)
        sz      = eminimum [EL as, EL bs, EL cs, EL ds, EL es, EL fs]

instance (Index i) => Applicative (Array i)
  where
    pure = single
    fs <*> es = (<$> es) `concatMap` fs

--------------------------------------------------------------------------------

{- Foldable, Scan and Traversable instances. -}

instance (Index i) => Foldable (Array i)
  where
    {-# INLINE foldr #-}
    foldr  f base arr = go 0
      where
        go i = arr ==. i ? base $ f (arr !# i) (go $ i + 1)
    
    {-# INLINE foldl #-}
    foldl  f base arr = go $ length arr - 1
      where
        go i = i == -1 ? base $ f (go $ i - 1) (arr !# i)
    
    {-# INLINE foldr' #-}
    foldr' f base arr = go (length arr - 1) base
      where
        go i a = i == -1 ? a $ go (i - 1) (f (arr !# i) $! a)
    
    {-# INLINE foldl' #-}
    foldl' f base arr = go 0 base
      where
        go i a = arr ==. i ? a $ go (i + 1) ((f $! a) $ arr !# i)
    
    {-# INLINE foldr1 #-}
    foldr1 f arr = null arr ? undEx "foldr1" $ go 0
      where
        go i = arr ==. (i + 1) ? e $ f e (go $ i + 1) where e = arr !# i
    
    {-# INLINE foldl1 #-}
    foldl1 f arr = null arr ? undEx "foldl1" $ go (length arr - 1)
      where
        go i = i == 0 ? e $ f (go $ i - 1) e where e = arr !# i
    
    {-# INLINE toList #-}
    toList arr@(Array _ _ n _) = [ arr !# i | i <- [0 .. n - 1] ]
    
    {-# INLINE null #-}
    null       (Array _ _ n _) = n < 1
    
    {-# INLINE length #-}
    length     (Array _ _ n _) = n

instance (Index i) => Scan (Array i)
  where
    scanl f w es = null es ? single w $ fromListN l (w : res w 0)
      where
        l = length es + 1
        -- res generates infinite list, but fromListN catches it.
        res !curr !n = nxt : res nxt (n + 1)
          where
            nxt = f curr (es !# n)
    
    scanr f w es = null es ? single w $ fromListN l (res w (l - 2) [w])
      where
        l = length es + 1
        res !curr !n ws = n < 0 ? ws $ res prv (n - 1) (prv : ws)
          where
            prv = f (es !# n) curr
    
    scanl' f w es = null es ? single w $ fromListN l (w : res w 0)
      where
        l = length es + 1
        res !curr !n = nxt : res nxt (n + 1)
          where
            nxt = f curr (es !# n)
    
    scanl1 f es = null es ? undEx "scanl1" $ fromListN l (res w 0)
      where
        l = length es
        w = head es
        res !curr !n = nxt : res nxt (n + 1)
          where
            nxt = f curr (es !# n)
    
    scanr1 f es = null es ? undEx "scanr1" $ fromList (res w (l - 2) [w])
      where
        l = length es
        w = last es
        res !curr !n ws = n < 0 ? ws $ res prv (n - 1) (prv : ws)
          where
            prv = f (es !# n) curr

instance (Index i) => Traversable (Array i)
  where
    traverse f arr = fromList <$> traverse f (toList arr)

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Index i) => Linear (Array i e) e
  where
    isNull = null
    
    {-# INLINE fromList #-}
    fromList es = fromFoldable es
    
    fromListN n es = runST $ ST $
        \ s1# -> case newArray# n# (undEx "fromList") s1# of
            (# s2#, marr# #) ->
              let go y r = \ i# s3# -> case writeArray# marr# i# y s3# of
                    s4# -> if isTrue# (i# ==# n# -# 1#)
                              then s4#
                              else r (i# +# 1#) s4#
              in done (l, u) n' marr#
              (
                if n' == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2#
              )
      where
        !n'@(I# n#) = max 0 $ (es <. n) ? length es $ n
        l = unsafeIndex 0
        u = unsafeIndex (n' - 1)
    
    fromFoldable es = runST $ ST $
        \ s1# -> case newArray# n# (undEx "fromList") s1# of
            (# s2#, marr# #) ->
              let go y r = \ i# s3# -> case writeArray# marr# i# y s3# of
                    s4# -> if isTrue# (i# ==# n# -# 1#)
                              then s4#
                              else r (i# +# 1#) s4#
              in done (l, u) n marr#
              (
                if n == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2#
              )
      where
        !n@(I# n#) = length es
        l = unsafeIndex 0
        u = unsafeIndex $ n - 1
    
    -- O(n + m) concatenation
    xs ++ ys = fromList $ (toList xs) ++ (toList ys)
    
    toHead e es = fromList (e : toList es)
    toLast es e = fromList (foldr (:) [e] es)
    
    head arr = arr !# 0
    last arr = arr !# (length arr - 1)
    
    tail es = fromListN (length es - 1) . tail $ toList es
    init es = fromListN (length es - 1) $ toList es
    
    -- O(n) reverse
    reverse es = fromListN (length es) (listR es)
    
    -- O (n) right view.
    listL   es = toList es
    
    -- O(n) right list view
    listR   es = [ es !# i | i <- [n - 1, n - 2 .. 0] ] where n = length es
    
    -- O(n * m) concatenation
    concatMap f = concat . map f . toList
    
    concat = fromList . foldr (\ a l -> toList a ++ l) []

instance (Index i) => Split (Array i e) e
  where
    -- O (n) take.
    take n es = fromListN n $ toList es
    
    -- O (n) drop.
    drop n es
        | n <= 0 = es
        | n >= l = Z
        |  True  = fromListN (l - n) [ es !# i | i <- [n .. l - 1] ]
      where
        l = length es
    
    split n es
        | n <= 0 = (Z, es)
        | n >= l = (es, Z)
        |  True  = (fromListN n take', fromListN (l - n) drop')
      where
        (take', drop') = split n $ toList es
        l = length es
    
    -- No more than O(n) comparing.
    isPrefixOf xs ys = xs .<=. ys && and equals
      where
        equals = [ xs !# i == ys !# i | i <- [0 .. ly - 1] ]
        ly = length ys
    
    -- No more than O(n) comparing.
    isSuffixOf xs ys = xs .<=. ys && and equals
      where
        equals  = [ xs !# i == xs !# (i + offset') | i <- [0 .. ly - 1] ]
        offset' = length xs - ly
        ly = length ys

instance (Index i) => Bordered (Array i e) i e
  where
    lower   (Array l _ _ _) = l
    upper   (Array _ u _ _) = u
    sizeOf  (Array _ _ n _) = n
    bounds  (Array l u _ _) = (l, u)
    indices (Array l u _ _) = range (l, u)
    
    assocs  arr = zip (indices arr) (toList arr)

--------------------------------------------------------------------------------

{- Indexed instance. -}

instance (Index i) => Indexed (Array i e) i e
  where
    assoc' bnds defvalue ascs = runST $ ST $ \ s1# -> case newArray# n# defvalue s1# of (# s2#, marr# #) -> writes marr# s2#
      where
        writes marr# = foldr (fill marr#) (done bnds n marr#) [ (offset bnds i, e) | (i, e) <- ascs ]
        !n@(I# n#)   = size bnds
    
    Z // ascs = null ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    arr@(Array l u n@(I# n#) _) // ascs = runST $ thaw >>= writes
      where
        writes (STArray l' u' n' marr#) = ST $ foldr (fill marr#) (done (l', u') n' marr#) ies
          where ies = [ (offset (l', u') i, e) | (i, e) <- ascs ]
        
        thaw = ST $ \ s1# -> case newArray# n# (undEx "(//)") s1# of
          (# s2#, marr# #) ->
            let copy i@(I# i#) s3# = if i == n then s3# else copy (i + 1) (writeArray# marr# i# (arr !# i) s3#)
            in case copy 0 s2# of s3# -> (# s3#, STArray l u n marr# #)
    
    (!)  arr@(Array l u _ _) i = arr !# offset (l, u) i
    
    (.$) p es = find   (\ i -> p $ es ! i) $ indices es
    (*$) p es = filter (\ i -> p $ es ! i) $ indices es

--------------------------------------------------------------------------------

instance (Index i) => Estimate (Array i)
  where
    (Array _ _ n1 _) <==> (Array _ _ n2 _) = n1 <=> n2
    (Array _ _ n1 _) .>.  (Array _ _ n2 _) = n1  >  n2
    (Array _ _ n1 _) .<.  (Array _ _ n2 _) = n1  <  n2
    (Array _ _ n1 _) .>=. (Array _ _ n2 _) = n1  >= n2
    (Array _ _ n1 _) .<=. (Array _ _ n2 _) = n1  <= n2
    
    (Array _ _ n1 _) >.  n2 = n1 >  n2
    (Array _ _ n1 _) <.  n2 = n1 <  n2
    (Array _ _ n1 _) >=. n2 = n1 >= n2
    (Array _ _ n1 _) <=. n2 = n1 <= n2

-- instance (Index i) => Set (Array i)

instance (Index i) => LineS (Array i e) e where stream = stream . toList

instance (Index i) => Default (Array i e) where def = Z

instance (Index i, Arbitrary e) => Arbitrary (Array i e)
  where
    arbitrary = fromList <$> arbitrary

--------------------------------------------------------------------------------

{-# INLINE (!#) #-}
(!#) :: Array i e -> Int -> e
(!#) (Array _ _ _ arr#) (I# i#) = case indexArray# arr# i# of (# e #) -> e

{-# INLINE done #-}
done :: (i, i) -> Int -> MutableArray# s e -> STRep s (Array i e)
done (l, u) n marr# = \s1# -> case unsafeFreezeArray# marr# s1# of
  (# s2#, arr# #) -> (# s2#, Array l u n arr# #)

undEx :: String -> a
undEx msg = throw . UndefinedValue $ "in SDP.Array." ++ msg

