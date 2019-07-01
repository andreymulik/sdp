{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

{- |
    Module      :  SDP.Array
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    Stability   :  stable
    
    SDP.Array provides immutable lazy boxed array type.
    This implementation of array no much different from Data.Array (array),
    but incopatible with it.
    The main difference is the Index class instead of Ix.
-}

module SDP.Array
(
  module SDP.Indexed,
  module SDP.Sort,
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

import SDP.Internal.MutableArrays ( STArray (..) )
import SDP.Indexed
import SDP.Sort
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
    liftEq eq xs ys = liftEq eq' (assocs xs) (assocs ys)
      where
        eq' (i1, x) (i2, y) = i1 == i2 && eq x y

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
    showsPrec p arr@(Array l u _ _) = showParen (p > appPrec) $ showString "array "
                                                              . shows (l, u)
                                                              . showChar ' '
                                                              . shows (assocs arr)

instance (Index i, Read i, Read e) => Read (Array i e)
  where
    readList = readListDefault
    readPrec = parens $ prec appPrec (lift . expect $ Ident "array") >> liftA2 assoc (step readPrec) (step readPrec)

--------------------------------------------------------------------------------

{- Functor, Zip and Applicative instances. -}

instance (Index i) => Functor (Array i)
  where
    fmap f arr@(Array l u n@(I# n#) _) = runST $ ST $ \ s1# ->
      case newArray# n# (unreachEx "fmap") s1# of
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
        go i = -1 == i ? base $ f (go $ i - 1) (arr !# i)
    
    {-# INLINE foldr' #-}
    foldr' f base arr = go (length arr - 1) base
      where
        go i !a = -1 == i ? a $ go (i - 1) (f (arr !# i) a)
    
    {-# INLINE foldl' #-}
    foldl' f base arr = go 0 base
      where
        go i !a = arr ==. i ? a $ go (i + 1) (f a $ arr !# i)
    
    {-# INLINE foldr1 #-}
    foldr1 f arr = null arr ? pfailEx "foldr1" $ go 0
      where
        go i = arr ==. (i + 1) ? e $ f e (go $ i + 1) where e = arr !# i
    
    {-# INLINE foldl1 #-}
    foldl1 f arr = null arr ? pfailEx "foldl1" $ go (length arr - 1)
      where
        go i = 0 == i ? e $ f (go $ i - 1) e where e = arr !# i
    
    {-# INLINE toList #-}
    toList arr@(Array _ _ n _) = [ arr !# i | i <- [ 0 .. n - 1 ] ]
    
    {-# INLINE null #-}
    null       (Array l u n _) = isEmpty (l, u) || n < 1
    
    {-# INLINE length #-}
    length     (Array _ _ n _) = max 0 n

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
    
    scanl1 f es = null es ? pfailEx "scanl1" $ fromListN l (res w 0)
      where
        l = length es
        w = head es
        res !curr !n = nxt : res nxt (n + 1)
          where
            nxt = f curr (es !# n)
    
    scanr1 f es = null es ? pfailEx "scanr1" $ fromList (res w (l - 2) [w])
      where
        l = length es
        w = last es
        res !curr !n ws = n < 0 ? ws $ res prv (n - 1) (prv : ws)
          where
            prv = f (es !# n) curr

instance (Index i) => Traversable (Array i) where traverse f arr = fromList <$> traverse f (toList arr)

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Index i) => Linear (Array i e) e
  where
    {-# INLINE isNull #-}
    isNull es = null es
    
    {-# INLINE lzero #-}
    lzero = runST $ ST $ \ s1# -> case newArray# 0# (unreachEx "lzero") s1# of
      (# s2#, marr# #) -> done (unsafeBounds 0) 0 marr# s2#
    
    {-# INLINE toHead #-}
    toHead e es = fromListN (n + 1) (e : toList es)    where n = length es
    
    {-# INLINE head #-}
    head Z  = pfailEx "(:>)"
    head es = es !# 0
    
    {-# INLINE tail #-}
    tail Z  = pfailEx "(:>)"
    tail es = fromListN (length es - 1) . tail $ toList es
    
    {-# INLINE toLast #-}
    toLast es e = fromListN (n + 1) $ foldr (:) [e] es where n = length es
    
    {-# INLINE last #-}
    last Z  = pfailEx "(:<)"
    last arr = arr !# (length arr - 1)
    
    {-# INLINE init #-}
    init Z  = pfailEx "(:<)"
    init es = fromListN (length es - 1) $ toList es
    
    {-# INLINE fromList #-}
    fromList es = fromFoldable es
    
    {-# INLINE fromFoldable #-}
    fromFoldable es = runST $ ST $ \ s1# -> case newArray# n# err s1# of
        (# s2#, marr# #) ->
          let go y r = \ i# s3# -> case writeArray# marr# i# y s3# of
                s4# -> if isTrue# (i# ==# n# -# 1#) then s4# else r (i# +# 1#) s4#
          in done (l, u) n marr# ( if n == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2# )
      where
        !n@(I# n#) = length es
        (l, u) = unsafeBounds n
        err    = unreachEx "fromList"
    
    single e = runST $ ST $ \ s1# -> case newArray# 1# e s1# of (# s2#, marr# #) -> done (unsafeBounds 1) 1 marr# s2#
    
    {-# INLINE (++) #-}
    xs ++ ys = fromListN (sizeOf xs + sizeOf ys) $ listL xs ++ listL ys
    
    {-# INLINE replicate #-}
    replicate n e = runST $ ST $ \ s1# -> case newArray# n# e s1# of
        (# s2#, marr# #) -> done (unsafeBounds n') n' marr# s2#
      where
        !n'@(I# n#) = max 0 n
    
    {-# INLINE reverse #-}
    -- O(n) reverse
    reverse es = fromListN (length es) (listR es)
    
    {-# INLINE listL #-}
    -- O (n) right view.
    listL es = toList es
    
    {-# INLINE listR #-}
    -- O(n) right list view
    listR es = [ es !# i | i <- [ n - 1, n - 2 .. 0 ] ] where n = length es
    
    {-# INLINE concatMap #-}
    -- O(n * m) concatenation
    concatMap f ess = fromList $ foldr (\ a l -> toList (f a) ++ l) [] ess
    
    {-# INLINE concat #-}
    concat ess = fromList $ foldr (\ a l -> toList a ++ l) [] ess

instance (Index i) => Split (Array i e) e
  where
    take n es = fromListN n $ toList es
    
    drop n es
        | n <= 0 = es
        | n >= l = Z
        |  True  = fromListN (l - n) [ es !# i | i <- [ n .. l - 1 ] ]
      where
        l = length es
    
    split n es
        | n <= 0 = (Z, es)
        | n >= l = (es, Z)
        |  True  = (fromListN n take', fromListN (l - n) drop')
      where
        (take', drop') = split n $ toList es
        l = length es
    
    isPrefixOf xs ys = xs .<=. ys && equals
      where
        equals = and [ xs !# i == ys !# i | i <- [ 0 .. length xs - 1 ] ]
    
    isSuffixOf xs ys = xs .<=. ys && and equals
      where
        equals  = [ xs !# i == xs !# (i + offset') | i <- [ 0 .. length xs - 1 ] ]
        offset' = length ys - length xs

instance (Index i) => Bordered (Array i e) i e
  where
    lower   (Array l _ _ _) = l
    upper   (Array _ u _ _) = u
    sizeOf  (Array _ _ n _) = n

--------------------------------------------------------------------------------

{- Indexed instance. -}

instance (Index i) => Indexed (Array i e) i e
  where
    assoc' bnds defvalue ascs = runST $ ST $ \ s1# -> case newArray# n# defvalue s1# of
        (# s2#, marr# #) -> writes marr# s2#
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
    
    (!) arr@(Array l u _ _) i = arr !# offset (l, u) i
    
    p .$ es = (\ i -> p $ es ! i)  `find`  indices es
    p *$ es = (\ i -> p $ es ! i) `filter` indices es

--------------------------------------------------------------------------------

instance (Index i) => Estimate (Array i) where xs <==> ys = length xs <=> length ys

-- instance (Index i) => Set (Array i e) e

instance (Index i) => Default (Array i e) where def = Z

instance (Index i, Arbitrary e) => Arbitrary (Array i e) where arbitrary = fromList <$> arbitrary

--------------------------------------------------------------------------------

{-# INLINE (!#) #-}
(!#) :: Array i e -> Int -> e
(!#) (Array _ _ _ arr#) (I# i#) = case indexArray# arr# i# of (# e #) -> e

{-# INLINE fill #-}
fill :: MutableArray# s e -> (Int, e) -> STRep s a -> STRep s a
fill marr# (I# i#, e) nxt = \ s1# -> case writeArray# marr# i# e s1# of s2# -> nxt s2#

{-# INLINE done #-}
done :: (i, i) -> Int -> MutableArray# s e -> STRep s (Array i e)
done (l, u) n marr# = \s1# -> case unsafeFreezeArray# marr# s1# of
  (# s2#, arr# #) -> (# s2#, Array l u n arr# #)

pfailEx       :: String -> a
pfailEx   msg =  throw . PatternMatchFail $ "in SDP.Array." ++ msg

unreachEx     :: String -> a
unreachEx msg =  throw . UnreachableException $ "in SDP.Array." ++ msg

undEx :: String -> a
undEx msg = throw . UndefinedValue $ "in SDP.Array." ++ msg




