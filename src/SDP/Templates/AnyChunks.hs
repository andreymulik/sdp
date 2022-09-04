{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE Trustworthy, CPP, UndecidableInstances, BangPatterns #-}
{-# LANGUAGE TypeFamilies, DeriveDataTypeable, DeriveGeneric #-}

#ifdef SDP_QUALIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{- |
    Module      :  SDP.Templates.AnyChunks
    Copyright   :  (c) Andrey Mulik 2020-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Templates.AnyChunks" provides 'AnyChunks' - list of data chunks.
-}
module SDP.Templates.AnyChunks
(
  -- * Export
  module SDP.IndexedM,
  module SDP.Scan,
  
  -- * Chunk list
  AnyChunks, fromChunks, fromChunksM, toChunks
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM
import SDP.SortM
import SDP.Scan

import qualified GHC.Exts as E

import GHC.Generics

import Data.Default.Class
import Data.Typeable
import Data.String
import Data.Data

import Text.Read.SDP
import Text.Show.SDP

import SDP.SortM.Tim

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

{- |
  'AnyChunks' is list of data chunks. AnyChunks shouldn't contain empty chunks,
  so the 'AnyChunks' constructor is made private (see 'fromChunks' and
  'fromChunksM').
  
  * Efficiency of operations on @'AnyChunks' rep e@ are very sensitive in the
  efficiency of 'Bordered' and 'Linear' on @rep e@.
  * @'AnyChunks' rep e@ is only defined for Int-indexed @rep e@.
  * 'Eq', 'Ord', 'Eq1' and 'Ord1' instances compare @'AnyChunks' rep e@ as
  streams of equal size chunks. To do this, the comparison @rep e@ must also be
  lexicographic, also for @rep e@ must implement 'Bordered' and 'Linear'.
  * 'Freeze' and 'Thaw' for @'AnyChunks' rep e@ are defined for all @rep e@ that
  already have 'Freeze' and 'Thaw' instances.
-}
newtype AnyChunks rep e = AnyChunks [rep e] deriving ( Typeable, Data, Generic )

-- | Construct immutable 'AnyChunks' safely.
fromChunks :: Nullable1 rep e => [rep e] -> AnyChunks rep e
fromChunks =  AnyChunks . except isNull

-- | Construct mutable 'AnyChunks' safely.
fromChunksM :: BorderedM1 m rep Int e => [rep e] -> m (AnyChunks rep e)
fromChunksM =  fmap AnyChunks . go
  where
    go (x : xs) = do n <- getSizeOf x; n == 0 ? go xs $ (x :) <$> go xs
    go    []    = return []

-- | Extract immutable 'AnyChunks' chunks safely.
toChunks :: AnyChunks rep e -> [rep e]
toChunks =  E.coerce

--------------------------------------------------------------------------------

{- Eq and Ord instances. -}

instance (Eq (rep e), Bordered1 rep Int e, Linear1 rep e) => Eq (AnyChunks rep e)
  where
    Z == Z = True
    xs@(AnyChunks (x : xs')) == ys@(AnyChunks (y : ys')) = if n1 > n2
        then take n2 x == y && drop n2 xs == AnyChunks ys'
        else take n1 y == x && drop n1 ys == AnyChunks xs'
      where
        n1 = sizeOf x
        n2 = sizeOf y
    _ == _ = False

instance (Ord (rep e), Bordered1 rep Int e, Linear1 rep e) => Ord (AnyChunks rep e)
  where
    compare Z Z = EQ
    compare xs@(AnyChunks (x : xs')) ys@(AnyChunks (y : ys')) = if n1 > n2
        then (take n2 x <=> y) <> (drop n2 xs <=> AnyChunks ys')
        else (x <=> take n1 y) <> (AnyChunks xs' <=> drop n1 ys)
      where
        n1 = sizeOf x
        n2 = sizeOf y
    compare Z _ = LT
    compare _ _ = GT

--------------------------------------------------------------------------------

{- Eq1 and Ord1 instances. -}

#ifdef SDP_QUALIFIED_CONSTRAINTS
instance (Bordered' rep Int, Linear' rep, Eq1 rep) => Eq1 (AnyChunks rep)
  where
    liftEq f xs ys
        | isNull xs && isNull ys = True
        | isNull xs || isNull ys = False
        |         n1 > n2        = liftEq f (take n2 x) y && liftEq f (drop n2 xs) (fromChunks ys')
        |          True          = liftEq f x (take n1 y) && liftEq f (fromChunks xs') (drop n1 ys)
      where
        (x, xs') = uncons (toChunks xs); n1 = sizeOf x
        (y, ys') = uncons (toChunks ys); n2 = sizeOf y

instance (Bordered' rep Int, Linear' rep, Ord1 rep) => Ord1 (AnyChunks rep)
  where
    liftCompare _ Z   Z = EQ
    liftCompare _ Z   _ = LT
    liftCompare _ _   Z = GT
    liftCompare f xs ys = if n1 > n2
        then liftCompare f (take n2 x) y <> liftCompare f (drop n2 xs) (fromChunks ys')
        else liftCompare f x (take n1 y) <> liftCompare f (fromChunks xs') (drop n1 ys)
      where
        (x, xs') = uncons (toChunks xs); n1 = sizeOf x
        (y, ys') = uncons (toChunks ys); n2 = sizeOf y
#endif

--------------------------------------------------------------------------------

{- Show and Read instances. -}

instance {-# OVERLAPPABLE #-} (Indexed1 rep Int e, Show e) => Show (AnyChunks rep e)
  where
    showsPrec = assocsPrec "unlist "

instance (Indexed1 rep Int Char) => Show (AnyChunks rep Char)
  where
    showsPrec = shows ... const listL

instance (Indexed1 rep Int e, Read e) => Read (AnyChunks rep e)
  where
    readPrec = indexedPrec' "ublist"
    readList = readListDefault

--------------------------------------------------------------------------------

{- Semigroup, Monoid and Default instances. -}

instance Semigroup (AnyChunks rep e)
  where
    AnyChunks xs <> AnyChunks ys = AnyChunks (xs ++ ys)

instance Monoid  (AnyChunks rep e) where mempty = AnyChunks []; mappend = (<>)
instance Default (AnyChunks rep e) where def    = AnyChunks []

--------------------------------------------------------------------------------

{- Overloaded Lists and Strings support. -}

instance (Linear1 (AnyChunks rep) Char) => IsString (AnyChunks rep Char)
  where
    fromString = fromList

instance (Linear1 (AnyChunks rep) e) => E.IsList (AnyChunks rep e)
  where
    type Item (AnyChunks rep e) = e
    
    fromListN = fromListN
    fromList  = fromList
    toList    = listL

--------------------------------------------------------------------------------

{- Functor, Applicative and Zip instances. -}

instance Functor rep => Functor (AnyChunks rep)
  where
    fmap f (AnyChunks es) = AnyChunks (fmap f <$> es)

instance Applicative rep => Applicative (AnyChunks rep)
  where
    AnyChunks fs <*> AnyChunks es = AnyChunks $ liftA2 (<*>) fs es
    pure e = AnyChunks [pure e]

#ifdef SDP_QUALIFIED_CONSTRAINTS
instance (Functor rep, Bordered' rep Int, Linear' rep) => Zip (AnyChunks rep)
  where
    all2 f as bs             = all2 f (listL as) (listL bs)
    any2 f as bs             = any2 f (listL as) (listL bs)
    all3 f as bs cs          = all3 f (listL as) (listL bs) (listL cs)
    any3 f as bs cs          = any3 f (listL as) (listL bs) (listL cs)
    all4 f as bs cs ds       = all4 f (listL as) (listL bs) (listL cs) (listL ds)
    any4 f as bs cs ds       = any4 f (listL as) (listL bs) (listL cs) (listL ds)
    all5 f as bs cs ds es    = all5 f (listL as) (listL bs) (listL cs) (listL ds) (listL es)
    any5 f as bs cs ds es    = any5 f (listL as) (listL bs) (listL cs) (listL ds) (listL es)
    all6 f as bs cs ds es fs = all6 f (listL as) (listL bs) (listL cs) (listL ds) (listL es) (listL fs)
    any6 f as bs cs ds es fs = any6 f (listL as) (listL bs) (listL cs) (listL ds) (listL es) (listL fs)
    
    zipWith  f as bs             = fromList $ zipWith  f (listL as) (listL bs)
    zipWith3 f as bs cs          = fromList $ zipWith3 f (listL as) (listL bs) (listL cs)
    zipWith4 f as bs cs ds       = fromList $ zipWith4 f (listL as) (listL bs) (listL cs) (listL ds)
    zipWith5 f as bs cs ds es    = fromList $ zipWith5 f (listL as) (listL bs) (listL cs) (listL ds) (listL es)
    zipWith6 f as bs cs ds es fs = fromList $ zipWith6 f (listL as) (listL bs) (listL cs) (listL ds) (listL es) (listL fs)
#endif

--------------------------------------------------------------------------------

{- Foldable and Traversable instances. -}

instance Foldable rep => Foldable (AnyChunks rep)
  where
    foldr f base (AnyChunks es) = flip (foldr f) `foldr` base $ es
    foldl f base (AnyChunks es) = foldl (foldl f) base es
    
    elem e (AnyChunks es) = foldr ((||) . elem e) False es
    length (AnyChunks es) = foldr' ((+) . length) 0 es
    null   (AnyChunks es) = null es

instance Traversable rep => Traversable (AnyChunks rep)
  where
    traverse f (AnyChunks es) = AnyChunks <$> traverse (traverse f) es

--------------------------------------------------------------------------------

{- Nullable and NullableM instances. -}

instance Nullable (AnyChunks rep e)
  where
    isNull = \ (AnyChunks es) -> null es
    lzero  = AnyChunks []

instance (NullableM m (rep e)) => NullableM m (AnyChunks rep e)
  where
    nowNull = fmap and . mapM nowNull . toChunks
    newNull = return (AnyChunks [])

--------------------------------------------------------------------------------

{- Estimate and EstimateM instances. -}

instance Bordered1 rep Int e => Estimate (AnyChunks rep e)
  where
    (<==>) = go 0
      where
        go o (AnyChunks [])   (AnyChunks []) = o <=> 0
        go o (AnyChunks [])               ys = o <=.> ys
        go o xs               (AnyChunks []) = xs <.=> (-o)
        go o (AnyChunks (x : xs)) (AnyChunks (y : ys)) =
          go (o + sizeOf x - sizeOf y) (AnyChunks xs) (AnyChunks ys)
    
    (AnyChunks       []) <.=> n = 0 <=> n
    (AnyChunks (x : xs)) <.=> n = c > n ? GT $ AnyChunks xs <.=> (n - c)
      where
        c = sizeOf x

instance (Monad m, BorderedM1 m rep Int e) => EstimateM m (AnyChunks rep e)
  where
    estimateM = go 0
      where
        go o (AnyChunks [])   (AnyChunks []) = return (o <=> 0)
        go o (AnyChunks [])               ys = lestimateM' ys o
        go o xs               (AnyChunks []) = lestimateM' xs (-o)
        go o (AnyChunks (x : xs)) (AnyChunks (y : ys)) = do
          sx <- getSizeOf x
          sy <- getSizeOf y
          go (o + sx - sy) (AnyChunks xs) (AnyChunks ys)
    
    lestimateM' (AnyChunks       []) n = return (0 <=> n)
    lestimateM' (AnyChunks (x : xs)) n = do
      c <- getSizeOf x
      c > n ? return GT $ lestimateM' (AnyChunks xs) (n - c)

--------------------------------------------------------------------------------

{- Bordered and BorderedM instances. -}

instance Bordered1 rep Int e => Bordered (AnyChunks rep e) Int
  where
    sizeOf (AnyChunks es) = foldr' ((+) . sizeOf) 0 es
    
    indexIn es = \ i -> i >= 0 && i <. es
    
    -- | Quick unchecked offset.
    offsetOf = const id
    
    -- | Quick unchecked index.
    indexOf  = const id
    
    lower   _ = 0
    upper  es = sizeOf es - 1
    bounds es = (0, sizeOf es - 1)
    
    rebound bnds es = AnyChunks (size bnds `go` toChunks es)
      where
        go c (x : xs) = let s = sizeOf x in case c <=> s of
          LT -> [defaultBounds c `rebound` x]
          GT -> x : go (c - s) xs
          EQ -> [x]
        go _    []    = []

instance BorderedM1 m rep Int e => BorderedM m (AnyChunks rep e) Int
  where
    getLower = const $ return 0
    getUpper = fmap pred . getSizeOf
    
    getSizeOf = foldr (liftA2 (+) . getSizeOf) (return 0) . toChunks
    
    getIndices es = do n <- getSizeOf es; return [0 .. n - 1]
    nowIndexIn es = \ i -> i < 0 ? return False $ do n <- getSizeOf es; return (i < n)
    
    rebounded' bnds (AnyChunks es) = AnyChunks <$> go (size bnds) es
      where
        go _    []    = return []
        go n (x : xs) = do
          s <- getSizeOf x
          n <= s ? return [] $ (x :) <$> go (n - s) xs

--------------------------------------------------------------------------------

{- Forceable and Linear instances. -}

instance Forceable1 rep e => Forceable (AnyChunks rep e)
  where
    force = AnyChunks . force . E.coerce

instance (Bordered1 rep Int e, Linear1 rep e) => Linear (AnyChunks rep e) e
  where
    single e = AnyChunks [single e]
    
    toHead e (AnyChunks es@(x : xs)) = AnyChunks $ sizeOf x < lim ? (e :> x) : xs $ single e : es
    toHead e                       _ = single e
    
    toLast (AnyChunks (xs :< x)) e = isNull x ? AnyChunks xs :< e $ AnyChunks (xs :< (x :< e))
    toLast _                     e = single e
    
    uncons = go . toChunks
      where
        go ((x :> xs) : xss) = (x, AnyChunks (xs : xss))
        go                 _ = pfailEx "(:>)"
    
    unsnoc = go . toChunks
      where
        go (xss :< (xs :< x)) = (AnyChunks (xss :< xs), x)
        go                  _ = pfailEx "(:<)"
    
    fromList = AnyChunks . fmap fromList . chunks lim
    listL    = foldr ((++) . listL) [] . toChunks
    
    (AnyChunks (x : xs)) !^ i = let n = sizeOf x in i < n ? x !^ i $ AnyChunks xs !^ (i - n)
    _                    !^ _ = error "in SDP.Unrolled.Unlist.(!^)"
    
    write es'@(AnyChunks es) n e =
      let
        go i (x : xs) = let c = sizeOf x in i < c ? write x i e : xs $ x : go (i - c) xs
        go _       xs = xs
      in  n < 0 ? es' $ AnyChunks (go n es)
    
    -- | Deduplicated chunks.
    replicate n e = AnyChunks $ replicate count chunk :< rest
      where
        (count, rst) = n `divMod` lim
        
        chunk = replicate lim e
        rest  = replicate rst e
    
    partition p = both fromList . partition p . listL
    
    select  f (AnyChunks es) = concatMap (select f) es
    reverse   (AnyChunks es) = AnyChunks (reverse <$> reverse es)
    extract f (AnyChunks es) = bimap concat AnyChunks . unzip $ extract f <$> es
    
    before (AnyChunks ess) i e = AnyChunks $ go (max 0 i) ess
      where
        go n (xs : xss) =
          let c = sizeOf xs
          in  n < c ? before xs n e : xss $ xs : go (n - c) xss
        go _     []     = [single e]
    
    remove i es@(AnyChunks ess) = i < 0 ? es $ AnyChunks (go i ess)
      where
        go n (xs : xss)
            | c == 1 = xss -- single chunk
            | n >= c = xs : go (n - c) xss
            |  True  = remove n xs : xss
          where
            c = sizeOf xs
        go _     []     = []
    
    selects fs = second fromList . selects fs . listL
    
    ofoldr f' base' = go 0 f' base' . toChunks
      where
        go o f base (x : xs) = ofoldr (f . (o +)) (go (o + sizeOf x) f base xs) x
        go _ _ base _ = base
    
    ofoldl f' base' = go 0 f' base' . toChunks
      where
        go o f base (x : xs) = go (o + sizeOf x) f (ofoldl (f . (o +)) base x) xs
        go _ _ base _ = base
    
    o_foldr f base = foldr (flip $ o_foldr f) base . toChunks
    o_foldl f base = foldl (o_foldl f) base . toChunks
    
    take n = AnyChunks . go n . toChunks
      where
        go c (x : xs) = let s = sizeOf x in case c <=> s of
          GT -> x : go (c - s) xs
          LT -> [take c x]
          EQ -> [x]
        go _    []    = []

    drop n = AnyChunks . go n . toChunks
      where
        go c (x : xs) = let s = sizeOf x in case c <=> s of
          GT -> go (c - s) xs
          LT -> drop c x : xs
          EQ -> xs
        go _    []    = []
    
    prefix f = foldr' (\ e n -> let p = prefix f e in p ==. e ? p + n $ p) 0 . toChunks
    suffix f = foldl' (\ n e -> let s = suffix f e in s ==. e ? s + n $ s) 0 . toChunks
    combo  f = foldr' (\ e n -> let c = combo  f e in c ==. e ? c + n $ c) 0 . toChunks

--------------------------------------------------------------------------------

{- Copyable and LinearM instances. -}

instance (Monad m, BorderedM1 m rep Int e, LinearM1 m rep e)
      => Copyable m (AnyChunks rep e)
  where
    copied = fmap AnyChunks . mapM copied . toChunks

instance (BorderedM1 m rep Int e, LinearM1 m rep e)
      => LinearM m (AnyChunks rep e) e
  where
    getHead = getHead . head . toChunks
    getLast = getLast . last . toChunks
    
    prepend e' es' = AnyChunks <$> go e' (toChunks es')
      where
        go e     []      = pure <$> newLinear [e]
        go e es@(x : xs) = do
          n <- getSizeOf x
          n < lim ? (: xs) <$> prepend e x $ (: es) <$> newLinear [e]
    
    append es' e' = AnyChunks <$> go e' (toChunks es')
      where
        go e      []      = pure <$> newLinear [e]
        go e es@(xs :< x) = do
          n <- getSizeOf x
          n < lim ? (xs :<) <$> append x e $ (es :<) <$> newLinear [e]
    
    newLinear = fmap AnyChunks . mapM newLinear . chunks lim
    
    (!#>) (AnyChunks es) = go es
      where
        go (x : xs) i = do n <- getSizeOf x; i < n ? x !#> i $ go xs (i - n)
        go _        _ = overEx "(>!)"
    
    {-# INLINE writeM #-}
    writeM = go . toChunks
      where
        go    []    _ _ = return ()
        go (x : xs) i e = do
          n <- getSizeOf x
          i < n ? writeM x i e $ go xs (i - n) e
    
    getLeft  (AnyChunks es) = concat <$> mapM getLeft es
    getRight (AnyChunks es) = (concat . reverse) <$> mapM getRight es
    reversed (AnyChunks es) = (AnyChunks . reverse) <$> mapM reversed es
    
    filled c e =
      let (d, n) = c `divMod` lim
      in  AnyChunks <$> sequence (replicate d (filled lim e) :< filled n e)
    
    copied' xs l n = copied =<< takeM n =<< dropM l xs
    
    copyTo src os trg ot c = when (c > 0) $ do
        when (os < 0 || ot < 0) $ underEx "copyTo"
        src' <- dropM os src
        trg' <- dropM ot trg
        go c src' trg'
      where
        go n xs@(AnyChunks (x : _)) ys@(AnyChunks (y : _)) = do
          n1 <- getSizeOf x
          n2 <- getSizeOf y
          let n' = n1 `min` n2 `min` n
          
          copyTo x 0 y 0 n'
          dropM n' xs >>=<< dropM n' ys $ go (n - n')
        go n _ _ = when (n > 0) $ overEx "copyTo"
    
    -- | Unsafe, returns joined stream of existing chunks.
    merged = return . AnyChunks . foldr ((++) . toChunks) []
    
    ofoldrM f base' = ofoldrCh 0 base' . toChunks
      where
        ofoldrCh !o base (x : xs) = do
          n   <- getSizeOf x
          xs' <- ofoldrCh (o + n) base xs
          ofoldrM (f . (o +)) xs' x
        ofoldrCh _ base _ = return base
    
    ofoldlM f base' = ofoldlCh 0 base' . toChunks
      where
        ofoldlCh !o base (x : xs) = do
          n  <- getSizeOf x
          x' <- ofoldlM (f . (o +)) base x
          ofoldlCh (o + n) x' xs
        ofoldlCh _ base _ = return base
    
    foldrM f base = foldr ((=<<) . flip (foldrM f)) (return base) . toChunks
    foldlM f base = foldl (flip $ (=<<) . flip (foldlM f)) (return base) . toChunks
    
    miterate n = fmap (AnyChunks  . pure) ... miterate n
    iterateM n = fmap (AnyChunks  . pure) ... iterateM n
    
    takeM n (AnyChunks (e : es)) = n < 1 ? newNull $ do
      c <- getSizeOf e
      case n <=> c of
        EQ -> return (AnyChunks [e])
        LT -> do t <- takeM n e; return (AnyChunks [t])
        GT -> do (AnyChunks ts) <- takeM (n - c) (AnyChunks es); return $ AnyChunks (e : ts)
    takeM _ es = return es
    
    dropM n es'@(AnyChunks (e : es)) = n < 1 ? return es' $ do
      c <- getSizeOf e
      case n <=> c of
        EQ -> return (AnyChunks es)
        GT -> dropM (n - c) (AnyChunks es)
        LT -> do d <- dropM n e; return $ AnyChunks (d : es)
    dropM _ es = return es
    
    prefixM f (AnyChunks es) = foldr (\ e p -> do n <- getSizeOf e; c <- prefixM f e; c == n ? (+ c) <$> p $ return c) (return 0) es
    suffixM f (AnyChunks es) = foldl (\ p e -> do n <- getSizeOf e; c <- suffixM f e; c == n ? (+ c) <$> p $ return c) (return 0) es
    mprefix f (AnyChunks es) = foldr (\ e p -> do n <- getSizeOf e; c <- mprefix f e; c == n ? (+ c) <$> p $ return c) (return 0) es
    msuffix f (AnyChunks es) = foldl (\ p e -> do n <- getSizeOf e; c <- msuffix f e; c == n ? (+ c) <$> p $ return c) (return 0) es

--------------------------------------------------------------------------------

{- Set and SetWith instances. -}

instance (Nullable (AnyChunks rep e), SetWith1 (AnyChunks rep) e, Ord e)
      => Set (AnyChunks rep e) e

instance (SetWith1 rep e, Linear1 rep e, Ord (rep e), Bordered1 rep Int e)
      => SetWith (AnyChunks rep e) e
  where
    insertWith f' e' = AnyChunks . go f' e' . toChunks
      where
        go f e (x : xs) = memberWith f e x ? insertWith f e x : xs $ x : go f e xs
        go _ e        _ = [single e]
    
    deleteWith f' e' = AnyChunks . go f' e' . toChunks
      where
        go f e (x : xs) = memberWith f e x ? deleteWith f e x : xs $ x : go f e xs
        go _ _        _ = []
    
    intersectionWith f = fromList ... on (intersectionWith f) listL
    differenceWith   f = fromList ... on (differenceWith   f) listL
    symdiffWith      f = fromList ... on (symdiffWith      f) listL
    unionWith        f = fromList ... on (unionWith        f) listL
    
    lookupLTWith f o = foldr ((<|>) . lookupLTWith f o) Nothing . toChunks
    lookupLEWith f o = foldr ((<|>) . lookupLEWith f o) Nothing . toChunks
    lookupGTWith f o = foldr ((<|>) . lookupGTWith f o) Nothing . toChunks
    lookupGEWith f o = foldr ((<|>) . lookupGEWith f o) Nothing . toChunks
    
    memberWith f x (AnyChunks es) = memberWith f x `any` es
    
    isSubsetWith f xs ys = o_foldr (\ e b -> memberWith f e ys && b) True xs

--------------------------------------------------------------------------------

{- Scan instances. -}

instance (Linear1 (AnyChunks rep) e) => Scan (AnyChunks rep e) e

--------------------------------------------------------------------------------

{- Map and Indexed instances. -}

instance (Indexed1 rep Int e) => Map (AnyChunks rep e) Int e
  where
    toMap ascs =
      let bnds = rangeBounds (fsts ascs)
      in  isNull ascs ? Z $ assoc bnds ascs
    
    toMap' defvalue ascs =
      let bnds = rangeBounds (fsts ascs)
      in  isNull ascs ? Z $ assoc' bnds defvalue ascs
    
    (.!) = (!^)
    
    Z            // ascs = toMap ascs
    AnyChunks es // ascs = AnyChunks (go 0 es ascs)
      where
        go _    []     _  = []
        go _    xs    [ ] = xs
        go l (x : xs) ies = x // as : go n xs bs
          where
            (as, bs) = partition (inRange (l, n - 1) . fst) ies
            n = l + sizeOf es
    
    p .$ AnyChunks (x : xs) = p .$ x <|> (+ sizeOf x) <$> p .$ AnyChunks xs
    _ .$                  _ = Nothing
    
    p *$ AnyChunks (x : xs) = p *$ x ++ fmap (+ sizeOf x) (p *$ AnyChunks xs)
    _ *$                  _ = []
    
    kfoldr f base es = let bnds = bounds es in kfoldr (f . index bnds) base es
    kfoldl f base es = let bnds = bounds es in kfoldl (f . index bnds) base es

instance (Indexed1 rep Int e) => Indexed (AnyChunks rep e) Int e
  where
    assoc bnds ascs = AnyChunks (go bnds ascs)
      where
        go (l, u) ies = isEmpty (l, u) ? [] $ assoc (l, n) as : go (n + 1, u) bs
          where
            (as, bs) = partition (inRange (l, n) . fst) ies
            n = min u (l + lim)
    
    assoc' bnds defvalue ascs = AnyChunks (go bnds ascs)
      where
        go (l, u) ies = isEmpty (l, u) ? [] $ assoc' (l, n) defvalue as : go (n + 1, u) bs
          where
            (as, bs) = partition (inRange (l, n) . fst) ies
            n = min u (l + lim)
    
    fromIndexed es = AnyChunks [fromIndexed es]

--------------------------------------------------------------------------------

{- MapM and IndexedM instances. -}

instance (LinearM1 m rep e, MapM1 m rep Int e, BorderedM1 m rep Int e)
      => MapM m (AnyChunks rep e) Int e
  where
    newMap ascs = AnyChunks <$> sequence (go bnds ascs)
      where
        bnds = rangeBounds (fsts ascs)
        
        go (l, u) ies = isEmpty (l, u) ? [] $ newMap as : go (n + 1, u) bs
          where
            (as, bs) = partition (inRange (l, n) . fst) ies
            n = min u (l + lim)
    
    newMap' defvalue ascs = AnyChunks <$> sequence (go bnds ascs)
      where
        bnds = rangeBounds (fsts ascs)
        
        go (l, u) ies = newMap' defvalue as : go (n + 1, u) bs
          where
            (as, bs) = partition (inRange (l, n) . fst) ies
            n = min u (l + lim)
    
    {-# INLINE writeM' #-}
    writeM' es i e = (i < 0) `unless` writeM es i e
    
    {-# INLINE (>!) #-}
    es >! i = i < 0 ? overEx "(>!)" $ es !#> i
    
    overwrite es'@(AnyChunks []) ascs = isNull ascs ? return es' $ newMap ascs
    overwrite es'@(AnyChunks es) ascs = es' <$ go 0 es ((< 0) . fst `except` ascs)
      where
        go o (x : xs) ie = unless (null ie) $ do
          n <- getSizeOf x
          let (as, bs) = partition (\ (i, _) -> i < n + o) ie
          overwrite x as >> go (n + o) xs bs
        go _ _ _ = return ()
    
    kfoldrM = ofoldrM
    kfoldlM = ofoldlM

instance (IndexedM1 m rep Int e) => IndexedM m (AnyChunks rep e) Int e
  where
    fromAssocs bnds ascs = AnyChunks <$> sequence (go bnds ascs)
      where
        go (l, u) ies = isEmpty (l, u) ? [] $ fromAssocs (l, n) as : go (n + 1, u) bs
          where
            (as, bs) = partition (inRange (l, n) . fst) ies
            n = min u (l + lim)
    
    fromAssocs' bnds defvalue ascs = AnyChunks <$> sequence (go bnds ascs)
      where
        go (l, u) ies = fromAssocs' (l, n) defvalue as : go (n + 1, u) bs
          where
            (as, bs) = partition (inRange (l, n) . fst) ies
            n = min u (l + lim)
    
    fromIndexed' = newLinear  .  listL
    fromIndexedM = newLinear <=< getLeft

--------------------------------------------------------------------------------

{- SortM instance. -}

instance (BorderedM1 m rep Int e, SortM1 m rep e, LinearM1 m rep e)
      => SortM m (AnyChunks rep e) e
  where
    sortMBy     = timSortBy
    sortedMBy f = go . toChunks
      where
        go (x1 : x2 : xs) =
          let restM = liftA2 f (getLast x1) (getHead x2) ?^ go (x2 : xs) $ return False
          in  sortedMBy f x1 ?^ restM $ return False
        go      [x1]      = sortedMBy f x1
        go       []       = return True

--------------------------------------------------------------------------------

-- | Creates new local immutable structure and thaw it unsafely.
instance {-# OVERLAPPABLE #-} (Linear1 imm e, Thaw1 m imm mut e)
      => Thaw m (AnyChunks imm e) (mut e)
  where
    -- @concat [e]@ may return e
    thaw (AnyChunks [e]) = thaw e
    -- any correct concat will create an intermediate structure in this case
    thaw (AnyChunks ess) = unsafeThaw (concat ess)
    
    unsafeThaw = unsafeThaw . concat . toChunks

-- | Creates one-chunk mutable stream, may be memory inefficient.
instance {-# OVERLAPPABLE #-} (Thaw1 m imm mut e)
      => Thaw m (imm e) (AnyChunks mut e)
  where
    unsafeThaw = fmap (AnyChunks . single) . unsafeThaw
    thaw       = fmap (AnyChunks . single) . thaw

instance {-# OVERLAPS #-} (Thaw1 m imm mut e)
      => Thaw m (AnyChunks imm e) (AnyChunks mut e)
  where
    unsafeThaw (AnyChunks imm) = AnyChunks <$> mapM unsafeThaw imm
    thaw       (AnyChunks imm) = AnyChunks <$> mapM thaw imm

-- | Creates one-chunk immutable stream, may be memory inefficient.
instance {-# OVERLAPPABLE #-} (Freeze1 m mut imm e)
      => Freeze m (mut e) (AnyChunks imm e)
  where
    unsafeFreeze = fmap (AnyChunks . single) . unsafeFreeze
    freeze       = fmap (AnyChunks . single) . freeze

-- | Creates new immutable structure using 'merged'.
instance {-# OVERLAPPABLE #-} (LinearM1 m mut e, Freeze1 m mut imm e)
      => Freeze m (AnyChunks mut e) (imm e)
  where
    unsafeFreeze (AnyChunks es) = unsafeFreeze =<< merged es
    freeze       (AnyChunks es) = freeze       =<< merged es

instance {-# OVERLAPS #-} (Freeze1 m mut imm e)
      => Freeze m (AnyChunks mut e) (AnyChunks imm e)
  where
    unsafeFreeze (AnyChunks mut) = AnyChunks <$> mapM unsafeFreeze mut
    freeze       (AnyChunks mut) = AnyChunks <$> mapM freeze mut

--------------------------------------------------------------------------------

overEx :: String -> a
overEx =  throw . IndexOverflow . showString "in SDP.Templates.AnyChunks."

underEx :: String -> a
underEx =  throw . IndexUnderflow . showString "in SDP.Templates.AnyChunks."

pfailEx :: String -> a
pfailEx =  throw . PatternMatchFail . showString "in SDP.Templates.AnyChunks."

lim :: Int
lim =  1024



