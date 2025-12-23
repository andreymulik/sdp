{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE Trustworthy, UndecidableInstances, TypeFamilies, GADTs #-}

{- |
    Module      :  SDP.Templates.AnyBorder
    Copyright   :  (c) Andrey Mulik 2020-2025
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Templates.AnyBorder" provides 'AnyBorder' - template of generalized by
    index type structure, based on 'Int'-indexed primitive.
-}
module SDP.Templates.AnyBorder
(
  -- * Export
  module SDP.IndexedM,
  module SDP.Sort,
  module SDP.Scan,
  
  -- * Border template
  AnyBorder (..)
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM
import SDP.SortM
import SDP.Sort
import SDP.Scan

import Data.String

import Text.Read.SDP
import Text.Show.SDP

import qualified GHC.Exts as E

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

{- |
  'AnyBorder' is template, that appends arbitrary bounds to any structure.
  
  * 'Eq', 'Ord', 'Eq1' and 'Ord1' instances ingores bounds.
  * 'Thaw' and 'Freeze' instances for @'AnyBorder' rep e@ inherit @rep e@
  behavior.
-}
data AnyBorder rep i e
  where
    AnyBorder :: Index i => !i -> !i -> !(rep e) -> AnyBorder rep i e

--------------------------------------------------------------------------------

{- Eq ad Eq1 instances. -}

instance Eq (rep e) => Eq (AnyBorder rep i e)
  where
    (==) = on (==) unpack

instance Eq1 rep => Eq1 (AnyBorder rep i)
  where
    liftEq f xs ys = liftEq f (unpack xs) (unpack ys)

--------------------------------------------------------------------------------

{- Ord and Ord1 instances. -}

instance Ord (rep e) => Ord (AnyBorder rep i e)
  where
    compare = on (<=>) unpack

instance Ord1 rep => Ord1 (AnyBorder rep i)
  where
    liftCompare f xs ys = liftCompare f (unpack xs) (unpack ys)

--------------------------------------------------------------------------------

{- Show and Read instances. -}

instance {-# OVERLAPPABLE #-} (Indexed1 rep Int e, Show i, Show e)
      => Show (AnyBorder rep i e)
  where
    showsPrec i es@(AnyBorder _ _ _) = assocsPrec "array " i es

instance (Indexed1 rep Int Char, Show i) => Show (AnyBorder rep i Char)
  where
    showsPrec _ es@(AnyBorder _ _ _) = shows (listL es)

instance (Indexed1 rep Int e, Index i, Read i, Read e) => Read (AnyBorder rep i e)
  where
    readPrec = indexedPrec' "array"
    readList = readListDefault

--------------------------------------------------------------------------------

{- Overloaded Lists and String support. -}

instance (Index i, IsString (rep Char), Estimate1 rep Char)
      => IsString (AnyBorder rep i Char)
  where
    fromString str =
      let bnds = defaultBounds (sizeOf str)
      in  uncurry AnyBorder bnds (fromString str)

instance (Index i, E.IsList (rep e), Estimate1 rep e)
      => E.IsList (AnyBorder rep i e)
  where
    type Item (AnyBorder rep i e) = E.Item (rep e)
    
    fromListN = withBounds ... E.fromListN
    fromList  = withBounds . E.fromList
    toList    = E.toList . unpack

--------------------------------------------------------------------------------

{- Semigroup and Monoid instances. -}

instance (Index i, Semigroup (rep e), Estimate1 rep e)
      => Semigroup (AnyBorder rep i e)
  where
    (<>) = withBounds ... on (<>) unpack

instance (Index i, Semigroup (AnyBorder rep i e), Nullable1 rep e)
      => Monoid (AnyBorder rep i e)
  where
    mappend = (<>)
    mempty  = Z

--------------------------------------------------------------------------------

{- Nullable and NullableM instances. -}

instance (Index i, Nullable1 rep e) => Nullable (AnyBorder rep i e)
  where
    lzero  = uncurry AnyBorder (defaultBounds 0) lzero
    isNull = \ (AnyBorder l u rep) -> isEmpty (l, u) || isNull rep

instance (Index i, NullableM1 m rep e) => NullableM m (AnyBorder rep i e)
  where
    newNull = uncurry AnyBorder (defaultBounds 0) <$> newNull
    isNullM (AnyBorder l u es) = isEmpty (l, u) ? return True $ isNullM es

--------------------------------------------------------------------------------

{- Estimate and EstimateM instances. -}

instance Estimate (AnyBorder rep i e)
  where
    sizeOf (AnyBorder l u _) = size (l, u)
    
    sizeHint = Just . SizeHintEQ . sizeOf
    
    shrinkTo n es@(AnyBorder l _ xs)
        | isEmpty bnds = es
        |    n >. es   = throw expandEx
        |     n < 1    = uncurry AnyBorder (defaultBounds 0) xs
        |     True     = AnyBorder l u xs
      where
        expandEx = UnacceptableExpansion
                 . showString "in SDP.Template.AnyBorder.shrinkTo: new borders "
                 $ shows bnds " can't be wider than range of list values"
        
        u = index bnds (n - 1)
        
        bnds = bounds es
    
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

instance Monad m => EstimateM m (AnyBorder rep i e)
  where
    getSizeHint = return . sizeHint
    getSizeOf   = return . sizeOf
    
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

instance Index i => Bordered (AnyBorder rep i e) i
  where
    lower    (AnyBorder l _ _) = l
    upper    (AnyBorder _ u _) = u
    bounds   (AnyBorder l u _) = (l, u)
    indices  (AnyBorder l u _) = range   (l, u)
    indexIn  (AnyBorder l u _) = inRange (l, u)
    indexOf  (AnyBorder l u _) = index   (l, u)
    offsetOf (AnyBorder l u _) = offset  (l, u)
    
    eitherViewOf bnds es = n <=. es ? Right (shrinkTo n es) $ Left err
      where
        err = UnacceptableExpansion
            . showString "in SDP.Template.AnyBorder.eitherViewOf: new borders "
            $ shows bnds " can't be wider than range of list values"
        
        n = size bnds

instance (Monad m, Index i) => BorderedM m (AnyBorder rep i e) i
  where
    getLower    = pure . lower
    getUpper    = pure . upper
    getBounds   = pure . bounds
    getIndices  = pure . indices
    getSizesOf  = pure . sizesOf
    nowIndexIn  = pure ... indexIn
    getIndexOf  = pure ... indexOf
    getOffsetOf = pure ... offsetOf
    
    getEitherViewOf = pure ... eitherViewOf

--------------------------------------------------------------------------------

{- Functor, Zip and Applicative instances. -}

instance Functor rep => Functor (AnyBorder rep i)
  where
    fmap f (AnyBorder l u rep) = AnyBorder l u (f <$> rep)

instance Zip rep => Zip (AnyBorder rep i)
  where
    all2 f as bs             = all2 f (unpack as) (unpack bs)
    all3 f as bs cs          = all3 f (unpack as) (unpack bs) (unpack cs)
    all4 f as bs cs ds       = all4 f (unpack as) (unpack bs) (unpack cs) (unpack ds)
    all5 f as bs cs ds es    = all5 f (unpack as) (unpack bs) (unpack cs) (unpack ds) (unpack es)
    all6 f as bs cs ds es fs = all6 f (unpack as) (unpack bs) (unpack cs) (unpack ds) (unpack es) (unpack fs)
    
    any2 f as bs             = any2 f (unpack as) (unpack bs)
    any3 f as bs cs          = any3 f (unpack as) (unpack bs) (unpack cs)
    any4 f as bs cs ds       = any4 f (unpack as) (unpack bs) (unpack cs) (unpack ds)
    any5 f as bs cs ds es    = any5 f (unpack as) (unpack bs) (unpack cs) (unpack ds) (unpack es)
    any6 f as bs cs ds es fs = any6 f (unpack as) (unpack bs) (unpack cs) (unpack ds) (unpack es) (unpack fs)
    
    zipWith f as@(AnyBorder _ _ _) bs =
      let (l, u) = defaultBounds $ minimum [sizeOf as, sizeOf bs]
      in  AnyBorder l u $ zipWith f (unpack as) (unpack bs)
    
    zipWith3 f as@(AnyBorder _ _ _) bs cs =
      let (l, u) = defaultBounds $ minimum [sizeOf as, sizeOf bs, sizeOf cs]
      in  AnyBorder l u $ zipWith3 f (unpack as) (unpack bs) (unpack cs)
    
    zipWith4 f as@(AnyBorder _ _ _) bs cs ds =
      let (l, u) = defaultBounds $ minimum [sizeOf as, sizeOf bs, sizeOf cs, sizeOf ds]
      in  AnyBorder l u $ zipWith4 f (unpack as) (unpack bs) (unpack cs) (unpack ds)
    
    zipWith5 f as@(AnyBorder _ _ _) bs cs ds es =
      let (l, u) = defaultBounds $ minimum [sizeOf as, sizeOf bs, sizeOf cs, sizeOf ds, sizeOf es]
      in  AnyBorder l u $ zipWith5 f (unpack as) (unpack bs) (unpack cs) (unpack ds) (unpack es)
    
    zipWith6 f as@(AnyBorder _ _ _) bs cs ds es fs =
      let (l, u) = defaultBounds $ minimum [sizeOf as, sizeOf bs, sizeOf cs, sizeOf ds, sizeOf es, sizeOf fs]
      in  AnyBorder l u $ zipWith6 f (unpack as) (unpack bs) (unpack cs) (unpack ds) (unpack es) (unpack fs)

instance (Index i, Applicative rep) => Applicative (AnyBorder rep i)
  where
    pure = uncurry AnyBorder (defaultBounds 1) . pure
    
    AnyBorder lf uf fs <*> AnyBorder le ue es =
      let (l, u) = defaultBounds (size (lf, uf) * size (le, ue))
      in  AnyBorder l u (fs <*> es)

--------------------------------------------------------------------------------

{- Foldable and Traversable instances. -}

instance Foldable rep => Foldable (AnyBorder rep i)
  where
    foldr  f base = foldr  f base . unpack
    foldl  f base = foldl  f base . unpack
    foldr' f base = foldr' f base . unpack
    foldl' f base = foldl' f base . unpack
    
    foldr1 f = foldr1 f . unpack
    foldl1 f = foldl1 f . unpack
    
    length = length . unpack
    toList = toList . unpack
    null   = null   . unpack

instance Traversable rep => Traversable (AnyBorder rep i)
  where
    traverse f (AnyBorder l u es) = AnyBorder l u <$> traverse f es

--------------------------------------------------------------------------------

{- Forceable instance. -}

instance Forceable1 rep e => Forceable (AnyBorder rep i e)
  where
    force (AnyBorder l u rep) = AnyBorder l u (force rep)

--------------------------------------------------------------------------------

{- Concat instance. -}

instance (Index i, Concat (rep e), Estimate (rep e)) => Concat (AnyBorder rep i e)
  where
    concatMap = withBounds ... concatMap . (unpack .)
    concat    = withBounds  .  concatMap unpack

--------------------------------------------------------------------------------

instance (Index i, Sequence1 rep e) => Sequence (AnyBorder rep i e) e
  where
    ofoldr f base = ofoldr f base . unpack
    ofoldl f base = ofoldl f base . unpack
    
    sfoldr f base = sfoldr f base . unpack
    sfoldl f base = sfoldl f base . unpack
    
    listL = listL . unpack
    listR = listR . unpack
    
    prefix p = prefix p . unpack
    suffix p = suffix p . unpack

instance (Index i, Linear1 rep e) => Linear (AnyBorder rep i e) e
  where
    toHead e es = withBounds (e :> unpack es)
    toLast es e = withBounds (unpack es :< e)
    
    uncons' es =
      let (l, u) = bounds es; u' = prev (l, u) u
      in  second (AnyBorder l u') <$> uncons' (unpack es)
    
    unsnoc' es =
      let (l, u) = bounds es; l' = next (l, u) l
      in  first (AnyBorder l' u) <$> unsnoc' (unpack es)
    
    take = fst ... split
    drop = snd ... split
    sans = fst ... divide
    keep = snd ... divide
    
    split n es
        | n <= 0 = (Z, es)
        | n >= c = (es, Z)
        |  True  = (AnyBorder l i ts, AnyBorder j u ds)
      where
        (ts, ds) = split n (unpack es)
        (l, u)   = bounds es
        
        i = index (l, u) (n - 1)
        j = index (l, u) n
        c = size  (l, u)
    
    divide n es
        | n <= 0 = (es, Z)
        | n >= c = (Z, es)
        |  True  = (AnyBorder l i ss, AnyBorder j u ks)
      where
        (ss, ks) = divide n (unpack es)
        (l, u)   = bounds es
        
        i = index (l, u) (c - n - 1)
        j = index (l, u) (c - n)
        c = size  (l, u)
    
    single = withBounds . single
    
    fromList     = withBounds  .  fromList
    fromListN    = withBounds ... fromListN
    replicate    = withBounds ... replicate
    fromFoldable = withBounds  .  fromFoldable
    
    reverse (AnyBorder l u rep) = AnyBorder l u (reverse rep)
    
    filter f = withBounds . filter f . unpack
    
    isSubseqOf = isSubseqOf `on` unpack
    isPrefixOf = isPrefixOf `on` unpack
    isSuffixOf = isSuffixOf `on` unpack
    isInfixOf  = isInfixOf  `on` unpack
    
    (!!) = (!!) . unpack
    
    write (AnyBorder l u es) i e = AnyBorder l u (write es i e)
    
    nubBy f = withBounds . nubBy f . unpack
    nub     = withBounds .   nub   . unpack
    
    pad n e = withBounds . pad n e . unpack

--------------------------------------------------------------------------------

{- ForceableM and LinearM instances. -}

instance ForceableM1 m rep e => ForceableM m (AnyBorder rep i e)
  where
    copied (AnyBorder l u es) = AnyBorder l u <$> copied es

instance (Index i, EstimateM m (rep e), ConcatM m (rep e)) => ConcatM m (AnyBorder rep i e)
  where
    (<~>)        = withBounds' <=<< on (<~>) unpack
    concatM      = withBounds' <=< concatMapM (pure . unpack)
    concatMapM f = withBounds' <=< concatMapM (fmap unpack . f)

instance (Index i, SequenceM1 m rep e) => SequenceM m (AnyBorder rep i e) e
  where
    ofoldrM f e = ofoldrM f e . unpack
    ofoldlM f e = ofoldlM f e . unpack
    
    foldrM f e = foldrM f e . unpack
    foldlM f e = foldlM f e . unpack
    
    getLeft  = getLeft  . unpack
    getRight = getRight . unpack
    
    mprefix p = mprefix p . unpack
    msuffix p = msuffix p . unpack

instance (Index i, BorderedM1 m rep Int e, LinearM1 m rep e) => LinearM m (AnyBorder rep i e) e
  where
    prepend e = withBounds' <=< prepend e . unpack
    append es = withBounds' <=< append (unpack es)
    
    unconsM' (AnyBorder l u es) =
      let f = AnyBorder l' u; l' = next (l, u) l
      in  (second f <$>) <$> unconsM' es
    
    unsnocM' (AnyBorder l u es) =
      let f = AnyBorder l u'; u' = prev (l, u) u
      in  (first f <$>) <$> unsnocM' es
    
    takeM = fsts ... splitM
    dropM = snds ... splitM
    sansM = fsts ... divideM
    keepM = snds ... divideM
    
    splitM n es@(AnyBorder l u rep)
        | n <= 0 = do e' <- newNull; return (e', es)
        | n >= c = do e' <- newNull; return (es, e')
        |  True  = bimap (AnyBorder l i) (AnyBorder j u) <$> splitM n rep
      where
        i = index (l, u) (n - 1)
        j = index (l, u) n
        c = size  (l, u)
    
    divideM n es@(AnyBorder l u rep)
        | n <= 0 = do e' <- newNull; return (es, e')
        | n >= c = do e' <- newNull; return (e', es)
        |  True  = bimap (AnyBorder l i) (AnyBorder j u) <$> divideM n rep
      where
        i = index (l, u) (c - n - 1)
        j = index (l, u) (c - n)
        c = size  (l, u)
    
    newLinear = withBounds' <=< newLinear
    
    writeM = writeM . unpack
    (!*)   = (!*)   . unpack
    
    unsafeWriteM    = unsafeWriteM . unpack
    unsafeReadByOff = unsafeReadByOff . unpack
    
    unsafeCopyTo src os trg ot = unsafeCopyTo (unpack src) os (unpack trg) ot

--------------------------------------------------------------------------------

{- Set and SetWith instances. -}

instance (Index i, SetWith (AnyBorder rep i e) e, Ord e, Sequence (AnyBorder rep i e) e)
      => Set (AnyBorder rep i e) e

instance (Index i, SetWith1 rep e) => SetWith (AnyBorder rep i e) e
  where
    setWith f = withBounds . setWith f . unpack
    
    isSubsetWith f = isSubsetWith f `on` unpack
    
    groupSetWith cmp f = withBounds . groupSetWith cmp f . unpack
    
    subsets = map withBounds . subsets . unpack
    
    insertWith f e = withBounds . insertWith f e . unpack
    deleteWith f e = withBounds . deleteWith f e . unpack
    
    intersectionWith f = withBounds ... on (intersectionWith f) unpack
    differenceWith   f = withBounds ... on (differenceWith   f) unpack
    symdiffWith      f = withBounds ... on (symdiffWith      f) unpack
    unionWith        f = withBounds ... on (unionWith        f) unpack
    
    memberWith   f e = memberWith   f e . unpack
    lookupLTWith f o = lookupLTWith f o . unpack
    lookupGTWith f o = lookupGTWith f o . unpack
    lookupLEWith f o = lookupLEWith f o . unpack
    lookupGEWith f o = lookupGEWith f o . unpack

--------------------------------------------------------------------------------

{- Scan and Sort instances. -}

instance (Linear2 (AnyBorder rep) i e) => Scan (AnyBorder rep i e) e

instance (Index i, Sort1 rep e) => Sort (AnyBorder rep i e) e
  where
    sortBy   f (AnyBorder l u es) = AnyBorder l u (sortBy f es)
    sortedBy f (AnyBorder _ _ es) = sortedBy f es

--------------------------------------------------------------------------------

{- Map, Indexed and Shaped instances. -}

instance (Index i, Indexed1 rep Int e) => Map (AnyBorder rep i e) i e
  where
    toMap ascs =
      let bnds = rangeBounds (fsts ascs)
      in  isNull ascs ? Z $ assoc bnds ascs
    
    {-# INLINE unsafeReadByKey #-}
    unsafeReadByKey (AnyBorder l u rep) = unsafeReadByKey rep . offset (l, u)
    
    Z // ascs = toMap ascs
    (AnyBorder l u rep) // ascs =
      let ies = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
      in  AnyBorder l u (rep // ies)
    
    p .$ (AnyBorder l u rep) = index (l, u) <$> p .$ rep
    p *$ (AnyBorder l u rep) = index (l, u) <$> p *$ rep
    
    kfoldr f base (AnyBorder l u es) = kfoldr (f . index (l, u)) base es
    kfoldl f base (AnyBorder l u es) = kfoldl (f . index (l, u)) base es

instance (Index i, Indexed1 rep Int e) => Indexed (AnyBorder rep i e) i e
  where
    assoc bnds@(l, u) ascs = AnyBorder l u (assoc bnds' ies)
      where
        ies   = [ (offset bnds i, e) | (i, e) <- ascs, inRange bnds i ]
        bnds' = defaultBounds (size bnds)
    
    assoc' def (l, u) ascs = AnyBorder l u (assoc' def bnds ies)
      where
        ies  = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        bnds = defaultBounds $ size (l, u)
    
    fromIndexed = withBounds . fromIndexed

--------------------------------------------------------------------------------

{- MapM, IndexedM instances. -}

instance (Index i, MapM1 m rep Int e, LinearM1 m rep e, BorderedM1 m rep Int e)
      => MapM m (AnyBorder rep i e) i e
  where
    newMap ascs = uncurry AnyBorder bnds <$> newMap ies
      where
        ies  = first (offset bnds) <$> ascs
        bnds = rangeBounds (fsts ascs)
    
    newMap' def ascs = uncurry AnyBorder bnds <$> newMap' def ies
      where
        ies  = first (offset bnds) <$> ascs
        bnds = rangeBounds (fsts ascs)
    
    {-# INLINE writeM' #-}
    writeM' (AnyBorder l u es) = writeM' es . offset (l, u)
    
    {-# INLINE unsafeReadMByKey #-}
    unsafeReadMByKey (AnyBorder l u es) = unsafeReadMByKey es . offset (l, u)
    
    overwrite (AnyBorder l u es) ascs =
      let ies = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
      in  overwrite es ies
    
    kfoldrM f base (AnyBorder l u es) = ofoldrM (f . index (l, u)) base es
    kfoldlM f base (AnyBorder l u es) = ofoldlM (f . index (l, u)) base es

instance (Index i, IndexedM1 m rep Int e) => IndexedM m (AnyBorder rep i e) i e
  where
    fromAssocs (l, u) ascs = AnyBorder l u <$> fromAssocs bnds ies
      where
        ies  = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        bnds = (0, size  (l, u) - 1)
    
    fromAssocs' (l, u) def ascs = AnyBorder l u <$> fromAssocs' bnds def ies
      where
        ies  = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        bnds = (0, size (l, u) - 1)
    
    fromIndexed' = withBounds' <=< fromIndexed'
    fromIndexedM = withBounds' <=< fromIndexedM

--------------------------------------------------------------------------------

{- SortM instance. -}

instance (Index i, SortM1 m rep e) => SortM m (AnyBorder rep i e) e
  where
    sortedMBy f = sortedMBy f . unpack
    sortMBy   f = sortMBy   f . unpack

--------------------------------------------------------------------------------

{- Freeze and Thaw instances. -}

-- Bordered (with any index) to prim.
instance {-# OVERLAPPABLE #-} (Index i, Thaw m (rep e) mut)
      => Thaw m (AnyBorder rep i e) mut
  where
    unsafeThaw = unsafeThaw . unpack
    thaw       = thaw . unpack

-- Prim to bordered (with any index).
instance {-# OVERLAPPABLE #-} (Index i, Thaw m imm (rep e), Estimate1 rep e)
      => Thaw m imm (AnyBorder rep i e)
  where
    unsafeThaw = fmap withBounds . unsafeThaw
    thaw       = fmap withBounds . thaw

-- Lift prim to prim on bordered on bordered (with same index).
instance {-# OVERLAPS #-} (Index i, Thaw1 m imm mut e)
      => Thaw m (AnyBorder imm i e) (AnyBorder mut i e)
  where
    unsafeThaw (AnyBorder l u imm) = AnyBorder l u <$> unsafeThaw imm
    thaw       (AnyBorder l u imm) = AnyBorder l u <$> thaw imm

-- Bordered (with any index) to prim.
instance {-# OVERLAPPABLE #-} (Index i, Freeze m (rep e) imm)
      => Freeze m (AnyBorder rep i e) imm
  where
    unsafeFreeze = unsafeFreeze . unpack
    freeze       = freeze . unpack

-- Prim to bordered (with any index).
instance {-# OVERLAPPABLE #-} (Index i, Freeze m mut (rep e), Estimate1 rep e)
      => Freeze m mut (AnyBorder rep i e)
  where
    unsafeFreeze = fmap withBounds . unsafeFreeze
    freeze       = fmap withBounds . freeze

-- Lift prim to prim on bordered to bordered (with same index).
instance {-# OVERLAPS #-} (Index i, Freeze1 m mut imm e)
      => Freeze m (AnyBorder mut i e) (AnyBorder imm i e)
  where
    unsafeFreeze (AnyBorder l u mut) = AnyBorder l u <$> unsafeFreeze mut
    freeze       (AnyBorder l u mut) = AnyBorder l u <$> freeze mut

--------------------------------------------------------------------------------

{-# INLINE unpack #-}
unpack :: AnyBorder rep i e -> rep e
unpack =  \ (AnyBorder _ _ es) -> es

{-# INLINE withBounds #-}
withBounds :: (Index i, Estimate1 rep e) => rep e -> AnyBorder rep i e
withBounds rep = uncurry AnyBorder (defaultBounds $ sizeOf rep) rep

{-# INLINE withBounds' #-}
withBounds' :: (Index i, EstimateM1 m rep e) => rep e -> m (AnyBorder rep i e)
withBounds' rep = (\ n -> uncurry AnyBorder (defaultBounds n) rep) <$> getSizeOf rep

