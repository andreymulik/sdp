{-# LANGUAGE Trustworthy, TypeFamilies, DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances, TypeOperators #-}

{- |
    Module      :  SDP.Templates.AnyBorder
    Copyright   :  (c) Andrey Mulik 2020-2023
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

import Data.Default.Class
import Data.Typeable
import Data.String
import Data.Data

import Text.Read.SDP
import Text.Show.SDP

import qualified GHC.Exts as E
import GHC.Generics

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

{- |
  'AnyBorder' is template, that appends arbitrary bounds to any structure.

  * 'Eq', 'Ord', 'Eq1' and 'Ord1' instances ingores bounds.
  * 'Thaw' and 'Freeze' instances for @'AnyBorder' rep e@ inherit @rep e@
  behavior.
-}
data AnyBorder rep i e = AnyBorder !i !i !(rep e)
  deriving ( Typeable, Data, Generic )

--------------------------------------------------------------------------------

{- Eq ad Eq1 instances. -}

instance (Index i, Eq (rep e)) => Eq (AnyBorder rep i e)
  where
    (==) = on (==) unpack

instance (Index i, Eq1 rep) => Eq1 (AnyBorder rep i)
  where
    liftEq f xs ys = liftEq f (unpack xs) (unpack ys)

--------------------------------------------------------------------------------

{- Ord and Ord1 instances. -}

instance (Index i, Ord (rep e)) => Ord (AnyBorder rep i e)
  where
    compare = on (<=>) unpack

instance (Index i, Ord1 rep) => Ord1 (AnyBorder rep i)
  where
    liftCompare f xs ys = liftCompare f (unpack xs) (unpack ys)

--------------------------------------------------------------------------------

{- Show and Read instances. -}

instance {-# OVERLAPPABLE #-} (Indexed1 rep Int e, Index i, Show i, Show e)
      => Show (AnyBorder rep i e)
  where
    showsPrec = assocsPrec "array "

instance (Indexed1 rep Int Char, Index i, Show i) => Show (AnyBorder rep i Char)
  where
    showsPrec = shows ... const listL

instance (Index i, Read i, Read e, Indexed1 rep Int e) => Read (AnyBorder rep i e)
  where
    readPrec = indexedPrec' "array"
    readList = readListDefault

--------------------------------------------------------------------------------

{- Overloaded Lists and String support. -}

instance (Index i, IsString (rep Char), Estimate1 rep Char)
      => IsString (AnyBorder rep i Char)
  where
    fromString = withBounds . fromString

instance (Index i, E.IsList (rep e), Estimate1 rep e)
      => E.IsList (AnyBorder rep i e)
  where
    type Item (AnyBorder rep i e) = E.Item (rep e)

    fromListN = withBounds ... E.fromListN
    fromList  = withBounds . E.fromList
    toList    = E.toList . unpack

--------------------------------------------------------------------------------

{- Semigroup, Monoid and Default instances. -}

instance (Index i, Nullable1 rep e) => Default (AnyBorder rep i e) where def = Z

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

instance (Num e, Num (rep e), Indexed1 rep Int e, Index i, Index ii, GIndex ii ~ I2 i)
      => Num (AnyBorder rep ii e)
  where
    fromInteger = single . fromInteger

    negate (AnyBorder l u rep) = AnyBorder l u (negate rep)
    signum (AnyBorder l u rep) = AnyBorder l u (signum rep)
    abs    (AnyBorder l u rep) = AnyBorder l u (abs    rep)

    AnyBorder l u xs + AnyBorder _ _ ys = AnyBorder l u (xs + ys)
    AnyBorder l u xs - AnyBorder _ _ ys = AnyBorder l u (xs - ys)

    xs' * ys' = mx /= ny ? underEx "(*)" $ viewOf (l, u) $ fromListN (nx * my) [
        sum
          [
            xs !^ xk * ys !^ yk | k <- [0 .. mx - 1],
            let xk = i * mx + k; yk = k * my + j

          ] | i <- [0 .. nx - 1], j <- [0 .. my - 1]
        ]

      where
        l = fromGIndex (E :& unsafeIndex  1 :& unsafeIndex  1)
        u = fromGIndex (E :& unsafeIndex nx :& unsafeIndex my)

        E :& nx :& mx = sizesOf xs'; xs = unpack xs'
        E :& ny :& my = sizesOf ys'; ys = unpack ys'

--------------------------------------------------------------------------------

{- Nullable and NullableM instances. -}

instance (Index i, Nullable1 rep e) => Nullable (AnyBorder rep i e)
  where
    isNull = \ (AnyBorder l u rep) -> isEmpty (l, u) || isNull rep
    lzero  = uncurry AnyBorder (defaultBounds 0) Z

instance (Index i, NullableM1 m rep e) => NullableM m (AnyBorder rep i e)
  where
    nowNull (AnyBorder l u es) = isEmpty (l, u) ? return True $ nowNull es
    newNull = uncurry AnyBorder (defaultBounds 0) <$> newNull

--------------------------------------------------------------------------------

{- Estimate and EstimateM instances. -}

instance Index i => Estimate (AnyBorder rep i e)
  where
    sizeOf (AnyBorder l u _) = size (l, u)

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

instance (Monad m, Index i) => EstimateM m (AnyBorder rep i e)
  where
    getSizeOf (AnyBorder l u _) = return $ size (l, u)

    estimateMLT = return ... (.<.)
    estimateMGT = return ... (.>.)
    estimateMLE = return ... (.<=.)
    estimateMGE = return ... (.>=.)
    estimateM   = return ... (<==>)

    lestimateMLT = return ... (.<)
    lestimateMGT = return ... (.>)
    lestimateMLE = return ... (.<=)
    lestimateMGE = return ... (.>=)
    lestimateM   = return ... (<.=>)

--------------------------------------------------------------------------------

{- Bordered and BorderedM instances. -}

instance Index i => Bordered (AnyBorder rep i e) i
  where
    lower    (AnyBorder l _ _) = l
    upper    (AnyBorder _ u _) = u
    bounds   (AnyBorder l u _) = (l, u)
    indices  (AnyBorder l u _) = range   (l, u)
    indexOf  (AnyBorder l u _) = index   (l, u)
    indexIn  (AnyBorder l u _) = inRange (l, u)
    offsetOf (AnyBorder l u _) = offset  (l, u)

    viewOf bnds es = size bnds >. es ? es $ uncurry AnyBorder bnds (unpack es)

instance (Index i, BorderedM1 m rep Int e) => BorderedM m (AnyBorder rep i e) i
  where
    nowIndexIn (AnyBorder l u _) = return . inRange (l, u)
    getIndices (AnyBorder l u _) = return $ range (l, u)
    getBounds  (AnyBorder l u _) = return (l, u)
    getLower   (AnyBorder l _ _) = return l
    getUpper   (AnyBorder _ u _) = return u

    getViewOf bnds es =
      let bnds' = defaultBounds (size bnds)
      in  uncurry AnyBorder bnds <$> getViewOf bnds' (unpack es)

--------------------------------------------------------------------------------

{- Functor, Zip and Applicative instances. -}

instance (Index i, Functor rep) => Functor (AnyBorder rep i)
  where
    fmap f (AnyBorder l u rep) = AnyBorder l u (f <$> rep)

instance (Index i, Zip rep) => Zip (AnyBorder rep i)
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

    zipWith f as bs =
      let (l, u) = defaultBounds $ minimum [sizeOf as, sizeOf bs]
      in  AnyBorder l u $ zipWith f (unpack as) (unpack bs)

    zipWith3 f as bs cs =
      let (l, u) = defaultBounds $ minimum [sizeOf as, sizeOf bs, sizeOf cs]
      in  AnyBorder l u $ zipWith3 f (unpack as) (unpack bs) (unpack cs)

    zipWith4 f as bs cs ds =
      let (l, u) = defaultBounds $ minimum [sizeOf as, sizeOf bs, sizeOf cs, sizeOf ds]
      in  AnyBorder l u $ zipWith4 f (unpack as) (unpack bs) (unpack cs) (unpack ds)

    zipWith5 f as bs cs ds es =
      let (l, u) = defaultBounds $ minimum [sizeOf as, sizeOf bs, sizeOf cs, sizeOf ds, sizeOf es]
      in  AnyBorder l u $ zipWith5 f (unpack as) (unpack bs) (unpack cs) (unpack ds) (unpack es)

    zipWith6 f as bs cs ds es fs =
      let (l, u) = defaultBounds $ minimum [sizeOf as, sizeOf bs, sizeOf cs, sizeOf ds, sizeOf es, sizeOf fs]
      in  AnyBorder l u $ zipWith6 f (unpack as) (unpack bs) (unpack cs) (unpack ds) (unpack es) (unpack fs)

instance (Index i, Applicative rep) => Applicative (AnyBorder rep i)
  where
    pure = uncurry AnyBorder (defaultBounds 1) . pure

    AnyBorder lf uf fs <*> AnyBorder le ue es =
      let bnds = defaultBounds (size (lf, uf) * size (le, ue))
      in  uncurry AnyBorder bnds (fs <*> es)

--------------------------------------------------------------------------------

{- Foldable and Traversable instances. -}

instance (Index i, Foldable rep) => Foldable (AnyBorder rep i)
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

instance (Index i, Traversable rep) => Traversable (AnyBorder rep i)
  where
    traverse f (AnyBorder l u es) = AnyBorder l u <$> traverse f es

--------------------------------------------------------------------------------

{- Forceable, Sequence and Linear instances. -}

instance Forceable1 rep e => Forceable (AnyBorder rep i e)
  where
    force (AnyBorder l u rep) = AnyBorder l u (force rep)

instance (Index i, Linear1 rep e) => Sequence (AnyBorder rep i e) e
  where
    single = withBounds . single

    toHead e es = withBounds (e :> unpack es)
    toLast es e = withBounds (unpack es :< e)

    head = head . unpack
    last = last . unpack
    tail = withBounds . tail . unpack
    init = withBounds . init . unpack

    fromList     = fromFoldable
    fromFoldable = withBounds . fromFoldable

    before es = withBounds ... before (unpack es)

    listL = listL . unpack
    listR = listR . unpack

    prefix p = prefix p . unpack
    suffix p = suffix p . unpack

    nubBy f = withBounds . nubBy f . unpack
    nub     = withBounds .   nub   . unpack

    ofoldr f base = ofoldr f base . unpack
    ofoldl f base = ofoldl f base . unpack

    sfoldr f base = sfoldr f base . unpack
    sfoldl f base = sfoldl f base . unpack

    isPrefixOf xs ys = xs .<=. ys && on isPrefixOf unpack xs ys
    isSuffixOf xs ys = xs .<=. ys && on isSuffixOf unpack xs ys

    {-# INLINE (!^) #-}
    (!^) = (!^) . unpack

    write   (AnyBorder l u es) n e = AnyBorder l u (write es n e)
    reverse (AnyBorder l u es)     = AnyBorder l u (reverse   es)

instance (Index i, Linear1 rep e) => Linear (AnyBorder rep i e) e
  where
    fromListN = withBounds ... fromListN
    replicate = withBounds ... replicate
    iterate n = withBounds ... iterate n

    concatMap = withBounds ... concatMap . (unpack .)
    concat    = withBounds  .  concatMap unpack

    filter  f = withBounds . filter f . unpack
    remove  n = withBounds . remove n . unpack

    take n = withBounds . take n . unpack
    drop n = withBounds . drop n . unpack
    keep n = withBounds . keep n . unpack
    sans n = withBounds . sans n . unpack

    padL n e = withBounds . padL n e . unpack
    padR n e = withBounds . padR n e . unpack

--------------------------------------------------------------------------------

{- ForceableM and LinearM instances. -}

instance ForceableM1 m rep e => ForceableM m (AnyBorder rep i e)
  where
    copied (AnyBorder l u es) = AnyBorder l u <$> copied es

instance (Index i, Concat m (rep e)) => Concat m (AnyBorder rep i e)
  where
    cat (AnyBorder _ _ xs) (AnyBorder _ _ ys) = uncurry
        AnyBorder (defaultBounds 0) <$> cat xs ys

instance (Index i, LinearM1 m rep e) => SequenceM m (AnyBorder rep i e) e
  where
    unconsM' es'@(AnyBorder l u es) = nowNull es' ?^ pure Nothing $ fmap (second new) <$> unconsM' es
      where
        bnds = defaultBounds $ size (l, u) - 1
        new  = uncurry AnyBorder bnds

    unsnocM' es'@(AnyBorder l u es) = nowNull es' ?^ pure Nothing $ fmap (first new) <$> unsnocM' es
      where
        bnds = defaultBounds $ size (l, u) - 1
        new  = uncurry AnyBorder bnds

    getHead = getHead . unpack
    getLast = getLast . unpack

    getLeft  = getLeft  . unpack
    getRight = getRight . unpack

    (+=) e es = withBounds' =<< (e += unpack es)
    (=+) es e = withBounds' =<< (unpack es =+ e)
    newLinear = withBounds' <=< newLinear

    reversed (AnyBorder l u es) = AnyBorder l u <$> reversed es

    {-# INLINE (!#>) #-}
    (!#>) = (!#>) . unpack

    {-# INLINE writeM #-}
    writeM = writeM . unpack

    ofoldrM f e = ofoldrM f e . unpack
    ofoldlM f e = ofoldlM f e . unpack

    foldrM f e = foldrM f e . unpack
    foldlM f e = foldlM f e . unpack

    prefixM p = prefixM p . unpack
    suffixM p = suffixM p . unpack
    mprefix p = mprefix p . unpack
    msuffix p = msuffix p . unpack

instance (Index i, NullableM m (rep e), LinearM1 m rep e)
      => LinearM m (AnyBorder rep i e) e
  where
    filled n = withBounds' <=< filled n

    copied' (AnyBorder l u es) = (AnyBorder l u <$>) ... copied' es

    copyTo src os trg ot = copyTo (unpack src) os (unpack trg) ot

    takeM n es@(AnyBorder l u rep)
        | n <= 0 = newNull
        | n >= c = return es
        |  True  = AnyBorder l (index (l, u) n) <$> takeM n rep
      where
        c = size (l, u)

    dropM n es@(AnyBorder l u rep)
        | n >= c = newNull
        | n <= 0 = return es
        |  True  = AnyBorder (index (l, u) n) u <$> dropM n rep
      where
        c = size (l, u)

    keepM n es@(AnyBorder l u rep)
        | n <= 0 = newNull
        | n >= c = return es
        |  True  = AnyBorder (index (l, u) (c - n)) u <$> keepM n rep
      where
        c = size (l, u)

    sansM n es@(AnyBorder l u rep)
        | n >= c = newNull
        | n <= 0 = return es
        |  True  = AnyBorder (index (l, u) (c - n)) u <$> sansM n rep
      where
        c = size (l, u)

    splitM n es@(AnyBorder l u rep)
        | n <= 0 = do e' <- newNull; return (e', es)
        | n >= c = do e' <- newNull; return (es, e')
        |  True  = bimap (AnyBorder l i) (AnyBorder i u) <$> splitM n rep
      where
        i = index (l, u) n
        c = size  (l, u)

    divideM n es@(AnyBorder l u rep)
        | n <= 0 = do e' <- newNull; return (es, e')
        | n >= c = do e' <- newNull; return (e', es)
        |  True  = bimap (AnyBorder l i) (AnyBorder i u) <$> divideM n rep
      where
        i = index (l, u) (c - n)
        c = size  (l, u)

    miterate n = fmap (uncurry AnyBorder (defaultBounds n)) ... miterate n
    iterateM n = fmap (uncurry AnyBorder (defaultBounds n)) ... iterateM n

--------------------------------------------------------------------------------

{- Set and SetWith instances. -}

instance (SetWith1 (AnyBorder rep i) e, Nullable (AnyBorder rep i e), Ord e)
      => Set (AnyBorder rep i e) e

instance (Index i, SetWith1 rep e, Linear1 rep e)
      => SetWith (AnyBorder rep i e) e
  where
    isSubsetWith f = isSubsetWith f `on` unpack

    setWith f = withBounds . setWith f . unpack

    subsetsWith f = map withBounds . subsetsWith f . unpack
    subsets       = map withBounds . subsets . unpack

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
    sortBy cmp = \ (AnyBorder l u rep) -> AnyBorder l u (sortBy cmp rep)
    sortedBy f = sortedBy f . unpack

--------------------------------------------------------------------------------

{- Map, Indexed and Shaped instances. -}

instance (Index i, Indexed1 rep Int e) => Map (AnyBorder rep i e) i e
  where
    toMap ascs = isNull ascs ? Z $ assoc (rangeBounds (fsts ascs)) ascs

    toMap' e ascs = isNull ascs ? Z $ assoc' (rangeBounds (fsts ascs)) e ascs

    {-# INLINE (.!) #-}
    AnyBorder l u rep .! i = rep !^ offset (l, u) i

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

    assoc' bnds@(l, u) defvalue ascs = AnyBorder l u (assoc' bnds' defvalue ies)
      where
        ies   = [ (offset bnds i, e) | (i, e) <- ascs, inRange bnds i ]
        bnds' = defaultBounds $ size bnds

    fromIndexed = withBounds . fromIndexed
    
    AnyBorder ls us rep !! ij = case inBounds (lj, uj) ij of
        IN -> AnyBorder l u (subline o n rep)
        ER -> emptyEx "(!!)"
        UR -> underEx "(!!)"
        OR -> overEx  "(!!)"
      where
        (lj, l) = splitDim ls
        (uj, u) = splitDim us
        
        o = offset (ls, us) (joinDim l ij)
        n = size (l, u)
    
    slices (AnyBorder ls us rep) = assoc (lj, uj) $
        range (lj, uj) `zip` (f <$> chunks n rep)
      where
        (lj, l) = splitDim ls
        (uj, u) = splitDim us
        
        f = AnyBorder l u
        n = size (l, u)
    
    unslice  Z  = Z -- border normalization
    unslice ess =
        let appendAscs ij = flip $ kfoldr (\ j e ies -> (joinDim j ij, e) : ies)
            -- ^ append a subarray to an association list
        in  assoc (joinDim l lj, joinDim u uj) $ kfoldr appendAscs [] ess
      where
        -- | If the subarrays are of different sizes, take the widest bounds.
        (l, u) = sfoldr (joinBounds . bounds) (defaultBounds 0) ess
        
        joinBounds = uncurry $ on (.) extendBounds
        (lj, uj)   = bounds ess

--------------------------------------------------------------------------------

{- MapM, IndexedM instances. -}

instance
    (
      Index i, MapM1 m rep Int e, LinearM1 m rep e, BorderedM1 m rep Int e,
      NullableM m (rep e)
    ) => MapM m (AnyBorder rep i e) i e
  where
    newMap ascs =
      let bnds@(l, u) = rangeBounds (fsts ascs)
      in  AnyBorder l u <$> newMap [ (offset bnds i, e) | (i, e) <- ascs ]

    newMap' defvalue ascs =
      let bnds@(l, u) = rangeBounds (fsts ascs)
      in  AnyBorder l u <$> newMap' defvalue [ (offset bnds i, e) | (i, e) <- ascs ]

    {-# INLINE writeM' #-}
    writeM' (AnyBorder l u es) = writeM' es . offset (l, u)

    {-# INLINE (>!) #-}
    (>!) (AnyBorder l u es) = (es !#>) . offset (l, u)

    overwrite (AnyBorder l u es) ascs = overwrite es
      [
        (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i
      ]

    kfoldrM f base (AnyBorder l u es) = ofoldrM (f . index (l, u)) base es
    kfoldlM f base (AnyBorder l u es) = ofoldlM (f . index (l, u)) base es

instance (Index i, NullableM m (rep e), IndexedM1 m rep Int e)
      => IndexedM m (AnyBorder rep i e) i e
  where
    fromAssocs (l, u) ascs = AnyBorder l u <$> fromAssocs bnds ies
      where
        ies  = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        bnds = (0, size  (l, u) - 1)

    fromAssocs' (l, u) defvalue ascs = AnyBorder l u <$> fromAssocs' bnds defvalue ies
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

--------------------------------------------------------------------------------

{-# NOINLINE emptyEx #-}
emptyEx :: String -> a
emptyEx =  throw . EmptyRange . showString "in SDP.Templates.AnyBorder."

{-# NOINLINE overEx #-}
overEx :: String -> a
overEx =  throw . EmptyRange . showString "in SDP.Templates.AnyBorder."

{-# NOINLINE underEx #-}
underEx :: String -> a
underEx =  throw . EmptyRange . showString "in SDP.Templates.AnyBorder."
