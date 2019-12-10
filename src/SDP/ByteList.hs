{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, TypeFamilies, RoleAnnotations, DeriveGeneric #-}

{- |
    Module      :  SDP.ByteList
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.ByteList@ provides 'ByteList' - strict unboxed unrolled linked list.
-}
module SDP.ByteList
(
  -- * Exports
  module SDP.Indexed,
  module SDP.Sort,
  module SDP.Set,
  
  -- * ByteList
  ByteList (..),
  
  -- * Ublist
  Ublist
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.IndexedM
import SDP.Indexed
import SDP.Unboxed
import SDP.Sort
import SDP.Set

import Test.QuickCheck

import GHC.Generics ( Generic (..) )

import GHC.Base ( Int (..) )
import GHC.ST   ( ST  (..) )

import qualified GHC.Exts as E
import Data.String ( IsString (..) )

import SDP.ByteList.Ublist
import SDP.ByteList.ST

import SDP.Internal.Read hiding ( pfail )
import SDP.Internal.Show
import SDP.Simple

default ()

--------------------------------------------------------------------------------

-- | ByteList is bordered strict unboxed unrolled linked list.
data ByteList i e = ByteList !i !i !(Ublist e) deriving ( Generic )

type role ByteList nominal representational

--------------------------------------------------------------------------------

{- Eq and Ord instances. -}

instance (Eq e, Unboxed e, Index i) => Eq (ByteList i e)
  where
    (ByteList l1 u1 xs) == (ByteList l2 u2 ys) =
      size (l1, u1) == size (l2, u2) && xs == ys

instance (Ord e, Unboxed e, Index i) => Ord (ByteList i e)
  where
    compare (ByteList l1 u1 xs) (ByteList l2 u2 ys) =
      (xs <=> ys) <> (size (l1, u1) <=> size (l2, u2))

--------------------------------------------------------------------------------

{- Show and Read instances. -}

instance (Index i, Show i, Unboxed e, Show e) => Show (ByteList i e)
  where
    showsPrec = assocsPrec "bytelist "

instance (Index i, Read i, Unboxed e, Read e) => Read (ByteList i e)
  where
    readList = readListDefault
    readPrec = readSDPPrec "bytelist"

--------------------------------------------------------------------------------

{- Semigroup, Monoid, Default, Arbitrary and Estimate instances. -}

instance (Index i, Unboxed e) => Semigroup (ByteList i e) where (<>) = (++)
instance (Index i, Unboxed e) => Monoid    (ByteList i e) where mempty = def

instance (Index i) => Default (ByteList i e)
  where
    def = let (l, u) = defaultBounds 0 in ByteList l u def

instance (Index i, Unboxed e, Arbitrary e) => Arbitrary (ByteList i e)
  where
    arbitrary = fromList <$> arbitrary

instance (Index i) => Estimate (ByteList i e)
  where
    (ByteList l1 u1 _) <==> (ByteList l2 u2 _) = size (l1, u1) <=> size (l2, u2)
    (ByteList l1 u1 _) .>.  (ByteList l2 u2 _) = size (l1, u1)  >  size (l2, u2)
    (ByteList l1 u1 _) .<.  (ByteList l2 u2 _) = size (l1, u1)  <  size (l2, u2)
    (ByteList l1 u1 _) .<=. (ByteList l2 u2 _) = size (l1, u1) <=  size (l2, u2)
    (ByteList l1 u1 _) .>=. (ByteList l2 u2 _) = size (l1, u1) >=  size (l2, u2)
    
    (ByteList l1 u1 _) <.=> n2 = size (l1, u1) <=> n2
    (ByteList l1 u1 _)  .>  n2 = size (l1, u1)  >  n2
    (ByteList l1 u1 _)  .<  n2 = size (l1, u1)  <  n2
    (ByteList l1 u1 _) .>=  n2 = size (l1, u1) >=  n2
    (ByteList l1 u1 _) .<=  n2 = size (l1, u1) <=  n2

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Index i, Unboxed e) => Linear (ByteList i e) e
  where
    isNull (ByteList l u bytes) = isEmpty (l, u) || isNull bytes
    
    lzero = def
    
    toHead e (ByteList l u es) = ByteList l' u' (e :> es)
      where
        (l', u') = defaultBounds $ size (l, u) + 1
    
    uncons Z = pfail "(:>)"
    uncons (ByteList l u es) = (x, sizeOf es < 2 ? Z $ ByteList l' u xs)
      where
        (x, xs) = uncons es
        l' = next (l, u) l
    
    head Z = pfail "(:>)"
    head (ByteList _ _ es) = head es
    
    tail Z = pfail "(:>)"
    tail (ByteList l u es) = ByteList l' u $ tail es where l' = next (l, u) l
    
    toLast (ByteList l u es) e = ByteList l' u' (es :< e)
      where
        (l', u') = defaultBounds $ size (l, u) + 1
    
    unsnoc Z = pfail "(:<)"
    unsnoc (ByteList l u es) = (sizeOf es < 2 ? Z $ ByteList l u' xs, x)
      where
        (xs, x) = unsnoc es
        u' = prev (l, u) u
    
    last Z = pfail "(:<)"
    last (ByteList _ _ es) = last es
    
    init Z = pfail "(:<)"
    init (ByteList l u es) = ByteList l u' $ init es where u' = prev (l, u) u
    
    fromList es = let (l, u) = defaultBounds $ sizeOf es in ByteList l u $ fromList es
    
    replicate n e = let (l, u) = defaultBounds $ max 0 n in ByteList l u $ replicate n e
    
    concat = \ xss -> case defaultBounds $ foldr' g 0 xss of
        (l', u') -> ByteList l' u' (foldr f Z xss)
      where
        f = \ (ByteList _ _ xs) ublist -> xs ++ ublist
        g = \ (ByteList l u  _) count  -> size (l, u) + count
    
    intersperse e (ByteList _ _ es) = ByteList l u $ intersperse e es
      where
        (l, u) = defaultBounds $ case n <=> 0 of {GT -> 2 * n - 1; _ -> 0}
        n = sizeOf es
    
    Z  ++ ys = ys
    xs ++  Z = xs
    (ByteList l1 u1 xs) ++ (ByteList l2 u2 ys) = ByteList l u $ xs ++ ys
      where
        (l, u) = defaultBounds $ size (l1, u1) + size (l2, u2)
    
    listL (ByteList _ _ bytes) = listL bytes
    listR (ByteList _ _ bytes) = listR bytes
    
    partitions ps es = fromList <$> partitions ps (listL es)

instance (Index i, Unboxed e) => Split (ByteList i e) e
  where
    take n xs@(ByteList l _ es)
      |   n < 1  = Z
      | n >=. xs = xs
      |   True   = let u' = indexOf xs (n - 1) in ByteList l u' (take n es)
    
    drop n xs@(ByteList _ u es)
      |   n < 1  = xs
      | n >=. xs = Z
      |   True   = let l' = indexOf xs n in ByteList l' u (drop n es)
    
    isPrefixOf xs ys = listL xs `isPrefixOf` listL ys
    isInfixOf  xs ys = listL xs `isInfixOf`  listL ys
    isSuffixOf xs ys = listL xs `isSuffixOf` listL ys
    
    prefix p (ByteList _ _ es) = prefix p es
    suffix p (ByteList _ _ es) = suffix p es

instance (Index i, Unboxed e) => Bordered (ByteList i e) i e
  where
    indexIn  (ByteList l u _) = inRange (l, u)
    offsetOf (ByteList l u _) = offset (l, u)
    indexOf  (ByteList l u _) = index (l, u)
    sizeOf   (ByteList l u _) = size (l, u)
    bounds   (ByteList l u _) = (l, u)
    lower    (ByteList l _ _) = l
    upper    (ByteList _ u _) = u

--------------------------------------------------------------------------------

{- Indexed, IFold, Set and Sort instances. -}

instance (Index i, Unboxed e) => Indexed (ByteList i e) i e
  where
    assoc (l, u) ascs = ByteList l u $ assoc bnds ies
      where
        ies  = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        bnds = (0, size (l, u) - 1)
    
    assoc' (l, u) defvalue ascs = ByteList l u $ assoc' bnds defvalue ies
      where
        ies  = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        bnds = (0, size (l, u) - 1)
    
    Z  // ascs = isNull ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    (ByteList l u es) // ascs = ByteList l' u' es'
      where
        es' = es // [ (offset (l, u) i, e) | (i, e) <- ascs ]
        (l', u') = defaultBounds $ sizeOf es'
    
    fromIndexed es = let (l, u) = defaultBounds $ sizeOf es in ByteList l u $ fromIndexed es
    
    {-# INLINE (!^) #-}
    (ByteList _ _ es) !^ i = es !^ i
    
    {-# INLINE (.!) #-}
    (.!) (ByteList l u es) i = es .! offset (l, u) i
    
    p .$ (ByteList l u es) = index (l, u) <$> p .$ es
    p *$ (ByteList l u es) = index (l, u) <$> p *$ es

instance (Index i, Unboxed e) => IFold (ByteList i e) i e
  where
    ifoldr  f base (ByteList l u es) = ifoldr (f . index (l, u)) base es
    ifoldl  f base (ByteList l u es) = ifoldl (f . index (l, u)) base es
    
    i_foldr f base (ByteList _ _ es) = i_foldr f base es
    i_foldl f base (ByteList _ _ es) = i_foldl f base es

instance (Index i, Unboxed e) => Set (ByteList i e) e
  where
    setWith f (ByteList _ _ es) = ByteList l u es'
      where
        es'    = setWith f es
        (l, u) = defaultBounds $ sizeOf es'
    
    intersectionWith f (ByteList _ _ xs) (ByteList _ _ ys) = ByteList l u es
      where
        es     = intersectionWith f xs ys
        (l, u) = defaultBounds $ sizeOf es
    
    unionWith f (ByteList _ _ xs) (ByteList _ _ ys) = ByteList l u es
      where
        es     = unionWith f xs ys
        (l, u) = defaultBounds $ sizeOf es
    
    differenceWith f (ByteList _ _ xs) (ByteList _ _ ys) = ByteList l u es
      where
        es     = differenceWith f xs ys
        (l, u) = defaultBounds $ sizeOf es
    
    symdiffWith f (ByteList _ _ xs) (ByteList _ _ ys) = ByteList l u es
      where
        es     = symdiffWith f xs ys
        (l, u) = defaultBounds $ sizeOf es
    
    insertWith f e (ByteList _ _ es) = ByteList l u es'
      where
        es'    = insertWith f e es
        (l, u) = defaultBounds $ sizeOf es'
    
    deleteWith f e (ByteList _ _ es) = ByteList l u es'
      where
        es'    = deleteWith f e es
        (l, u) = defaultBounds $ sizeOf es'
    
    isSubsetWith f (ByteList _ _ xs) (ByteList _ _ ys) = isSubsetWith f xs ys
    
    isContainedIn f e (ByteList _ _ es) = isContainedIn f e es
    lookupLTWith  f e (ByteList _ _ es) = lookupLTWith  f e es
    lookupGTWith  f e (ByteList _ _ es) = lookupGTWith  f e es
    lookupLEWith  f e (ByteList _ _ es) = lookupLEWith  f e es
    lookupGEWith  f e (ByteList _ _ es) = lookupGEWith  f e es

instance (Index i, Unboxed e) => Sort (ByteList i e) e
  where
    sortBy cmp (ByteList l u es) = ByteList l u $ sortBy cmp es

--------------------------------------------------------------------------------

instance (Index i, Unboxed e) => E.IsList (ByteList i e)
  where
    type Item (ByteList i e) = e
    
    fromListN = fromListN
    fromList  = fromList
    toList    = listL

instance (Index i) => IsString (ByteList i Char) where fromString = fromList

--------------------------------------------------------------------------------

instance (Index i, Unboxed e) => Thaw (ST s) (ByteList i e) (STByteList s i e)
  where
    thaw       (ByteList l u es) = STByteList l u <$> thaw es
    unsafeThaw (ByteList l u es) = STByteList l u <$> unsafeThaw es

instance (Index i, Unboxed e) => Freeze (ST s) (STByteList s i e) (ByteList i e)
  where
    freeze       (STByteList l u es) = ByteList l u <$> freeze es
    unsafeFreeze (STByteList l u es) = ByteList l u <$> unsafeFreeze es

--------------------------------------------------------------------------------

pfail :: String -> a
pfail msg = throw . PatternMatchFail $ "in SDP.ByteList." ++ msg




