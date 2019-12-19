{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, RoleAnnotations, DeriveGeneric #-}

{- |
    Module      :  SDP.ByteList.Ublist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.ByteList.Ublist@ provides 'Ublist' - strict boxed unrolled linked list.
-}
module SDP.ByteList.Ublist
(
  -- * Exports
  module SDP.Indexed,
  module SDP.Unboxed,
  module SDP.Sort,
  module SDP.Scan,
  module SDP.Set,
  
  -- * Ublist
  Ublist (..)
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.Indexed
import SDP.Unboxed
import SDP.Sort
import SDP.Scan
import SDP.Set

import Test.QuickCheck

import GHC.Generics ( Generic (..) )

import GHC.Base ( Int (..) )
import GHC.ST   ( ST  (..), runST )

import Data.String ( IsString (..) )

import SDP.ByteList.STUblist
import SDP.SortM.Tim

import SDP.Internal.SBytes
import SDP.Internal.Show
import SDP.Simple

default ()

--------------------------------------------------------------------------------

-- | Ublist is unrolled linked list of unboxed values.
data Ublist e = UBEmpty | Ublist !(SBytes# e) (Ublist e) deriving ( Generic )

type role Ublist representational

{-# COMPLETE Z, Ublist #-}

--------------------------------------------------------------------------------

{- Eq and Eq1 instances. -}

instance (Unboxed e) => Eq (Ublist e)
  where
    Z == Z = True
    xs@(Ublist bytes1# bytes1) == ys@(Ublist bytes2# bytes2) = if n1 > n2
        then take n2 bytes1# == bytes2# && drop n2 xs == bytes2
        else take n1 bytes2# == bytes1# && drop n1 ys == bytes1
      where
        n1 = sizeOf bytes1#
        n2 = sizeOf bytes2#
    _ == _ = False

--------------------------------------------------------------------------------

{- Ord and Ord1 instances. -}

instance (Ord e, Unboxed e) => Ord (Ublist e)
  where
    compare Z Z = EQ
    compare Z _ = LT
    compare _ Z = GT
    compare xs@(Ublist bytes1# bytes1) ys@(Ublist bytes2# bytes2) = if n1 > n2
        then (take n2 bytes1# <=> bytes2#) <> compare (drop n2 xs) bytes2
        else (bytes1# <=> take n2 bytes2#) <> compare bytes1 (drop n2 ys)
      where
        n1 = sizeOf bytes1#
        n2 = sizeOf bytes2#

--------------------------------------------------------------------------------

{- Show instance. -}

instance (Show e, Unboxed e) => Show (Ublist e)
  where
    showsPrec = assocsPrec "ublist "

--------------------------------------------------------------------------------

{- Semigroup, Monoid, Default, Arbitrary and Estimate instances. -}

instance (Unboxed e) => Semigroup (Ublist e) where (<>) = (++)
instance (Unboxed e) => Monoid    (Ublist e) where mempty = def

instance Default (Ublist e) where def = UBEmpty

instance (Arbitrary e, Unboxed e) => Arbitrary (Ublist e)
  where
    arbitrary = fromList <$> arbitrary

instance (Unboxed e) => Estimate (Ublist e)
  where
    (<==>) = go 0
      where
        go o Z   Z = o <=> 0
        go o xs  Z = xs <.=> (-o)
        go o Z  ys = o <=.> ys
        go o (Ublist bytes1# bytes1) (Ublist bytes2# bytes2) =
          let n1 = sizeOf bytes1#; n2 = sizeOf bytes2#
          in  go (o + n1 - n2) bytes1 bytes2
    
    Z <.=> n = 0 <=> n
    (Ublist bytes# bytes) <.=> m = bytes# .> m ? GT $ bytes <.=> (m - sizeOf bytes#)

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Unboxed e) => Linear (Ublist e) e
  where
    isNull es = case es of {UBEmpty -> True; Ublist Z UBEmpty -> True; _ -> False}
    
    lzero = UBEmpty
    
    toHead e Z = single e
    toHead e es@(Ublist bytes# bytes) = bytes# .< lim ? Ublist (e :> bytes#) bytes $ Ublist (single e) es
    
    head Z  = pfailEx "(:>)"
    head es = es !^ 0
    
    tail Z = pfailEx "(:<)"
    tail (Ublist bytes# bytes) = isNull bytes# ? tail bytes $ Ublist (tail bytes#) bytes
    
    toLast Z e = single e
    toLast (Ublist bytes# bytes) e = isNull bytes# ? (bytes :< e) $ Ublist bytes# (bytes :< e)
    
    last Z = pfailEx "(:<)"
    last (Ublist bytes# bytes) = isNull bytes ? last bytes# $ last bytes
    
    init Z = pfailEx "(:>)"
    init (Ublist bytes# bytes) = isNull bytes ? Ublist (init bytes#) Z $ Ublist bytes# (init bytes)
    
    single = replicate 1
    
    listL = i_foldr (:) []
    
    fromList = i_foldr (\ list -> Ublist $ fromList list) Z . chunks lim
    
    Z ++ ys = ys
    xs ++ Z = xs
    (Ublist bytes# bytes) ++ ys = Ublist bytes# (bytes ++ ys)
    
    -- | Deduplicated Unlist: O(1), O(1) memory (limited by a constant on top).
    replicate n e = copy count
      where
        (count, rst) = n `divMod` lim
        copy c = case c <=> 0 of
          LT -> Z
          EQ -> Ublist rest Z
          GT -> Ublist chunk (copy $ c - 1)
        
        chunk = replicate lim e
        rest  = replicate rst e
    
    reverse = fromList . i_foldl (flip (:)) []
    
    partition p es = (fromList x, fromList y)
      where
        (x, y) = partition p $ listL es
    
    partitions ps = fmap fromList . partitions ps . listL

instance (Unboxed e) => Split (Ublist e) e
  where
    take n es | n < 1 = Z | es .<= n = es | True = take' n es
      where
        take' _  Z = Z
        take' n' (Ublist bytes# bytes) = case n <=.> bytes# of
          LT -> Ublist (take n' bytes#) Z
          EQ -> bytes
          GT -> Ublist bytes# (take (n' - sizeOf bytes#) bytes)
    
    drop n es | n < 1 = es | es .<= n = Z | True = drop' n es
      where
        drop' _ Z = Z
        drop' n' (Ublist bytes# bytes) = case n' <=.> bytes# of
          LT -> Ublist (drop n' bytes#) bytes
          EQ -> bytes
          GT -> drop' (n' - sizeOf bytes#) bytes
    
    isPrefixOf xs ys = listL xs `isPrefixOf` listL ys
    isInfixOf  xs ys = listL xs `isInfixOf`  listL ys
    isSuffixOf xs ys = listL xs `isSuffixOf` listL ys
    
    prefix p = i_foldr (\ e c -> p e ? c + 1 $ 0) 0
    suffix p = i_foldl (\ c e -> p e ? c + 1 $ 0) 0

instance (Unboxed e) => Bordered (Ublist e) Int e
  where
    sizeOf es = case es of {Ublist bytes# bytes -> sizeOf bytes# + sizeOf bytes; _ -> 0}
    
    indexIn es = \ i -> i >= 0 && i <. es
    
    -- | Quick unchecked offset.
    offsetOf = const id
    
    -- | Quick unchecked index.
    indexOf = const id
    
    lower  _  = 0
    upper  es = sizeOf es - 1
    bounds es = (0, sizeOf es - 1)

--------------------------------------------------------------------------------

{- Set, Scan and Sort instances. -}

instance (Unboxed e) => Set (Ublist e) e
  where
    setWith f = nubSorted f . sortBy f
    
    insertWith _ e Z = single e
    insertWith f e (Ublist bytes# bytes) = isContainedIn f e bytes# ?
      Ublist (insertWith f e bytes#) bytes $ Ublist bytes# (insertWith f e bytes)
    
    deleteWith _ _ Z = Z
    deleteWith f e (Ublist bytes# bytes) = isContainedIn f e bytes# ?
      Ublist (deleteWith f e bytes#) bytes $ Ublist bytes# (deleteWith f e bytes)
    
    intersectionWith f xs ys = fromList $ on (intersectionWith f) listL xs ys
    
    unionWith f xs ys = fromList $ on (unionWith f) listL xs ys
    
    differenceWith f xs ys = fromList $ on (differenceWith f) listL xs ys
    
    symdiffWith f xs ys = fromList $ on (symdiffWith f) listL xs ys
    
    lookupLTWith _ _ Z  = Nothing
    lookupLTWith f o es
        | GT <- o `f` last' = Just last'
        | GT <- o `f` head' = look' head' 0 (upper es)
        |       True        = Nothing
      where
        head' = es !^ 0
        last' = es !^ upper es
        
        look' r l u = if l > u then Just r else case o `f` e of
            LT -> look' r l (j - 1)
            EQ -> Just (j < 1 ? r $ es !^ (j - 1))
            GT -> look' e (j + 1) u
          where
            j = center l u; e = es !^ j
    
    lookupLEWith _ _ Z  = Nothing
    lookupLEWith f o es
        | GT <- o `f` last' = Just last'
        | LT <- o `f` head' = Nothing
        |       True        = look' head' 0 (upper es)
      where
        head' = es !^ 0
        last' = es !^ upper es
        
        look' r l u = if l > u then Just r else case o `f` e of
            LT -> look' r l (j - 1)
            _  -> look' e (j + 1) u
          where
            j = center l u; e = es !^ j
    
    lookupGTWith _ _ Z  = Nothing
    lookupGTWith f o es
        | LT <- o `f` head' = Just head'
        | LT <- o `f` last' = look' last' 0 (upper es)
        |       True        = Nothing
      where
        head' = es !^ 0
        last' = es !^ upper es
        
        look' r l u = if l > u then Just r else case o `f` e of
            LT -> look' e l (j - 1)
            EQ -> (j + 1) >=. es ? Nothing $ Just (es !^ (j + 1))
            GT -> look' r (j + 1) u
          where
            j = center l u; e = es !^ j
    
    lookupGEWith _ _ Z  = Nothing
    lookupGEWith f o es
        | GT <- o `f` last' = Nothing
        | GT <- o `f` head' = look' last' 0 (upper es)
        |       True        = Just head'
      where
        head' = es !^ 0
        last' = es !^ upper es
        
        look' r l u = if l > u then Just r else case o `f` e of
            LT -> look' e l (j - 1)
            EQ -> Just e
            GT -> look' r (j + 1) u
          where
            j = center l u; e = es !^ j
    
    isContainedIn = binaryContain
    
    isSubsetWith f xs ys = i_foldr (\ e b -> b && isContainedIn f e ys) True xs

instance (Unboxed e) => Scan (Ublist e) e

instance (Unboxed e) => Sort (Ublist e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'

--------------------------------------------------------------------------------

{- Indexed and IFold instances. -}

instance (Unboxed e) => Indexed (Ublist e) Int e
  where
    assoc bnds ascs = runST $ fromAssocs bnds ascs >>= done
    
    assoc' bnds defvalue ascs = runST $ fromAssocs' bnds defvalue ascs >>= done
    
    Z // ascs = isNull ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    es // ascs = isNull ascs ? es $ runST $ thaw es >>= (`overwrite` ascs) >>= done
    
    fromIndexed es = runST $ do
        copy <- filled n (unreachEx "fromIndexed")
        forM_ [0 .. n - 1] $ \ i -> writeM_ copy i (es !^ i)
        done copy
      where
        n = sizeOf es
    
    {-# INLINE (!^) #-}
    Z !^ _ = error "in SDP.ByteList.Ublist.(!^)"
    (Ublist bytes# bytes) !^ i = i < c ? bytes# !^ i $ bytes !^ (i - c)
      where
        c = sizeOf bytes#
    
    (.!) = (!^)
    
    p .$ es = go es 0
      where
        go Z _ = Nothing
        go (Ublist bytes# bytes) o = case p .$ bytes# of
          Just  i -> Just (i + o)
          Nothing -> go bytes $! o + sizeOf bytes#
    
    (*$) p es = go es 0
      where
        go Z _ = []
        go (Ublist bytes# bytes) o = (p *$ bytes#) ++ (go bytes $! o + sizeOf bytes#)

instance (Unboxed e) => IFold (Ublist e) Int e
  where
    ifoldr  f base = \ es -> case es of {Z -> base; (Ublist bytes# bytes) -> ifoldr  f (ifoldr  f base bytes) bytes#}
    ifoldl  f base = \ es -> case es of {Z -> base; (Ublist bytes# bytes) -> ifoldl  f (ifoldl  f base bytes#) bytes}
    
    i_foldr f base = \ es -> case es of {Z -> base; (Ublist bytes# bytes) -> i_foldr f (i_foldr f base bytes) bytes#}
    i_foldl f base = \ es -> case es of {Z -> base; (Ublist bytes# bytes) -> i_foldl f (i_foldl f base bytes#) bytes}

--------------------------------------------------------------------------------

instance IsString (Ublist Char) where fromString = fromList

--------------------------------------------------------------------------------

instance (Unboxed e) => Thaw (ST s) (Ublist e) (STUblist s e)
  where
    thaw Z = return STUBEmpty
    thaw (Ublist bytes# bytes) = liftA2 STUblist (thaw bytes#) (thaw bytes)
    
    unsafeThaw Z = return STUBEmpty
    unsafeThaw (Ublist bytes# bytes) = liftA2 STUblist (unsafeThaw bytes#) (unsafeThaw bytes)

instance (Unboxed e) => Freeze (ST s) (STUblist s e) (Ublist e)
  where
    freeze STUBEmpty = return Z
    freeze (STUblist mbytes# mbytes) = liftA2 Ublist (freeze mbytes#) (freeze mbytes)
    
    unsafeFreeze STUBEmpty = return Z
    unsafeFreeze (STUblist mbytes# mbytes) = liftA2 Ublist (unsafeFreeze mbytes#) (unsafeFreeze mbytes)

--------------------------------------------------------------------------------

{-# INLINE done #-}
done :: (Unboxed e) => STUblist s e -> ST s (Ublist e)
done = unsafeFreeze

{-# INLINE center #-}
center :: Int -> Int -> Int
center l u = l + (u - l) `div` 2

{-# INLINE nubSorted #-}
nubSorted :: (Unboxed e) => Compare e -> Ublist e -> Ublist e
nubSorted f (xs :< x) = fromList $ i_foldr (\ e ls@(l : _) -> e `f` l == EQ ? ls $ e : ls) [x] xs
nubSorted _ _ = Z

pfailEx :: String -> a
pfailEx msg = throw . PatternMatchFail $ "in SDP.ByteList.Ublist." ++ msg

unreachEx :: String -> a
unreachEx msg = throw . UnreachableException $ "in SDP.ByteList.Ublist." ++ msg

lim :: Int
lim =  1024





