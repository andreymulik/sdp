{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Trustworthy, CPP, TypeOperators, TypeFamilies, DefaultSignatures #-}
{-# LANGUAGE ConstraintKinds #-}

#ifdef SDP_QUALIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.Set
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
  
    "SDP.Set" provides 'Set' - class for basic set operations.
-}
module SDP.Set
(
  -- * SetWith
  SetWith (..), SetWith1, SetWith2,
  
  -- * Set
  Set (..), Set1, Set2,
  
#ifdef SDP_QUALIFIED_CONSTRAINTS
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  SetWith', SetWith'', Set', Set''
#endif
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Linear

import Data.Maybe ( isJust  )
import Data.List  ( groupBy )

import GHC.Types

default ()

--------------------------------------------------------------------------------

{- |
  'SetWith' is a class of data structures, that can represent sets.
  
  'SetWith' doesn't provide data protection/validation before each first action.
  All functions (except 'setWith') works correctly only with correct sets.
  'SetWith' guarantee only that the returned data is correct. So if you need
  maximum reliability and security, use @containers@. But if you want
  simplicity, openness and a lot of non-set functions without extra conversions,
  then you are at the right place.
  
  Note that function of type @Compare o@ must follow basic order laws
  (comparability, transitivity, reflexivity and antisymmetry). With wrong
  comparator, the result may become implementation-dependent.
-}
class Nullable s => SetWith s o | s -> o
  where
    {-# MINIMAL intersectionWith, unionWith, differenceWith,
      lookupLTWith, lookupGTWith #-}
    
    {- Creation functions. -}
    
    -- | Creates ordered set from linear structure.
    default setWith :: Linear s o => Compare o -> s -> s
    setWith :: Compare o -> s -> s
    setWith f = fromList . setWith f . listL
    
    {- |
      Creates set from linear structure using additional function for
      choice/merge equal elements.
    -}
    groupSetWith :: Compare o -> (o -> o -> o) -> s -> s
    groupSetWith cmp f = fromList . groupSetWith cmp f . listL
    default groupSetWith :: Linear s o => Compare o -> (o -> o -> o) -> s -> s
    
    -- | Adding element to set.
    default insertWith :: Linear s o => Compare o -> o -> s -> s
    insertWith :: Compare o -> o -> s -> s
    insertWith f = unionWith f . single
    
    -- | Deleting element from set.
    deleteWith :: Compare o -> o -> s -> s
    deleteWith f = flip (differenceWith f) . single
    default deleteWith :: Linear s o => Compare o -> o -> s -> s
    
    {- Basic operations on sets. -}
    
    -- | Intersection of two sets.
    intersectionWith :: Compare o -> s -> s -> s
    
    -- | Difference (relative complement, aka A / B) of two sets.
    differenceWith :: Compare o -> s -> s -> s
    
    -- | Symmetric difference of two sets.
    symdiffWith :: Compare o -> s -> s -> s
    symdiffWith f xs ys = differenceWith f (unionWith f xs ys) (intersectionWith f xs ys)
    
    -- | Union of two sets.
    unionWith :: Compare o -> s -> s -> s
    
    {- Generalization of basic set operations on foldable. -}
    
    -- | Fold by 'intersectionWith'.
    intersectionsWith :: Foldable f => Compare o -> f s -> s
    intersectionsWith =  (`foldl` Z) . intersectionWith
    
    -- | Fold by 'differenceWith'.
    differencesWith :: Foldable f => Compare o -> f s -> s
    differencesWith =  (`foldl` Z) . differenceWith
    
    -- | Fold by 'unionWith'.
    unionsWith :: Foldable f => Compare o -> f s -> s
    unionsWith =  (`foldl` Z) . unionWith
    
    -- | Fold by 'symdiffWith'.
    symdiffsWith :: Foldable f => Compare o -> f s -> s
    symdiffsWith =  (`foldl` Z) . symdiffWith
    
    {- Сomparsion operations. -}
    
    -- | Compares sets on intersection.
    isIntersectsWith :: Compare o -> s -> s -> Bool
    isIntersectsWith f = not ... isDisjointWith f
    
    -- | Compares sets on disjoint.
    isDisjointWith :: Compare o -> s -> s -> Bool
    isDisjointWith f = isNull ... intersectionWith f
    
    -- | Same as 'elem', but can work faster. By default, uses 'find'.
    default memberWith :: (t o ~~ s, Foldable t) => Compare o -> o -> s -> Bool
    memberWith :: Compare o -> o -> s -> Bool
    memberWith f e = isJust . find (\ x -> f e x == EQ)
    
    -- | Сhecks whether a first set is a subset of second.
    default isSubsetWith :: (t o ~~ s, Foldable t) => Compare o -> s -> s -> Bool
    isSubsetWith :: Compare o -> s -> s -> Bool
    isSubsetWith f xs ys = all (\ x -> memberWith f x ys) xs
    
    -- | Generates a list of different subsets (including empty and equivalent).
    default subsets :: (Linear s o, Ord o) => s -> [s]
    subsets :: (Ord o) => s -> [s]
    subsets =  subsequences . setWith compare
    
    {- Lookups. -}
    
    -- | 'lookupLTWith' trying to find lesser element in set.
    lookupLTWith :: Compare o -> o -> s -> Maybe o
    
    -- | 'lookupGTWith' trying to find greater element in set.
    lookupGTWith :: Compare o -> o -> s -> Maybe o
    
    -- | 'lookupGEWith' trying to find greater or equal element in set.
    lookupGEWith :: Compare o -> o -> s -> Maybe o
    lookupGEWith f e es = memberWith f e es ? Just e $ lookupGTWith f e es
    
    -- | 'lookupLEWith' trying to find lesser or equal element in set.
    lookupLEWith :: Compare o -> o -> s -> Maybe o
    lookupLEWith f e es = memberWith f e es ? Just e $ lookupLTWith f e es

--------------------------------------------------------------------------------

{- |
  'Set' is a class of data structures, that can represent any sets. 'Set' is
  intended for more specific sets than ordered linear structures. In particular,
  it may not work with an arbitrary comparator, and also (unlike the early
  implementation) does not impose restrictions on the element type.
  
  'Set', as well as 'SetWith', doesn't provide data protection/validation.
-}
class Nullable s => Set s o | s -> o
  where
    -- | The same as @'setWith' 'compare'@.
    default set :: (SetWith s o, Ord o) => s -> s
    set :: s -> s
    set =  setWith compare
    
    -- | Same as @'insert' 'compare'@.
    default insert :: (SetWith s o, Ord o) => o -> s -> s
    insert :: o -> s -> s
    insert =  insertWith compare
    
    -- | Same as @'deleteWith' 'compare'@.
    default delete :: (SetWith s o, Ord o) => o -> s -> s
    delete :: o -> s -> s
    delete =  deleteWith compare
    
    -- | Same as @'intersectionWith' 'compare'@.
    default (/\) :: (SetWith s o, Ord o) => s -> s -> s
    (/\) :: s -> s -> s
    (/\) =  intersectionWith compare
    
    -- | Same as @'unionWith' 'compare'@.
    default (\/) :: (SetWith s o, Ord o) => s -> s -> s
    (\/) :: s -> s -> s
    (\/) =  unionWith compare
    
    -- | Same as @'differenceWith' 'compare'@.
    default (\\) :: (SetWith s o, Ord o) => s -> s -> s
    (\\) :: s -> s -> s
    (\\) =  differenceWith compare
    
    -- | Same as @'symdiffWith' 'compare'@.
    default (\^/) :: (SetWith s o, Ord o) => s -> s -> s
    (\^/) :: s -> s -> s
    (\^/) =  symdiffWith compare
    
    -- | Same as @'isDisjointWith' 'compare'@.
    default (/?\) :: (SetWith s o, Ord o) => s -> s -> Bool
    (/?\) :: s -> s -> Bool
    (/?\) =  isDisjointWith compare
    
    -- | Same as @'isIntersectsWith' 'compare'@.
    default (\?/) :: (SetWith s o, Ord o) => s -> s -> Bool
    (\?/) :: s -> s -> Bool
    (\?/) =  isIntersectsWith compare
    
    -- | Same as @'isSubsetWith' 'compare'@.
    default (\+/) :: (SetWith s o, Ord o) => s -> s -> Bool
    (\+/) :: s -> s -> Bool
    (\+/) =  isSubsetWith compare
    
    -- | Same as @'intersectionsWith' 'compare'@.
    default intersections :: (Foldable f, SetWith s o, Ord o) => f s -> s
    intersections :: Foldable f => f s -> s
    intersections =  intersectionsWith compare
    
    -- | Same as @'unionsWith' 'compare'@.
    default unions :: (Foldable f, SetWith s o, Ord o) => f s -> s
    unions :: Foldable f => f s -> s
    unions =  unionsWith compare
    
    -- | Same as @'differencesWith' 'compare'@.
    differences :: Foldable f => f s -> s
    differences =  differencesWith compare
    default differences :: (Foldable f, SetWith s o, Ord o) => f s -> s
    
    -- | Same as @'symdiffsWith' compare'@.
    default symdiffs :: (Foldable f, SetWith s o, Ord o) => f s -> s
    symdiffs :: Foldable f => f s -> s
    symdiffs =  symdiffsWith compare
    
    -- | Same as @'memberWith' 'compare'@.
    member :: o -> s -> Bool
    member =  memberWith compare
    default member :: (SetWith s o, Ord o) => o -> s -> Bool
    
    -- | Same as @'lookupLTWith' 'compare'@.
    default lookupLT :: (SetWith s o, Ord o) => o -> s -> Maybe o
    lookupLT :: Ord o => o -> s -> Maybe o
    lookupLT =  lookupLTWith compare
    
    -- | Same as @'lookupGTWith' 'compare'@.
    default lookupGT :: (SetWith s o, Ord o) => o -> s -> Maybe o
    lookupGT :: Ord o => o -> s -> Maybe o
    lookupGT =  lookupGTWith compare
    
    -- | Same as @'lookupLEWith' 'compare'@.
    default lookupLE :: (SetWith s o, Ord o) => o -> s -> Maybe o
    lookupLE :: Ord o => o -> s -> Maybe o
    lookupLE =  lookupLEWith compare
    
    -- | Same as @'lookupGEWith' 'compare'@.
    default lookupGE :: (SetWith s o, Ord o) => o -> s -> Maybe o
    lookupGE :: Ord o => o -> s -> Maybe o
    lookupGE =  lookupGEWith compare

--------------------------------------------------------------------------------

-- | 'Set' contraint for @(Type -> Type)@-kind types.
type Set1 s o = Set (s o) o

-- | 'SetWith' contraint for @(Type -> Type)@-kind types.
type SetWith1 s o = SetWith (s o) o

-- | 'Set' contraint for @(Type -> Type -> Type)@-kind types.
type Set2 s i o = Set (s i o) o

-- | 'SetWith' contraint for @(Type -> Type -> Type)@-kind types.
type SetWith2 s i o = SetWith (s i o) o

--------------------------------------------------------------------------------

#ifdef SDP_QUALIFIED_CONSTRAINTS
-- | 'Set' quantified contraint for @(Type -> Type)@-kind types.
type Set' s = forall o . Set (s o) o

-- | 'SetWith' quantified contraint for @(Type -> Type)@-kind types.
type SetWith' s = forall o . SetWith (s o) o

-- | 'Set' quantified contraint for @(Type -> Type -> Type)@-kind types.
type Set'' s = forall i o . Set (s i o) o

-- | 'SetWith' quantified contraint for @(Type -> Type -> Type)@-kind types.
type SetWith'' s = forall i o . SetWith (s i o) o
#endif

--------------------------------------------------------------------------------

instance Ord o => Set [o] o

instance SetWith [o] o
  where
    setWith f = sortBy f . nubBy ((EQ ==) ... f)
    
    insertWith f e es@(x : xs) = case e `f` x of {GT -> x : insertWith f e xs; LT -> e : es; EQ -> es}
    insertWith _ e _ = [e]
    
    deleteWith f e es@(x : xs) = case e `f` x of {GT -> x : deleteWith f e xs; LT -> es; EQ -> xs}
    deleteWith _ _ _ = []
    
    memberWith f e (x : xs) = case e `f` x of {GT -> memberWith f e xs; LT -> False; EQ -> True}
    memberWith _ _ _ = False
    
    intersectionWith f xs'@(x : xs) ys'@(y : ys) = case x `f` y of
      LT -> intersectionWith f xs  ys'
      GT -> intersectionWith f xs' ys
      EQ -> x : intersectionWith f xs ys
    intersectionWith _ _ _ = []
    
    unionWith f xs'@(x : xs) ys'@(y : ys) = case x `f` y of
      LT -> x : unionWith f xs  ys'
      EQ -> x : unionWith f xs  ys
      GT -> y : unionWith f xs' ys
    unionWith _ xs ys = xs ++ ys
    
    differenceWith f xs'@(x : xs) ys'@(y : ys) = case f x y of
      LT -> x : differenceWith f xs ys'
      EQ -> differenceWith f xs  ys
      GT -> differenceWith f xs' ys
    differenceWith _ xs _ = xs
    
    symdiffWith f xs'@(x : xs) ys'@(y : ys) = case f x y of
      EQ -> symdiffWith f xs ys
      LT -> x : symdiffWith f xs  ys'
      GT -> y : symdiffWith f xs' ys
    symdiffWith _ xs ys = xs ++ ys
    
    isIntersectsWith f xs'@(x : xs) ys'@(y : ys) = case f x y of
      LT -> isIntersectsWith f xs  ys'
      GT -> isIntersectsWith f xs' ys
      EQ -> True
    isIntersectsWith _ _ _ = False
    
    isDisjointWith f xs'@(x : xs) ys'@(y : ys) = case f x y of
      LT -> isDisjointWith f xs  ys'
      GT -> isDisjointWith f xs' ys
      EQ -> False
    isDisjointWith _ _ _ = True
    
    lookupLTWith f o (x : xs) = case o `f` x of {GT -> look x xs; _ -> Nothing}
      where
        look r (e : es) = case o `f` e of {GT -> look e es; _ -> Just r}
        look r _ = Just r
    lookupLTWith _ _ _ = Nothing
    
    lookupGTWith f o (x : xs) = case o `f` x of {LT -> Just x; _ -> look xs}
      where
        look (e : es) = case o `f` e of {LT -> Just e; _ -> look es}
        look _ = Nothing
    lookupGTWith _ _ _ = Nothing
    
    lookupLEWith f o (x : xs) = case o `f` x of {LT -> Nothing; _ -> look x xs}
      where
        look r (e : es) = case o `f` e of {LT -> Just r; _ -> look e es}
        look r _ = Just r
    lookupLEWith _ _ _ = Nothing
    
    lookupGEWith f o (x : xs) = case o `f` x of {GT -> look xs; _ -> Just x}
      where
        look (e : es) = case o `f` e of {GT -> look es; _ -> Just e}
        look _ = Nothing
    lookupGEWith _ _ _ = Nothing
    
    groupSetWith cmp f = map (foldr1 f) . groupBy ((== EQ) ... cmp) . sortBy cmp

