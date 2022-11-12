{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Safe, CPP, BangPatterns, ConstraintKinds, DefaultSignatures #-}

#ifdef SDP_QUALIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.Map
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Map" provides 'Map' - class of immutable associative arrays.
-}
module SDP.Map
(
  -- * Exports
  module SDP.Set,
  
  -- * Map
  Map (..), Map1, Map2,
  
#ifdef SDP_QUALIFIED_CONSTRAINTS
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  Map', Map''
#endif
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Linear
import SDP.Set

import Data.List ( findIndex, findIndices )

import Control.Exception.SDP

default ()

infixl 9 .!, !, !?

--------------------------------------------------------------------------------

{- |
  @since 0.2
  
  'Map' is a class of dictionaries (associative arrays).
  
  'Map' provides a set of operations on associative arrays that aren't specific
  to 'Linear' data structures and aren't limited by the 'Bordered' context
  (doesn't restrict key type).
-}
class Nullable map => Map map key e | map -> key, map -> e
  where
    {-# MINIMAL toMap', ((.!) | (!?)) #-}
    
    -- | Returns list of associations @(index, element)@.
    default assocs :: (Bordered map key, Linear map e) => map -> [(key, e)]
    assocs :: map -> [(key, e)]
    assocs es = indices es `zip` listL es
    
    {- |
      A less specific version of "SDP.Indexed.Indexed.assoc" that creates a new
      associative array. For 'Linear' structures without gaps, it may be less
      effective.
      
      > Z // ascs = toMap -- forall ascs
    -}
    toMap :: [(key, e)] -> map
    toMap =  toMap' (undEx "toMap {default}")
    
    {- |
      Strict version of 'toMap' with default value.
      
      Note that the default value is set only for elements included in the range
      of the created structure and will not be set for values outside its range
      (when expanding, concatenating, etc.) for most structures since they don't
      store it.
    -}
    toMap' :: e -> [(key, e)] -> map
    
    {- |
      @'insert'' key e map@ inserts @e@ with @key@ to @map@. If @map@ already
      contains an element with @key@, the element will be overwritten.
      
      If @map@ doesn't allow gaps, then the missing elements should be filled
      with default values.
    -}
    insert' :: key -> e -> map -> map
    insert' k e es = toMap $ assocs es :< (k, e)
    
    {- |
      'delete'' removes element with given key.
      
      If the structure has boundaries, when removed from the beginning (end),
      they should change accordingly. If the structure doesn't allow gaps, then
      when removed from the middle, the actual value should be replaced with the
      default value.
    -}
    delete' :: key -> map -> map
    default delete' :: Eq key => key -> map -> map
    delete' k = toMap . except ((== k) . fst) . assocs
    
    -- | @'member'' key map@ checks if @key@ in @map@.
    default member' :: Bordered map key => key -> map -> Bool
    member' :: key -> map -> Bool
    member' =  flip indexIn
    
    -- | Update elements of immutable structure (by copying).
    (//) :: map -> [(key, e)] -> map
    (//) =  toMap ... (++) . assocs
    
    {- |
      @since 0.3
      
      Safe index-based immutable writer. Earlier defined in "SDP.Indexed".
    -}
    {-# INLINE write' #-}
    write' :: map -> key -> e -> map
    write' es = write es . offsetOf es
    default write' :: (Bordered map key, Linear map e) => map -> key -> e -> map
    
    {- |
      @since 0.3
      
      Update element by given function. Earlier defined in "SDP.Indexed".
    -}
    update' :: map -> (e -> e) -> key -> map
    update' es f i = write' es i . f $ es!i
    
    -- | @('.!')@ is unsafe reader, can be faster @('!')@ by skipping checks.
    {-# INLINE (.!) #-}
    (.!) :: map -> key -> e
    (.!) =  (undEx "(.!)" +?) ... (!?)
    
    -- | @('!')@ is well-safe reader, may 'throw' 'IndexException'.
    (!) :: map -> key -> e
    default (!) :: Bordered map key => map -> key -> e
    (!) es i = case inBounds (bounds es) i of
        IN -> es .! i
        ER -> empEx   msg
        OR -> overEx  msg
        UR -> underEx msg
      where
        msg = "(!) {default}"
    
    -- | @('!?')@ is completely safe, but very boring function.
    (!?) :: map -> key -> Maybe e
    (!?) es = flip member' es ?+ (es .!)
    
    -- | Filter with key.
    filter' :: (key -> e -> Bool) -> map -> map
    filter' f = toMap . filter (uncurry f) . assocs
    
    {- |
      'union'' is 'groupSetWith' for maps but works with real groups of
      elements, not with consequentive equal elements.
      
      'union'' merges/chooses elements with equal keys from two maps.
    -}
    union' :: Ord key => (e -> e -> e) -> map -> map -> map
    union' f = toMap ... on go assocs
      where
        go xs'@(x'@(i, x) : xs) ys'@(y'@(j, y) : ys) = case i <=> j of
          LT -> x' : go xs ys'
          EQ -> (i, f x y) : go xs ys
          GT -> y' : go xs' ys
        go xs'   Z = xs'
        go Z   ys' = ys'
    
    {- |
      @'difference'' f mx my@ applies @comb@ to values with equal keys.
      If @f x y@ (where @(k1, x) <- mx@, @(k2, y) <- my@, @k1 == k2@) is
      'Nothing', element isn't included to result map.
      
      Note that 'difference'' is poorer than a similar functions in containers.
    -}
    difference' :: Ord key => (e -> e -> Maybe e) -> map -> map -> map
    difference' f = toMap ... on go assocs
      where
        go xs'@(x'@(i, x) : xs) ys'@((j, y) : ys) = case i <=> j of
          GT -> go xs' ys
          LT -> x' : go xs ys'
          EQ -> case f x y of {(Just e) -> (i, e) : go xs ys; _ -> go xs ys}
        go xs' _ = xs'
    
    {- |
      @'intersection'' f mx my@ combines elements of 'intersection'' by @f@:
      if @isJust (f x y)@ (where @(k1, x) <- mx, (k2, y) <- my, k1 == k2@),
      then element is added to result map.
    -}
    intersection' :: Ord key => (e -> e -> e) -> map -> map -> map
    intersection' f = toMap ... on go assocs
      where
        go xs'@((i, x) : xs) ys'@((j, y) : ys) = case i <=> j of
          LT -> go xs ys'
          GT -> go xs' ys
          EQ -> (i, f x y) : go xs ys
        go _ _ = []
    
    -- | Update function, by default uses ('//').
    update :: map -> (key -> e -> e) -> map
    update es f = es // [ (i, f i e) | (i, e) <- assocs es ]
    
    {- |
      @lookupLT' k map@ finds pair @(key, value)@ with smallest @key@, where
      @key < k@ (if any). @k@ may not be a @map@ element.
    -}
    lookupLT' :: Ord key => key -> map -> Maybe (key, e)
    lookupLT' k = lookupLTWith cmpfst (k, unreachEx "lookupLT'") . assocs
    
    {- |
      @lookupGT' k map@ finds pair @(key, value)@ with greatest @key@, where
      @key > k@ (if any). @k@ may not be a @map@ element.
    -}
    lookupGT' :: Ord key => key -> map -> Maybe (key, e)
    lookupGT' k = lookupGTWith cmpfst (k, unreachEx "lookupGT'") . assocs
    
    {- |
      @lookupLE' k map@ finds pair @(key, value)@ with smallest @key@, where
      @key <= k@ (if any). If @k@ is a @map@ element, returns @(k, e)@.
    -}
    lookupLE' :: Ord key => key -> map -> Maybe (key, e)
    lookupLE' k me = (,) k <$> (me !? k) <|> lookupLEWith cmpfst
      (k, unreachEx "lookupLE'") (assocs me)
    
    {- |
      @lookupGE' k map@ finds pair @(key, value)@ with  @key@, where
      @key >= k@ (if any).
    -}
    lookupGE' :: Ord key => key -> map -> Maybe (key, e)
    lookupGE' k me = (,) k <$> (me !? k) <|> lookupGEWith cmpfst
      (k, unreachEx "lookupGE'") (assocs me)
    
    -- | Returns list of map keys.
    keys :: map -> [key]
    keys =  fsts . assocs
    
    -- | Searches the key of first matching element.
    (.$) :: (e -> Bool) -> map -> Maybe key
    (.$) =  null ?- head ... (*$)
    
    -- | Searches the keys of all matching elements.
    (*$) :: (e -> Bool) -> map -> [key]
    (*$) f = select (f . snd ?+ fst) . assocs
    
    {- Folds with key. -}
    
    -- | 'kfoldr' is 'foldr' with key.
    kfoldr :: (key -> e -> b -> b) -> b -> map -> b
    kfoldr f base = foldr (uncurry f) base . assocs
    
    -- | 'kfoldl' is 'foldl' with key.
    kfoldl :: (key -> b -> e -> b) -> b -> map -> b
    kfoldl f base = foldl (\ acc (i, e) -> f i acc e) base . assocs
    
    -- | 'kfoldr'' is strict version of 'kfoldr'.
    kfoldr' :: (key -> e -> b -> b) -> b -> map -> b
    kfoldr' f = kfoldr (\ !i e !b -> f i e b)
    
    -- | 'kfoldl'' is strict version of 'kfoldl'.
    kfoldl' :: (key -> b -> e -> b) -> b -> map -> b
    kfoldl' f = kfoldl (\ !i !b e -> f i b e)

--------------------------------------------------------------------------------

-- | 'Map' contraint for @(Type -> Type)@-kind types.
type Map1 map key e = Map (map e) key e

-- | 'Map' contraint for @(Type -> Type -> Type)@-kind types.
type Map2 map key e = Map (map key e) key e

--------------------------------------------------------------------------------

#ifdef SDP_QUALIFIED_CONSTRAINTS
-- | 'Map' contraint for @(Type -> Type)@-kind types.
type Map' map key = forall e . Map (map e) key e

-- | 'Map' contraint for @(Type -> Type -> Type)@-kind types.
type Map'' map = forall key e . Map (map key e) key e
#endif

--------------------------------------------------------------------------------

instance Map [e] Int e
  where
    toMap' e = snds . fill . setWith cmpfst
      where
        fill (ix@(i1, _) : iy@(i2, _) : ies) =
          let rest = i1 + 1 == i2 ? iy : ies $ (i1 + 1, e) : iy : ies
          in  ix : fill rest
        fill xs = xs
    
    assocs = zip [0 ..] . listL
    
    insert' k e es = k < 0 ? es $ go k es
      where
        go 0    xs    = isNull xs ? [e] $ e : tail xs
        go i    []    = err : go (i - 1) []
        go i (x : xs) = x : go (i - 1) xs
        
        err = undEx "insert'"
    
    (x : xs) .! n = n == 0 ? x $ xs .! (n - 1)
    _        .! _ = error "in SDP.Map.(.!)"
    
    (!) [] _ = empEx "(!)"
    (!) es n = n >= 0 ? es !# n $ underEx "(!)"
      where
        []       !# _  = overEx "(!)"
        (x : xs) !# n' = n' == 0 ? x $ xs !# (n' - 1)
    
    []       !? _ = Nothing
    (x : xs) !? n = case n <=> 0 of
      GT -> xs !? (n - 1)
      EQ -> Just x
      LT -> Nothing
    
    xs // es = snds $ unionWith cmpfst (setWith cmpfst es) (assocs xs)
    
    (.$) = findIndex
    (*$) = findIndices
    
    kfoldr f base =
      let go i es = case es of {(x : xs) -> f i x $ go (i + 1) xs; _ -> base}
      in  go 0
    
    kfoldl f =
      let go i e es = case es of {(x : xs) -> go (i + 1) (f i e x) xs; _ -> e}
      in  go 0

--------------------------------------------------------------------------------

empEx :: String -> a
empEx =  throw . EmptyRange . showString "in SDP.Map."

undEx :: String -> a
undEx =  throw . UndefinedValue . showString "in SDP.Map."

overEx :: String -> a
overEx =  throw . IndexOverflow . showString "in SDP.Map."

underEx :: String -> a
underEx =  throw . IndexUnderflow . showString "in SDP.Map."

unreachEx :: String -> a
unreachEx =  throw . UnreachableException . showString "in SDP.Map."




