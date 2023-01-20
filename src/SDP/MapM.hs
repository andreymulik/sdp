{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Safe, CPP, BangPatterns, ConstraintKinds, DefaultSignatures #-}

#ifdef SDP_QUALIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.MapM
    Copyright   :  (c) Andrey Mulik 2020-2023
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.MapM" provides 'MapM' - class of mutable associative arrays.
-}
module SDP.MapM
(
  -- * Mutable maps
  MapM (..), MapM1, MapM2, fromMap', unionM', differenceM', intersectionM',
  
#ifdef SDP_QUALIFIED_CONSTRAINTS
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  MapM', MapM''
#endif
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.LinearM
import SDP.Map

import Data.Maybe ( listToMaybe )

import Control.Exception.SDP

default ()

infixl 5 >!, !>, !?>

--------------------------------------------------------------------------------

-- | 'MapM' is class of mutable associative arrays.
class (Monad m, Eq key) => MapM m map key e | map -> m, map -> key, map -> e
  where
    {-# MINIMAL newMap', overwrite, ((>!)|(!?>)), kfoldrM, kfoldlM #-}
    
    -- | 'getAssocs' is version of 'SDP.Map.assocs' for mutable maps.
    default getAssocs :: LinearM m map e => map -> m [(key, e)]
    getAssocs :: map -> m [(key, e)]
    getAssocs es = liftA2 zip (getKeys es) (getLeft es)
    
    -- | Create new mutable map from list of @(key, element)@ associations.
    newMap :: [(key, e)] -> m map
    newMap =  newMap' (undEx "newMap {default}")
    
    -- | Create new mutable map from list of @(key, element)@ associations.
    newMap' :: e -> [(key, e)] -> m map
    
    -- | 'mfilter' with key.
    mfilter' :: (key -> e -> Bool) -> map -> m map
    mfilter' f = newMap <=< kfoldrM (\ k x xs -> return (f k x ? (k, x) : xs $ xs)) []
    
    -- | 'filterM' with key.
    filterM' :: (key -> e -> m Bool) -> map -> m map
    filterM' f = newMap <=< kfoldrM (\ k x xs ->
        f k x ?^ return ((k, x) : xs) $ return xs
      ) []
    
    {- |
      @'overwrite' es ascs@ - analog of @('//')@ for mutable data structures,
      designed to update a large number of structure elements in one operation.
      
      In version sdp-0.3, structures with fixed and variable bounds have
      different rules for handling a list of key-value pairs.
      
      * Structures with fixed bounds ('Bordered') should ignore elements with
      keys outside their range
      * Structures with variable bounds ('BorderedM') should be automatically
      expanded to accommodate new elements.
      
      In older versions (sdp < 0.3), 'overwrite' could \"resize\" a fixed-size
      structure in the following way: the argument after applying the function
      was considered undefined, and instead it was proposed to use the resulting
      structure.
      This approach worked well for dealing with "one-shot" structures that
      could simply be discarded, but for slices and long-lived mutable
      structures, the function was more trouble than it was worth.
      Another "feature" of the old 'overwrite' was the creation of a new
      structure when passing an empty @es@ with a non-empty @ascs@.
      Since @sdp-0.3@, only structures with mutable bounds can be extended.
    -}
    overwrite :: map -> [(key, e)] -> m ()
    
    -- | Update elements by mapping with indices.
    updateM :: map -> (key -> e -> e) -> m ()
    updateM es f = overwrite es =<< kfoldrM (\ key e ->
        return . (:) (key, f key e)
      ) [] es
    
    -- | Update elements by mapping with indices.
    mupdate :: map -> (key -> e -> m e) -> m ()
    mupdate es go = overwrite es =<< kfoldrM (\ key e ies ->
        (: ies) . (,) key <$> go key e
      ) [] es
    
    -- | Returns list of map keys.
    default getKeys :: BorderedM m map key => map -> m [key]
    getKeys :: map -> m [key]
    getKeys =  getIndices
    
    -- | Checks if key in map.
    default memberM' :: BorderedM m map key => map -> key -> m Bool
    memberM' :: map -> key -> m Bool
    memberM' =  nowIndexIn
    
    {- |
      @unionM'' f xs ys@ returns the union of the @xs@ and @ys@ map by keys. If
      the maps @xs@ and @ys@ contain value with the same key, the conflict is
      resolved by the @f@ function, which choices, joins or replaces the value.
    -}
    default unionM'' :: Index key => (key -> e -> e -> e) -> map -> map -> m map
    unionM'' :: (key -> e -> e -> e) -> map -> map -> m map
    unionM'' f = (newMap <=<< liftA2 go) `on` getAssocs
      where
        go xs'@(x'@(i, x) : xs) ys'@(y'@(j, y) : ys) = case i <=> j of
          LT -> x' : go xs ys'
          EQ -> (i, f i x y) : go xs ys
          GT -> y' : go xs' ys
        go xs'   Z = xs'
        go Z   ys' = ys'
    
    {- |
      @differenceM'' f xs ys@ returns the difference of the @xs@ and @ys@ map by
      keys. If the maps @xs@ and @ys@ contain value with the same key, the
      conflict is resolved by the @f@ function, which choices, joins, replaces
      or discards the value.
      
      If you need the difference of maps with different element types, use the
      @containers@ library or equivalents. @sdp@ doesn't provide such
      capabilities.
    -}
    default differenceM'' :: Index key => (key -> e -> e -> Maybe e)
                                       -> map -> map -> m map
    
    differenceM'' :: (key -> e -> e -> Maybe e) -> map -> map -> m map
    differenceM'' f = (newMap <=<< liftA2 go) `on` getAssocs
      where
        go xs'@(x'@(i, x) : xs) ys'@((j, y) : ys) = case i <=> j of
          GT -> go xs' ys
          LT -> x' : go xs ys'
          EQ -> case f i x y of {Just e -> (i, e) : go xs ys; _ -> go xs ys}
        go xs' _ = xs'
    
    {- |
      @intersectionM'' f xs ys@ returns the intersection of the @xs@ and @ys@ map
      by keys. Function @f@ choices, joins or replaces elements of @xs@ and @ys@
      with same key.
    -}
    default intersectionM'' :: Index key => (key -> e -> e -> e)
                                         -> map -> map -> m map
    
    intersectionM'' :: (key -> e -> e -> e) -> map -> map -> m map
    intersectionM'' f = (newMap <=<< liftA2 go) `on` getAssocs
      where
        go xs'@((i, x) : xs) ys'@((j, y) : ys) = case i <=> j of
          LT -> go xs ys'
          GT -> go xs' ys
          EQ -> (i, f i x y) : go xs ys
        go _ _ = []
    
    -- | 'kfoldrM' is right monadic fold with key.
    kfoldrM :: (key -> e -> acc -> m acc) -> acc -> map -> m acc
    kfoldrM f base = foldr ((=<<) . uncurry f) (pure base) <=< getAssocs
    
    -- | 'kfoldlM' is left monadic fold with key.
    kfoldlM :: (key -> acc -> e -> m acc) -> acc -> map -> m acc
    kfoldlM f base = foldl (\ ies (i, e) ->
        flip (f i) e =<< ies
      ) (pure base) <=< getAssocs
    
    -- | 'kfoldrM'' is strict version of 'kfoldrM'.
    kfoldrM' :: (key -> e -> acc -> m acc) -> acc -> map -> m acc
    kfoldrM' f = kfoldrM (\ !i e !r -> f i e r)
    
    -- | 'kfoldlM'' is strict version of 'kfoldlM'.
    kfoldlM' :: (key -> acc -> e -> m acc) -> acc -> map -> m acc
    kfoldlM' f = kfoldlM (\ !i !r e -> f i r e)
    
    {- |
      @since 0.3
      
      @insertM' key e es@ inserts or replaces the @e@ value with the key @key@
      to the @es@ map. For fixed-size structures, 'insertM'' works like
      'writeM'' and cannot add new element with @key@ beyond specified bounds.
    -}
    default insertM' :: Bordered map key => map -> key -> e -> m ()
    insertM' :: map -> key -> e -> m ()
    insertM' =  writeM'
    
    {- |
      @since 0.3
      
      @'writeM' map key e@ writes element @e@ to @key@ position safely (if @key@
      is out of @map@ range, do nothing). The 'writeM' function is intended to
      overwrite only existing values, so its behavior is identical for
      structures with both static and dynamic boundaries.
      
      Earlier defined in "SDP.IndexedM".
    -}
    default writeM' :: (BorderedM m map key, LinearM m map e)
                    => map -> key -> e -> m ()
    
    writeM' :: map -> key -> e -> m ()
    writeM' es i e = do bnds <- getBounds es; writeM es (offset bnds i) e
    
    {- |
      @since 0.3
      
      Update element by given function. Earlier defined in "SDP.IndexedM".
    -}
    updateM' :: map -> (e -> e) -> key -> m ()
    updateM' es f i = writeM' es i . f =<< es >! i
    
    {- |
      @since 0.3
      
      @deleteM' key es@ deletes the element with the key @key@ from the @es@
      map. For fixed-size structures, marks element as removed (usually by
      exception or default value).
    -}
    default deleteM' :: Bordered map key => map -> key -> m ()
    deleteM' :: map -> key -> m ()
    deleteM' es key = writeM' es key (empEx "deleteM': value is removed")
    
    -- | @('>!')@ is unsafe monadic reader.
    {-# INLINE (>!) #-}
    (>!) :: map -> key -> m e
    (>!) =  fmap (undEx "(!) {default}" +?) ... (!?>)
    
    -- | @('!>')@ is well-safe monadic reader.
    {-# INLINE (!>) #-}
    default (!>) :: BorderedM m map key => map -> key -> m e
    (!>) :: map -> key -> m e
    es !> i = do
      let msg = "(!>) {default}"
      bnds <- getBounds es
      case inBounds bnds i of
        IN -> es >! i
        ER -> empEx   msg
        OR -> overEx  msg
        UR -> underEx msg
    
    -- | @('!?>')@ is completely safe monadic reader.
    (!?>) :: map -> key -> m (Maybe e)
    es !?> i = do b <- memberM' es i; b ? Just <$> (es >! i) $ pure empty
    
    -- | Create mutable map from immutable.
    fromMap :: Map map' key e => map' -> (e -> e) -> m map
    fromMap es f = newMap $ kfoldr (\ key e -> (:) (key, f e)) [] es
    
    -- | Create mutable map from another mutable.
    fromMapM :: MapM m map' key e => map' -> (e -> m e) -> m map
    fromMapM es go = newMap =<< kfoldrM (\ key val ies ->
        (\ e -> (key, e) : ies) <$> go val
      ) [] es
    
    -- | Create mutable map from immutable.
    fromKeyMap :: Map map' key e => map' -> (key -> e -> e) -> m map
    fromKeyMap es f = newMap $ kfoldr (\ key e -> (:) (key, f key e)) [] es
    
    -- | Create mutable map from another mutable.
    fromKeyMapM :: Map map' key e => map' -> (key -> e -> m e) -> m map
    fromKeyMapM es go = newMap =<< kfoldr (\ key ->
        liftA2 ((:) . (,) key) . go key
      ) (return []) es
    
    -- | @('.?')@ is monadic version of @('.$')@.
    (.?) :: (e -> Bool) -> map -> m (Maybe key)
    (.?) =  fmap listToMaybe ... (*?)
    
    -- | @('*?')@ is monadic version of @('*$')@.
    (*?) :: (e -> Bool) -> map -> m [key]
    (*?) p = (select (p . snd ?+ fst) <$>) . getAssocs

--------------------------------------------------------------------------------

-- | Create mutable map from another mutable.
fromMap' :: (MapM m map' key e, MapM m map key e) => map' -> (e -> e) -> m map
fromMap' es f = fromMapM es (return . f)

{- |
  @since 0.3
  
  'unionM''' without key argument.
-}
unionM' :: MapM m map key e => (e -> e -> e) -> map -> map -> m map
unionM' =  unionM'' . const

{- |
  @since 0.3
  
  'differenceM''' without key argument.
-}
differenceM' :: MapM m map key e => (e -> e -> Maybe e) -> map -> map -> m map
differenceM' =  differenceM'' . const

{- |
  @since 0.3
  
  'intersectionM''' without key argument.
-}
intersectionM' :: MapM m map key e => (e -> e -> e) -> map -> map -> m map
intersectionM' =  intersectionM'' . const

--------------------------------------------------------------------------------

-- | 'MapM' contraint for @(Type -> Type)@-kind types.
type MapM1 m map key e = MapM m (map e) key e

-- | 'MapM' contraint for @(Type -> Type -> Type)@-kind types.
type MapM2 m map key e = MapM m (map key e) key e

#ifdef SDP_QUALIFIED_CONSTRAINTS
-- | 'MapM' contraint for @(Type -> Type)@-kind types.
type MapM' m map key = forall e . MapM m (map e) key e

-- | 'MapM' contraint for @(Type -> Type -> Type)@-kind types.
type MapM'' m map = forall key e . MapM m (map key e) key e
#endif

--------------------------------------------------------------------------------

empEx :: String -> a
empEx =  throw . EmptyRange . showString "in SDP.MapM."

undEx :: String -> a
undEx =  throw . UndefinedValue . showString "in SDP.MapM."

overEx :: String -> a
overEx =  throw . IndexOverflow . showString "in SDP.MapM."

underEx :: String -> a
underEx =  throw . IndexUnderflow . showString "in SDP.MapM."




