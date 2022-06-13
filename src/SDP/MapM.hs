{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Safe, CPP, BangPatterns, ConstraintKinds, DefaultSignatures #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.MapM
    Copyright   :  (c) Andrey Mulik 2020-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.MapM" provides 'MapM' - class of mutable associative arrays.
-}
module SDP.MapM
(
  -- * Mutable maps
  MapM (..), MapM1, MapM2,
  
#if __GLASGOW_HASKELL__ >= 806
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
class (Monad m) => MapM m map key e | map -> m, map -> key, map -> e
  where
    {-# MINIMAL newMap', overwrite, ((>!)|(!?>)), kfoldrM, kfoldlM #-}
    
    -- | Create new mutable map from list of @(key, element)@ associations.
    newMap :: [(key, e)] -> m map
    newMap =  newMap' (undEx "newMap {default}")
    
    -- | Create new mutable map from list of @(key, element)@ associations.
    newMap' :: e -> [(key, e)] -> m map
    
    -- | 'getAssocs' is version of 'SDP.Map.assocs' for mutable maps.
    default getAssocs :: (LinearM m map e) => map -> m [(key, e)]
    getAssocs :: map -> m [(key, e)]
    getAssocs es = liftA2 zip (getKeys es) (getLeft es)
    
    -- | @('>!')@ is unsafe monadic reader.
    {-# INLINE (>!) #-}
    (>!) :: map -> key -> m e
    (>!) =  fmap (undEx "(!) {default}" +?) ... (!?>)
    
    -- | @('!>')@ is well-safe monadic reader.
    {-# INLINE (!>) #-}
    default (!>) :: (BorderedM m map key) => map -> key -> m e
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
    
    {- |
      @since 0.3
      
      @'writeM' map key e@ writes element @e@ to @key@ position safely (if @key@
      is out of @map@ range, do nothing). The 'writeM' function is intended to
      overwrite only existing values, so its behavior is identical for
      structures with both static and dynamic boundaries.
      
      Earlier defined in "SDP.IndexedM".
    -}
    writeM' :: map -> key -> e -> m ()
    default writeM' :: (BorderedM m map key, LinearM m map e) => map -> key -> e -> m ()
    writeM' es i e = do bnds <- getBounds es; writeM es (offset bnds i) e
    
    -- | Update elements by mapping with indices.
    updateM :: map -> (key -> e -> e) -> m map
    updateM es f = do
      ascs <- kfoldrM (\ key e -> return . (:) (key, f key e)) [] es
      overwrite es ascs
    
    -- | Update elements by mapping with indices.
    mupdate :: map -> (key -> e -> m e) -> m map
    mupdate es go = do
      ies <- kfoldrM (\ key e ies -> (\ e' -> (key, e') : ies) <$> go key e) [] es
      overwrite es ies
    
    -- | Create mutable map from immutable.
    fromMap :: Map map' key e => map' -> (e -> e) -> m map
    fromMap es f = newMap $ kfoldr (\ key e -> (:) (key, f e)) [] es
    
    -- | Create mutable map from another mutable.
    fromMap' :: MapM m map' key e => map' -> (e -> e) -> m map
    fromMap' es f = do
      ies <- kfoldrM (\ key val ies -> return $ (key, f val) : ies) [] es
      newMap ies
    
    -- | Create mutable map from another mutable.
    fromMapM :: MapM m map' key e => map' -> (e -> m e) -> m map
    fromMapM es go = do
      ies <- kfoldrM (\ key val ies -> do e <- go val; return $ (key, e) : ies) [] es
      newMap ies
    
    -- | Create mutable map from immutable.
    fromKeyMap :: Map map' key e => map' -> (key -> e -> e) -> m map
    fromKeyMap es f = newMap $ kfoldr (\ key e -> (:) (key, f key e)) [] es
    
    -- | Create mutable map from another mutable.
    fromKeyMapM :: Map map' key e => map' -> (key -> e -> m e) -> m map
    fromKeyMapM es go = do
      ies <- kfoldr (\ key -> liftA2 ((:) . (,) key) . go key) (return []) es
      newMap ies
    
    {- |
      @since 0.3
      
      Update element by given function. Earlier defined in "SDP.IndexedM".
    -}
    updateM' :: map -> (e -> e) -> key -> m ()
    updateM' es f i = writeM' es i . f =<< es >! i
    
    {- |
      This function designed to overwrite large enough fragments of the
      structure (unlike 'writeM' and 'SDP.IndexedM.writeM'')
      
      In addition to write operations, 'overwrite' can move and clean, optimize
      data presentation, etc. of a particular structure. Since the reference to
      the original structure may not be the same as reference to the result
      (which implementation is undesirable, but acceptable), the original
      reference (argument) shouldn't be used after 'overwrite'.
      
      All standard @sdp@ structures support safe in-place 'overwrite'.
      
      If the structure uses unmanaged memory, then all unused fragments in the
      resulting structure must be deallocated, regardless of reachability by
      original reference (argument).
      
      Please note that @overwrite@ require a list of associations with indices
      in the current structure bounds and ignore any other, therefore:
      
      > fromAssocs bnds ascs /= (fromAssocs bnds ascs >>= flip overwrite ascs)
    -}
    overwrite :: map -> [(key, e)] -> m map
    
    -- | Checks if key in map.
    default memberM' :: (BorderedM m map key) => map -> key -> m Bool
    memberM' :: map -> key -> m Bool
    memberM' =  nowIndexIn
    
    -- | Returns list of map keys.
    default getKeys :: (BorderedM m map key) => map -> m [key]
    getKeys :: map -> m [key]
    getKeys =  getIndices
    
    -- | (.?) is monadic version of (.$).
    (.?) :: (e -> Bool) -> map -> m (Maybe key)
    (.?) =  fmap listToMaybe ... (*?)
    
    -- | (*?) is monadic version of (*$).
    (*?) :: (e -> Bool) -> map -> m [key]
    (*?) p = (select (p . snd ?+ fst) <$>) . getAssocs
    
    -- | 'kfoldrM' is right monadic fold with key.
    kfoldrM :: (key -> e -> acc -> m acc) -> acc -> map -> m acc
    kfoldrM f base = foldr ((=<<) . uncurry f) (pure base) <=< getAssocs
    
    -- | 'kfoldlM' is left monadic fold with key.
    kfoldlM :: (key -> acc -> e -> m acc) -> acc -> map -> m acc
    kfoldlM f base = foldl (flip $ \ (i, e) -> (flip (f i) e =<<)) (pure base) <=< getAssocs
    
    -- | 'kfoldrM'' is strict version of 'kfoldrM'.
    kfoldrM' :: (key -> e -> acc -> m acc) -> acc -> map -> m acc
    kfoldrM' f = kfoldrM (\ !i e !r -> f i e r)
    
    -- | 'kfoldlM'' is strict version of 'kfoldlM'.
    kfoldlM' :: (key -> acc -> e -> m acc) -> acc -> map -> m acc
    kfoldlM' f = kfoldlM (\ !i !r e -> f i r e)

--------------------------------------------------------------------------------

-- | 'MapM' contraint for @(Type -> Type)@-kind types.
type MapM1 m map key e = MapM m (map e) key e

-- | 'MapM' contraint for @(Type -> Type -> Type)@-kind types.
type MapM2 m map key e = MapM m (map key e) key e

#if __GLASGOW_HASKELL__ >= 806
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

