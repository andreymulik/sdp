{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures, ConstraintKinds #-}

{- |
    Module      :  SDP.MapM
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    @SDP.MapM@ provides 'MapM' - class of mutable dictionaries.
-}
module SDP.MapM
(
  -- * Mutable maps
  MapM (..), MapM1, MapM2,
  
  -- * IFoldM
  IFoldM (..), IFoldM1, IFoldM2
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Internal
import SDP.LinearM

default ()

infixl 5 >!, !>, !?>

--------------------------------------------------------------------------------

-- | Class for work with mutable indexed structures.
class (Monad m) => MapM m map key e | map -> m, map -> key, map -> e
  where
    {-# MINIMAL newMap', overwrite, ((>!)|(!?>)) #-}
    
    -- | Create new mutable map from list of associations.
    newMap :: [(key, e)] -> m map
    newMap =  newMap' (undEx "newMap {default}")
    
    -- | Create new mutable map from list of associations.
    newMap' :: e -> [(key, e)] -> m map
    
    -- | getAssocs returns 'assocs' of mutable data structure.
    default getAssocs :: (LinearM m map e) => map -> m [(key, e)]
    getAssocs :: map -> m [(key, e)]
    getAssocs es = liftA2 zip (getKeys es) (getLeft es)
    
    -- | (>!) is unsafe monadic reader.
    {-# INLINE (>!) #-}
    (>!) :: map -> key -> m e
    (>!) =  fmap (fromMaybe $ undEx "(!) {default}") ... (!?>)
    
    -- | (!>) is well-safe monadic reader.
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
    
    -- | (!?>) is completely safe monadic reader.
    (!?>) :: map -> key -> m (Maybe e)
    es !?> i = do b <- memberM' es i; b ? Just <$> (es >! i) $ return empty
    
    {- |
      This function designed to overwrite large enough fragments of the
      structure (unlike 'writeM' and 'writeM_')
      
      In addition to write operations, 'overwrite' can perform move and cleanup
      operations, various optimization of data presentation, depending on the
      implementation of a particular structure. Since the reference to the
      original structure may not be the same as reference to the result (which
      implementation is undesirable, but acceptable), the original reference
      (argument) shouldn't be used after 'overwrite'.
      
      All standard SDP structures support secure in-place 'overwrite'.
      
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

--------------------------------------------------------------------------------

-- | IFoldM is monadic version of IFold.
class (Monad m) => IFoldM m v i e | v -> m, v -> i, v -> e
  where
    {-# MINIMAL (ifoldrM | ofoldrM), (ifoldlM | ofoldlM) #-}
    
    -- | 'ifoldrM' is right monadic fold with index
    default ifoldrM :: (BorderedM m v i) => (i -> e -> r -> m r) -> r -> v -> m r
    ifoldrM :: (i -> e -> r -> m r) -> r -> v -> m r
    ifoldrM f base es = do bnds <- getBounds es; ofoldrM (f . index bnds) base es
    
    -- | 'ifoldlM' is left  monadic fold with index
    default ifoldlM :: (BorderedM m v i) => (i -> r -> e -> m r) -> r -> v -> m r
    ifoldlM :: (i -> r -> e -> m r) -> r -> v -> m r
    ifoldlM f base es = do bnds <- getBounds es; ofoldlM (f . index bnds) base es
    
    -- | 'ofoldrM' is right monadic fold with offset
    default ofoldrM  :: (BorderedM m v i) => (Int -> e -> r -> m r) -> r -> v -> m r
    ofoldrM  :: (Int -> e -> r -> m r) -> r -> v -> m r
    ofoldrM f base es = ifoldrM (\ i e r -> do o <- getOffsetOf es i; f o e r) base es
    
    -- | 'ofoldlM' is left  monadic fold with offset
    default ofoldlM  :: (BorderedM m v i) => (Int -> r -> e -> m r) -> r -> v -> m r
    ofoldlM  :: (Int -> r -> e -> m r) -> r -> v -> m r
    ofoldlM f base es = ifoldlM (\ i r e -> do o <- getOffsetOf es i; f o r e) base es
    
    -- | 'i_foldrM' is just 'foldrM' in 'IFoldM' context
    i_foldrM :: (e -> r -> m r) -> r -> v -> m r
    i_foldrM =  ifoldrM . const
    
    -- | 'i_foldlM' is just 'foldlM' in 'IFoldM' context
    i_foldlM :: (r -> e -> m r) -> r -> v -> m r
    i_foldlM =  ifoldlM . const

--------------------------------------------------------------------------------

-- | Rank @(* -> *)@ 'MapM'.
type MapM1 m map key e = MapM m (map e) key e

-- | Rank @(* -> * -> *)@ 'MapM'.
type MapM2 m map key e = MapM m (map key e) key e

-- | Kind @(* -> *)@ 'IFoldM'.
type IFoldM1 m v i e = IFoldM m (v e) i e

-- | Kind @(* -> * -> *)@ 'IFoldM'.
type IFoldM2 m v i e = IFoldM m (v i e) i e

--------------------------------------------------------------------------------

empEx :: String -> a
empEx =  throw . EmptyRange . showString "in SDP.MapM."

undEx :: String -> a
undEx =  throw . UndefinedValue . showString "in SDP.MapM."

overEx :: String -> a
overEx =  throw . IndexOverflow . showString "in SDP.MapM."

underEx :: String -> a
underEx =  throw . IndexUnderflow . showString "in SDP.MapM."




