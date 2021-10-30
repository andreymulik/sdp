{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Safe, CPP, ConstraintKinds, DefaultSignatures #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.Indexed
    Copyright   :  (c) Andrey Mulik 2019-2021
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
  
  "SDP.Indexed" provides 'Indexed' and 'Freeze' classes.
-}
module SDP.Indexed
(
  -- * Exports
  module SDP.Linear,
  module SDP.Map,
  
  -- * Indexed
  Indexed (..), Indexed1, Indexed2, binaryContain,
  
  -- * Freeze
  Freeze (..), Freeze1, Freeze2,
  
#if __GLASGOW_HASKELL__ >= 806
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  Indexed', Indexed'', Freeze', Freeze''
#endif
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Linear
import SDP.Map

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

-- | 'Indexed' is class of ordered associative arrays with static bounds.
class (Linear v e, Bordered v i, Map v i e) => Indexed v i e | v -> i, v -> e
  where
    {-# MINIMAL fromIndexed #-}
    
    {- |
      @assoc bnds ascs@ create new structure from list of associations, without
      default element. Note that @bnds@ is @ascs@ bounds and may not match with
      the result bounds (not always possible).
    -}
    assoc :: (i, i) -> [(i, e)] -> v
    assoc =  flip assoc' (undEx "assoc {default}")
    
    {- |
      @assoc' bnds def ascs@ creates new structure from list of associations
      with default element. Note that @bnds@ is @ascs@ bounds and may not match
      with the result bounds (not always possible).
    -}
    assoc' :: (i, i) -> e -> [(i, e)] -> v
    assoc' bnds defvalue = toMap' defvalue . filter (inRange bnds . fst)
    
    -- | 'fromIndexed' converts this indexed structure to another one.
    fromIndexed :: (Indexed m j e) => m -> v
    
    {- |
      @'accum' f es ies@ create a new structure from @es@ elements selectively
      updated by function @f@ and @ies@ associations list.
    -}
    accum :: (e -> e' -> e) -> v -> [(i, e')] -> v
    accum f es ies = bounds es `assoc` [ (i, es!i `f` e') | (i, e') <- ies ]
    
    -- | 'imap' creates new indexed structure from old with reshaping.
    imap :: (Map m j e) => (i, i) -> m -> (i -> j) -> v
    imap bnds es f = assoc bnds [ (i, es!f i) | i <- range bnds ]

--------------------------------------------------------------------------------

-- | Service class of mutable to immutable conversions.
class (Monad m) => Freeze m v' v | v' -> m
  where
    {- |
      @freeze@ is a safe way to convert a mutable structure to a immutable.
      @freeze@ should copy the old structure or ensure that it will not be used
      after calling the procedure.
    -}
    freeze :: v' -> m v
    
    {- |
      @unsafeFreeze@ is unsafe version of 'freeze'. @unsafeFreeze@ doesn't
      guarantee that the structure will be copied or locked. It only guarantees
      that if the old structure isn't used, no error will occur.
    -}
    unsafeFreeze :: v' -> m v
    unsafeFreeze =  freeze

--------------------------------------------------------------------------------

-- | 'Indexed' contraint for @(Type -> Type)@-kind types.
type Indexed1 v i e = Indexed (v e) i e

-- | 'Indexed' contraint for @(Type -> Type -> Type)@-kind types.
type Indexed2 v i e = Indexed (v i e) i e

-- | 'Freeze' contraint for @(Type -> Type)@-kind types.
type Freeze1 m v' v e = Freeze m (v' e) (v e)

-- | 'Freeze' contraint for @(Type -> Type -> Type)@-kind types.
type Freeze2 m v' v i e = Freeze m (v' i e) (v i e)

#if __GLASGOW_HASKELL__ >= 806
-- | 'Indexed' contraint for @(Type -> Type)@-kind types.
type Indexed' v i = forall e . Indexed (v e) i e

-- | 'Indexed' contraint for @(Type -> Type -> Type)@-kind types.
type Indexed'' v = forall i e . Indexed (v i e) i e

-- | 'Freeze' contraint for @(Type -> Type)@-kind types.
type Freeze' m v' v = forall e . Freeze m (v' e) (v e)

-- | 'Freeze' contraint for @(Type -> Type -> Type)@-kind types.
type Freeze'' m v' v = forall i e . Freeze m (v' i e) (v i e)
#endif

--------------------------------------------------------------------------------

instance Indexed [e] Int e
  where
    assoc' bnds  e = toMap' e . filter (inRange bnds . fst)
    fromIndexed es = (es !) <$> indices es

--------------------------------------------------------------------------------

-- | binaryContain checks that sorted structure has equal element.
binaryContain :: (Linear v e, Bordered v i) => Compare e -> e -> v -> Bool
binaryContain _ _ Z  = False
binaryContain f e es =
  let
    contain l u = not (l > u) && case f e (es !^ j) of
        LT -> contain l (j - 1)
        EQ -> True
        GT -> contain (j + 1) u
      where
        j = u - l `div` 2 + l
  in  f e (head es) /= LT && f e (last es) /= GT && contain 0 (sizeOf es - 1)

undEx :: String -> a
undEx =  throw . UndefinedValue . showString "in SDP.Indexed."

