{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE Safe, CPP, ConstraintKinds #-}

#ifdef SDP_QUALIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.Forceable
    Copyright   :  (c) Andrey Mulik 2021-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Forceable" provides 'Forceable' class.
    
    @since 0.3
-}
module SDP.Forceable
(
  -- * Forceable
  Forceable (..), Forceable1, Forceable2,
  
#ifdef SDP_QUALIFIED_CONSTRAINTS
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  Forceable', Forceable'',
#endif
  
  -- * ForceableM
  ForceableM (..), ForceableM1, ForceableM2,
  
#ifdef SDP_QUALIFIED_CONSTRAINTS
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  ForceableM', ForceableM'',
#endif
)
where

default ()

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  'Forceable' is a class of types whose values can be defined as references to
  parts of other values of the same type (e.g. slice of array).
-}
class Forceable f
  where
    -- | Force the value, default: @force = id@.
    force :: f -> f
    force =  id

{- |
  @since 0.3
  
  'Forceable' version for mutable strutures.
-}
class Monad m => ForceableM m f
  where
    -- | Create copy of given structure.
    copied :: f -> m f
    copied =  return

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  'Forceable' contraint for @(Type -> Type)@-kind types.
-}
type Forceable1 f e = Forceable (f e)

{- |
  @since 0.3
  
  'Forceable' contraint for @(Type -> Type -> Type)@-kind types.
-}
type Forceable2 f i e = Forceable (f i e)

{- |
  @since 0.3
  
  'ForceableM' contraint for @(Type -> Type)@-kind types.
-}
type ForceableM1 m c e = ForceableM m (c e)

{- |
  @since 0.3
  
  'ForceableM' contraint for @(Type -> Type -> Type)@-kind types.
-}
type ForceableM2 m c i e = ForceableM m (c i e)

#ifdef SDP_QUALIFIED_CONSTRAINTS
{- |
  @since 0.3
  
  'Forceable' contraint for @(Type -> Type)@-kind types.
-}
type Forceable' f = forall e . Forceable (f e)

{- |
  @since 0.3
  
  'Forceable' contraint for @(Type -> Type -> Type)@-kind types.
-}
type Forceable'' f = forall i e . Forceable (f i e)

{- |
  @since 0.3
  
  'ForceableM' contraint for @(Type -> Type)@-kind types.
-}
type ForceableM' m c = forall e . ForceableM m (c e)

{- |
  @since 0.3
  
  'ForceableM' contraint for @(Type -> Type -> Type)@-kind types.
-}
type ForceableM'' m c = forall i e . ForceableM m (c i e)
#endif

--------------------------------------------------------------------------------

{- |
  The list doesn't imply variations in value representation and don't cause
  memory, so 'force' is just 'id'.
-}
instance Forceable [a]

instance Monad m => ForceableM m [a]




