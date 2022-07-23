{-# LANGUAGE Safe, CPP, ConstraintKinds, MultiParamTypeClasses #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.Copyable
    Copyright   :  (c) Andrey Mulik 2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Copyable" provides 'Copyable' class.
    
    @since 0.3
-}
module SDP.Copyable
(
  -- * Copyable
  Copyable (..), Copyable1, Copyable2,
  
#if __GLASGOW_HASKELL__ >= 806
  Copyable', Copyable'',
#endif
)
where

default ()

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  'SDP.Forceable.Forceable' version for mutable strutures.
-}
class Monad m => Copyable m c
  where
    -- | Create copy of given structure.
    copied :: c -> m c

--------------------------------------------------------------------------------

-- | @since 0.3 'Copyable' contraint for @(Type -> Type)@-kind types.
type Copyable1 m c e = Copyable m (c e)

-- | @since 0.3 'Copyable' contraint for @(Type -> Type -> Type)@-kind types.
type Copyable2 m c i e = Copyable m (c i e)

#if __GLASGOW_HASKELL__ >= 806
-- | @since 0.3 'Copyable' contraint for @(Type -> Type@-kind types.
type Copyable' m c = forall e . Copyable m (c e)

-- | @since 0.3 'Copyable' contraint for @(Type -> Type -> Type)@-kind types.
type Copyable'' m c = forall i e . Copyable m (c i e)
#endif


