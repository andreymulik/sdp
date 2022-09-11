{-# LANGUAGE Safe, CPP, MultiParamTypeClasses, ConstraintKinds #-}

#ifdef SDP_QUALIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.NullableM
    Copyright   :  (c) Andrey Mulik 2021-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.NullableM" provides 'NullableM' - class of types with empty values.
-}
module SDP.NullableM
(
  -- * Monadic nullable
  NullableM (..), NullableM1, NullableM2,
  
#ifdef SDP_QUALIFIED_CONSTRAINTS
  NullableM', NullableM''
#endif
)
where

default ()

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  'NullableM' is class of types which value may be empty.
-}
class Monad m => NullableM m e
  where
    -- | Monadic 'SDP.Nullable.lzero'.
    newNull :: m e
    
    -- | Monadic 'SDP.Nullable.isNull'.
    nowNull :: e -> m Bool

{- |
  @since 0.3
  
  'NullableM' contraint for @(Type -> Type)@-kind types.
-}
type NullableM1 m rep e = NullableM m (rep e)

{- |
  @since 0.3
  
  'NullableM' contraint for @(Type -> Type -> Type)@-kind types.
-}
type NullableM2 m rep i e = NullableM m (rep i e)

#ifdef SDP_QUALIFIED_CONSTRAINTS

{- |
  @since 0.3
  
  'NullableM' contraint for @(Type -> Type)@-kind types.
  Only for GHC >= 8.6.1
-}
type NullableM' m rep = forall e . NullableM m (rep e)

{- |
  @since 0.3
  
  'NullableM' contraint for @(Type -> Type -> Type)@-kind types.
  Only for GHC >= 8.6.1
-}
type NullableM'' m rep = forall i e . NullableM m (rep i e)

#endif




