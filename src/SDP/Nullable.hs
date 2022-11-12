{-# LANGUAGE CPP, MagicHash, PatternSynonyms, ViewPatterns, DefaultSignatures #-}
{-# LANGUAGE Trustworthy, MultiParamTypeClasses, ConstraintKinds #-}

#ifdef SDP_QUALIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.Nullable
    Copyright   :  (c) Andrey Mulik 2020-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Nullable" provides 'Nullable' - class of types with empty values.
-}
module SDP.Nullable
(
  -- * Nullable
  Nullable (..), Nullable1, Nullable2,
  
#ifdef SDP_QUALIFIED_CONSTRAINTS
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  Nullable', Nullable'',
#endif
  
  -- * Monadic Nullable
  NullableM (..), NullableM1, NullableM2,
  
#ifdef SDP_QUALIFIED_CONSTRAINTS
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  NullableM', NullableM'',
#endif
  
  -- ** Patterns
  pattern NULL, pattern Z
)
where

import Foreign.Ptr

import GHC.ForeignPtr
import GHC.Stable
import GHC.Base
import GHC.Exts

#if !MIN_VERSION_base(4,15,0)
import Control.Exception.SDP
#endif

default ()

--------------------------------------------------------------------------------

{- |
  'Nullable' is class of types which have empty values.
  
  Nullable instances must follow some rules:
  
  @
    isNull Z === True
    x == Z ==> isNull x == True
    x == y === isNull x == isNull y
    
    -- For 'Foldable' instances
    toList Z === []
    fold   Z === mempty
    isNull x === null x
    isNull x === length x == 0
    
    sum      Z === 0
    product  Z === 1
    elem   x Z === False
    foldr  f Z === foldl  f Z === id
    foldr1 f Z === foldl1 f Z === undefined
  @
-}
class Nullable e
  where
    -- | Empty value.
    default lzero :: Monoid e => e
    lzero :: e
    lzero =  mempty
    
    -- | Is value empty?
    default isNull :: Eq e => e -> Bool
    isNull :: e -> Bool
    isNull =  (== lzero)

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

--------------------------------------------------------------------------------

{-# COMPLETE NULL, Just #-}
{-# COMPLETE Z,    Just #-}

-- | Originally defined in @sdp-ctypes@ (now @sdp-foreign@), same as @Z@ now.
pattern NULL :: Nullable e => e
pattern NULL <- (isNull -> True) where NULL = lzero

{- |
  Other empty value pattern: @Z === NULL@.
  
  Defined in "SDP.Nullable" since @sdp-0.2.1@, earlier - in "SDP.Linear".
-}
pattern Z :: Nullable e => e
pattern Z =  NULL

--------------------------------------------------------------------------------

{- |
  @since 0.2.1
  'Nullable' contraint for @(Type -> Type)@-kind types.
-}
type Nullable1 rep e = Nullable (rep e)

{- |
  @since 0.2.1
  'Nullable' contraint for @(Type -> Type -> Type)@-kind types.
-}
type Nullable2 rep i e = Nullable (rep i e)

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

--------------------------------------------------------------------------------

#ifdef SDP_QUALIFIED_CONSTRAINTS
{- |
  @since 0.2.1
  'Nullable' contraint for @(Type -> Type)@-kind types.
-}
type Nullable' rep = forall e . Nullable (rep e)

{- |
  @since 0.2.1
  'Nullable' contraint for @(Type -> Type -> Type)@-kind types.
-}
type Nullable'' rep = forall i e . Nullable (rep i e)

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

--------------------------------------------------------------------------------

instance Nullable (Maybe e)
  where
    isNull = \ mx -> case mx of {Nothing -> True; _ -> False}
    lzero  = Nothing

{-# COMPLETE Z,    (:) #-}
{-# COMPLETE NULL, (:) #-}

instance Nullable [e] where isNull = null

instance Nullable (Ptr e) where lzero = nullPtr

-- Stolen from @bytestring@ package.
instance Nullable (ForeignPtr e)
  where
#if MIN_VERSION_base(4,15,0)
    lzero = ForeignPtr nullAddr# FinalPtr
#else
    lzero =
      let err = throw $ UnreachableException "in SDP.Nullable.lzero :: ForeignPtr e"
      in  ForeignPtr nullAddr# err
#endif
    
    isNull (ForeignPtr addr# _) = Ptr addr# == nullPtr

instance Nullable (StablePtr e) where lzero = StablePtr (unsafeCoerce# 0#)

-- | @since 0.2.1
instance Nullable (FunPtr e) where lzero = nullFunPtr

