{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Safe, CPP, ConstraintKinds, DefaultSignatures #-}

#ifdef SDP_QUALIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.Estimate
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)

    "SDP.Estimate" provides 'Estimate' and 'EstimateM' classes, type synonyms
    and some common comparators.

    This module is exported by "SDP.SafePrelude".
-}
module SDP.Estimate
(
  -- * Exports
  module Data.Functor.Classes,

  -- * Estimate
  Estimate (..), Estimate1, Estimate2, SizeHint (..),

#ifdef SDP_QUALIFIED_CONSTRAINTS
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  Estimate', Estimate'',
#endif

  -- ** Right-side Estimate functions.
  (<=.>), (<.), (>.), (<=.), (>=.), (==.), (/=.),

  -- * EstimateM
  EstimateM (..), EstimateM1, EstimateM2, restimateM,

#ifdef SDP_QUALIFIED_CONSTRAINTS
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  EstimateM', EstimateM'',
#endif

  -- ** Right-side EstimateM functions
  restimateMLT, restimateMGT,
  restimateMLE, restimateMGE,
  restimateMEQ, restimateMNE
)
where

import SDP.Internal.Utils
import SDP.Comparing
import SDP.Index

import Data.Functor.Classes

default ()

infixl 4 <==>, .<., .>., .<=., .>=., .==., ./=.,
         <.=>, .<, .>, .<=, .>=, .==, ./=,
         <=.>, <., >., <=., >=., ==., /=.

infixl 4 `lestimateMLT`, `lestimateMGT`, `lestimateMLE`, `lestimateMGE`,
         `restimateMLT`, `restimateMGT`, `restimateMLE`, `restimateMGE`,
         `lestimateMEQ`, `lestimateMNE`, `restimateMEQ`, `restimateMNE`,
         `estimateM`, `lestimateM`, `restimateM`

--------------------------------------------------------------------------------

{- |
  @since 0.3

  * @'SizeHint' l u@ - size of structure is between @l@ and @u@
  * @'SizeHintEQ' n@ - size of structure is strictly equal to @n@
  * @'SizeHintLE' l@ - size of structure is lesser or equal than @l@
  * @'SizeHintGE' u@ - size of structure is greater or equal than @u@

  Rules:

  @
  SizeHint l u <- sizeHint es === es .>= l && es .<= u
  SizeHintEQ n <- sizeHint es === es .== n
  SizeHintLE u <- sizeHint es === es .<= u
  SizeHintGE l <- sizeHint es === es .>= l
  @
-}
data SizeHint = SizeHint   {-# UNPACK #-} !Int {-# UNPACK #-} !Int
              | SizeHintEQ {-# UNPACK #-} !Int
              | SizeHintLE {-# UNPACK #-} !Int
              | SizeHintGE {-# UNPACK #-} !Int
  deriving ( Eq, Show, Read )

--------------------------------------------------------------------------------

{- |
  'Estimate' class provides the lazy comparsion structures by length.

  For some types (e.g., lists), this allows you to speed up the comparison or
  make it finite. For others (e.g., arrays), it may be convenient abbreviation.
-}
class Estimate e
  where
    {-# MINIMAL sizeOf, (<.=>), (<==>) #-}

    -- | Faster, but less precise version of 'SDP.Bordered.Bordered.sizeOf'.
    sizeHint :: e -> Maybe SizeHint
    sizeHint =  const Z

    -- | Returns actual size of structure.
    sizeOf :: e -> Int

    -- | Compare structure length with given number.
    (<.=>) :: e -> Int -> Ordering

    -- | Compare pair of structures by length.
    (<==>) :: Compare e

    -- | Compare structure length with given number.
    (.==), (./=), (.<=), (.>=), (.<), (.>) :: e -> Int -> Bool

    -- | Compare pair of structures by length.
    (.<.), (.>.), (.<=.), (.>=.), (.==.), (./=.) :: e -> e -> Bool

    e .<  i = case e <.=> i of {LT -> True; _ -> False}
    e .>  i = case e <.=> i of {GT -> True; _ -> False}
    e .<= i = case e <.=> i of {GT -> False; _ -> True}
    e .>= i = case e <.=> i of {LT -> False; _ -> True}
    e .== i = case e <.=> i of {EQ -> True; _ -> False}
    e ./= i = case e <.=> i of {EQ -> False; _ -> True}

    e1 .<.  e2 = case e1 <==> e2 of {LT -> True; _ -> False}
    e1 .>.  e2 = case e1 <==> e2 of {GT -> True; _ -> False}
    e1 .<=. e2 = case e1 <==> e2 of {GT -> False; _ -> True}
    e1 .>=. e2 = case e1 <==> e2 of {LT -> False; _ -> True}
    e1 .==. e2 = case e1 <==> e2 of {EQ -> True; _ -> False}
    e1 ./=. e2 = case e1 <==> e2 of {EQ -> False; _ -> True}

--------------------------------------------------------------------------------

{- |
  @since 0.3

  'EstimateM' class provides the lazy comparsion structures by length.

  For some types (e.g., lists), this allows you to speed up the comparison or
  make it finite. For others (e.g., arrays), it may be convenient abbreviation.
-}
class Monad m => EstimateM m e
  where
    getSizeHint :: e -> m (Maybe SizeHint)
    getSizeHint =  const $ return Z

    -- | 'getSizeOf' returns size'of mutable data structure.
    default getSizeOf :: Estimate e => e -> m Int
    getSizeOf :: e -> m Int
    getSizeOf =  return . sizeOf

    -- | Compare pair of structures by length.
    default estimateM :: Estimate e => e -> e -> m Ordering
    estimateM :: e -> e -> m Ordering
    estimateM =  return ... (<==>)

    -- | Compare structure length with given number.
    default lestimateM :: Estimate e => e -> Int -> m Ordering
    lestimateM :: e -> Int -> m Ordering
    lestimateM =  return ... (<.=>)

    -- | Compare structure length with given number.
    lestimateMLT :: e -> Int -> m Bool
    lestimateMLT e i = (== LT) <$> lestimateM e i

    -- | Compare structure length with given number.
    lestimateMGT :: e -> Int -> m Bool
    lestimateMGT e i = (== GT) <$> lestimateM e i

    -- | Compare structure length with given number.
    lestimateMLE :: e -> Int -> m Bool
    lestimateMLE e i = (/= GT) <$> lestimateM e i

    -- | Compare structure length with given number.
    lestimateMGE :: e -> Int -> m Bool
    lestimateMGE e i = (/= LT) <$> lestimateM e i

    -- | Compare structure length with given number.
    lestimateMEQ :: e -> Int -> m Bool
    lestimateMEQ e i = (== EQ) <$> lestimateM e i

    -- | Compare structure length with given number.
    lestimateMNE :: e -> Int -> m Bool
    lestimateMNE e i = (/= EQ) <$> lestimateM e i

    -- | Compare pair of structures by length.
    estimateMLT :: e -> e -> m Bool
    estimateMLT e1 e2 = (== LT) <$> estimateM e1 e2

    -- | Compare pair of structures by length.
    estimateMGT :: e -> e -> m Bool
    estimateMGT e1 e2 = (== GT) <$> estimateM e1 e2

    -- | Compare pair of structures by length.
    estimateMLE :: e -> e -> m Bool
    estimateMLE e1 e2 = (/= GT) <$> estimateM e1 e2

    -- | Compare pair of structures by length.
    estimateMGE :: e -> e -> m Bool
    estimateMGE e1 e2 = (/= LT) <$> estimateM e1 e2

    -- | Compare pair of structures by length.
    estimateMEQ :: e -> e -> m Bool
    estimateMEQ e1 e2 = (== EQ) <$> estimateM e1 e2

    -- | Compare pair of structures by length.
    estimateMNE :: e -> e -> m Bool
    estimateMNE e1 e2 = (/= EQ) <$> estimateM e1 e2

--------------------------------------------------------------------------------

-- | @(Type -> Type)@ kind 'Estimate'.
type Estimate1 rep e = Estimate (rep e)

-- | @(Type -> Type -> Type)@ kind 'Estimate'.
type Estimate2 rep i e = Estimate (rep i e)

#ifdef SDP_QUALIFIED_CONSTRAINTS
-- | 'Estimate' quantified contraint for @(Type -> Type)@-kind types.
type Estimate' rep = forall e . Estimate (rep e)

-- | 'Estimate' quantified contraint for @(Type -> Type -> Type)@-kind types.
type Estimate'' rep = forall i e . Estimate (rep i e)
#endif

--------------------------------------------------------------------------------

{- |
  @since 0.3

  @(Type -> Type)@ kind 'EstimateM'.
-}
type EstimateM1 m rep e = EstimateM m (rep e)

{- |
  @since 0.3

  @(Type -> Type -> Type)@ kind 'EstimateM'.
-}
type EstimateM2 m rep i e = EstimateM m (rep i e)

#ifdef SDP_QUALIFIED_CONSTRAINTS
{- |
  @since 0.3

  'EstimateM' quantified contraint for @(Type -> Type)@-kind types.
-}
type EstimateM' m rep = forall e . EstimateM m (rep e)

{- |
  @since 0.3

  'EstimateM' quantified contraint for @(Type -> Type -> Type)@-kind types.
-}
type EstimateM'' m rep = forall i e . EstimateM m (rep i e)
#endif

--------------------------------------------------------------------------------

-- | Compare given number with structure length.
(<=.>) :: Estimate e => Int -> e -> Ordering
i <=.> e = case e <.=> i of {LT -> GT; EQ -> EQ; GT -> LT}

-- | Compare given number with structure length.
(==.) :: Estimate e => Int -> e -> Bool
(==.) =  flip (.==)

-- | Compare given number with structure length.
(/=.) :: Estimate e => Int -> e -> Bool
(/=.) =  flip (./=)

-- | Compare given number with structure length.
(<=.) :: Estimate e => Int -> e -> Bool
(<=.) =  flip (.>=)

-- | Compare given number with structure length.
(>=.) :: Estimate e => Int -> e -> Bool
(>=.) =  flip (.<=)

-- | Compare given number with structure length.
(<.) :: Estimate e => Int -> e -> Bool
(<.)  =  flip (.>)

-- | Compare given number with structure length.
(>.) :: Estimate e => Int -> e -> Bool
(>.)  =  flip (.<)

--------------------------------------------------------------------------------

{- |
  @since 0.3

  Compare given number with structure length.
-}
restimateM :: EstimateM m e => Int -> e -> m Ordering
restimateM i e = (\ b -> case b of {LT -> GT; EQ -> EQ; GT -> LT}) <$> lestimateM e i

{- |
  @since 0.3

  Compare given number with structure length.
-}
restimateMLT :: EstimateM m e => Int -> e -> m Bool
restimateMLT =  flip lestimateMLT

{- |
  @since 0.3

  Compare given number with structure length.
-}
restimateMGT :: EstimateM m e => Int -> e -> m Bool
restimateMGT =  flip lestimateMGT

{- |
  @since 0.3

  Compare given number with structure length.
-}
restimateMLE :: EstimateM m e => Int -> e -> m Bool
restimateMLE =  flip lestimateMLE

{- |
  @since 0.3

  Compare given number with structure length.
-}
restimateMGE :: EstimateM m e => Int -> e -> m Bool
restimateMGE =  flip lestimateMGE

{- |
  @since 0.3

  Compare given number with structure length.
-}
restimateMEQ :: EstimateM m e => Int -> e -> m Bool
restimateMEQ =  flip lestimateMEQ

{- |
  @since 0.3

  Compare given number with structure length.
-}
restimateMNE :: EstimateM m e => Int -> e -> m Bool
restimateMNE =  flip lestimateMNE

--------------------------------------------------------------------------------

instance Index i => Estimate (i, i)
  where
    sizeHint = Just . SizeHintEQ . size
    sizeOf   = size

    (<==>) = on (<=>) size
    (.<=.) = on (<=)  size
    (.>=.) = on (>=)  size
    (.>.)  = on (>)   size
    (.<.)  = on (<)   size

    (<.=>) = (<=>) . size
    (.<=)  = (<=)  . size
    (.>=)  = (>=)  . size
    (.>)   = (>)   . size
    (.<)   = (<)   . size

instance (Monad m, Index i) => EstimateM m (i, i)
  where
    getSizeOf   = return . size
    getSizeHint = return . Just . SizeHintEQ . size

    estimateM   = return ... on (<=>) size
    estimateMEQ = return ... on (==)  size
    estimateMLE = return ... on (<=)  size
    estimateMGE = return ... on (>=)  size
    estimateMLT = return ... on (<)   size
    estimateMGT = return ... on (>)   size

    lestimateM   = return ... (<=>) . size
    lestimateMEQ = return ... (==)  . size
    lestimateMLE = return ... (<=)  . size
    lestimateMGE = return ... (>=)  . size
    lestimateMLT = return ... (<)   . size
    lestimateMGT = return ... (>)   . size

--------------------------------------------------------------------------------

instance Estimate [a]
  where
    sizeOf = length

    sizeHint [] = Just (SizeHintEQ 0)
    sizeHint  _ = Just (SizeHintGE 1)

    [] <==> [] = EQ
    [] <==>  _ = LT
    _  <==> [] = GT
    xs <==> ys = tail xs <==> tail ys

    [] <.=> n = 0 <=> n
    es <.=> n =
      let go xs c | c == 0 = GT | null xs = 0 <=> c | True = tail xs `go` (c - 1)
      in  if n < 0 then LT else go es n

instance Monad m => EstimateM m [a]
  where
    getSizeOf   = return . length
    getSizeHint = return . sizeHint

    estimateM  = return ... (<==>)
    lestimateM = return ... (<.=>)



