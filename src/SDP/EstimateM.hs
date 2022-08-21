{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Safe, CPP, ConstraintKinds #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.EstimateM
    Copyright   :  (c) Andrey Mulik 2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.EstimateM" provides 'EstimateM' class, type synonyms and some common
    comparators. This module is exported by "SDP.SafePrelude".
    
    @since 0.3
-}
module SDP.EstimateM
(
  -- * Exports
  module Data.Functor.Classes,
  
  -- * Estimate
  EstimateM (..), EstimateM1, EstimateM2, restimateM',
  
#if __GLASGOW_HASKELL__ >= 806
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  EstimateM', EstimateM'',
#endif
  
  -- ** Right-side Estimate functions.
  restimateMLT', restimateMGT',
  restimateMLE', restimateMGE',
  restimateMEQ', restimateMNE'
)
where

import Data.Functor.Classes

default ()

infixl 4 `estimateM`, `lestimateM'`, `restimateM'`

infixl 4 `lestimateMLT'`, `lestimateMGT'`, `lestimateMLE'`, `lestimateMGE'`
infixl 4 `restimateMLT'`, `restimateMGT'`, `restimateMLE'`, `restimateMGE'`
infixl 4 `lestimateMEQ'`, `lestimateMNE'`, `restimateMEQ'`, `restimateMNE'`

--------------------------------------------------------------------------------

{- |
  'EstimateM' class provides the lazy comparsion structures by length.
  
  For some types (e.g., lists), this allows you to speed up the comparison or
  make it finite. For others (e.g., arrays), it may be convenient abbreviation.
-}
class Monad m => EstimateM m e
  where
    {-# MINIMAL estimateM, lestimateM' #-}
    
    -- | Compare pair of structures by length.
    estimateM :: e -> e -> m Ordering
    
    -- | Compare structure length with given number.
    lestimateM' :: e -> Int -> m Ordering
    
    -- | Compare structure length with given number.
    lestimateMLT' :: e -> Int -> m Bool
    lestimateMLT' e i = (== LT) <$> lestimateM' e i
    
    -- | Compare structure length with given number.
    lestimateMGT' :: e -> Int -> m Bool
    lestimateMGT' e i = (== GT) <$> lestimateM' e i
    
    -- | Compare structure length with given number.
    lestimateMLE' :: e -> Int -> m Bool
    lestimateMLE' e i = (/= GT) <$> lestimateM' e i
    
    -- | Compare structure length with given number.
    lestimateMGE' :: e -> Int -> m Bool
    lestimateMGE' e i = (/= LT) <$> lestimateM' e i
    
    -- | Compare structure length with given number.
    lestimateMEQ' :: e -> Int -> m Bool
    lestimateMEQ' e i = (== EQ) <$> lestimateM' e i
    
    -- | Compare structure length with given number.
    lestimateMNE' :: e -> Int -> m Bool
    lestimateMNE' e i = (/= EQ) <$> lestimateM' e i
    
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

-- | Compare given number with structure length.
restimateM' :: EstimateM m e => Int -> e -> m Ordering
restimateM' i e = (\ b -> case b of {LT -> GT; EQ -> EQ; GT -> LT}) <$> lestimateM' e i

-- | Compare given number with structure length.
restimateMLT' :: EstimateM m e => Int -> e -> m Bool
restimateMLT' =  flip lestimateMLT'

-- | Compare given number with structure length.
restimateMGT' :: EstimateM m e => Int -> e -> m Bool
restimateMGT' =  flip lestimateMGT'

-- | Compare given number with structure length.
restimateMLE' :: EstimateM m e => Int -> e -> m Bool
restimateMLE' =  flip lestimateMLE'

-- | Compare given number with structure length.
restimateMGE' :: EstimateM m e => Int -> e -> m Bool
restimateMGE' =  flip lestimateMGE'

-- | Compare given number with structure length.
restimateMEQ' :: EstimateM m e => Int -> e -> m Bool
restimateMEQ' =  flip lestimateMEQ'

-- | Compare given number with structure length.
restimateMNE' :: EstimateM m e => Int -> e -> m Bool
restimateMNE' =  flip lestimateMNE'

--------------------------------------------------------------------------------

-- | @(Type -> Type)@ kind 'EstimateM'.
type EstimateM1 m rep e = EstimateM m (rep e)

-- | @(Type -> Type -> Type)@ kind 'EstimateM'.
type EstimateM2 m rep i e = EstimateM m (rep i e)

#if __GLASGOW_HASKELL__ >= 806
-- | 'EstimateM' quantified contraint for @(Type -> Type)@-kind types.
type EstimateM' m rep = forall e . EstimateM m (rep e)

-- | 'EstimateM' quantified contraint for @(Type -> Type -> Type)@-kind types.
type EstimateM'' m rep = forall i e . EstimateM m (rep i e)
#endif




