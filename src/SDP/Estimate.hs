{-# LANGUAGE Safe, CPP, ConstraintKinds, FlexibleInstances #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.Estimate
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Estimate" provides 'Estimate' class, type synonyms and some common
    comparators. This module is exported by "SDP.SafePrelude".
-}
module SDP.Estimate
(
  -- * Exports
  module Data.Functor.Classes,
  
  -- * Estimate
  Estimate (..), Estimate1, Estimate2,
  
#if __GLASGOW_HASKELL__ >= 806
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  Estimate', Estimate'',
#endif
  
  -- ** Right-side Estimate functions.
  (<=.>), (<.), (>.), (<=.), (>=.), (==.), (/=.)
)
where

import Data.Functor.Classes
import Data.Function

import SDP.Comparing
import SDP.Index

default ()

infixl 4 <==>, .<., .>., .<=., .>=., .==., ./=.

infixl 4 <.=>, .<, .>, .<=, .>=, .==, ./=
infixl 4 <=.>, <., >., <=., >=., ==., /=.

--------------------------------------------------------------------------------

{- |
  'Estimate' class provides the lazy comparsion structures by length.
  
  For some types (e.g., lists), this allows you to speed up the comparison or
  make it finite. For others (e.g., arrays), it may be convenient abbreviation.
-}
class Estimate e
  where
    {-# MINIMAL (<.=>), (<==>) #-}
    
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

-- | Compare given number with structure length.
(<=.>) :: (Estimate e) => Int -> e -> Ordering
i <=.> e = case e <.=> i of {LT -> GT; EQ -> EQ; GT -> LT}

-- | Compare given number with structure length.
(==.), (/=.), (<=.), (>=.), (<.), (>.) :: (Estimate e) => Int -> e -> Bool

(==.) = flip (.==)
(/=.) = flip (./=)
(<=.) = flip (.>=)
(>=.) = flip (.<=)
(<.)  = flip (.>)
(>.)  = flip (.<)

--------------------------------------------------------------------------------

-- | @(Type -> Type)@ kind 'Estimate'.
type Estimate1 rep e = Estimate (rep e)

-- | @(Type -> Type -> Type)@ kind 'Estimate'.
type Estimate2 rep i e = Estimate (rep i e)

#if __GLASGOW_HASKELL__ >= 806
-- | 'Estimate' quantified contraint for @(Type -> Type)@-kind types.
type Estimate' rep = forall e . Estimate (rep e)

-- | 'Estimate' quantified contraint for @(Type -> Type -> Type)@-kind types.
type Estimate'' rep = forall i e . Estimate (rep i e)
#endif

--------------------------------------------------------------------------------

instance Index i => Estimate (i, i)
  where
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

instance Estimate [a]
  where
    [] <==> [] = EQ
    [] <==>  _ = LT
    _  <==> [] = GT
    xs <==> ys = tail xs <==> tail ys
    
    [] <.=> n = 0 <=> n
    es <.=> n =
      let go xs c | c == 0 = GT | null xs = 0 <=> c | True = tail xs `go` (c - 1)
      in  if n < 0 then LT else go es n


