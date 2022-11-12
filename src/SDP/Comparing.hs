{-# LANGUAGE Safe #-}

{- |
    Module      :  SDP.Comparing
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Comparing" provide common comparators and combinators.
-}
module SDP.Comparing
(
  -- * Exports
  module Data.Ord,
  module Data.Eq,
  
  -- * Type synonyms
  Equal, Compare,
  
  -- * Common comparators
  (<=>), eqfst, eqsnd, cmpfst, cmpsnd, invertcmp
)
where

import Data.Function
import Data.Ord
import Data.Eq

default ()

infixl 4 <=>

--------------------------------------------------------------------------------

{- Type synonyms. -}

-- | 'Equal' is just synonym of @(e -> e -> Bool)@.
type Equal e = e -> e -> Bool

-- | 'Compare' is just synonym of @(e -> e -> Ordering)@.
type Compare e = e -> e -> Ordering

--------------------------------------------------------------------------------

-- | Spaceship operator - infix version of compare.
(<=>) :: Ord o => Compare o
(<=>) = compare

-- | Compare tuples by first elements.
eqfst :: Eq e => Equal (e, s)
eqfst =  (==) `on` fst

-- | Compare tuples by second elements.
eqsnd :: Eq e => Equal (f, e)
eqsnd =  (==) `on` snd

-- | Compare tuples by first elements.
cmpfst :: Ord o => Compare (o, s)
cmpfst =  comparing fst

-- | Compare tuples by second elements.
cmpsnd :: Ord o => Compare (f, o)
cmpsnd =  comparing snd

-- | Common compare combinator
invertcmp :: Compare e -> Compare e
invertcmp =  flip


