{-# LANGUAGE Safe #-}

{- |
    Module      :  SDP.Tuple
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Tuple" provides tuple type synonyms.
-}
module SDP.Tuple
(
  -- * Tuple synonyms
  T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15,
  
  -- * Related combinators
  fsts, snds, both, swap
)
where

import Data.Function ( on )
import Data.Tuple ( swap )

default ()

--------------------------------------------------------------------------------

-- | Return all first elements in pairs.
fsts :: Functor f => f (a, b) -> f a
fsts =  fmap fst

-- | Return all first elements in pairs.
snds :: Functor f => f (a, b) -> f b
snds =  fmap snd

-- | Applies function to both elements of pair.
both :: (a -> b) -> (a, a) -> (b, b)
both =  uncurry . on (,)

--------------------------------------------------------------------------------

-- | Tuple synonym
type T2  i = (i,i)

-- | Tuple synonym
type T3  i = (i,i,i)

-- | Tuple synonym
type T4  i = (i,i,i,i)

-- | Tuple synonym
type T5  i = (i,i,i,i,i)

-- | Tuple synonym
type T6  i = (i,i,i,i,i,i)

-- | Tuple synonym
type T7  i = (i,i,i,i,i,i,i)

-- | Tuple synonym
type T8  i = (i,i,i,i,i,i,i,i)

-- | Tuple synonym
type T9  i = (i,i,i,i,i,i,i,i,i)

-- | Tuple synonym
type T10 i = (i,i,i,i,i,i,i,i,i,i)

-- | Tuple synonym
type T11 i = (i,i,i,i,i,i,i,i,i,i,i)

-- | Tuple synonym
type T12 i = (i,i,i,i,i,i,i,i,i,i,i,i)

-- | Tuple synonym
type T13 i = (i,i,i,i,i,i,i,i,i,i,i,i,i)

-- | Tuple synonym
type T14 i = (i,i,i,i,i,i,i,i,i,i,i,i,i,i)

-- | Tuple synonym
type T15 i = (i,i,i,i,i,i,i,i,i,i,i,i,i,i,i)


