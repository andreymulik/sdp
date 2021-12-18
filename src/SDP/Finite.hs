{-# LANGUAGE TypeFamilies, TypeOperators, DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE Trustworthy, UndecidableInstances, FlexibleInstances #-}

{- |
    Module      :  SDP.Finite
    Copyright   :  (c) Andrey Mulik 2019-2021
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Finite" provide generalized finite-dimensional index type (':&') based
    on @repa@ @(:.)@.
    
    Since @sdp-0.2@, for (':&') available @OverloadedLists@-based syntactic sugar.
    For example, instead of the inconvenient @es!(ind4 0 1 2 3)@ or just awful
    @es!(E:&0:&1:&1:&2:&3)@ you can write: @es![0, 1, 2, 3]@.
    
    Note that @OverloadedLists@ instances requires a strictly defined number of
    subindexes.
-}
module SDP.Finite
(
  -- * Generalized index
  E (..), (:&) (..),
  
  -- * Type synonyms
  I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15,
  
  -- * Old constructors
  ind2,  ind3,  ind4,  ind5,  ind6,  ind7,  ind8,  ind9,
  ind10, ind11, ind12, ind13, ind14, ind15
)
where

import Prelude ( (++) )
import SDP.SafePrelude
import SDP.Nullable

import Data.Default.Class
import Data.Typeable
import Data.Data

import GHC.Generics
import GHC.Types
import GHC.Read

import qualified GHC.Exts as E
import GHC.Exts ( IsList )

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

{- Zero-dimensional type. -}

-- | Service type, that represents zero-dimensional index.
data E = E deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

instance Default E where def = E

instance IsList E
  where
    type Item E = E
    
    fromList = const E
    toList   = const []

-- | @since 0.2.1
instance Nullable E where lzero = E; isNull = const True

--------------------------------------------------------------------------------

{- N-dimensional index type. -}

{- |
  N-dimensional index type. The type (head :& tail) allows working with any
  finite dimension number.
-}
data tail :& head = !tail :& !head deriving ( Eq, Ord, Typeable, Data, Generic )

instance (Enum i) => Enum (E :& i)
  where
    fromEnum (E :& e) = fromEnum  e
    succ     (E :& e) = E :& succ e
    pred     (E :& e) = E :& pred e
    
    toEnum = (E :&) . toEnum
    
    enumFrom                (E :& f)          = (E :&) <$> [f ..]
    enumFromTo         (E :& f) (E :& l)      = (E :&) <$> [f .. l]
    enumFromThen       (E :& f)  (E :& n)     = (E :&) <$> [f, n ..]
    enumFromThenTo (E :& f) (E :& n) (E :& l) = (E :&) <$> [f, n .. l]

instance (Default d, Default d') => Default (d :& d') where def = def :& def

--------------------------------------------------------------------------------

{- Overloaded indices. -}

instance (IsList (i' :& i), E.Item (i' :& i) ~~ i, Show i) => Show (i' :& i)
  where
    showsPrec p = showsPrec p . E.toList

instance (IsList (i' :& i), E.Item (i' :& i) ~~ i, Read i) => Read (i' :& i)
  where
    readPrec = E.fromList <$> readPrec

instance IsList (E :& i)
  where
    type Item (E :& i) = i
    
    fromList [i] = E :& i
    fromList  _  = throw $ UnexpectedRank "in SDP.Finite.fromList"
    
    toList (E :& i) = [i]

instance (E.Item (i' :& i) ~~ i, IsList (i' :& i)) => IsList (i' :& i :& i)
  where
    type Item (i' :& i :& i) = i
    
    toList (i' :& i) = E.toList i' ++ [i]
    
    fromList = (uncurry $ (:&) . E.fromList) . unsnoc

--------------------------------------------------------------------------------

{- Type synonyms are declared up to 15 dimensions. -}

-- | 1-dimensional index (@(E :& i)@ without @TypeOperators@).
type I1  i = E :& i
-- | 2-dimensional index
type I2  i = E :& i  :& i
-- | 3-dimensional index
type I3  i = (I2  i) :& i
-- | 4-dimensional index
type I4  i = (I3  i) :& i
-- | 5-dimensional index
type I5  i = (I4  i) :& i
-- | 6-dimensional index
type I6  i = (I5  i) :& i
-- | 7-dimensional index
type I7  i = (I6  i) :& i
-- | 8-dimensional index
type I8  i = (I7  i) :& i
-- | 9-dimensional index
type I9  i = (I8  i) :& i
-- | 10-dimensional index
type I10 i = (I9  i) :& i
-- | 11-dimensional index
type I11 i = (I10 i) :& i
-- | 12-dimensional index
type I12 i = (I11 i) :& i
-- | 13-dimensional index
type I13 i = (I12 i) :& i
-- | 14-dimensional index
type I14 i = (I13 i) :& i
-- | 15-dimensional index
type I15 i = (I14 i) :& i

-- | 2-dimensional index constructor.
ind2  :: i -> i                                                                  -> I2  i
-- | 3-dimensional index constructor.
ind3  :: i -> i -> i                                                             -> I3  i
-- | 4-dimensional index constructor.
ind4  :: i -> i -> i -> i                                                        -> I4  i
-- | 5-dimensional index constructor.
ind5  :: i -> i -> i -> i -> i                                                   -> I5  i
-- | 6-dimensional index constructor.
ind6  :: i -> i -> i -> i -> i -> i                                              -> I6  i
-- | 7-dimensional index constructor.
ind7  :: i -> i -> i -> i -> i -> i -> i                                         -> I7  i
-- | 8-dimensional index constructor.
ind8  :: i -> i -> i -> i -> i -> i -> i -> i                                    -> I8  i
-- | 9-dimensional index constructor.
ind9  :: i -> i -> i -> i -> i -> i -> i -> i -> i                               -> I9  i
-- | 10-dimensional index constructor.
ind10 :: i -> i -> i -> i -> i -> i -> i -> i -> i -> i                          -> I10 i
-- | 11-dimensional index constructor.
ind11 :: i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i                     -> I11 i
-- | 12-dimensional index constructor.
ind12 :: i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i                -> I12 i
-- | 13-dimensional index constructor.
ind13 :: i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i           -> I13 i
-- | 14-dimensional index constructor.
ind14 :: i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i      -> I14 i
-- | 15-dimensional index constructor.
ind15 :: i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> I15 i

ind2  a b                           = E:&a:&b
ind3  a b c                         = E:&a:&b:&c
ind4  a b c d                       = E:&a:&b:&c:&d
ind5  a b c d e                     = E:&a:&b:&c:&d:&e
ind6  a b c d e f                   = E:&a:&b:&c:&d:&e:&f
ind7  a b c d e f g                 = E:&a:&b:&c:&d:&e:&f:&g
ind8  a b c d e f g h               = E:&a:&b:&c:&d:&e:&f:&g:&h
ind9  a b c d e f g h i             = E:&a:&b:&c:&d:&e:&f:&g:&h:&i
ind10 a b c d e f g h i j           = E:&a:&b:&c:&d:&e:&f:&g:&h:&i:&j
ind11 a b c d e f g h i j k         = E:&a:&b:&c:&d:&e:&f:&g:&h:&i:&j:&k
ind12 a b c d e f g h i j k l       = E:&a:&b:&c:&d:&e:&f:&g:&h:&i:&j:&k:&l
ind13 a b c d e f g h i j k l m     = E:&a:&b:&c:&d:&e:&f:&g:&h:&i:&j:&k:&l:&m
ind14 a b c d e f g h i j k l m n   = E:&a:&b:&c:&d:&e:&f:&g:&h:&i:&j:&k:&l:&m:&n
ind15 a b c d e f g h i j k l m n o = E:&a:&b:&c:&d:&e:&f:&g:&h:&i:&j:&k:&l:&m:&n:&o

unsnoc :: [i] -> ([i], i)
unsnoc    [i]   = ([], i)
unsnoc (i : is) = (i :) `first` unsnoc is
unsnoc     _    = throw $ UnexpectedRank "in SDP.Finite.fromList"

