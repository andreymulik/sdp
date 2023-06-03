{-# LANGUAGE Trustworthy, CPP, MagicHash, ConstraintKinds, DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds #-}

{- |
    Module      :  SDP.Shape
    Copyright   :  (c) Andrey Mulik 2020-2023
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Shape" module provides 'Shape' - class of generalized indices.
-}
module SDP.Shape
(
  -- * Exports
  module SDP.Finite,
  module SDP.Tuple,
  module SDP.Proxy,
  
  module Data.Word,
  module Data.Int,
  
  -- * Shapes
  Shape (..), GIndex, toGBounds, fromGBounds, rank, rank#,
  
  -- ** Rank constraints
  Rank0, Rank1, Rank2,  Rank3,  Rank4,  Rank5,  Rank6,  Rank7,
  Rank8, Rank9, Rank10, Rank11, Rank12, Rank13, Rank14, Rank15
)
where

import SDP.Finite
import SDP.Tuple
import SDP.Proxy

import Data.Word
import Data.Int

import GHC.TypeLits
import GHC.Types
import GHC.Exts

import Foreign.C.Types

default ()

--------------------------------------------------------------------------------

-- | Type operator 'GIndex' returns generalized equivalent of index.
type family GIndex i
  where
    GIndex     E     = E
    GIndex (i' :& i) = i' :& i
    GIndex     i     = GIndex (DimInit i) :& DimLast i

--------------------------------------------------------------------------------

{- |
  'Shape' is service class that constraints 'SDP.Index.Index'.
  
  Rules:
  
  > rank i == rank   (j `asTypeOf` i)
  > rank i == length (sizes (i, i))
  
  > rank (lastDim E) = 0
  > rank (lastDim i) = 1
  > rank (initDim E) = 0
  > rank (lastDim i) = rank i - 1
  
  > fromGIndex . toGIndex = id
  > toGIndex . fromGIndex = id
-}
class
    (
      Eq sh, Eq (GIndex sh),
      Rank sh ~ Rank (GIndex sh),
      Eq (DimInit sh), Eq (DimLast sh)
    ) => Shape sh
  where
    -- | Create index from generalized index.
    {-# INLINE fromGIndex #-}
    default fromGIndex :: GIndex sh ~ (E :& sh) => GIndex sh -> sh
    fromGIndex :: GIndex sh -> sh
    fromGIndex =  \ (E :& sh) -> sh
    
    -- | Create generalized index from index.
    {-# INLINE toGIndex #-}
    default toGIndex :: GIndex sh ~ (E :& sh) => sh -> GIndex sh
    toGIndex :: sh -> GIndex sh
    toGIndex =  (E :&)
    
    -- 'Rank' is shape dimension (number of components in shape).
    type Rank sh :: Nat
    
    {- |
      Count of dimensions in represented space (must be finite and constant).
      Returns @'Rank' i@ as an unboxed 'Int#' value.
    -}
    default rank## :: KnownNat (Rank sh) => Proxy# sh -> Int#
    rank## :: Proxy# sh -> Int#
    rank## =  go proxy#
      where
        go :: KnownNat (Rank sh) => Proxy# (Rank sh) -> Proxy# sh -> Int#
        go =  \ p# _ -> case fromEnum (natVal' p#) of I# sh# -> sh#
    
    {- Subshape index. -}
    
    -- | Type of index top dimension.
    type DimLast sh :: Type
    type DimLast sh =  sh
    
    {-# INLINE lastDim #-}
    default lastDim :: DimLast sh ~ sh => sh -> DimLast sh
    lastDim :: sh -> DimLast sh
    lastDim =  id
    
    {- Subshape. -}
    
    {- |
      The type of subspace of 'rank' @n - 1@, where @n@ is the 'rank' of the
      space specified by this 'SDP.Index.Index' type.
    -}
    type DimInit sh :: Type
    type DimInit sh =  E
    
    {-# INLINE initDim #-}
    default initDim :: DimInit sh ~ E => sh -> DimInit sh
    initDim :: sh -> DimInit sh
    initDim =  const E
    
    {- Construct/destruct shape. -}
    
    -- | Add new dimension.
    {-# INLINE consDim #-}
    default consDim :: DimLast sh ~ sh => DimInit sh -> DimLast sh -> sh
    consDim :: DimInit sh -> DimLast sh -> sh
    consDim =  const id
    
    {-# INLINE unconsDim #-}
    unconsDim :: sh -> (DimInit sh, DimLast sh)
    unconsDim =  \ sh -> (initDim sh, lastDim sh)

--------------------------------------------------------------------------------

{- Rank constraints. -}

-- | A constraint corresponding to @rank === 0@.
type Rank0 sh = Rank sh ~ 0

-- | A constraint corresponding to @rank === 1@.
type Rank1 sh = Rank sh ~ 1

-- | A constraint corresponding to @rank === 2@.
type Rank2 sh = Rank sh ~ 2

-- | A constraint corresponding to @rank === 3@.
type Rank3 sh = Rank sh ~ 3

-- | A constraint corresponding to @rank === 4@.
type Rank4 sh = Rank sh ~ 4

-- | A constraint corresponding to @rank === 5@.
type Rank5 sh = Rank sh ~ 5

-- | A constraint corresponding to @rank === 6@.
type Rank6 sh = Rank sh ~ 6

-- | A constraint corresponding to @rank === 7@.
type Rank7 sh = Rank sh ~ 7

-- | A constraint corresponding to @rank === 8@.
type Rank8 sh = Rank sh ~ 8

-- | A constraint corresponding to @rank === 9@.
type Rank9 sh = Rank sh ~ 9

-- | A constraint corresponding to @rank === 10@.
type Rank10 sh = Rank sh ~ 10

-- | A constraint corresponding to @rank === 11@.
type Rank11 sh = Rank sh ~ 11

-- | A constraint corresponding to @rank === 12@.
type Rank12 sh = Rank sh ~ 12

-- | A constraint corresponding to @rank === 13@.
type Rank13 sh = Rank sh ~ 13

-- | A constraint corresponding to @rank === 14@.
type Rank14 sh = Rank sh ~ 14

-- | A constraint corresponding to @rank === 15@.
type Rank15 sh = Rank sh ~ 15

--------------------------------------------------------------------------------

{- Basic instances. -}

instance Shape E
  where
    type Rank E = 0
    
    toGIndex   = id
    fromGIndex = id

instance Shape Int   where type Rank Int   = 1
instance Shape Int8  where type Rank Int8  = 1
instance Shape Int16 where type Rank Int16 = 1
instance Shape Int32 where type Rank Int32 = 1
instance Shape Int64 where type Rank Int64 = 1

instance Shape Word   where type Rank Word   = 1
instance Shape Word8  where type Rank Word8  = 1
instance Shape Word16 where type Rank Word16 = 1
instance Shape Word32 where type Rank Word32 = 1
instance Shape Word64 where type Rank Word64 = 1

instance Shape ()      where type Rank ()      = 1
instance Shape Char    where type Rank Char    = 1
instance Shape Integer where type Rank Integer = 1

--------------------------------------------------------------------------------

{- Foreign C instances. -}

instance Shape CChar   where type Rank CChar   = 1
instance Shape CUChar  where type Rank CUChar  = 1
instance Shape CSChar  where type Rank CSChar  = 1
instance Shape CWchar  where type Rank CWchar  = 1
instance Shape CShort  where type Rank CShort  = 1
instance Shape CUShort where type Rank CUShort = 1

instance Shape CInt     where type Rank CInt     = 1
instance Shape CUInt    where type Rank CUInt    = 1
instance Shape CLong    where type Rank CLong    = 1
instance Shape CLLong   where type Rank CLLong   = 1
instance Shape CULong   where type Rank CULong   = 1
instance Shape CULLong  where type Rank CULLong  = 1
instance Shape CIntPtr  where type Rank CIntPtr  = 1
instance Shape CUIntPtr where type Rank CUIntPtr = 1

instance Shape CIntMax  where type Rank CIntMax  = 1
instance Shape CUIntMax where type Rank CUIntMax = 1

instance Shape CSize where type Rank CSize = 1

#if MIN_VERSION_base(4,10,0)
-- | since @base-4.10.0.0@
instance Shape CBool where type Rank CBool = 1
#endif

instance Shape CPtrdiff   where type Rank CPtrdiff   = 1
instance Shape CSigAtomic where type Rank CSigAtomic = 1

--------------------------------------------------------------------------------

{- N-dimensional instances. -}

instance (Shape sh, Rank1 sh) => Shape (E :& sh)
  where
    type Rank (E :& sh) = 1
    
    type DimInit (E :& sh) = E
    type DimLast (E :& sh) = sh
    
    rank## sh = rank## (glast# sh)
      where
        glast# :: Proxy# (E :& sh) -> Proxy# sh
        glast# _ = proxy#
    
    fromGIndex = id
    toGIndex   = id
    
    consDim   = (:&)
    initDim   = \ (is :& _) -> is
    lastDim   = \ (_  :& i) -> i
    unconsDim = \ (is :& i) -> (is, i)

instance (Shape sh, Enum sh, Bounded sh, Shape (sh' :& sh), Rank1 sh)
      => Shape (sh' :& sh :& sh)
  where
    type Rank (sh' :& sh :& sh) = Rank (sh' :& sh) + 1
    
    type DimInit (sh' :& sh :& sh) = sh' :& sh
    type DimLast (sh' :& sh :& sh) = sh
    
    rank## = go proxy# proxy#
      where
        go :: (Shape sh, Shape (sh' :& sh)) => Proxy# (sh' :& sh) -> Proxy# sh
           -> Proxy# (sh' :& sh :& sh) -> Int#
        go init# last# _ = rank## init# +# rank## last#
    
    fromGIndex = id
    toGIndex   = id
    
    consDim   = (:&)
    initDim   = \ (is :& _) -> is
    lastDim   = \ (_  :& i) -> i
    unconsDim = \ (is :& i) -> (is, i)

-- | Convert any index type bounds to generalized index bounds.
toGBounds :: Shape sh => (sh, sh) -> (GIndex sh, GIndex sh)
toGBounds =  both toGIndex

-- | Convert generalized index bounds to any index type bounds.
fromGBounds :: Shape sh => (GIndex sh, GIndex sh) -> (sh, sh)
fromGBounds =  both fromGIndex

--------------------------------------------------------------------------------

{- Tuple instances. -}

#define SHAPE_INSTANCE(Type,Last,RANK)\
instance (Shape sh, Enum sh, Bounded sh) => Shape (Type sh)\
where\
{\
type Rank (Type sh) = RANK;\
type DimLast (Type sh) = sh;\
type DimInit (Type sh) = Last sh;\
rank## _ = RANK#;\
initDim = fst . unconsDim;\
lastDim = snd . unconsDim;

SHAPE_INSTANCE(T2, I1, 2)
fromGIndex = \ (E:&a:&b) -> (a,b);
consDim    = \ (E:&a) b  -> (a,b);
unconsDim  (a,b) = (E:&a,b);
toGIndex   (a,b) = E:&a:&b;
}

SHAPE_INSTANCE(T3, T2, 3)
fromGIndex = \ (E:&a:&b:&c) -> (a,b,c);
toGIndex   (a,b,c) = E:&a:&b:&c;
unconsDim  (a,b,c) = ((a,b),c);
consDim    (a,b) c = (a,b,c);
}

SHAPE_INSTANCE(T4, T3, 4)
fromGIndex = \ (E:&a:&b:&c:&d) -> (a,b,c,d);
toGIndex   (a,b,c,d) = E:&a:&b:&c:&d;
unconsDim  (a,b,c,d) = ((a,b,c),d);
consDim    (a,b,c) d = (a,b,c,d);
}

SHAPE_INSTANCE(T5, T4, 5)
fromGIndex = \ (E:&a:&b:&c:&d:&e) -> (a,b,c,d,e);
toGIndex   (a,b,c,d,e) = E:&a:&b:&c:&d:&e;
unconsDim  (a,b,c,d,e) = ((a,b,c,d),e);
consDim    (a,b,c,d) e = (a,b,c,d,e);
}

SHAPE_INSTANCE(T6, T5, 6)
fromGIndex = \ (E:&a:&b:&c:&d:&e:&f) -> (a,b,c,d,e,f);
toGIndex   (a,b,c,d,e,f) = E:&a:&b:&c:&d:&e:&f;
unconsDim  (a,b,c,d,e,f) = ((a,b,c,d,e),f);
consDim    (a,b,c,d,e) f = (a,b,c,d,e,f);
}

SHAPE_INSTANCE(T7, T6, 7)
fromGIndex = \ (E:&a:&b:&c:&d:&e:&f:&g) -> (a,b,c,d,e,f,g);
toGIndex   (a,b,c,d,e,f,g) = E:&a:&b:&c:&d:&e:&f:&g;
unconsDim  (a,b,c,d,e,f,g) = ((a,b,c,d,e,f),g);
consDim    (a,b,c,d,e,f) g = (a,b,c,d,e,f,g);
}

SHAPE_INSTANCE(T8, T7, 8)
fromGIndex = \ (E:&a:&b:&c:&d:&e:&f:&g:&h) -> (a,b,c,d,e,f,g,h);
toGIndex   (a,b,c,d,e,f,g,h) = E:&a:&b:&c:&d:&e:&f:&g:&h;
unconsDim  (a,b,c,d,e,f,g,h) = ((a,b,c,d,e,f,g),h);
consDim    (a,b,c,d,e,f,g) h = (a,b,c,d,e,f,g,h);
}

SHAPE_INSTANCE(T9, T8, 9)
fromGIndex = \ (E:&a:&b:&c:&d:&e:&f:&g:&h:&i) -> (a,b,c,d,e,f,g,h,i);
toGIndex   (a,b,c,d,e,f,g,h,i) = E:&a:&b:&c:&d:&e:&f:&g:&h:&i;
unconsDim  (a,b,c,d,e,f,g,h,i) = ((a,b,c,d,e,f,g,h),i);
consDim    (a,b,c,d,e,f,g,h) i = (a,b,c,d,e,f,g,h,i);
}

SHAPE_INSTANCE(T10, T9, 10)
fromGIndex = \ (E:&a:&b:&c:&d:&e:&f:&g:&h:&i:&j) -> (a,b,c,d,e,f,g,h,i,j);
toGIndex   (a,b,c,d,e,f,g,h,i,j) = E:&a:&b:&c:&d:&e:&f:&g:&h:&i:&j;
unconsDim  (a,b,c,d,e,f,g,h,i,j) = ((a,b,c,d,e,f,g,h,i),j);
consDim    (a,b,c,d,e,f,g,h,i) j = (a,b,c,d,e,f,g,h,i,j);
}

SHAPE_INSTANCE(T11, T10, 11)
fromGIndex = \ (E:&a:&b:&c:&d:&e:&f:&g:&h:&i:&j:&k) -> (a,b,c,d,e,f,g,h,i,j,k);
toGIndex   (a,b,c,d,e,f,g,h,i,j,k) = E:&a:&b:&c:&d:&e:&f:&g:&h:&i:&j:&k;
unconsDim  (a,b,c,d,e,f,g,h,i,j,k) = ((a,b,c,d,e,f,g,h,i,j),k);
consDim    (a,b,c,d,e,f,g,h,i,j) k = (a,b,c,d,e,f,g,h,i,j,k);
}

SHAPE_INSTANCE(T12, T11, 12)
fromGIndex = \ (E:&a:&b:&c:&d:&e:&f:&g:&h:&i:&j:&k:&l) -> (a,b,c,d,e,f,g,h,i,j,k,l);
toGIndex   (a,b,c,d,e,f,g,h,i,j,k,l) = E:&a:&b:&c:&d:&e:&f:&g:&h:&i:&j:&k:&l;
unconsDim  (a,b,c,d,e,f,g,h,i,j,k,l) = ((a,b,c,d,e,f,g,h,i,j,k),l);
consDim    (a,b,c,d,e,f,g,h,i,j,k) l = (a,b,c,d,e,f,g,h,i,j,k,l);
}

SHAPE_INSTANCE(T13, T12, 13)
fromGIndex = \ (E:&a:&b:&c:&d:&e:&f:&g:&h:&i:&j:&k:&l:&m) -> (a,b,c,d,e,f,g,h,i,j,k,l,m);
toGIndex   (a,b,c,d,e,f,g,h,i,j,k,l,m) = E:&a:&b:&c:&d:&e:&f:&g:&h:&i:&j:&k:&l:&m;
unconsDim  (a,b,c,d,e,f,g,h,i,j,k,l,m) = ((a,b,c,d,e,f,g,h,i,j,k,l),m);
consDim    (a,b,c,d,e,f,g,h,i,j,k,l) m = (a,b,c,d,e,f,g,h,i,j,k,l,m);
}

SHAPE_INSTANCE(T14, T13, 14)
fromGIndex = \ (E:&a:&b:&c:&d:&e:&f:&g:&h:&i:&j:&k:&l:&m:&n) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n);
toGIndex   (a,b,c,d,e,f,g,h,i,j,k,l,m,n) = E:&a:&b:&c:&d:&e:&f:&g:&h:&i:&j:&k:&l:&m:&n;
unconsDim  (a,b,c,d,e,f,g,h,i,j,k,l,m,n) = ((a,b,c,d,e,f,g,h,i,j,k,l,m),n);
consDim    (a,b,c,d,e,f,g,h,i,j,k,l,m) n = (a,b,c,d,e,f,g,h,i,j,k,l,m,n);
}

SHAPE_INSTANCE(T15, T14, 15)
fromGIndex = \ (E:&a:&b:&c:&d:&e:&f:&g:&h:&i:&j:&k:&l:&m:&n:&o) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o);
toGIndex   (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) = E:&a:&b:&c:&d:&e:&f:&g:&h:&i:&j:&k:&l:&m:&n:&o;
unconsDim  (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) = ((a,b,c,d,e,f,g,h,i,j,k,l,m,n),o);
consDim    (a,b,c,d,e,f,g,h,i,j,k,l,m,n) o = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o);
}
#undef SHAPE_INSTANCE

--------------------------------------------------------------------------------

-- | Boxed 'rank' of given 'Shape' type, see 'rank##'.
rank :: Shape sh => sh -> Int
rank sh = I# (rank# sh)

{- |
  @since 0.3
  
  Unboxed 'rank' of given 'Shape' type, see 'rank##'.
-}
rank# :: Shape sh => sh -> Int#
rank# sh = rank## (toProxy## sh)



