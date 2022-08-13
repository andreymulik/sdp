{-# LANGUAGE TypeFamilies, TypeOperators, DefaultSignatures, ConstraintKinds #-}
{-# LANGUAGE CPP, FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
{-# LANGUAGE Trustworthy #-}

{- |
    Module      :  SDP.Shape
    Copyright   :  (c) Andrey Mulik 2020-2022
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
  
  module Data.Word,
  module Data.Int,
  
  -- * Shapes
  Shape (..), GIndex, toGBounds, fromGBounds,
  
  -- ** Rank constraints
  RANK0, RANK1, RANK2,  RANK3,  RANK4,  RANK5,  RANK6,  RANK7,
  RANK8, RANK9, RANK10, RANK11, RANK12, RANK13, RANK14, RANK15
)
where

import SDP.Finite
import SDP.Tuple

import Data.Word
import Data.Int

import GHC.Types

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
class Shape i
  where
    -- | Type of index top dimension.
    type DimLast i :: Type
    type DimLast i =  i
    
    {- |
      The type of subspace of 'rank' @n - 1@, where @n@ is the 'rank' of the
      space specified by this 'SDP.Index.Index' type.
    -}
    type DimInit i :: Type
    type DimInit i =  E
    
    -- | Create index from generalized index.
    {-# INLINE fromGIndex #-}
    default fromGIndex :: RANK1 i => GIndex i -> i
    fromGIndex :: GIndex i -> i
    fromGIndex =  \ (E :& i) -> i
    
    -- | Create generalized index from index.
    {-# INLINE toGIndex #-}
    default toGIndex :: RANK1 i => i -> GIndex i
    toGIndex :: i -> GIndex i
    toGIndex =  (E :&)
    
    -- | Count of dimensions in represented space (must be finite and constant).
    {-# INLINE rank #-}
    rank :: i -> Int
    rank =  const 1
    
    -- | Add new dimension.
    {-# INLINE consDim #-}
    default consDim :: DimLast i ~~ i => DimInit i -> DimLast i -> i
    consDim :: DimInit i -> DimLast i -> i
    consDim =  const id
    
    {-# INLINE initDim #-}
    default initDim :: DimInit i ~~ E => i -> DimInit i
    initDim :: i -> DimInit i
    initDim =  const E
    
    {-# INLINE lastDim #-}
    default lastDim :: DimLast i ~~ i => i -> DimLast i
    lastDim :: i -> DimLast i
    lastDim =  id
    
    {-# INLINE unconsDim #-}
    unconsDim :: i -> (DimInit i, DimLast i)
    unconsDim =  \ i -> (initDim i, lastDim i)

--------------------------------------------------------------------------------

{- Rank constraints. -}

-- | A constraint corresponding to rank 0 indices ('E').
type RANK0  i = i ~~ E
-- | The restriction corresponding to rank indices 1 (сhecks 'GIndex').
type RANK1  i = GIndex i ~~ (E :& i)
-- | The restriction corresponding to rank indices 2 (сhecks 'GIndex').
type RANK2  i = GIndex i ~~ I2  i
-- | The restriction corresponding to rank indices 3 (сhecks 'GIndex').
type RANK3  i = GIndex i ~~ I3  i
-- | The restriction corresponding to rank indices 4 (сhecks 'GIndex').
type RANK4  i = GIndex i ~~ I4  i
-- | The restriction corresponding to rank indices 5 (сhecks 'GIndex').
type RANK5  i = GIndex i ~~ I5  i
-- | The restriction corresponding to rank indices 6 (сhecks 'GIndex').
type RANK6  i = GIndex i ~~ I6  i
-- | The restriction corresponding to rank indices 7 (сhecks 'GIndex').
type RANK7  i = GIndex i ~~ I7  i
-- | The restriction corresponding to rank indices 8 (сhecks 'GIndex').
type RANK8  i = GIndex i ~~ I8  i
-- | The restriction corresponding to rank indices 9 (сhecks 'GIndex').
type RANK9  i = GIndex i ~~ I9  i
-- | The restriction corresponding to rank indices 10 (сhecks 'GIndex').
type RANK10 i = GIndex i ~~ I10 i
-- | The restriction corresponding to rank indices 11 (сhecks 'GIndex').
type RANK11 i = GIndex i ~~ I11 i
-- | The restriction corresponding to rank indices 12 (сhecks 'GIndex').
type RANK12 i = GIndex i ~~ I12 i
-- | The restriction corresponding to rank indices 13 (сhecks 'GIndex').
type RANK13 i = GIndex i ~~ I13 i
-- | The restriction corresponding to rank indices 14 (сhecks 'GIndex').
type RANK14 i = GIndex i ~~ I14 i
-- | The restriction corresponding to rank indices 15 (сhecks 'GIndex').
type RANK15 i = GIndex i ~~ I15 i

--------------------------------------------------------------------------------

{- Basic instances. -}

instance Shape E where rank = const 0; toGIndex = id; fromGIndex = id
instance Shape ()
instance Shape Char
instance Shape Integer

instance Shape Int
instance Shape Int8
instance Shape Int16
instance Shape Int32
instance Shape Int64

instance Shape Word
instance Shape Word8
instance Shape Word16
instance Shape Word32
instance Shape Word64

--------------------------------------------------------------------------------

{- Foreign C instances. -}

instance Shape CChar
instance Shape CUChar
instance Shape CSChar
instance Shape CWchar
instance Shape CShort
instance Shape CUShort

instance Shape CInt
instance Shape CUInt
instance Shape CLong
instance Shape CLLong
instance Shape CULong
instance Shape CULLong
instance Shape CIntPtr
instance Shape CUIntPtr

instance Shape CIntMax
instance Shape CUIntMax

instance Shape CSize

#if MIN_VERSION_base(4,10,0)
-- | since @base-4.10.0.0@
instance Shape CBool
#endif

instance Shape CPtrdiff
instance Shape CSigAtomic

--------------------------------------------------------------------------------

{- N-dimensional instances. -}

instance Shape i => Shape (E :& i)
  where
    type DimInit (E :& i) = E
    type DimLast (E :& i) = i
    
    fromGIndex = id
    toGIndex   = id
    
    consDim   = (:&)
    initDim   = \ (is :& _) -> is
    lastDim   = \ (_  :& i) -> i
    unconsDim = \ (is :& i) -> (is, i)

instance (Shape i, Enum i, Bounded i, Shape (i' :& i)) => Shape (i' :& i :& i)
  where
    type DimInit (i' :& i :& i) = i' :& i
    type DimLast (i' :& i :& i) = i
    
    rank = (const . succ . rank :: Shape (i' :& i) => i' :& i -> (i' :& i :& i) -> Int) undefined
    
    fromGIndex = id
    toGIndex   = id
    
    consDim   = (:&)
    initDim   = \ (is :& _) -> is
    lastDim   = \ (_  :& i) -> i
    unconsDim = \ (is :& i) -> (is, i)

-- | Convert any index type bounds to generalized index bounds.
toGBounds :: Shape i => (i, i) -> (GIndex i, GIndex i)
toGBounds =  both toGIndex

-- | Convert generalized index bounds to any index type bounds.
fromGBounds :: Shape i => (GIndex i, GIndex i) -> (i, i)
fromGBounds =  both fromGIndex

--------------------------------------------------------------------------------

{- Tuple instances. -}

#define SHAPE_INSTANCE(Type,Last,RANK)\
instance (Shape i, Enum i, Bounded i) => Shape (Type i)\
where\
{\
type DimLast (Type i) = i;\
type DimInit (Type i) = Last i;\
rank = const RANK;\
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


