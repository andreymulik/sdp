{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, UndecidableInstances, DefaultSignatures, CPP #-}
{-# LANGUAGE Trustworthy, ConstraintKinds, TypeOperators, MagicHash #-}

{- |
    Module      :  SDP.Index.Class
    Copyright   :  (c) Andrey Mulik 2019-2023
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)

    The 'Index' class is an alternative to 'Data.Ix.Ix' with a richer interface,
    generalized indexes and more convenient function names.
-}
module SDP.Index.Class
(
  -- * Exports
  module SDP.Shape,

  -- * Shapes
  (:|:), SubIndex (..), splitDim,

  -- * Indices
  Index (..), SizesOf,

  -- ** Helpers
  InBounds (..), offsetIntegral, defaultBoundsUnsign
)
where

import SDP.Internal.Utils
import SDP.Shape

import Data.Maybe
import Data.Char ( ord )

import GHC.Exts

import Foreign.C.Types

import Control.Exception.SDP
import Control.Applicative

default ()

--------------------------------------------------------------------------------

-- | InBounds - service type that specifies index and bounds status.
data InBounds = ER -- ^ Empty range
              | UR -- ^ Underflow range
              | IN -- ^ Index in range
              | OR -- ^ Overflow range
  deriving ( Eq, Show, Read, Enum )

--------------------------------------------------------------------------------

{- |
  @since 0.3

  'SizesOf' is type of @(':&')@-based list of sizes. @Rank i = Rank (SizesOf i)@.
-}
type family SizesOf i
  where
    SizesOf     E     = E
    SizesOf (i' :& i) = SizesOf i' :& Int
    SizesOf     i     = SizesOf (GIndex i)

--------------------------------------------------------------------------------

-- | @(':|:')@ is closed type family of shape differences.
type family i :|: j
  where
    i :|: E = i
    i :|: j = DimInit i :|: DimInit j

--------------------------------------------------------------------------------

-- Internal class of shape differences.
class (Index i, Index j, Index (i :|: j)) => SubIndex i j
  where
    {- |
      Drop some dimensions (second argument used as type variable).

      @
      dropDim ([1,2,3,4] :: I4 Int)    ([] :: E)      === [1,2,3,4]
      dropDim ([1,2,3,4] :: I4 Int) ([1,2] :: I2 Int) ===   [3,4]@
    -}
    dropDim :: i -> j -> i :|: j

    {- |
      Join some dimensions.

      @
      joinDim ([1,2] :: I2 Int)  [3]  ===  [1,2,3]  :: I3 Int
      joinDim ([1,2] :: I2 Int) [3,4] === [1,2,3,4] :: I4 Int@
    -}
    joinDim :: j -> i :|: j -> i

    {- |
      Take some dimensions.

      @
      takeDim ([1,2,3,4] :: I4 Int) === [1] :: I1 Int
      takeDim ([1,2,3,4] :: I4 Int) === E@
    -}
    takeDim :: i -> j

instance {-# OVERLAPS #-} (Index i, E ~ (i :|: i)) => SubIndex i i
  where
    dropDim = \ _ _ -> E
    joinDim = const
    takeDim = id

instance
  (
    ij ~ (i :|: j), DimInit ij ~ (DimInit i :|: j), DimLast ij ~ DimLast i,
    Index i, Index j, Index ij, SubIndex (DimInit i) j
  ) => SubIndex i j
  where
    dropDim i' j' = let (is, i) = unconsDim i' in consDim (dropDim is j') i
    joinDim j' ij = let (is, i) = unconsDim ij in consDim (joinDim j' is) i
    takeDim = takeDim . initDim

-- | 'splitDim' returns pair of shape difference and subshape.
splitDim :: SubIndex i j => i -> (i :|: j, j)
splitDim i = let j = takeDim i in (dropDim i j, j)

--------------------------------------------------------------------------------

{- |
  Index is service class based on @base@ Ix and @repa@ Shape.

  Basic rules:

  @
  size bnds >= 0

  size bnds === product (sizes bnds)

  isEmpty bnds === (size bnds == 0)
  isEmpty bnds === inRange bnds (safeElem bnds i)

  isEmpty bnds => isOverflow  bnds i
  isEmpty bnds => isUnderflow bnds i

  inRange bnds i =/= isEmpty     bnds
  inRange bnds i =/= isOverflow  bnds i
  inRange bnds i =/= isUnderflow bnds i
  inRange bnds i === (safeElem bnds i == i)
  @

  Note:

  * 'E' is (and should remain) the one and only one index of @rank 0@.
  * 'Index' is a generalization of 'Enum', so all @rank 1@ indices must satisfy
  'Enum' laws.
  * The cardinality of the set of permissible values for indices mustn't
  exceed 1 (cardinality of a series of natural numbers), so 'Fractional' types
  cannot be indices.
-}
class
  (
    Shape (DimLast i), Shape (DimInit i), Shape (GIndex i),
    SizesOf (GIndex i) ~ SizesOf i, Ord i, Shape i, Show i
  ) => Index i
  where
    -- | Returns the size of range.
    {-# INLINE size #-}
    size :: (i, i) -> Int
    default size :: Enum i => (i, i) -> Int
    size bnds@(l, u) = isEmpty bnds ? 0 $ u -. l + 1

    -- | Returns the sizes of range dimensionwise.
    {-# INLINE sizes #-}
    sizes :: (i, i) -> SizesOf i
    sizes =  sizes . toGBounds
    default sizes :: Index (GIndex i) => (i, i) -> SizesOf i

    -- | Returns the index belonging to the given range.
    {-# INLINE safeElem #-}
    safeElem :: (i, i) -> i -> i
    safeElem (l, u) = min u . max l

    -- | Returns bounds of nonempty range (swaps bounds in each empty subshape).
    {-# INLINE ordBounds #-}
    ordBounds :: (i, i) -> (i, i)
    ordBounds bs = isEmpty bs ? swap bs $ bs
    default ordBounds :: GIndex i ~ I1 i => (i, i) -> (i, i)

    -- | Returns size of biggest range, that may be represented by this type.
    defLimit :: i -> Maybe Integer
    defLimit i = Just (1 + toInteger (asTypeOf maxBound i))
    default defLimit :: (Integral i, Bounded i) => i -> Maybe Integer

    -- | Returns default range by size.
    {-# INLINE defaultBounds #-}
    defaultBounds :: Int -> (i, i)
    defaultBounds n = both unsafeIndex (0, max 0 n - 1)

    {- |
      @since 0.3

      Boundaries of the minimum region to which the given set of indices belongs.
    -}
    rangeBounds :: Foldable f => f i -> (i, i)
    rangeBounds =  foldr extendBounds (defaultBounds 0)

    {- |
      @since 0.3

      Returns the bounds of the minimum region that includes the given index and
      region.
    -}
    extendBounds :: i -> (i, i) -> (i, i)
    extendBounds i bnds@(l, u) = case inBounds bnds i of
      ER -> (i, i)
      OR -> (l, i)
      UR -> (i, u)
      IN -> bnds

    -- | Returns index by offset in default range.
    {-# INLINE unsafeIndex #-}
    unsafeIndex :: Int -> i
    unsafeIndex =  toEnum
    default unsafeIndex :: Enum i => Int -> i

    -- | Variant of 'unsafeIndex' for 'Integer's.
    {-# INLINE unsafeIndex' #-}
    unsafeIndex' :: Integer -> i
    unsafeIndex' =  unsafeIndex . fromInteger

    -- | Checks if the bounds is empty.
    {-# INLINE isEmpty #-}
    isEmpty :: (i, i) -> Bool
    isEmpty =  uncurry (>)

    -- | Checks the index status in bounds.
    inBounds :: (i, i) -> i -> InBounds
    inBounds (l, u) i | l > u = ER | i > u = OR | i < l = UR | True = IN

    -- | Checks if the index is overflow.
    {-# INLINE isOverflow  #-}
    isOverflow :: (i, i) -> i -> Bool
    isOverflow (l, u) i = i > u || l > u

    -- | Checks if the index is underflow.
    {-# INLINE isUnderflow #-}
    isUnderflow :: (i, i) -> i -> Bool
    isUnderflow (l, u) i = i < l || l > u

    -- | Checks if the index is in 'range'.
    {-# INLINE inRange #-}
    inRange :: (i, i) -> i -> Bool
    inRange (l, u) i = l <= i && i <= u

    -- | Returns previous index in 'range'.
    prev :: (i, i) -> i -> i
    default prev :: Enum i => (i, i) -> i -> i
    prev (l, u) i
      | isEmpty (l, u) = emptyEx "prev: range must be non-empty {default}"
      |     i <= l     = l
      |     i >  u     = u
      |      True      = pred i

    -- | Returns next index in range.
    next :: (i, i) -> i -> i
    default next :: Enum i => (i, i) -> i -> i
    next (l, u) i
      | isEmpty (l, u) = emptyEx "next: range must be non-empty {default}"
      |     i >= u     = u
      |     i <  l     = l
      |      True      = succ i

    -- | Returns 'offset' (indent) of 'index' in 'range'.
    {-# INLINE offset #-}
    offset :: (i, i) -> i -> Int
    default offset :: Enum i => (i, i) -> i -> Int
    offset bnds@(l, _) i = checkBounds bnds i (i -. l) "offset {default}"

    -- | Returns 'index' by this 'offset' (indent) in 'range'.
    {-# INLINE index #-}
    index :: (i, i) -> Int -> i
    default index :: Enum i => (i, i) -> Int -> i
    index bnds@(l, _) n =
      let res = toEnum (n + fromEnum l)
      in  checkBounds (0, size bnds - 1) n res "index {default}"

    -- | Returns the ordered list of indices in this range.
    {-# INLINE range #-}
    range :: (i, i) -> [i]
    range =  uncurry enumFromTo
    default range :: Enum i => (i, i) -> [i]

    {- |
      @subshape bnds ij@ returns subshape of @bnds@.

      Checks if @ij@ in @bnds@ subshape, may 'throw' 'IndexException'.
    -}
    subshape :: SubIndex i j => (i, i) -> i :|: j -> (j, j)
    subshape (l, u) ij = checkBounds (l', u') ij (lj, uj) "subshape {default}"
      where
        (l', lj) = splitDim l
        (u', uj) = splitDim u

    slice :: SubIndex i j => (i, i) -> i :|: j -> ((i :|: j, i :|: j), (j, j))
    slice (l, u) ij = checkBounds (ls, us) ij ((ls, us), (lj, uj)) "slice {default}"
      where
        (ls, lj) = splitDim l
        (us, uj) = splitDim u

--------------------------------------------------------------------------------

{- Basic instances. -}

instance Index E
  where
    unsafeIndex' = const (emptyEx "unsafeIndex {E}")
    unsafeIndex  = const (emptyEx "unsafeIndex {E}")
    defLimit     = Just . const (-1)

    size  = const 0
    sizes = const E
    range = const []

    offset _ _ = emptyEx "offset {E}"
    index  _ _ = emptyEx "index {E}"
    ordBounds  = id
    next   _ _ = E
    prev   _ _ = E

    inRange     _ _ = False
    isUnderflow _ _ = True
    isOverflow  _ _ = True
    isEmpty       _ = True
    inBounds    _ _ = ER

    extendBounds = const id
    rangeBounds  = const (E, E)

instance Index ()
  where
    size  = const 1
    sizes = const (E :& 1)
    range = const [()]

    defLimit = Just . const 0
    next _ _ = ()
    prev _ _ = ()

    inBounds    _ _ = IN
    inRange     _ _ = True
    isEmpty       _ = False
    isOverflow  _ _ = False
    isUnderflow _ _ = False

    defaultBounds = const ((), ())
    index         = const unsafeIndex
    offset  _  _  = 0

    unsafeIndex 0 = ()
    unsafeIndex _ = emptyEx "unsafeIndex ()"

    unsafeIndex' 0 = ()
    unsafeIndex' _ = emptyEx "unsafeIndex' ()"

    extendBounds = const id
    rangeBounds  = const ((), ())

instance Index Char
  where
    defaultBounds = defaultBoundsUnsign
    defLimit = const . Just $ toInteger (ord maxBound)

{- |
  Note that 'Integer' isn't 'Bounded', so it can't be used in multidimensional
  indices.
-}
instance Index Integer
  where
    defLimit = const Nothing
    offset   = offsetIntegral

instance Index Int     where offset = offsetIntegral
instance Index Int8    where offset = offsetIntegral
instance Index Int16   where offset = offsetIntegral
instance Index Int32   where offset = offsetIntegral
instance Index Int64   where offset = offsetIntegral

instance Index Word    where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign
instance Index Word8   where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign
instance Index Word16  where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign
instance Index Word32  where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign
instance Index Word64  where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign

--------------------------------------------------------------------------------

{- Foreign C instances. -}

instance Index CChar      where offset = offsetIntegral
instance Index CSChar     where offset = offsetIntegral
instance Index CWchar     where offset = offsetIntegral
instance Index CShort     where offset = offsetIntegral

instance Index CInt       where offset = offsetIntegral
instance Index CLong      where offset = offsetIntegral
instance Index CLLong     where offset = offsetIntegral
instance Index CIntPtr    where offset = offsetIntegral
instance Index CIntMax    where offset = offsetIntegral
instance Index CPtrdiff   where offset = offsetIntegral
instance Index CSigAtomic where offset = offsetIntegral

#if MIN_VERSION_base(4,10,0)
-- | @since base-4.10.0.0
instance Index CBool where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign
#endif

instance Index CSize      where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign
instance Index CUChar     where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign
instance Index CUShort    where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign

instance Index CUInt      where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign
instance Index CULong     where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign
instance Index CULLong    where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign
instance Index CUIntPtr   where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign
instance Index CUIntMax   where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign

--------------------------------------------------------------------------------

{- N-dimensional index instances. -}

instance (Index i, Rank1 i) => Index (E :& i)
  where
    defLimit = go proxy#
      where
        go :: Index i => Proxy# i -> (E :& i) -> Maybe Integer
        go e _ = defLimit (fromProxy## e)

    size  (E:&l, E:&u) = size (l, u)
    sizes (E:&l, E:&u) = E :& size (l, u)
    range (E:&l, E:&u) = (E :&) <$> range (l, u)

    next = \ (E:&l, E:&u) (E:&i) -> E :& next (l, u) i
    prev = \ (E:&l, E:&u) (E:&i) -> E :& prev (l, u) i

    inRange     = \ (E:&l, E:&u) (E:&i) -> inRange     (l, u) i
    isOverflow  = \ (E:&l, E:&u) (E:&i) -> isOverflow  (l, u) i
    isUnderflow = \ (E:&l, E:&u) (E:&i) -> isUnderflow (l, u) i
    safeElem    = \ (E:&l, E:&u) (E:&i) -> E :& safeElem (l, u) i

    isEmpty   (E:&l, E:&u) = isEmpty (l, u)
    ordBounds (E:&l, E:&u) = let (l', u') = ordBounds (l, u) in (E:&l', E:&u')

    offset = \ (E:&l, E:&u) (E:&i) -> offset (l, u) i
    index  = \ (E:&l, E:&u) -> (E:&) . index (l, u)

    defaultBounds = both (E :&) . defaultBounds
    unsafeIndex'  = (E :&) . unsafeIndex'
    unsafeIndex   = (E :&) . unsafeIndex

    extendBounds (E:&i) (E:&l, E:&u) = both (E:&) $ extendBounds i (l, u)

-- [internal]: undecidable
instance (Index i, Enum i, Bounded i, Index (i' :& i), Show (i' :& i :& i), Rank1 i)
      => Index (i' :& i :& i)
  where
    defLimit is = lim err (rank is) is
      where
        lim :: Index i => i -> Int -> (i' :& i :& i) -> Maybe Integer
        lim i l _ = (^ l) <$> defLimit i

        err = unreachEx "defLimit {i' :& i :& i}"

    size  (ls :& l, us :& u) = size (l, u) * size (ls, us)
    range (ls :& l, us :& u) = liftA2 (:&) (range (ls, us)) (range (l, u))
    sizes (ls :& l, us :& u) = sizes (ls, us) :& s
      where
        (E :& s) = sizes (E :& l, E :& u)

    prev bs@(ls :& l, us :& u) ix
        | isEmpty bs = emptyEx "prev {i' :& i :& i}"
        |   i /= l   = is :& pred i
        |  is /= ls  = prev (ls, us) is :& u
        |    True    = ls :& l
      where
        (is :& i) = safeElem bs ix

    next bs@(ls :& l, us :& u) ix
        | isEmpty bs = emptyEx "next {i' :& i :& i}"
        |   i /= u   = is :& succ i
        |  is /= us  = prev (ls, us) is :& u
        |    True    = ls :& l
      where
        (is :& i) = safeElem bs ix

    inBounds bs i
      |    isEmpty bs    = ER
      | isUnderflow bs i = UR
      | isOverflow  bs i = OR
      |       True       = IN

    inRange     (ls :& l, us :& u) (is :& i) = inRange     (l, u) i && inRange     (ls, us) is
    isOverflow  (ls :& l, us :& u) (is :& i) = isOverflow  (l, u) i || isOverflow  (ls, us) is
    isUnderflow (ls :& l, us :& u) (is :& i) = isUnderflow (l, u) i || isUnderflow (ls, us) is

    safeElem  (ls :& l, us :& u) (is :& i) = safeElem (ls, us) is :& safeElem (l, u) i
    isEmpty   (ls :& l, us :& u) = isEmpty (l, u) || isEmpty (ls, us)
    ordBounds (ls :& l, us :& u) = (ls' :& l', us' :& u')
      where
        (ls', us') = ordBounds (ls, us)
        (l',   u') = ordBounds (l,   u)

    index bnds@(ls :& l, us :& u) c = checkBounds (0, size bnds - 1) c res err
      where
        (cs, i) = c `divMod` size (l, u)
        res = index (ls, us) cs :& index (l, u) i
        err = "index {i' :& i :& i}"

    offset bnds ix@(is :& i) = checkBounds bnds ix res "offset {i' :& i :& i}"
      where
        res = offset (ls, us) is * size (l, u) + offset (l, u) i
        (ls :& l, us :& u) = bnds

    unsafeIndex = unsafeIndex' . toInteger

    unsafeIndex' n = res
      where
        (d, m) = n <= l ? (0, n) $ l `divMod` n
        res    = unsafeIndex' d :& unsafeIndex' m

        l = lim res proxy#

        lim :: Index i => (i' :& i :& i) -> Proxy# i -> Integer
        lim _ i = fromMaybe n (defLimit (fromProxy## i))

    extendBounds i'@(i :& is) bnds@(l :& ls, u :& us) =
        isEmpty bnds ? (i', i') $ (l' :& ls', u' :& us')
      where
        (ls', us') = extendBounds is (ls, us)
        (l',   u') = extendBounds i  (l,   u)

--------------------------------------------------------------------------------

{- Tuple instances. -}

#define INDEX_INSTANCE(Type)\
instance (Index i, Enum i, Bounded i, Rank1 i)\
      => Index (Type i) where\
{\
size         = size . toGBounds;\
sizes        = sizes . toGBounds;\
isEmpty      = isEmpty . toGBounds;\
defLimit     = defLimit . toGIndex;\
unsafeIndex  = fromGIndex . unsafeIndex;\
index        = fromGIndex ... index . toGBounds;\
range        = fmap fromGIndex . range . toGBounds;\
ordBounds    = fromGBounds . ordBounds . toGBounds;\
offset       = \ bs -> offset (toGBounds bs) . toGIndex;\
inRange      = \ bs -> inRange (toGBounds bs) . toGIndex;\
isOverflow   = \ bs -> isOverflow  (toGBounds bs) . toGIndex;\
isUnderflow  = \ bs -> isUnderflow (toGBounds bs) . toGIndex;\
next         = \ bs -> fromGIndex . next (toGBounds bs) . toGIndex;\
prev         = \ bs -> fromGIndex . prev (toGBounds bs) . toGIndex;\
safeElem     = \ bs -> fromGIndex . safeElem (toGBounds bs) . toGIndex;\
extendBounds = \ i  -> fromGBounds . extendBounds (toGIndex i) . toGBounds;\
}

INDEX_INSTANCE(T2)
INDEX_INSTANCE(T3)
INDEX_INSTANCE(T4)
INDEX_INSTANCE(T5)
INDEX_INSTANCE(T6)
INDEX_INSTANCE(T7)
INDEX_INSTANCE(T8)
INDEX_INSTANCE(T9)
INDEX_INSTANCE(T10)
INDEX_INSTANCE(T11)
INDEX_INSTANCE(T12)
INDEX_INSTANCE(T13)
INDEX_INSTANCE(T14)
INDEX_INSTANCE(T15)

#undef INDEX_INSTANCE

--------------------------------------------------------------------------------

(-.) :: Enum i => i -> i -> Int
(-.) =  on (-) fromEnum

-- | Default 'offset' for 'Integral' types.
{-# INLINE offsetIntegral #-}
offsetIntegral :: (Index i, Integral i) => (i, i) -> i -> Int
offsetIntegral bnds@(l, _) i = checkBounds bnds i (i -. l) "offset {default}"

-- | Default 'defaultBounds' for unsigned types.
{-# INLINE defaultBoundsUnsign #-}
defaultBoundsUnsign :: (Index i, Bounded i) => Int -> (i, i)
defaultBoundsUnsign n = n < 1 ? ub 1 0 $ ub 0 (n - 1) where ub = on (,) unsafeIndex

-- | Check bounds and 'throw' 'IndexException' if needed.
checkBounds :: Index i => (i, i) -> i -> res -> String -> res
checkBounds bnds i res = case inBounds bnds i of
  IN -> const res
  ER -> emptyEx
  UR -> underEx
  OR -> overEx

--------------------------------------------------------------------------------

{-# NOINLINE emptyEx #-}
emptyEx :: String -> a
emptyEx =  throw . EmptyRange . showString "in SDP.Index."

{-# NOINLINE overEx #-}
overEx :: String -> a
overEx =  throw . EmptyRange . showString "in SDP.Index."

{-# NOINLINE underEx #-}
underEx :: String -> a
underEx =  throw . EmptyRange . showString "in SDP.Index."

{-# NOINLINE unreachEx #-}
unreachEx :: String -> a
unreachEx =  throw . UnreachableException . showString "in SDP.Index."


