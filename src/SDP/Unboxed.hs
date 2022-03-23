{-# LANGUAGE Trustworthy, MagicHash, UnboxedTuples, BangPatterns, TypeOperators #-}
{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

{- |
    Module      :  SDP.Unboxed
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Unboxed" provide service class 'Unboxed', that needed for
    "SDP.Prim.SBytes"-based structures.
-}
module SDP.Unboxed
(
  -- * Unboxed
  Unboxed (..), cloneUnboxed#, cloneUnboxedM#, thawUnboxed#, freezeUnboxed#,
  bytewiseEqUnboxed#,
  
  -- ** Kind @(Type -> Type)@ proxies
  fromProxy, psizeof#, psizeof, pchunkof, pchunkof#, poffsetof#, poffsetof,
  pnewUnboxed, pcopyUnboxed, pcopyUnboxedM, pcloneUnboxed, pcloneUnboxedM,
  pthawUnboxed, pfreezeUnboxed, cloneUnboxed1#, peqUnboxed,
  
  -- ** Kind @(Type -> Type -> Type)@ proxies
  fromProxy1, pnewUnboxed1, pcloneUnboxed1, pcopyUnboxed1, pcopyUnboxedM1,
  pcloneUnboxedM1,
  
  -- Wrap helper
  Wrap (..), lzero#, single#, fromList#, fromFoldable#, fromListN#, calloc#,
  newLinear#, newLinearN#, fromFoldableM#, concat#, pconcat
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Nullable
import SDP.Finite
import SDP.Shape
import SDP.Ratio

import GHC.Stable
import GHC.Base   hiding ( (.), foldr )
import GHC.Word
import GHC.Int
import GHC.Ptr
import GHC.ST

import Data.Complex

import Foreign.C.Types

#include "MachDeps.h"

default ()

--------------------------------------------------------------------------------

{-# DEPRECATED (!>#) "in favor of readUnboxed#, will be removed in sdp-0.4" #-}
{-# DEPRECATED writeByteArray# "in favor of writeUnboxed#, will be removed in sdp-0.4" #-}

{- |
  'Unboxed' is a layer between untyped raw data and parameterized unboxed data
  structures. Also it prevents direct interaction with primitives.
-}
class Eq e => Unboxed e
  where
    {-# MINIMAL (sizeof#|sizeof), (!#), ((!>#)|readUnboxed#|readUnboxedOff#),
        (writeByteArray#|writeUnboxed#), (filler|newUnboxed) #-}
    
    {- |
      @since 0.3
      
      Default value for clearing memory.
      
      @
        'newUnboxed'' 'filler' = 'newUnboxed' (x `'asTypeOf'` 'filler')
        -- ^ forall x, but 'newUnboxed' can be faster
      @
    -}
    filler :: e
    filler =  let res = runST $ ST $ \ s1# -> case newUnboxed res 1# s1# of
                    (# s2#, marr# #) -> readUnboxed# marr# 0# s2#
              in  res
    
    {- |
      @since 0.3
      
      @offsetof# e o#@ returns the index of the element in the unboxed array
      that the offset @o#@ corresponds to (in bytes).
      
      @
        offsetof# e (sizeof# e (8# *# i#)) === 8# *# i#
        
        offsetof#       False   3# === 24#
        offsetof# (0 :: Int32) 12# ===  3#
      @
    -}
    {-# INLINE offsetof# #-}
    offsetof# :: e -> Int# -> Int#
    offsetof# e i# = case i# ># 0# of {1# -> (8# *# i#) `quotInt#` sizeof# e 8#; _ -> 0#}
    
    -- | @since 0.3 See 'offsetof#'.
    {-# INLINE offsetof #-}
    offsetof :: e -> Int -> Int
    offsetof e (I# i#) = I# (offsetof# e i#)
    
    -- | See 'sizeof#'.
    {-# INLINE sizeof #-}
    sizeof :: e -> Int -> Int
    sizeof e (I# c#) = I# (sizeof# e c#)
    
    -- | 'sizeof#' is unboxed 'sizeof'.
    {-# INLINE sizeof# #-}
    sizeof# :: e -> Int# -> Int#
    sizeof# e c# = case sizeof e (I# c#) of I# n# -> n#
    
    {- |
      @since 0.3
      
      The size of the minimal block of memory (in bytes) and the maximum number
      of values in it for a given type.
      
      @
        -- 16 bytes (8 bytes for each component) and one value
        chunkof# (undefined :: Ratio Int64) === (# 16#, 1# #)
        
        -- 8 bytes, one value
        chunkof# (undefined :: Int64) === (# 8#, 1# #)
        
        -- 4 bytes, one value
        chunkof# (undefined :: Int32) === (# 4#, 1# #)
        chunkof# (undefined ::  Char) === (# 4#, 1# #)
        
        -- 1 byte, 8 values
        chunkof# (undefined ::  Bool) === (# 1#, 8# #)
      @
    -}
    chunkof# :: e -> (# Int#, Int# #)
    chunkof# e =
      let l# = sizeof# e 64# `quotInt#` 8#; d# = gcd# 8# l#
      in  (# quotInt# l# d#, quotInt# 8# d# #)
    
    {-# INLINE chunkof #-}
    -- | @since 0.3 See 'chunkof#'.
    chunkof :: e -> (Int, Int)
    chunkof e = case chunkof# e of (# l#, c# #) -> (I# l#, I# c#)
    
    {- |
      @since 0.3
      
      @eqUnboxed# e xs# ox# ys# oy# n#@ compares two byte @n#@-element arrays
      @xs#@ and @ys#@ beginning from @ox#@ and @oy#@ elements resp.
    -}
    eqUnboxed# :: e -> ByteArray# -> Int# -> ByteArray# -> Int# -> Int# -> Bool
    eqUnboxed# e xs# xi# ys# yi# n# = case n# ># 0# of {1# -> go xi# yi# n# 1#; _ -> True}
      where
        go  _   _  _  0# = False
        go  _   _  0# b# = isTrue# b#
        go xo# yo# i# b# = go (xo# +# 1#) (yo# +# 1#) (i# -# 1#)
          (case asTypeOf (xs# !# xo#) e == (ys# !# yo#) of {True -> b#; _ -> 0#})
    
    -- | Unsafe 'ByteArray#' reader (by index) with overloaded result type.
    (!#) :: ByteArray# -> Int# -> e
    
    {- |
      @since 0.3
      
      Unsafe 'ByteArray#' (by offset) reader with overloaded result type.
    -}
    indexUnboxedOff# :: ByteArray# -> Int# -> e
    indexUnboxedOff# bytes# o# = let e = bytes# !# (offsetof# e o#) in e
    
    {- |
      Unsafe reader for unboxed data (by index). Defined only on non-negative
      indices not exceeding the number of elements in the given array.
    -}
    (!>#) :: MutableByteArray# s -> Int# -> State# s -> (# State# s, e #)
    (!>#) =  readUnboxed#
    
    {- |
      @since 0.3
      
      Unsafe reader for unboxed data (by index). Defined only on non-negative
      indices not exceeding the number of elements in the given array.
    -}
    readUnboxed# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, e #)
    readUnboxed# mbytes# i# = go undefined
      where
        go e = \ s1# -> case readUnboxedOff# mbytes# (sizeof# e i#) s1# of
          (# s2#, res #) -> (# s2#, res `asTypeOf` e #)
    
    {- |
      @sinze 0.3
      
      Unsafe reader for unboxed data (by offset).
    -}
    {-# INLINE readUnboxedOff# #-}
    readUnboxedOff# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, e #)
    readUnboxedOff# mbytes# o# = go undefined
      where
        go e = \ s1# -> case (!>#) mbytes# (offsetof# e o#) s1# of
          (# s2#, res #) -> (# s2#, res `asTypeOf` e #)
    
    {-# INLINE writeByteArray# #-}
    -- | Unsafe 'MutableByteArray#' writer (by index).
    writeByteArray# :: MutableByteArray# s -> Int# -> e -> State# s -> State# s
    writeByteArray# =  writeUnboxed#
    
    {-# INLINE writeUnboxed# #-}
    -- | @since 0.3 Unsafe unboxed writer (by index).
    writeUnboxed# :: MutableByteArray# s -> Int# -> e -> State# s -> State# s
    writeUnboxed# =  writeByteArray#
    
    {-# INLINE writeUnboxedOff# #-}
    -- | @since 0.3 Unsafe unboxed writer (by offset).
    writeUnboxedOff# :: MutableByteArray# s -> Int# -> e -> State# s -> State# s
    writeUnboxedOff# mbytes# o# e = writeUnboxed# mbytes# (offsetof# e o#) e
    
    {-# INLINE fillByteArray# #-}
    -- | Procedure for filling the array with the default value.
    fillByteArray# :: MutableByteArray# s -> Int# -> e -> State# s -> State# s
    fillByteArray# mbytes# n# e = I# n# > 0 ? go (n# -# 1#) $ \ s1# -> s1#
      where
        go -1# = \ s2# -> s2#
        go  c# = \ s2# -> go (c# -# 1#) (writeUnboxed# mbytes# c# e s2#)
    
    {-# INLINE fillByteArrayOff# #-}
    fillByteArrayOff# :: MutableByteArray# s -> Int# -> Int# -> e -> State# s -> State# s
    fillByteArrayOff# =  defaultFillByteArrayOff#
    
    {- |
      'newUnboxed' creates new 'MutableByteArray#' of given count of elements.
      First argument used as type variable.
    -}
    newUnboxed :: e -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
    newUnboxed =  newUnboxed' . asTypeOf filler
    
    {-# INLINE newUnboxed' #-}
    {- |
      'newUnboxed'' is version of 'newUnboxed', that use first argument as
      initial value. May fail when trying to write 'error' or 'undefined'.
    -}
    newUnboxed' :: e -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
    newUnboxed' e n# = \ s1# -> case newByteArray# (sizeof# e n#) s1# of
      (# s2#, mbytes# #) -> case fillByteArray# mbytes# n# e s2# of
        s3# -> (# s3#, mbytes# #)
    
    {- |
      @'copyUnboxed#' e bytes\# o1\# mbytes\# o2\# n\#@ unsafely writes elements
      from @bytes\#@ to @mbytes\#@, where o1\# and o2\# - offsets (element
      count), @n\#@ - count of elements to copy.
    -}
    copyUnboxed# :: e -> ByteArray# -> Int# -> MutableByteArray# s -> Int# ->
                    Int# -> State# s -> State# s
    copyUnboxed# e bytes# o1# mbytes# o2# n# = copyByteArray# bytes#
      (sizeof# e o1#) mbytes# (sizeof# e o2#) (sizeof# e n#)
    
    {- |
      @'copyUnboxedM#' e msrc\# o1\# mbytes\# o2\# n\#@ unsafely writes elements
      from @msrc\#@ to @mbytes\#@, where o1\# and o2\# - offsets (element
      count), @n\#@ - count of elements to copy.
    -}
    copyUnboxedM# :: e -> MutableByteArray# s -> Int# ->
                          MutableByteArray# s -> Int# ->
                          Int# -> State# s -> State# s
    copyUnboxedM# e msrc# o1# mbytes# o2# n# = copyMutableByteArray# msrc#
      (sizeof# e o1#) mbytes# (sizeof# e o2#) (sizeof# e n#)
    
    {- |
      @'hashUnboxedWith' e len# off# bytes# salt@ returns @bytes#@ @FNV-1@ hash,
      where @off#@ and @len#@ is offset and length (in elements).
      
      Note: the standard definition of this function is written in Haskell using
      low-level functions, but this implementation mayn't be as efficient as the
      foreign procedure in the @hashable@ package.
    -}
    hashUnboxedWith :: e -> Int# -> Int# -> ByteArray# -> Int# -> Int#
    hashUnboxedWith e len# off# bytes# = go (sizeof# e off#) (sizeof# e len#)
      where
        go _  0# salt# = salt#
        go o# n# salt# = go (o# +# 1#) (n# -# 1#) (word2Int# hash#)
          where
            prod# = int2Word# (salt# *# 16777619#)
            elem# = indexWord8Array# bytes# o#
            hash# = prod# `xor#` elem#
    
    sortUnboxed :: Ord e => e -> MutableByteArray# s
                -> Int# -> Int# -> State# s -> State# s
    sortUnboxed =  undefined -- TODO: make required

--------------------------------------------------------------------------------

{- Unboxed helpers. -}

{- |
  @since 0.2
  
  @cloneUnboxed# e bytes# o# c#@ creates new @c#@-element length immutable slice
  of @bytes#@ beginning from @o#@-th element.
-}
cloneUnboxed# :: Unboxed e => e -> ByteArray# -> Int# -> Int# -> ByteArray#
cloneUnboxed# e bytes# o# c# = unwrap $ runST $ ST $
  \ s1# -> case newUnboxed e c# s1# of
    (# s2#, mbytes# #) -> case copyUnboxed# e bytes# o# mbytes# 0# c# s2# of
      s3# -> case unsafeFreezeByteArray# mbytes# s3# of
        (# s4#, bytes'# #) -> (# s4#, (Wrap bytes'#) #)

{- |
  @since 0.2.1
  
  @cloneUnboxedM# e mbytes# o# c#@ creates new @c#@-element length mutable slice
  of @bytes#@ beginning from @o#@-th element.
-}
cloneUnboxedM# :: Unboxed e => e -> MutableByteArray# s -> Int# -> Int# ->
  State# s -> (# State# s, MutableByteArray# s #)
cloneUnboxedM# e mbytes# o# n# = \ s1# -> case newByteArray# (sizeof# e n#) s1# of
  (# s2#, copy# #) -> case copyUnboxedM# e mbytes# o# copy# 0# n# s2# of
    s3# -> (# s3#, copy# #)

{- |
  @since 0.2.1
  
  @'thawUnboxed#' e bytes# c#@ creates new @sizeof# e c#@ bytes length
  'MutableByteArray#' and copy @bytes#@ to it.
-}
thawUnboxed# :: Unboxed e => e -> ByteArray# -> Int# ->
  State# s -> (# State# s, MutableByteArray# s #)
thawUnboxed# e bytes# c# = \ s1# -> case newByteArray# n# s1# of
    (# s2#, mbytes# #) -> case copyByteArray# bytes# 0# mbytes# 0# n# s2# of
      s3# -> (# s3#, mbytes# #)
  where
    n# = sizeof# e c#

{- |
  @since 0.2.1
  
  @'freezeUnboxed#' e mbytes# c#@ creates new @sizeof# e c#@ bytes length
  'ByteArray#' and copy @mbytes#@ to it.
-}
freezeUnboxed# :: Unboxed e => e -> MutableByteArray# s -> Int# ->
  State# s -> (# State# s, ByteArray# #)
freezeUnboxed# e mbytes# n# = \ s1# -> case cloneUnboxedM# e mbytes# 0# n# s1# of
  (# s2#, copy# #) -> unsafeFreezeByteArray# copy# s2#

--------------------------------------------------------------------------------

{- Rank 1 Unboxed proxies. -}

-- | Returns 'undefined' of suitable type.
fromProxy :: proxy e -> e
fromProxy =  const undefined

{- |
  @since 0.2.1
  
  'psizeof#' is proxy version of 'sizeof#'.
-}
psizeof# :: Unboxed e => proxy e -> Int# -> Int#
psizeof# =  sizeof# . fromProxy

{- |
  @since 0.2
  
  'psizeof' is proxy version of 'sizeof'.
-}
psizeof :: Unboxed e => proxy e -> Int -> Int
psizeof =  sizeof . fromProxy

{- |
  @since 0.3
  
  'pchunkof' is proxy version of 'chunkof'.
-}
pchunkof :: Unboxed e => proxy e -> (Int, Int)
pchunkof =  chunkof . fromProxy

{- |
  @since 0.3
  
  'pchunkof#' is proxy version of 'chunkof#'.
-}
pchunkof# :: Unboxed e => proxy e -> (# Int#, Int# #)
pchunkof# e = chunkof# (fromProxy e)

poffsetof# :: Unboxed e => proxy e -> Int# -> Int#
poffsetof# e = offsetof# (fromProxy e)

poffsetof :: Unboxed e => proxy e -> Int -> Int
poffsetof =  offsetof . fromProxy

peqUnboxed :: Unboxed e => proxy e -> ByteArray# -> Int# -> ByteArray# -> Int# -> Int# -> Bool
peqUnboxed =  eqUnboxed# . fromProxy

{- |
  @since 0.2
  
  Kind @(Type -> Type)@ proxy version of 'newUnboxed'.
-}
pnewUnboxed :: Unboxed e => proxy e -> Int# ->
  State# s -> (# State# s, MutableByteArray# s #)
pnewUnboxed =  newUnboxed . fromProxy

{- |
  @since 0.2
  
  Kind @(Type -> Type)@ proxy version if 'copyUnboxed#'.
-}
pcopyUnboxed :: Unboxed e => proxy e -> ByteArray# -> Int# ->
  MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
pcopyUnboxed =  copyUnboxed# . fromProxy

{- |
  @since 0.2
  
  Kind @(Type -> Type)@ proxy version if 'copyUnboxedM#'.
-}
pcopyUnboxedM :: Unboxed e => proxy e -> MutableByteArray# s -> Int# ->
  MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
pcopyUnboxedM =  copyUnboxedM# . fromProxy

{- |
  @since 0.2
  
  Kind @(Type -> Type)@ proxy version of 'cloneUnboxed#'.
-}
cloneUnboxed1# :: Unboxed e => proxy e -> ByteArray# -> Int# -> Int# -> ByteArray#
cloneUnboxed1# =  cloneUnboxed# . fromProxy

{- |
  @since 0.2.1
  
  Same as @sdp-0.2@ 'cloneUnboxed1#'. Use only if you don't need @sdp-0.2@
  compatibility.
-}
pcloneUnboxed :: Unboxed e => proxy e -> ByteArray# -> Int# -> Int# -> ByteArray#
pcloneUnboxed =  cloneUnboxed1#

{- |
  @since 0.2.1
  
  Kind @(Type -> Type)@ proxy version of 'cloneUnboxed#'.
-}
pcloneUnboxedM :: Unboxed e => proxy e -> MutableByteArray# s -> Int# -> Int# ->
  State# s -> (# State# s, MutableByteArray# s #)
pcloneUnboxedM =  cloneUnboxedM# . fromProxy

{- |
  @since 0.2.1
  
  Kind @(Type -> Type)@ proxy version of 'thawUnboxed#'.
-}
pthawUnboxed :: Unboxed e => proxy e -> ByteArray# -> Int# ->
  State# s -> (# State# s, MutableByteArray# s #)
pthawUnboxed =  thawUnboxed# . fromProxy

{- |
  @since 0.2.1
  
  Kind @(Type -> Type)@ proxy version of 'pfreezeUnboxed'.
-}
pfreezeUnboxed :: Unboxed e => proxy e -> MutableByteArray# s -> Int# ->
  State# s -> (# State# s, ByteArray# #)
pfreezeUnboxed =  freezeUnboxed# . fromProxy

--------------------------------------------------------------------------------

{- (Type -> Type -> Type)-kind Unboxed proxies. -}

-- | Returns 'undefined' of suitable type.
fromProxy1 :: m (proxy e) -> e
fromProxy1 =  const undefined

{- |
  @since 0.2
  
  Kind @(Type -> Type -> Type)@ proxy version of 'newUnboxed'.
-}
pnewUnboxed1 :: Unboxed e => p (proxy e) -> Int# ->
  State# s -> (# State# s, MutableByteArray# s #)
pnewUnboxed1 =  newUnboxed . fromProxy1

{- |
  @since 0.2
  
  Kind @(Type -> Type -> Type)@ proxy version of 'copyUnboxed#'.
-}
pcopyUnboxed1 :: Unboxed e => p (proxy e) -> ByteArray# -> Int# ->
  MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
pcopyUnboxed1 =  copyUnboxed# . fromProxy1

{- |
  @since 0.2.1
  
  Kind @(Type -> Type -> Type)@ proxy version of 'copyUnboxedM#'.
-}
pcopyUnboxedM1 :: Unboxed e => p (proxy e) -> MutableByteArray# s -> Int# ->
  MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
pcopyUnboxedM1 =  copyUnboxedM# . fromProxy1

{- |
  @since 0.2.1
  
  Kind @(Type -> Type -> Type)@ proxy version of 'cloneUnboxed#'.
-}
pcloneUnboxed1 :: Unboxed e => p (proxy e) -> ByteArray# -> Int# -> Int# -> ByteArray#
pcloneUnboxed1 =  cloneUnboxed# . fromProxy1

{- |
  @since 0.2.1
  
  Kind @(Type -> Type -> Type)@ proxy version of 'cloneUnboxed#'.
-}
pcloneUnboxedM1 :: Unboxed e => p (proxy e) -> MutableByteArray# s -> Int# -> Int# ->
  State# s -> (# State# s, MutableByteArray# s #)
pcloneUnboxedM1 =  cloneUnboxedM# . fromProxy1

--------------------------------------------------------------------------------

{- Numeric instances. -}

instance Unboxed Int
  where
    filler = 0
    
    {-# INLINE sizeof# #-}
    sizeof# _ n# = case n# ># 0# of {1# -> n# *# SIZEOF_HSWORD#; _ -> 0#}
    
    eqUnboxed# = bytewiseEqUnboxed#
    
    {-# INLINE chunkof# #-}
    chunkof# _ = (# SIZEOF_HSWORD#, 1# #)
    
    {-# INLINE offsetof# #-}
    offsetof# _ i# = case i# ># 0# of {1# -> quotInt# i# SIZEOF_HSWORD#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes# !# i# = I# (indexIntArray# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readIntArray# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, I# e# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (I# e#) = writeIntArray# mbytes# n# e#
    
    {-# INLINE newUnboxed #-}
    newUnboxed = calloc#

instance Unboxed Int8
  where
    filler = 0
    
    eqUnboxed# = bytewiseEqUnboxed#
    
    {-# INLINE sizeof# #-}
    sizeof# _ n# = case n# ># 0# of {1# -> n#; _ -> 0#}
    
    {-# INLINE chunkof# #-}
    chunkof# _ = (# 1#, 1# #)
    
    {-# INLINE offsetof# #-}
    offsetof# _ i# = case i# ># 0# of {1# -> quotInt# i# 8#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes# !# i# = I8# (indexInt8Array# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readInt8Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, I8# e# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (I8# e#) = writeInt8Array# mbytes# n# e#
    
    {-# INLINE newUnboxed #-}
    newUnboxed = calloc#
    
    sortUnboxed _ bs# n# o# = \ s1# -> case newUnboxed (0 :: Word) 256# s1# of
        (# s2#, cache# #) -> case count# cache# (n# +# o# -# 1#) s2# of
          s3# -> write# cache# 0# 0# s3#
      where
        count# cache# i# = \ s1# -> if isTrue# (i# <# o#) then s1# else
          case readInt8Array# bs# i# s1# of
            (# s2#, w8# #) -> let i8# = w8# +# 0x80# in case readWordArray# cache# i8# s2# of
              (# s3#, c# #) -> case writeWordArray# cache# i8# (c# `plusWord#` int2Word# 1#) s3# of
                s4# -> count# cache# (i# -# 1#) s4#
        
        write# _      0xff# _  = \ s1# -> s1#
        write# cache# i#    j# = \ s1# -> case readIntArray# cache# i# s1# of
          (# s2#, c# #) -> case go# (i# -# 0x80#) j# c# s2# of
            s3# -> write# cache# (i# +# 1#) (j# +# c#) s3#
          where
            go# _  _  0# = \ s1# -> s1#
            go# v# k# c# = \ s1# -> case writeInt8Array# bs# k# v# s1# of
              s2# -> go# v# (k# +# 1#) (c# -# 1#) s2#

instance Unboxed Int16
  where
    filler = 0
    
    eqUnboxed# = bytewiseEqUnboxed#
    
    {-# INLINE sizeof# #-}
    sizeof# _ n# = case n# ># 0# of {1# -> n# *# 2#; _ -> 0#}
    
    {-# INLINE chunkof# #-}
    chunkof# _ = (# 2#, 1# #)
    
    {-# INLINE offsetof# #-}
    offsetof# _ i# = case i# ># 0# of {1# -> quotInt# i# 4#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes# !# i# = I16# (indexInt16Array# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readInt16Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, I16# e# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (I16# e#) = writeInt16Array# mbytes# n# e#
    
    {-# INLINE newUnboxed #-}
    newUnboxed = calloc#

instance Unboxed Int32
  where
    filler = 0
    
    eqUnboxed# = bytewiseEqUnboxed#
    
    {-# INLINE sizeof# #-}
    sizeof# _ n# = case n# ># 0# of {1# -> n# *# 4#; _ -> 0#}
    
    {-# INLINE chunkof# #-}
    chunkof# _ = (# 4#, 1# #)
    
    {-# INLINE offsetof# #-}
    offsetof# _ i# = case i# ># 0# of {1# -> quotInt# i# 2#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes# !# i# = I32# (indexInt32Array# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readInt32Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, I32# e# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (I32# e#) = writeInt32Array# mbytes# n# e#
    
    {-# INLINE newUnboxed #-}
    newUnboxed = calloc#

instance Unboxed Int64
  where
    filler = 0
    
    eqUnboxed# = bytewiseEqUnboxed#
    
    {-# INLINE sizeof# #-}
    sizeof# _ n# = case n# ># 0# of {1# -> n# *# 8#; _ -> 0#}
    
    {-# INLINE chunkof# #-}
    chunkof# _ = (# 8#, 1# #)
    
    {-# INLINE offsetof# #-}
    offsetof# _ i# = case i# ># 0# of {1# -> i#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes# !# i# = I64# (indexInt64Array# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readInt64Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, I64# e# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (I64# e#) = writeInt64Array# mbytes# n# e#
    
    {-# INLINE newUnboxed #-}
    newUnboxed = calloc#

instance Unboxed Word
  where
    filler = 0
    
    eqUnboxed# = bytewiseEqUnboxed#
    
    {-# INLINE sizeof# #-}
    sizeof# _ n# = case n# ># 0# of {1# -> n# *# SIZEOF_HSWORD#; _ -> 0#}
    
    {-# INLINE chunkof# #-}
    chunkof# _ = (# SIZEOF_HSWORD#, 1# #)
    
    {-# INLINE offsetof# #-}
    offsetof# _ i# = case i# ># 0# of {1# -> quotInt# i# SIZEOF_HSWORD#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes# !# i# = W# (indexWordArray# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readWordArray# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, W# e# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (W# e#) = writeWordArray# mbytes# n# e#
    
    {-# INLINE newUnboxed #-}
    newUnboxed = calloc#

instance Unboxed Word8
  where
    filler = 0
    
    eqUnboxed# = bytewiseEqUnboxed#
    
    {-# INLINE sizeof# #-}
    sizeof# _ n# = case n# ># 0# of {1# -> n#; _ -> 0#}
    
    {-# INLINE chunkof# #-}
    chunkof# _ = (# 1#, 1# #)
    
    {-# INLINE (!#) #-}
    bytes# !# i# = W8# (indexWord8Array# bytes# i#)
    
    {-# INLINE offsetof# #-}
    offsetof# _ i# = case i# ># 0# of {1# -> quotInt# i# 8#; _ -> 0#}
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readWord8Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, W8# e# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (W8#  e#) = writeWord8Array# mbytes# n# e#
    
    {-# INLINE newUnboxed #-}
    newUnboxed = calloc#
    
    sortUnboxed _ bs# n# o# = \ s1# -> case newUnboxed (0 :: Word) 256# s1# of
        (# s2#, cache# #) -> case count# cache# (n# +# o# -# 1#) s2# of
          s3# -> write# cache# 0# 0# s3#
      where
        count# cache# i# = \ s1# -> if isTrue# (i# <# o#) then s1# else
          case readWord8Array# bs# i# s1# of
            (# s2#, w8# #) -> let i8# = word2Int# w8# in case readWordArray# cache# i8# s2# of
              (# s3#, c# #) -> case writeWordArray# cache# i8# (c# `plusWord#` int2Word# 1#) s3# of
                s4# -> count# cache# (i# -# 1#) s4#
        
        write# _      0xff# _  = \ s1# -> s1#
        write# cache# i#    j# = \ s1# -> case readIntArray# cache# i# s1# of
          (# s2#, c# #) -> case go# (int2Word# i#) j# c# s2# of
            s3# -> write# cache# (i# +# 1#) (j# +# c#) s3#
          where
            go# _  _  0# = \ s1# -> s1#
            go# v# k# c# = \ s1# -> case writeWord8Array# bs# k# v# s1# of
              s2# -> go# v# (k# +# 1#) (c# -# 1#) s2#

instance Unboxed Word16
  where
    filler = 0
    
    eqUnboxed# = bytewiseEqUnboxed#
    
    {-# INLINE sizeof# #-}
    sizeof# _ n# = case n# ># 0# of {1# -> n# *# 2#; _ -> 0#}
    
    {-# INLINE chunkof# #-}
    chunkof# _ = (# 2#, 1# #)
    
    {-# INLINE (!#) #-}
    bytes# !# i# = W16# (indexWord16Array# bytes# i#)
    
    {-# INLINE offsetof# #-}
    offsetof# _ i# = case i# ># 0# of {1# -> quotInt# i# 4#; _ -> 0#}
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readWord16Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, W16# e# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (W16# e#) = writeWord16Array# mbytes# n# e#
    
    {-# INLINE newUnboxed #-}
    newUnboxed = calloc#

instance Unboxed Word32
  where
    filler = 0
    
    eqUnboxed# = bytewiseEqUnboxed#
    
    {-# INLINE sizeof# #-}
    sizeof# _ n# = case n# ># 0# of {1# -> n# *# 4#; _ -> 0#}
    
    {-# INLINE chunkof# #-}
    chunkof# _ = (# 4#, 1# #)
    
    {-# INLINE offsetof# #-}
    offsetof# _ i# = case i# ># 0# of {1# -> quotInt# i# 2#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes# !# i# = W32# (indexWord32Array# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readWord32Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, W32# e# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (W32# e#) = writeWord32Array# mbytes# n# e#
    
    {-# INLINE newUnboxed #-}
    newUnboxed = calloc#

instance Unboxed Word64
  where
    filler = 0
    
    eqUnboxed# = bytewiseEqUnboxed#
    
    {-# INLINE sizeof# #-}
    sizeof# _ n# = case n# ># 0# of {1# -> n# *# 8#; _ -> 0#}
    
    {-# INLINE chunkof# #-}
    chunkof# _ = (# 8#, 1# #)
    
    {-# INLINE offsetof# #-}
    offsetof# _ i# = case i# ># 0# of {1# -> i#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes# !# i# = W64# (indexWord64Array# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readWord64Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, W64# e# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (W64# e#) = writeWord64Array# mbytes# n# e#
    
    {-# INLINE newUnboxed #-}
    newUnboxed = calloc#

instance Unboxed Float
  where
    filler = 0
    
    eqUnboxed# = bytewiseEqUnboxed#
    
    {-# INLINE sizeof# #-}
    sizeof# _ n# = case n# ># 0# of {1# -> n# *# SIZEOF_HSFLOAT#; _ -> 0#}
    
    {-# INLINE chunkof# #-}
    chunkof# _ = (# SIZEOF_HSFLOAT#, 1# #)
    
    {-# INLINE offsetof# #-}
    offsetof# _ i# = case i# ># 0# of {1# -> quotInt# i# SIZEOF_HSFLOAT#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes# !# i# = F# (indexFloatArray# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readFloatArray# mbytes# i# s1# of
      (# s2#, f# #) -> (# s2#, F# f# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (F# e#) = writeFloatArray# mbytes# n# e#
    
    {-# INLINE newUnboxed #-}
    newUnboxed = calloc#

instance Unboxed Double
  where
    filler = 0
    
    eqUnboxed# = bytewiseEqUnboxed#
    
    {-# INLINE sizeof# #-}
    sizeof# _ n# = case n# ># 0# of {1# -> n# *# SIZEOF_HSDOUBLE#; _ -> 0#}
    
    {-# INLINE chunkof# #-}
    chunkof# _ = (# SIZEOF_HSDOUBLE#, 1# #)
    
    {-# INLINE offsetof# #-}
    offsetof# _ i# = case i# ># 0# of {1# -> quotInt# i# SIZEOF_HSDOUBLE#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes# !# i# = D# (indexDoubleArray# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readDoubleArray# mbytes# i# s1# of
      (# s2#, d# #) -> (# s2#, D# d# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (D# e#) = writeDoubleArray# mbytes# n# e#
    
    {-# INLINE newUnboxed #-}
    newUnboxed = calloc#

instance (Unboxed a, Integral a) => Unboxed (Ratio a)
  where
    filler = 0 :% 0
    
    {-# INLINE sizeof# #-}
    sizeof# e n# = 2# *# psizeof# e n#
    
    eqUnboxed# = bytewiseEqUnboxed#
    
    sizeof e n = 2 * psizeof e n
    
    bytes# !# i# = bytes# !# i2# :% (bytes# !# (i2# +# 1#)) where i2# = 2# *# i#
    
    readUnboxed# mbytes# i# = let i2# = 2# *# i# in
      \ s1# -> case readUnboxed# mbytes# i2# s1# of
        (# s2#, n #) -> case readUnboxed# mbytes# (i2# +# 1#) s2# of
          (# s3#, d #) -> (# s3#, n :% d #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# i# (n :% d) = let i2# = 2# *# i# in
      \ s1# -> case writeUnboxed# mbytes# i2# n s1# of
        s2# -> writeUnboxed# mbytes# (i2# +# 1#) d s2#

instance (Unboxed a, Num a) => Unboxed (Complex a)
  where
    filler = 0 :+ 0
    
    eqUnboxed# = bytewiseEqUnboxed#
    
    {-# INLINE sizeof# #-}
    sizeof# e n# = 2# *# psizeof# e n#
    
    bytes# !# i# = bytes# !# i2# :+ (bytes# !# (i2# +# 1#)) where i2# = 2# *# i#
    
    readUnboxed# mbytes# i# = let i2# = 2# *# i# in
      \ s1# -> case readUnboxed# mbytes# i2# s1# of
        (# s2#, n #) -> case readUnboxed# mbytes# (i2# +# 1#) s2# of
          (# s3#, d #) -> (# s3#, n :+ d #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# i# (n :+ d) = let i2# = 2# *# i# in
      \ s1# -> case writeUnboxed# mbytes# i2# n s1# of
        s2# -> writeUnboxed# mbytes# (i2# +# 1#) d s2#
    
    {-# INLINE newUnboxed #-}
    newUnboxed e n# = pnewUnboxed e (2# *# n#)

--------------------------------------------------------------------------------

{- Pointer instances. -}

instance Unboxed (Ptr a)
  where
    filler = NULL
    
    eqUnboxed# = bytewiseEqUnboxed#
    
    {-# INLINE sizeof# #-}
    sizeof# _ n# = case n# ># 0# of {1# -> n# *# SIZEOF_HSWORD#; _ -> 0#}
    
    {-# INLINE chunkof# #-}
    chunkof# _ = (# SIZEOF_HSWORD#, 1# #)
    
    {-# INLINE offsetof# #-}
    offsetof# _ i# = case i# ># 0# of {1# -> quotInt# i# SIZEOF_HSWORD#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes# !# i# = Ptr (indexAddrArray# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readAddrArray# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, Ptr e# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (Ptr e) = writeAddrArray# mbytes# n# e
    
    {-# INLINE newUnboxed #-}
    newUnboxed = calloc#

instance Unboxed (FunPtr a)
  where
    filler = NULL
    
    eqUnboxed# = bytewiseEqUnboxed#
    
    {-# INLINE sizeof# #-}
    sizeof# _ n# = case n# ># 0# of {1# -> n# *# SIZEOF_HSWORD#; _ -> 0#}
    
    {-# INLINE chunkof# #-}
    chunkof# _ = (# SIZEOF_HSWORD#, 1# #)
    
    {-# INLINE offsetof# #-}
    offsetof# _ i# = case i# ># 0# of {1# -> quotInt# i# SIZEOF_HSWORD#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes#  !#  i# = FunPtr (indexAddrArray# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readAddrArray# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, FunPtr e# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (FunPtr e) = writeAddrArray# mbytes# n# e
    
    {-# INLINE newUnboxed #-}
    newUnboxed = calloc#

instance Unboxed (StablePtr a)
  where
    filler = NULL
    
    eqUnboxed# = bytewiseEqUnboxed#
    
    {-# INLINE sizeof# #-}
    sizeof# _ n# = case n# ># 0# of {1# -> n# *# SIZEOF_HSWORD#; _ -> 0#}
    
    {-# INLINE chunkof# #-}
    chunkof# _ = (# SIZEOF_HSWORD#, 1# #)
    
    {-# INLINE offsetof# #-}
    offsetof# _ i# = case i# ># 0# of {1# -> quotInt# i# SIZEOF_HSWORD#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes# !# i# = StablePtr (indexStablePtrArray# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readStablePtrArray# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, StablePtr e# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (StablePtr e) = writeStablePtrArray# mbytes# n# e
    
    {-# INLINE newUnboxed #-}
    newUnboxed = calloc#

--------------------------------------------------------------------------------

{- Foreign C instances. -}

#define deriving_instance_Unboxed(Type)\
instance Unboxed Type where\
{\
  filler     = Type filler;\
  eqUnboxed# = bytewiseEqUnboxed#;\
  arr# !# i# = Type ( arr# !# i# );\
  sizeof   e = sizeof (consSizeof Type e);\
  \
  fillByteArray# marr# i# (Type e) = fillByteArray# marr# i# e;\
  writeUnboxed#  marr# i# (Type e) = writeUnboxed#  marr# i# e;\
  readUnboxed#   marr# i# = \ s1# -> case readUnboxed# marr# i# s1# of\
    {(# s2#, e #) -> (# s2#, Type e #)};\
  \
  newUnboxed  e = newUnboxed (consSizeof Type e);\
  newUnboxed' (Type e) = newUnboxed' e;\
  \
  sortUnboxed e bs# n# o# = \ s1# -> sortUnboxed (consSizeof Type e) bs# n# o# s1#;\
}

deriving_instance_Unboxed(CChar)
deriving_instance_Unboxed(CSChar)
deriving_instance_Unboxed(CWchar)
deriving_instance_Unboxed(CShort)
deriving_instance_Unboxed(CUShort)

deriving_instance_Unboxed(CInt)
deriving_instance_Unboxed(CUInt)
deriving_instance_Unboxed(CLong)
deriving_instance_Unboxed(CULong)
deriving_instance_Unboxed(CLLong)
deriving_instance_Unboxed(CULLong)
deriving_instance_Unboxed(CIntPtr)
deriving_instance_Unboxed(CUIntPtr)
deriving_instance_Unboxed(CIntMax)
deriving_instance_Unboxed(CUIntMax)
deriving_instance_Unboxed(CPtrdiff)

deriving_instance_Unboxed(CTime)
deriving_instance_Unboxed(CClock)
deriving_instance_Unboxed(CUSeconds)
deriving_instance_Unboxed(CSUSeconds)

deriving_instance_Unboxed(CSize)

#if MIN_VERSION_base(4,10,0)
-- | @since base-4.10.0.0
deriving_instance_Unboxed(CBool)
#endif

deriving_instance_Unboxed(CFloat)
deriving_instance_Unboxed(CDouble)
deriving_instance_Unboxed(CSigAtomic)

#undef deriving_instance_Unboxed

--------------------------------------------------------------------------------

{- Other instances. -}

instance Unboxed Bool
  where
    filler = False
    
    {-# INLINE sizeof# #-}
    sizeof# _ n# = case n# ># 0# of {1# -> case n# `quotRemInt#` 8# of {(# q#, r# #) -> q# +# (r# /=# 0#)}; _ -> 0#}
    
    {-# INLINE chunkof# #-}
    chunkof# _ = (# 1#, 8# #)
    
    {-# INLINE offsetof# #-}
    offsetof# _ i# = case i# ># 0# of {1# -> 8# *# i#; _ -> 0#}
    
    {-# INLINE newUnboxed #-}
    newUnboxed = calloc#
    
    {-# INLINE (!#) #-}
    bytes# !# i# = isTrue# ((indexWordArray# bytes# (bool_index i#) `and#` bool_bit i#) `neWord#` int2Word# 0#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readWordArray# mbytes# (bool_index i#) s1# of
      (# s2#, e# #) -> (# s2#, isTrue# ((e# `and#` bool_bit i#) `neWord#` int2Word# 0#) #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# e = \ s1# -> case readWordArray# mbytes# i# s1# of
        (# s2#, old_byte# #) -> writeWordArray# mbytes# i# (bitWrite old_byte#) s2#
      where
        bitWrite old_byte# = if e then old_byte# `or#` bool_bit n# else old_byte# `and#` bool_not_bit n#
        i# = bool_index n#
    
    -- TODO: big-endian?
    fillByteArray# bs# c# e = \ s1# -> case setByteArray# bs# 0# n# val# s1# of
        s2# -> if isTrue# (b# ==# 0#) then s2# else case readWord8Array# bs# n# s2# of
          (# s3#, w8# #) -> writeWord8Array# bs# n# (if e then or# w8# (not# mask#) else and# w8# mask#) s3#
      where
        mask# = uncheckedShiftL# (int2Word# 0xff#) b#
        val#  = if e then 0xff# else 0x00#
        !(# n#, b# #) = quotRemInt# c# 8#
    
    -- TODO: big-endian?
    fillByteArrayOff# bs# c# 0# e = fillByteArray# bs# c# e
    fillByteArrayOff# bs# c# o# e
        -- I really don't like to discern arrays like
        -- [**001***], [**110001] or [****0010][110*****]
        | 1# <- c# <# 8# = defaultFillByteArrayOff# bs# c# o# e
        -- Normal byte array with whole byte offset
        | 0# <-      bo# = \ s1# -> case setByteArray# bs# no# nc# val# s1# of
          s2# -> if isTrue# (bc# ==# 0#) then s2# else case readWord8Array# bs# (nc# +# 1#) s2# of
            (# s3#, w8# #) ->
              let mask# = uncheckedShiftL# oxff# bc#
              in  writeWord8Array# bs# (nc# +# 1#)
                    (if e then or# w8# (not# mask#) else and# w8# mask#) s3#
        -- Now deal with incomplete first byte if any
        | True           = \ s1# -> case readWord8Array# bs# no# s1# of
          (# s2#, w8# #) ->
            let mask# = uncheckedShiftL# oxff# bo#
            in  case writeWord8Array# bs# no#
                  (if e then or# w8# mask# else and# w8# (not# mask#)) s2# of
                    s3# -> fillByteArrayOff# bs# (c# -# ob#) (o# +# ob#) e s3#
      where
        !(# no#, bo# #) = quotRemInt# o# 8#; val#  = if e then 0xff# else 0x00#
        !(# nc#, bc# #) = quotRemInt# c# 8#; oxff# = int2Word# 0xff#
        ob# = 8# -# bo#
    
    copyUnboxed# e bytes# o1# mbytes# o2# c# = isTrue# (c# <# 1#) ? (\ s1# -> s1#) $
      \ s1# -> case writeUnboxed# mbytes# o2# ((bytes# !# o1#) `asTypeOf` e) s1# of
        s2# -> copyUnboxed# e bytes# (o1# +# 1#) mbytes# (o2# +# 1#) (c# -# 1#) s2#
    
    copyUnboxedM# e src# o1# mbytes# o2# n# = \ s1# -> case readUnboxed# src# o1# s1# of
      (# s2#, x #) -> case writeUnboxed# mbytes# o2# (x `asTypeOf` e) s2# of
        s3# -> copyUnboxedM# e src# (o1# +# 1#) mbytes# (o2# +# 1#) (n# -# 1#) s3#
    
    -- TODO: big-endian?
    hashUnboxedWith e len# off# bytes#
        | 1# <- len# <# 1# = \ salt# -> salt#
        | 1# <- off# <# 0# = hashUnboxedWith e len# 0# bytes#
        | 0# <-   bit_off# = go0 byte_cnt# byte_off#
        |             True = goo byte_cnt# (byte_off# +# 1#) (indexWord8Array# bytes# byte_off#)
      where
        go0 0# _  salt# = salt#
        go0 1# o# salt# = hash# salt# (indexWord8Array# bytes# o# `and#` mask#)
        go0 n# o# salt# = go0 (n# -# 1#) (o# +# 1#) (salt# `hash#` indexWord8Array# bytes# o#)
        
        goo 0# _    _   salt# = salt#
        goo 1# _  temp# salt# = hash# salt# (shiftRL# temp# bit_off# `and#` mask#)
        goo n# o# temp# salt# = goo (n# -# 1#) (o# +# 1#) byte# (hash# salt# curr#)
          where
            curr# = shiftRL# temp# bit_off# `or#` shiftL# byte# (8# -# bit_off#)
            byte# = indexWord8Array# bytes# o#
        
        hash# = \ s# v# -> word2Int# (int2Word# (s# *# 16777619#) `xor#` v#)
        mask# = int2Word# 0xff# `shiftRL#` bit_rest#
        
        !(# byte_off#, bit_off# #) = off# `quotRemInt#` 8#
        
        bit_rest# = 8# -# remInt# len# 8#
        byte_cnt# = sizeof# e len#

instance Unboxed Char
  where
    filler = '\0'
    
    eqUnboxed# = bytewiseEqUnboxed#
    
    {-# INLINE sizeof# #-}
    sizeof# _ n# = case n# ># 0# of {1# -> n# *# 4#; _ -> 0#}
    
    {-# INLINE chunkof# #-}
    chunkof# _ = (# 4#, 1# #)
    
    {-# INLINE offsetof# #-}
    offsetof# _ i# = case i# ># 0# of {1# -> quotInt# i# 4#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes# !# i# = C# (indexWideCharArray# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readWideCharArray# mbytes# i# s1# of
      (# s2#, c# #) -> (# s2#, C# c# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (C# e#) = writeWideCharArray# mbytes# n# e#
    
    {-# INLINE newUnboxed #-}
    newUnboxed = calloc#

--------------------------------------------------------------------------------

instance Unboxed E
  where
    filler = E
    
    eqUnboxed# _ _ _ _ _ _ = True
    
    {-# INLINE sizeof# #-}
    sizeof# _ _ = 0#
    
    {-# INLINE offsetof# #-}
    offsetof# _ _ = 0#
    
    {-# INLINE (!#) #-}
    readUnboxed# = \ _ _ s# -> (# s#, E #)
    (!#)  = \ _ _ -> E
    
    newUnboxed  _ _ = newByteArray# 0#
    newUnboxed' _ _ = newByteArray# 0#
    
    writeUnboxed#   _ _ = \ _ s# -> s#
    fillByteArray#  _ _ = \ _ s# -> s#

instance Unboxed e => Unboxed (I1 e)
  where
    filler = E :& filler
    
    eqUnboxed# = peqUnboxed
    
    sizeof# = psizeof#
    sizeof  = psizeof
    
    bytes# !# i# = E :& (bytes# !# i#)
    
    readUnboxed# bytes# i# = \ s1# -> case readUnboxed# bytes# i# s1# of
      (# s2#, e #) -> (# s2#, E :& e #)
    
    writeUnboxed#  bytes# n# (E :& e) = writeUnboxed#  bytes# n# e
    fillByteArray# bytes# n# (E :& e) = fillByteArray# bytes# n# e
    
    newUnboxed' = \ (E :& i) -> newUnboxed i
    newUnboxed  = pnewUnboxed

instance (Enum e, Shape e, Bounded e, Unboxed e, Shape (e' :& e), Unboxed (e' :& e)) => Unboxed (e' :& e :& e)
  where
    filler = filler :& filler
    
    eqUnboxed# e xs# o1# ys# o2# n# = let r# = rank# e in peqUnboxed e xs# (o1# *# r#) ys# (o2# *# r#) (n# *# r#)
    
    sizeof# e n# = psizeof# e (rank# e *# n#)
    sizeof  e  n = psizeof  e (rank  e *   n)
    
    bytes# !# i# = go undefined
      where
        go t =
          let r# = rank# t; o# = i#*#r# +# i#
          in  ((bytes# !# o#) `asTypeOf` t) :& (bytes# !# (o# +# r#))
    
    readUnboxed# bytes# i# = go undefined
      where
        go t = let r# = rank# t; o# = i#*#r# +# i# in
          \ s1# -> case readUnboxed# bytes# o# s1# of
            (# s2#, es #) -> case readUnboxed# bytes# (o# +# r#) s2# of
              (# s3#, e #) -> (# s3#, (es `asTypeOf` t) :& e #)
    
    writeUnboxed# bytes# i# (es :& e) = let r# = rank# es; o# = i#*#r# +# i# in
      \ s1# -> case writeUnboxed# bytes# o# es s1# of
        s2# -> writeUnboxed# bytes# (o# +# r#) e s2#
    
    newUnboxed e n# = pnewUnboxed e (rank# e *# n#)

--------------------------------------------------------------------------------

{- Tuple instances. -}

instance Unboxed ()
  where
    filler = ()
    
    eqUnboxed# _ _ _ _ _ _ = True
    
    {-# INLINE sizeof# #-}
    sizeof# _ _ = 0#
    
    {-# INLINE (!#) #-}
    readUnboxed# = \ _ _ s# -> (# s#, () #)
    (!#)  = \ _ _ -> ()
    
    newUnboxed  _ _ = newByteArray# 0#
    newUnboxed' _ _ = newByteArray# 0#
    
    writeUnboxed#  _ _ = \ _ s# -> s#
    fillByteArray# _ _ = \ _ s# -> s#

instance Unboxed e => Unboxed (T2 e)
  where
    sizeof  e2 n  = psizeof  e2 (2  *   n)
    sizeof# e2 n# = psizeof# e2 (2# *# n#)
    
    eqUnboxed# e xs# o1# ys# o2# n# = peqUnboxed e xs# (o1# *# 2#) ys# (o2# *# 2#) (n# *# 2#)
    
    bytes# !# n# = let o# = 2# *# n# in (bytes# !# o#, bytes# !# (o#+#1#))
    
    readUnboxed# bytes# n# = let o# = 2# *# n# in
      \ s1# -> case readUnboxed# bytes# o# s1# of
        (# s2#, e1 #) -> case readUnboxed# bytes# (o# +# 1#) s2# of
          (# s3#, e2 #) -> (# s3#, (e1,e2) #)
    
    writeUnboxed# mbytes# n# (e1,e2) = let o# = 2# *# n# in
      \ s1# -> case writeUnboxed# mbytes# o# e1 s1# of
        s2# -> writeUnboxed# mbytes# (o# +# 1#) e2 s2#
    
    newUnboxed e n# = pnewUnboxed e (2# *# n#)

instance Unboxed e => Unboxed (T3 e)
  where
    sizeof  e2 n  = psizeof  e2 (3  *   n)
    sizeof# e2 n# = psizeof# e2 (3# *# n#)
    
    eqUnboxed# e xs# o1# ys# o2# n# = peqUnboxed e xs# (o1# *# 3#) ys# (o2# *# 3#) (n# *# 3#)
    
    bytes# !# n# =
      let o# = 3# *# n#
      in  (bytes# !# o#, bytes# !# (o#+#1#), bytes# !# (o#+#2#))
    
    readUnboxed# bytes# n# = let o# = 3# *# n# in
      \ s1# -> case readUnboxed# bytes# o# s1# of
        (# s2#, e1 #) -> case readUnboxed# bytes# (o# +# 1#) s2# of
          (# s3#, e2 #) -> case readUnboxed# bytes# (o# +# 2#) s3# of
            (# s4#, e3 #) -> (# s4#, (e1,e2,e3) #)
    
    writeUnboxed# mbytes# n# (e1,e2,e3) = let o# = 3# *# n# in
      \ s1# -> case writeUnboxed# mbytes# o# e1 s1# of
        s2# -> case writeUnboxed# mbytes# (o# +# 1#) e2 s2# of
          s3# -> writeUnboxed# mbytes# (o# +# 2#) e3 s3#
    
    newUnboxed e n# = pnewUnboxed e (3# *# n#)

instance Unboxed e => Unboxed (T4 e)
  where
    sizeof  e2 n  = psizeof  e2 (4  *   n)
    sizeof# e2 n# = psizeof# e2 (4# *# n#)
    
    eqUnboxed# e xs# o1# ys# o2# n# = peqUnboxed e xs# (o1# *# 4#) ys# (o2# *# 4#) (n# *# 4#)
    
    bytes# !# n# =
      let o# = 4# *# n#
      in  (bytes# !# o#, bytes# !# (o#+#1#), bytes# !# (o#+#2#), bytes# !# (o#+#3#))
    
    readUnboxed# bytes# n# = let o# = 4# *# n# in
      \ s1# -> case readUnboxed# bytes# o# s1# of
        (# s2#, e1 #) -> case readUnboxed# bytes# (o# +# 1#) s2# of
          (# s3#, e2 #) -> case readUnboxed# bytes# (o# +# 2#) s3# of
            (# s4#, e3 #) -> case readUnboxed# bytes# (o# +# 3#) s4# of
              (# s5#, e4 #) -> (# s5#, (e1,e2,e3,e4) #)
    
    writeUnboxed# mbytes# n# (e1,e2,e3,e4) = let o# = 4# *# n# in
      \ s1# -> case writeUnboxed# mbytes# o# e1 s1# of
        s2# -> case writeUnboxed# mbytes# (o# +# 1#) e2 s2# of
          s3# -> case writeUnboxed# mbytes# (o# +# 2#) e3 s3# of
            s4# -> writeUnboxed# mbytes# (o# +# 3#) e4 s4#
    
    newUnboxed e n# = pnewUnboxed e (4# *# n#)

instance Unboxed e => Unboxed (T5 e)
  where
    sizeof  e2 n  = psizeof  e2 (5  *   n)
    sizeof# e2 n# = psizeof# e2 (5# *# n#)
    
    eqUnboxed# e xs# o1# ys# o2# n# = peqUnboxed e xs# (o1# *# 5#) ys# (o2# *# 5#) (n# *# 5#)
    
    bytes# !# n# =
      let o# = 5# *# n#
      in
        (
          bytes# !#       o#, bytes# !# (o#+#1#), bytes# !# (o#+#2#),
          bytes# !# (o#+#3#), bytes# !# (o#+#4#)
        )
    
    readUnboxed# bytes# n# = let o# = 5# *# n# in
      \ s1# -> case readUnboxed# bytes# o# s1# of
        (# s2#, e1 #) -> case readUnboxed# bytes# (o# +# 1#) s2# of
          (# s3#, e2 #) -> case readUnboxed# bytes# (o# +# 2#) s3# of
            (# s4#, e3 #) -> case readUnboxed# bytes# (o# +# 3#) s4# of
              (# s5#, e4 #) -> case readUnboxed# bytes# (o# +# 4#) s5# of
                (# s6#, e5 #) -> (# s6#, (e1,e2,e3,e4,e5) #)
    
    writeUnboxed# mbytes# n# (e1,e2,e3,e4,e5) = let o# = 5# *# n# in
      \ s1# -> case writeUnboxed# mbytes# o# e1 s1# of
        s2# -> case writeUnboxed# mbytes# (o# +# 1#) e2 s2# of
          s3# -> case writeUnboxed# mbytes# (o# +# 2#) e3 s3# of
            s4# -> case writeUnboxed# mbytes# (o# +# 3#) e4 s4# of
              s5# -> writeUnboxed# mbytes# (o# +# 4#) e5 s5#
    
    newUnboxed e n# = pnewUnboxed e (5# *# n#)

instance Unboxed e => Unboxed (T6 e)
  where
    sizeof  e2 n  = psizeof  e2 (6  *   n)
    sizeof# e2 n# = psizeof# e2 (6# *# n#)
    
    eqUnboxed# e xs# o1# ys# o2# n# = peqUnboxed e xs# (o1# *# 6#) ys# (o2# *# 6#) (n# *# 6#)
    
    bytes# !# n# =
      let o# = 6# *# n#
      in
        (
          bytes# !#       o#, bytes# !# (o#+#1#), bytes# !# (o#+#2#),
          bytes# !# (o#+#3#), bytes# !# (o#+#4#), bytes# !# (o#+#5#)
        )
    
    readUnboxed# bytes# n# = let o# = 6# *# n# in
      \ s1# -> case readUnboxed# bytes# o# s1# of
        (# s2#, e1 #) -> case readUnboxed# bytes# (o# +# 1#) s2# of
          (# s3#, e2 #) -> case readUnboxed# bytes# (o# +# 2#) s3# of
            (# s4#, e3 #) -> case readUnboxed# bytes# (o# +# 3#) s4# of
              (# s5#, e4 #) -> case readUnboxed# bytes# (o# +# 4#) s5# of
                (# s6#, e5 #) -> case readUnboxed# bytes# (o# +# 5#) s6# of
                  (# s7#, e6 #) -> (# s7#, (e1,e2,e3,e4,e5,e6) #)
    
    writeUnboxed# mbytes# n# (e1,e2,e3,e4,e5,e6) = let o# = 6# *# n# in
      \ s1# -> case writeUnboxed# mbytes# o# e1 s1# of
        s2# -> case writeUnboxed# mbytes# (o# +# 1#) e2 s2# of
          s3# -> case writeUnboxed# mbytes# (o# +# 2#) e3 s3# of
            s4# -> case writeUnboxed# mbytes# (o# +# 3#) e4 s4# of
              s5# -> case writeUnboxed# mbytes# (o# +# 4#) e5 s5# of
                s6# -> writeUnboxed# mbytes# (o# +# 5#) e6 s6#
    
    newUnboxed e n# = pnewUnboxed e (6# *# n#)

instance Unboxed e => Unboxed (T7 e)
  where
    sizeof  e2 n  = psizeof  e2 (7  *   n)
    sizeof# e2 n# = psizeof# e2 (7# *# n#)
    
    eqUnboxed# e xs# o1# ys# o2# n# = peqUnboxed e xs# (o1# *# 7#) ys# (o2# *# 7#) (n# *# 7#)
    
    bytes# !# n# =
      let o# = 7# *# n#
      in
        (
          bytes# !#       o#, bytes# !# (o#+#1#), bytes# !# (o#+#2#),
          bytes# !# (o#+#3#), bytes# !# (o#+#4#), bytes# !# (o#+#5#),
          bytes# !# (o#+#6#)
        )
    
    readUnboxed# bytes# n# = let o# = 7# *# n# in
      \ s1# -> case readUnboxed# bytes# o# s1# of
        (# s2#, e1 #) -> case readUnboxed# bytes# (o# +# 1#) s2# of
          (# s3#, e2 #) -> case readUnboxed# bytes# (o# +# 2#) s3# of
            (# s4#, e3 #) -> case readUnboxed# bytes# (o# +# 3#) s4# of
              (# s5#, e4 #) -> case readUnboxed# bytes# (o# +# 4#) s5# of
                (# s6#, e5 #) -> case readUnboxed# bytes# (o# +# 5#) s6# of
                  (# s7#, e6 #) -> case readUnboxed# bytes# (o# +# 6#) s7# of
                    (# s8#, e7 #) -> (# s8#, (e1,e2,e3,e4,e5,e6,e7) #)
    
    writeUnboxed# mbytes# n# (e1,e2,e3,e4,e5,e6,e7) = let o# = 7# *# n# in
      \ s1# -> case writeUnboxed# mbytes# o# e1 s1# of
        s2# -> case writeUnboxed# mbytes# (o# +# 1#) e2 s2# of
          s3# -> case writeUnboxed# mbytes# (o# +# 2#) e3 s3# of
            s4# -> case writeUnboxed# mbytes# (o# +# 3#) e4 s4# of
              s5# -> case writeUnboxed# mbytes# (o# +# 4#) e5 s5# of
                s6# -> case writeUnboxed# mbytes# (o# +# 5#) e6 s6# of
                  s7# -> writeUnboxed# mbytes# (o# +# 6#) e7 s7#
    
    newUnboxed e n# = pnewUnboxed e (7# *# n#)

instance Unboxed e => Unboxed (T8 e)
  where
    sizeof  e2 n  = psizeof  e2 (8  *   n)
    sizeof# e2 n# = psizeof# e2 (8# *# n#)
    
    eqUnboxed# e xs# o1# ys# o2# n# = peqUnboxed e xs# (o1# *# 8#) ys# (o2# *# 8#) (n# *# 8#)
    
    bytes# !# n# =
      let o# = 8# *# n#
      in
        (
          bytes# !#       o#, bytes# !# (o#+#1#), bytes# !# (o#+#2#),
          bytes# !# (o#+#3#), bytes# !# (o#+#4#), bytes# !# (o#+#5#),
          bytes# !# (o#+#6#), bytes# !# (o#+#7#)
        )
    
    readUnboxed# bytes# n# = let o# = 8# *# n# in
      \ s1# -> case readUnboxed# bytes# o# s1# of
        (# s2#, e1 #) -> case readUnboxed# bytes# (o# +# 1#) s2# of
          (# s3#, e2 #) -> case readUnboxed# bytes# (o# +# 2#) s3# of
            (# s4#, e3 #) -> case readUnboxed# bytes# (o# +# 3#) s4# of
              (# s5#, e4 #) -> case readUnboxed# bytes# (o# +# 4#) s5# of
                (# s6#, e5 #) -> case readUnboxed# bytes# (o# +# 5#) s6# of
                  (# s7#, e6 #) -> case readUnboxed# bytes# (o# +# 6#) s7# of
                    (# s8#, e7 #) -> case readUnboxed# bytes# (o# +# 7#) s8# of
                      (# s9#, e8 #) -> (# s9#, (e1,e2,e3,e4,e5,e6,e7,e8) #)
    
    writeUnboxed# mbytes# n# (e1,e2,e3,e4,e5,e6,e7,e8) = let o# = 8# *# n# in
      \ s1# -> case writeUnboxed# mbytes# o# e1 s1# of
        s2# -> case writeUnboxed# mbytes# (o# +# 1#) e2 s2# of
          s3# -> case writeUnboxed# mbytes# (o# +# 2#) e3 s3# of
            s4# -> case writeUnboxed# mbytes# (o# +# 3#) e4 s4# of
              s5# -> case writeUnboxed# mbytes# (o# +# 4#) e5 s5# of
                s6# -> case writeUnboxed# mbytes# (o# +# 5#) e6 s6# of
                  s7# -> case writeUnboxed# mbytes# (o# +# 6#) e7 s7# of
                    s8# -> writeUnboxed# mbytes# (o# +# 7#) e8 s8#
    
    newUnboxed e n# = pnewUnboxed e (8# *# n#)

instance Unboxed e => Unboxed (T9 e)
  where
    sizeof  e2 n  = psizeof  e2 (9  *   n)
    sizeof# e2 n# = psizeof# e2 (9# *# n#)
    
    eqUnboxed# e xs# o1# ys# o2# n# = peqUnboxed e xs# (o1# *# 9#) ys# (o2# *# 9#) (n# *# 9#)
    
    bytes# !# n# =
      let o# = 9# *# n#
      in
        (
          bytes# !#       o#, bytes# !# (o#+#1#), bytes# !# (o#+#2#),
          bytes# !# (o#+#3#), bytes# !# (o#+#4#), bytes# !# (o#+#5#),
          bytes# !# (o#+#6#), bytes# !# (o#+#7#), bytes# !# (o#+#8#)
        )
    
    readUnboxed# bytes# n# = let o# = 9# *# n# in
      \ s1# -> case readUnboxed# bytes# o# s1# of
        (# s2#, e1 #) -> case readUnboxed# bytes# (o# +# 1#) s2# of
          (# s3#, e2 #) -> case readUnboxed# bytes# (o# +# 2#) s3# of
            (# s4#, e3 #) -> case readUnboxed# bytes# (o# +# 3#) s4# of
              (# s5#, e4 #) -> case readUnboxed# bytes# (o# +# 4#) s5# of
                (# s6#, e5 #) -> case readUnboxed# bytes# (o# +# 5#) s6# of
                  (# s7#, e6 #) -> case readUnboxed# bytes# (o# +# 6#) s7# of
                    (# s8#, e7 #) -> case readUnboxed# bytes# (o# +# 7#) s8# of
                      (# s9#, e8 #) -> case readUnboxed# bytes# (o# +# 8#) s9# of
                        (# s10#, e9 #) -> (# s10#, (e1,e2,e3,e4,e5,e6,e7,e8,e9) #)
    
    writeUnboxed# mbytes# n# (e1,e2,e3,e4,e5,e6,e7,e8,e9) = let o# = 9# *# n# in
      \ s1# -> case writeUnboxed# mbytes# o# e1 s1# of
        s2# -> case writeUnboxed# mbytes# (o# +# 1#) e2 s2# of
          s3# -> case writeUnboxed# mbytes# (o# +# 2#) e3 s3# of
            s4# -> case writeUnboxed# mbytes# (o# +# 3#) e4 s4# of
              s5# -> case writeUnboxed# mbytes# (o# +# 4#) e5 s5# of
                s6# -> case writeUnboxed# mbytes# (o# +# 5#) e6 s6# of
                  s7# -> case writeUnboxed# mbytes# (o# +# 6#) e7 s7# of
                    s8# -> case writeUnboxed# mbytes# (o# +# 7#) e8 s8# of
                      s9# -> writeUnboxed# mbytes# (o# +# 8#) e9 s9#
    
    newUnboxed e n# = pnewUnboxed e (9# *# n#)

instance Unboxed e => Unboxed (T10 e)
  where
    sizeof  e2 n  = psizeof  e2 (10  *   n)
    sizeof# e2 n# = psizeof# e2 (10# *# n#)
    
    eqUnboxed# e xs# o1# ys# o2# n# = peqUnboxed e xs# (o1# *# 10#) ys# (o2# *# 10#) (n# *# 10#)
    
    bytes# !# n# =
      let o# = 10# *# n#
      in
        (
          bytes# !#       o#, bytes# !# (o#+#1#), bytes# !# (o#+#2#),
          bytes# !# (o#+#3#), bytes# !# (o#+#4#), bytes# !# (o#+#5#),
          bytes# !# (o#+#6#), bytes# !# (o#+#7#), bytes# !# (o#+#8#),
          bytes# !# (o#+#9#)
        )
    
    readUnboxed# bytes# n# = let o# = 10# *# n# in
      \ s1# -> case readUnboxed# bytes# o# s1# of
        (# s2#, e1 #) -> case readUnboxed# bytes# (o# +# 1#) s2# of
          (# s3#, e2 #) -> case readUnboxed# bytes# (o# +# 2#) s3# of
            (# s4#, e3 #) -> case readUnboxed# bytes# (o# +# 3#) s4# of
              (# s5#, e4 #) -> case readUnboxed# bytes# (o# +# 4#) s5# of
                (# s6#, e5 #) -> case readUnboxed# bytes# (o# +# 5#) s6# of
                  (# s7#, e6 #) -> case readUnboxed# bytes# (o# +# 6#) s7# of
                    (# s8#, e7 #) -> case readUnboxed# bytes# (o# +# 7#) s8# of
                      (# s9#, e8 #) -> case readUnboxed# bytes# (o# +# 8#) s9# of
                        (# s10#, e9 #) -> case readUnboxed# bytes# (o# +# 9#) s10# of
                          (# s11#, e10 #) -> (# s11#, (e1,e2,e3,e4,e5,e6,e7,e8,e9,e10) #)
    
    writeUnboxed# mbytes# n# (e1,e2,e3,e4,e5,e6,e7,e8,e9,e10) = let o# = 10# *# n# in
      \ s1# -> case writeUnboxed# mbytes# o# e1 s1# of
        s2# -> case writeUnboxed# mbytes# (o# +# 1#) e2 s2# of
          s3# -> case writeUnboxed# mbytes# (o# +# 2#) e3 s3# of
            s4# -> case writeUnboxed# mbytes# (o# +# 3#) e4 s4# of
              s5# -> case writeUnboxed# mbytes# (o# +# 4#) e5 s5# of
                s6# -> case writeUnboxed# mbytes# (o# +# 5#) e6 s6# of
                  s7# -> case writeUnboxed# mbytes# (o# +# 6#) e7 s7# of
                    s8# -> case writeUnboxed# mbytes# (o# +# 7#) e8 s8# of
                      s9# -> case writeUnboxed# mbytes# (o# +# 8#) e9 s9# of
                        s10# -> writeUnboxed# mbytes# (o# +# 9#) e10 s10#
    
    newUnboxed e n# = pnewUnboxed e (10# *# n#)

instance Unboxed e => Unboxed (T11 e)
  where
    sizeof  e2 n  = psizeof  e2 (11  *   n)
    sizeof# e2 n# = psizeof# e2 (11# *# n#)
    
    eqUnboxed# e xs# o1# ys# o2# n# = peqUnboxed e xs# (o1# *# 11#) ys# (o2# *# 11#) (n# *# 11#)
    
    bytes# !# n# =
      let o# = 11# *# n#
      in
        (
          bytes# !#       o#, bytes# !# (o#+#1#), bytes# !# (o#+#2#),
          bytes# !# (o#+#3#), bytes# !# (o#+#4#), bytes# !# (o#+#5#),
          bytes# !# (o#+#6#), bytes# !# (o#+#7#), bytes# !# (o#+#8#),
          bytes# !# (o#+#9#), bytes# !# (o#+#10#)
        )
    
    readUnboxed# bytes# n# = let o# = 11# *# n# in
      \ s1# -> case readUnboxed# bytes# o# s1# of
        (# s2#, e1 #) -> case readUnboxed# bytes# (o# +# 1#) s2# of
          (# s3#, e2 #) -> case readUnboxed# bytes# (o# +# 2#) s3# of
            (# s4#, e3 #) -> case readUnboxed# bytes# (o# +# 3#) s4# of
              (# s5#, e4 #) -> case readUnboxed# bytes# (o# +# 4#) s5# of
                (# s6#, e5 #) -> case readUnboxed# bytes# (o# +# 5#) s6# of
                  (# s7#, e6 #) -> case readUnboxed# bytes# (o# +# 6#) s7# of
                    (# s8#, e7 #) -> case readUnboxed# bytes# (o# +# 7#) s8# of
                      (# s9#, e8 #) -> case readUnboxed# bytes# (o# +# 8#) s9# of
                        (# s10#, e9 #) -> case readUnboxed# bytes# (o# +# 9#) s10# of
                          (# s11#, e10 #) -> case readUnboxed# bytes# (o# +# 10#) s11# of
                            (# s12#, e11 #) -> (# s12#, (e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11) #)
    
    writeUnboxed# mbytes# n# (e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11) = let o# = 11# *# n# in
      \ s1# -> case writeUnboxed# mbytes# o# e1 s1# of
        s2# -> case writeUnboxed# mbytes# (o# +# 1#) e2 s2# of
          s3# -> case writeUnboxed# mbytes# (o# +# 2#) e3 s3# of
            s4# -> case writeUnboxed# mbytes# (o# +# 3#) e4 s4# of
              s5# -> case writeUnboxed# mbytes# (o# +# 4#) e5 s5# of
                s6# -> case writeUnboxed# mbytes# (o# +# 5#) e6 s6# of
                  s7# -> case writeUnboxed# mbytes# (o# +# 6#) e7 s7# of
                    s8# -> case writeUnboxed# mbytes# (o# +# 7#) e8 s8# of
                      s9# -> case writeUnboxed# mbytes# (o# +# 8#) e9 s9# of
                        s10# -> case writeUnboxed# mbytes# (o# +# 9#) e10 s10# of
                          s11# -> writeUnboxed# mbytes# (o# +# 10#) e11 s11#
    
    newUnboxed e n# = pnewUnboxed e (11# *# n#)

instance Unboxed e => Unboxed (T12 e)
  where
    sizeof  e2 n  = psizeof  e2 (12  *   n)
    sizeof# e2 n# = psizeof# e2 (12# *# n#)
    
    eqUnboxed# e xs# o1# ys# o2# n# = peqUnboxed e xs# (o1# *# 12#) ys# (o2# *# 12#) (n# *# 12#)
    
    bytes# !# n# =
      let o# = 12# *# n#
      in
        (
          bytes# !#       o#, bytes# !#  (o#+#1#), bytes# !#  (o#+#2#),
          bytes# !# (o#+#3#), bytes# !#  (o#+#4#), bytes# !#  (o#+#5#),
          bytes# !# (o#+#6#), bytes# !#  (o#+#7#), bytes# !#  (o#+#8#),
          bytes# !# (o#+#9#), bytes# !# (o#+#10#), bytes# !# (o#+#11#)
        )
    
    readUnboxed# bytes# n# = let o# = 12# *# n# in
      \ s1# -> case readUnboxed# bytes# o# s1# of
        (# s2#, e1 #) -> case readUnboxed# bytes# (o# +# 1#) s2# of
          (# s3#, e2 #) -> case readUnboxed# bytes# (o# +# 2#) s3# of
            (# s4#, e3 #) -> case readUnboxed# bytes# (o# +# 3#) s4# of
              (# s5#, e4 #) -> case readUnboxed# bytes# (o# +# 4#) s5# of
                (# s6#, e5 #) -> case readUnboxed# bytes# (o# +# 5#) s6# of
                  (# s7#, e6 #) -> case readUnboxed# bytes# (o# +# 6#) s7# of
                    (# s8#, e7 #) -> case readUnboxed# bytes# (o# +# 7#) s8# of
                      (# s9#, e8 #) -> case readUnboxed# bytes# (o# +# 8#) s9# of
                        (# s10#, e9 #) -> case readUnboxed# bytes# (o# +# 9#) s10# of
                          (# s11#, e10 #) -> case readUnboxed# bytes# (o# +# 10#) s11# of
                            (# s12#, e11 #) -> case readUnboxed# bytes# (o# +# 11#) s12# of
                              (# s13#, e12 #) -> (# s13#, (e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12) #)
    
    writeUnboxed# mbytes# n# (e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12) = let o# = 12# *# n# in
      \ s1# -> case writeUnboxed# mbytes# o# e1 s1# of
        s2# -> case writeUnboxed# mbytes# (o# +# 1#) e2 s2# of
          s3# -> case writeUnboxed# mbytes# (o# +# 2#) e3 s3# of
            s4# -> case writeUnboxed# mbytes# (o# +# 3#) e4 s4# of
              s5# -> case writeUnboxed# mbytes# (o# +# 4#) e5 s5# of
                s6# -> case writeUnboxed# mbytes# (o# +# 5#) e6 s6# of
                  s7# -> case writeUnboxed# mbytes# (o# +# 6#) e7 s7# of
                    s8# -> case writeUnboxed# mbytes# (o# +# 7#) e8 s8# of
                      s9# -> case writeUnboxed# mbytes# (o# +# 8#) e9 s9# of
                        s10# -> case writeUnboxed# mbytes# (o# +# 9#) e10 s10# of
                          s11# -> case writeUnboxed# mbytes# (o# +# 10#) e11 s11# of
                            s12# -> writeUnboxed# mbytes# (o# +# 11#) e12 s12#
    
    newUnboxed e n# = pnewUnboxed e (12# *# n#)

instance Unboxed e => Unboxed (T13 e)
  where
    sizeof  e2 n  = psizeof  e2 (13  *   n)
    sizeof# e2 n# = psizeof# e2 (13# *# n#)
    
    eqUnboxed# e xs# o1# ys# o2# n# = peqUnboxed e xs# (o1# *# 13#) ys# (o2# *# 13#) (n# *# 13#)
    
    bytes# !# n# =
      let o# = 13# *# n#
      in
        (
          bytes# !#        o#, bytes# !#  (o#+#1#), bytes# !#  (o#+#2#),
          bytes# !#  (o#+#3#), bytes# !#  (o#+#4#), bytes# !#  (o#+#5#),
          bytes# !#  (o#+#6#), bytes# !#  (o#+#7#), bytes# !#  (o#+#8#),
          bytes# !#  (o#+#9#), bytes# !# (o#+#10#), bytes# !# (o#+#11#),
          bytes# !# (o#+#12#)
        )
    
    readUnboxed# bytes# n# = let o# = 13# *# n# in
      \ s1# -> case readUnboxed# bytes# o# s1# of
        (# s2#, e1 #) -> case readUnboxed# bytes# (o# +# 1#) s2# of
          (# s3#, e2 #) -> case readUnboxed# bytes# (o# +# 2#) s3# of
            (# s4#, e3 #) -> case readUnboxed# bytes# (o# +# 3#) s4# of
              (# s5#, e4 #) -> case readUnboxed# bytes# (o# +# 4#) s5# of
                (# s6#, e5 #) -> case readUnboxed# bytes# (o# +# 5#) s6# of
                  (# s7#, e6 #) -> case readUnboxed# bytes# (o# +# 6#) s7# of
                    (# s8#, e7 #) -> case readUnboxed# bytes# (o# +# 7#) s8# of
                      (# s9#, e8 #) -> case readUnboxed# bytes# (o# +# 8#) s9# of
                        (# s10#, e9 #) -> case readUnboxed# bytes# (o# +# 9#) s10# of
                          (# s11#, e10 #) -> case readUnboxed# bytes# (o# +# 10#) s11# of
                            (# s12#, e11 #) -> case readUnboxed# bytes# (o# +# 11#) s12# of
                              (# s13#, e12 #) -> case readUnboxed# bytes# (o# +# 12#) s13# of
                                (# s14#, e13 #) -> (# s14#, (e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13) #)
    
    writeUnboxed# mbytes# n# (e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13) = let o# = 13# *# n# in
      \ s1# -> case writeUnboxed# mbytes# o# e1 s1# of
        s2# -> case writeUnboxed# mbytes# (o# +# 1#) e2 s2# of
          s3# -> case writeUnboxed# mbytes# (o# +# 2#) e3 s3# of
            s4# -> case writeUnboxed# mbytes# (o# +# 3#) e4 s4# of
              s5# -> case writeUnboxed# mbytes# (o# +# 4#) e5 s5# of
                s6# -> case writeUnboxed# mbytes# (o# +# 5#) e6 s6# of
                  s7# -> case writeUnboxed# mbytes# (o# +# 6#) e7 s7# of
                    s8# -> case writeUnboxed# mbytes# (o# +# 7#) e8 s8# of
                      s9# -> case writeUnboxed# mbytes# (o# +# 8#) e9 s9# of
                        s10# -> case writeUnboxed# mbytes# (o# +# 9#) e10 s10# of
                          s11# -> case writeUnboxed# mbytes# (o# +# 10#) e11 s11# of
                            s12# -> case writeUnboxed# mbytes# (o# +# 11#) e12 s12# of
                              s13# -> writeUnboxed# mbytes# (o# +# 12#) e13 s13#
    
    newUnboxed e n# = pnewUnboxed e (13# *# n#)

instance Unboxed e => Unboxed (T14 e)
  where
    sizeof  e2 n  = psizeof  e2 (14  *   n)
    sizeof# e2 n# = psizeof# e2 (14# *# n#)
    
    eqUnboxed# e xs# o1# ys# o2# n# = peqUnboxed e xs# (o1# *# 14#) ys# (o2# *# 14#) (n# *# 14#)
    
    bytes# !# n# =
      let o# = 14# *# n#
      in
        (
          bytes# !#        o#, bytes# !#  (o#+#1#), bytes# !#  (o#+#2#),
          bytes# !#  (o#+#3#), bytes# !#  (o#+#4#), bytes# !#  (o#+#5#),
          bytes# !#  (o#+#6#), bytes# !#  (o#+#7#), bytes# !#  (o#+#8#),
          bytes# !#  (o#+#9#), bytes# !# (o#+#10#), bytes# !# (o#+#11#),
          bytes# !# (o#+#12#), bytes# !# (o#+#13#)
        )
    
    readUnboxed# bytes# n# = let o# = 14# *# n# in
      \ s1# -> case readUnboxed# bytes# o# s1# of
        (# s2#, e1 #) -> case readUnboxed# bytes# (o# +# 1#) s2# of
          (# s3#, e2 #) -> case readUnboxed# bytes# (o# +# 2#) s3# of
            (# s4#, e3 #) -> case readUnboxed# bytes# (o# +# 3#) s4# of
              (# s5#, e4 #) -> case readUnboxed# bytes# (o# +# 4#) s5# of
                (# s6#, e5 #) -> case readUnboxed# bytes# (o# +# 5#) s6# of
                  (# s7#, e6 #) -> case readUnboxed# bytes# (o# +# 6#) s7# of
                    (# s8#, e7 #) -> case readUnboxed# bytes# (o# +# 7#) s8# of
                      (# s9#, e8 #) -> case readUnboxed# bytes# (o# +# 8#) s9# of
                        (# s10#, e9 #) -> case readUnboxed# bytes# (o# +# 9#) s10# of
                          (# s11#, e10 #) -> case readUnboxed# bytes# (o# +# 10#) s11# of
                            (# s12#, e11 #) -> case readUnboxed# bytes# (o# +# 11#) s12# of
                              (# s13#, e12 #) -> case readUnboxed# bytes# (o# +# 12#) s13# of
                                (# s14#, e13 #) -> case readUnboxed# bytes# (o# +# 13#) s14# of
                                  (# s15#, e14 #) -> (# s15#, (e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14) #)
    
    writeUnboxed# mbytes# n# (e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14) =
      \ s1# -> let o# = 14# *# n# in case writeUnboxed# mbytes# o# e1 s1# of
        s2# -> case writeUnboxed# mbytes# (o# +# 1#) e2 s2# of
          s3# -> case writeUnboxed# mbytes# (o# +# 2#) e3 s3# of
            s4# -> case writeUnboxed# mbytes# (o# +# 3#) e4 s4# of
              s5# -> case writeUnboxed# mbytes# (o# +# 4#) e5 s5# of
                s6# -> case writeUnboxed# mbytes# (o# +# 5#) e6 s6# of
                  s7# -> case writeUnboxed# mbytes# (o# +# 6#) e7 s7# of
                    s8# -> case writeUnboxed# mbytes# (o# +# 7#) e8 s8# of
                      s9# -> case writeUnboxed# mbytes# (o# +# 8#) e9 s9# of
                        s10# -> case writeUnboxed# mbytes# (o# +# 9#) e10 s10# of
                          s11# -> case writeUnboxed# mbytes# (o# +# 10#) e11 s11# of
                            s12# -> case writeUnboxed# mbytes# (o# +# 11#) e12 s12# of
                              s13# -> case writeUnboxed# mbytes# (o# +# 12#) e13 s13# of
                                s14# -> writeUnboxed# mbytes# (o# +# 13#) e14 s14#
    
    newUnboxed e n# = pnewUnboxed e (14# *# n#)

instance Unboxed e => Unboxed (T15 e)
  where
    sizeof  e2 n  = psizeof  e2 (15  *   n)
    sizeof# e2 n# = psizeof# e2 (15# *# n#)
    
    eqUnboxed# e xs# o1# ys# o2# n# = peqUnboxed e xs# (o1# *# 15#) ys# (o2# *# 15#) (n# *# 15#)
    
    bytes# !# n# =
      let o# = 15# *# n#
      in
        (
          bytes# !#        o#, bytes# !#  (o#+#1#), bytes# !#  (o#+#2#),
          bytes# !#  (o#+#3#), bytes# !#  (o#+#4#), bytes# !#  (o#+#5#),
          bytes# !#  (o#+#6#), bytes# !#  (o#+#7#), bytes# !#  (o#+#8#),
          bytes# !#  (o#+#9#), bytes# !# (o#+#10#), bytes# !# (o#+#11#),
          bytes# !# (o#+#12#), bytes# !# (o#+#13#), bytes# !# (o#+#14#)
        )
    
    readUnboxed# bytes# n# = let o# = 15# *# n# in
      \ s1# -> case readUnboxed# bytes# o# s1# of
        (# s2#, e1 #) -> case readUnboxed# bytes# (o# +# 1#) s2# of
          (# s3#, e2 #) -> case readUnboxed# bytes# (o# +# 2#) s3# of
            (# s4#, e3 #) -> case readUnboxed# bytes# (o# +# 3#) s4# of
              (# s5#, e4 #) -> case readUnboxed# bytes# (o# +# 4#) s5# of
                (# s6#, e5 #) -> case readUnboxed# bytes# (o# +# 5#) s6# of
                  (# s7#, e6 #) -> case readUnboxed# bytes# (o# +# 6#) s7# of
                    (# s8#, e7 #) -> case readUnboxed# bytes# (o# +# 7#) s8# of
                      (# s9#, e8 #) -> case readUnboxed# bytes# (o# +# 8#) s9# of
                        (# s10#, e9 #) -> case readUnboxed# bytes# (o# +# 9#) s10# of
                          (# s11#, e10 #) -> case readUnboxed# bytes# (o# +# 10#) s11# of
                            (# s12#, e11 #) -> case readUnboxed# bytes# (o# +# 11#) s12# of
                              (# s13#, e12 #) -> case readUnboxed# bytes# (o# +# 12#) s13# of
                                (# s14#, e13 #) -> case readUnboxed# bytes# (o# +# 13#) s14# of
                                  (# s15#, e14 #) -> case readUnboxed# bytes# (o# +# 14#) s15# of
                                    (# s16#, e15 #) -> (# s16#, (e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15) #)
    
    writeUnboxed# mbytes# n# (e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15) =
      \ s1# -> let o# = 15# *# n# in case writeUnboxed# mbytes# o# e1 s1# of
        s2# -> case writeUnboxed# mbytes# (o# +# 1#) e2 s2# of
          s3# -> case writeUnboxed# mbytes# (o# +# 2#) e3 s3# of
            s4# -> case writeUnboxed# mbytes# (o# +# 3#) e4 s4# of
              s5# -> case writeUnboxed# mbytes# (o# +# 4#) e5 s5# of
                s6# -> case writeUnboxed# mbytes# (o# +# 5#) e6 s6# of
                  s7# -> case writeUnboxed# mbytes# (o# +# 6#) e7 s7# of
                    s8# -> case writeUnboxed# mbytes# (o# +# 7#) e8 s8# of
                      s9# -> case writeUnboxed# mbytes# (o# +# 8#) e9 s9# of
                        s10# -> case writeUnboxed# mbytes# (o# +# 9#) e10 s10# of
                          s11# -> case writeUnboxed# mbytes# (o# +# 10#) e11 s11# of
                            s12# -> case writeUnboxed# mbytes# (o# +# 11#) e12 s12# of
                              s13# -> case writeUnboxed# mbytes# (o# +# 12#) e13 s13# of
                                s14# -> case writeUnboxed# mbytes# (o# +# 13#) e14 s14# of
                                  s15# -> writeUnboxed# mbytes# (o# +# 14#) e15 s15#
    
    newUnboxed e n# = pnewUnboxed e (15# *# n#)

--------------------------------------------------------------------------------

-- | 'ByteArray#' wrapper.
data Wrap = Wrap {unwrap :: ByteArray#}

{- |
  @since 0.2.1
  Wrapped empty 'ByteArray#'.
-}
lzero# :: Wrap
lzero# =  runST $ ST $ \ s1# -> case newByteArray# 0# s1# of
  (# s2#, marr# #) -> case unsafeFreezeByteArray# marr# s2# of
    (# s3#, arr# #) -> (# s3#, Wrap arr# #)

{- |
  @since 0.2.1
  'ByteArray#' singleton.
-}
single# :: Unboxed e => e -> ByteArray#
single# e = unwrap $ runST $ ST $ \ s1# -> case newUnboxed' e 1# s1# of
  (# s2#, marr# #) -> case unsafeFreezeByteArray# marr# s2# of
    (# s3#, arr# #) -> (# s3#, Wrap arr# #)

{- |
  @since 0.2.1
  Create immutable 'Unboxed' array from given list.
-}
fromList# :: Unboxed e => [e] -> ByteArray#
fromList# es = let !(I# n#) = length es in fromListN# n# es

{- |
  @since 0.2.1
  Create immutable 'Unboxed' array from 'Foldable' stream.
-}
fromFoldable# :: (Foldable f, Unboxed e) => f e -> (# Int, ByteArray# #)
fromFoldable# es = unpack' $ runST $ ST $ \ s1# -> case fromFoldableM# es s1# of
    (# s2#, n, marr# #) -> case unsafeFreezeByteArray# marr# s2# of
      (# s3#, arr# #) -> (# s3#, (n, Wrap arr#) #)
  where
    unpack' (i, Wrap arr#) = (# i, arr# #)

{- |
  @since 0.2.1
  Create immutable 'Unboxed' array from known size list.
-}
fromListN# :: Unboxed e => Int# -> [e] -> ByteArray#
fromListN# n# es = unwrap $ runST $ ST $ \ s1# -> case newLinearN# n# es s1# of
  (# s2#, marr# #) -> case unsafeFreezeByteArray# marr# s2# of
    (# s3#, arr# #) -> (# s3#, Wrap arr# #)

{- |
  @since 0.2.1
  Create mutable 'Unboxed' array from given list.
-}
newLinear# :: Unboxed e => [e] -> State# s ->
  (# State# s, MutableByteArray# s #)
newLinear# es = let !(I# n#) = length es in newLinearN# n# es

{- |
  @since 0.2.1
  Create mutable 'Unboxed' array from known size list.
-}
newLinearN# :: Unboxed e => Int# -> [e] -> State# s ->
  (# State# s, MutableByteArray# s #)
newLinearN# c# es = \ s1# -> case pnewUnboxed es n# s1# of
    (# s2#, marr# #) ->
      let
        go y r = \ i# s4# -> case writeUnboxed# marr# i# y s4# of
          s5# -> if isTrue# (i# ==# n# -# 1#) then s5# else r (i# +# 1#) s5#
      in case if n == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2# of
          s3# -> (# s3#, marr# #)
  where
    !n@(I# n#) = max 0 (I# c#)

{- |
  @since 0.2.1
  Create mutable 'Unboxed' array from 'Foldable' stream.
-}
fromFoldableM# :: (Foldable f, Unboxed e) => f e -> State# s ->
  (# State# s, Int, MutableByteArray# s #)
fromFoldableM# es = \ s1# -> case pnewUnboxed es n# s1# of
    (# s2#, marr# #) ->
      let
        go y r = \ i# s4# -> case writeUnboxed# marr# i# y s4# of
          s5# -> if isTrue# (i# ==# n# -# 1#) then s5# else r (i# +# 1#) s5#
      in case if n == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2# of
          s3# -> (# s3#, n, marr# #)
  where
    !n@(I# n#) = length es

{- |
  @since 0.2.1
  Concatenation of two 'Unboxed' arrays.
-}
concat# :: Unboxed e => e ->
  ByteArray# -> Int# -> Int# ->
  ByteArray# -> Int# -> Int# -> State# s ->
  (# State# s, Int#, MutableByteArray# s #)
concat# e arr1# n1# o1# arr2# n2# o2# = \ s1# -> case newUnboxed e n# s1# of
    (# s2#, marr# #) -> case copyUnboxed# e arr1# o1# marr# 0# n1# s2# of
      s3# -> case copyUnboxed# e arr2# o2# marr# n1# n2# s3# of
        s4# -> (# s4#, n#, marr# #)
  where
    n# = n1# +# n2#

-- | Proxy concatenation of two byte arrays representing 'Unboxed' structures.
pconcat :: Unboxed e => proxy e ->
  ByteArray# -> Int# -> Int# -> ByteArray# -> Int# -> Int# ->
  State# s -> (# State# s, Int#, MutableByteArray# s #)
pconcat = concat# . fromProxy

{- |
  @since 0.3
  
  Allocate new mutable byte array and fill it by zero. Note that 'calloc#' use
  'setByteArray#', not 'fillByteArrayOff#' or 'fillByteArray#'.
-}
calloc# :: Unboxed e => e -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
calloc# e n# = let c# = sizeof# e n# in \ s1# -> case newByteArray# c# s1# of
  (# s2#, mbytes# #) -> case setByteArray# mbytes# 0# c# 0# s2# of
    s3# -> (# s3#, mbytes# #)

{-# INLINE defaultFillByteArrayOff# #-}
{- |
  @since 0.3
  
  Default 'fillByteArrayOff#' implementation.
-}
defaultFillByteArrayOff# :: Unboxed e => MutableByteArray# s -> Int# -> Int#
                         -> e -> State# s -> State# s
defaultFillByteArrayOff# bs# c# o# e = go# o#
  where
    go# i# = \ s1# -> if isTrue# (i# >=# n#) then s1# else
      go# (i# +# 1#) (writeByteArray# bs# i# e s1#)
    
    n# = c# +# o#

{- |
  @since 0.3
  
  Default 'eqUnboxed#' implementation.
-}
bytewiseEqUnboxed# :: Unboxed e => e -> ByteArray# -> Int#
                                     -> ByteArray# -> Int# -> Int# -> Bool
bytewiseEqUnboxed# e xs# o1# ys# o2# c# =
  let i1# = sizeof# e o1#; i2# = sizeof# e o2#; n# = sizeof# e c#
  in  case c# <# 0# of {1# -> True; _ -> isTrue# (compareByteArrays# xs# i1# ys# i2# n# ==# 0#)}

--------------------------------------------------------------------------------

{-# INLINE rank# #-}
rank# :: (Shape i) => i -> Int#
rank# i = case rank i of I# r# -> r#

{-# INLINE gcd# #-}
gcd# :: Int# -> Int# -> Int#
gcd# a# 0# = a#
gcd# a# b# = gcd# b# (remInt# a# b#)

{-# INLINE bool_bit #-}
bool_bit :: Int# -> Word#
bool_bit n# = case (SIZEOF_HSWORD * 8 - 1) of
  W# mask# -> int2Word# 1# `uncheckedShiftL#` word2Int# (int2Word# n# `and#` mask#)

{-# INLINE bool_not_bit #-}
bool_not_bit :: Int# -> Word#
bool_not_bit n# = case maxBound of W# mb# -> bool_bit n# `xor#` mb#

{-# INLINE bool_index #-}
bool_index :: Int# -> Int#
#if   SIZEOF_HSWORD == 4
bool_index =  (`uncheckedIShiftRA#` 5#)
#elif SIZEOF_HSWORD == 8
bool_index =  (`uncheckedIShiftRA#` 6#)
#endif

consSizeof :: (a -> b) -> b -> a
consSizeof =  \ _ _ -> undefined



