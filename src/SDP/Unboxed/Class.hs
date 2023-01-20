{-# LANGUAGE Trustworthy, MagicHash, UnboxedTuples, BangPatterns, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE CPP, TypeFamilies #-}

{- |
    Module      :  SDP.Unboxed.Class
    Copyright   :  (c) Andrey Mulik 2019-2023
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Unboxed.Class" provide service class 'Unboxed', that needed for
    "SDP.Prim.SBytes"-based structures.
-}
module SDP.Unboxed.Class
(
  -- * Exports
  module SDP.Proxy,
  
  -- * Unboxed
  Unboxed (..), cloneUnboxed##, concat#, sizeof#, offsetof#,
  bytewiseEqUnboxed##, radixSortUnboxed#, pnewUnboxed, peqUnboxed
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Nullable
import SDP.Finite
import SDP.Shape
import SDP.Ratio
import SDP.Proxy

import SDP.Unboxed.Utils

import GHC.Stable
import GHC.Exts
import GHC.Word
import GHC.Int
import GHC.ST

import Data.Complex

import Foreign.C.Types

#include <ghcautoconf.h>
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
    {-# MINIMAL sizeof##, (filler|newUnboxed), (writeByteArray#|writeUnboxed#),
                (!#), sortUnboxed#, ((!>#)|readUnboxed#|readUnboxedOff#) #-}
    
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
      
      @offsetof## e o#@ returns the index of the element in the unboxed array
      that the offset @o#@ corresponds to (in bytes).
      
      @
        offsetof## e (sizeof## e (8# *# i#)) === 8# *# i#
        
        offsetof##       False   3# === 24#
        offsetof## (0 :: Int32) 12# ===  3#
      @
    -}
    {-# INLINE offsetof## #-}
    offsetof## :: Unboxed e => Proxy# e -> Int# -> Int#
    offsetof## e i# = case i# ># 0# of {1# -> (8# *# i#) `quotInt#` sizeof## e 8#; _ -> 0#}
    
    -- | 'sizeof##' is unboxed 'sizeof'.
    sizeof## :: Proxy# e -> Int# -> Int#
    
    {- |
      @since 0.3
      
      See 'chunkof##'.
    -}
    chunkof## :: Proxy# e -> (# Int#, Int# #)
    chunkof## e =
      let l# = sizeof## e 64# `quotInt#` 8#; d# = gcd# 8# l#
      in  (# quotInt# l# d#, quotInt# 8# d# #)
    
    {- |
      @since 0.3
      
      @eqUnboxed## e xs# ox# ys# oy# n#@ compares two byte @n#@-element arrays
      @xs#@ and @ys#@ beginning from @ox#@ and @oy#@ elements resp.
    -}
    {-# INLINE eqUnboxed## #-}
    eqUnboxed## :: Proxy# e -> ByteArray# -> Int# -> ByteArray# -> Int# -> Int# -> Bool
    eqUnboxed## =  defaulteqUnboxed##
    
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
    readUnboxed# mbytes# i# = go proxy#
      where
        go e = \ s1# -> case readUnboxedOff# mbytes# (sizeof## e i#) s1# of
          (# s2#, res #) -> (# s2#, asProxy## e res #)
    
    {- |
      @sinze 0.3
      
      Unsafe reader for unboxed data (by offset).
    -}
    {-# INLINE readUnboxedOff# #-}
    readUnboxedOff# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, e #)
    readUnboxedOff# mbytes# o# = go proxy#
      where
        go e = \ s1# -> case (!>#) mbytes# (offsetof## e o#) s1# of
          (# s2#, res #) -> (# s2#, asProxy## e res #)
    
    {-# INLINE writeByteArray# #-}
    -- | Unsafe 'MutableByteArray#' writer (by index).
    writeByteArray# :: MutableByteArray# s -> Int# -> e -> State# s -> State# s
    writeByteArray# =  writeUnboxed#
    
    {- |
      @since 0.3
      
      Unsafe unboxed writer (by index).
    -}
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# :: MutableByteArray# s -> Int# -> e -> State# s -> State# s
    writeUnboxed# =  writeByteArray#
    
    {- |
      @since 0.3
      
      Unsafe unboxed writer (by offset).
    -}
    {-# INLINE writeUnboxedOff# #-}
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
    -- | Fill unboxed array by 'filler'.
    fillByteArrayOff# :: MutableByteArray# s -> Int# -> Int# -> e -> State# s -> State# s
    fillByteArrayOff# =  defaultFillByteArrayOff#
    
    {- |
      'newUnboxed' creates new 'MutableByteArray#' of given count of elements.
      First argument used as type variable.
    -}
    newUnboxed :: e -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
    newUnboxed e = newUnboxed# (toProxy## e)
    
    {- |
      'newUnboxed#' creates new 'MutableByteArray#' of given count of elements.
      First argument used as type variable.
    -}
    newUnboxed# :: Proxy# e -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
    newUnboxed# e = newUnboxed' (asProxy## e filler)
    
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
      @'copyUnboxed##' e bytes\# o1\# mbytes\# o2\# n\#@ unsafely writes elements
      from @bytes\#@ to @mbytes\#@, where o1\# and o2\# - offsets (element
      count), @n\#@ - count of elements to copy.
    -}
    copyUnboxed## :: Proxy# e -> ByteArray# -> Int# -> MutableByteArray# s
                  -> Int# -> Int# -> State# s -> State# s
    copyUnboxed## e bytes# o1# mbytes# o2# n# = copyByteArray# bytes#
      (sizeof## e o1#) mbytes# (sizeof## e o2#) (sizeof## e n#)
    
    {- |
      @'copyUnboxedM##' e msrc\# o1\# mbytes\# o2\# n\#@ unsafely writes elements
      from @msrc\#@ to @mbytes\#@, where o1\# and o2\# - offsets (element
      count), @n\#@ - count of elements to copy.
    -}
    copyUnboxedM## :: Proxy# e -> MutableByteArray# s -> Int#
                  -> MutableByteArray# s -> Int# -> Int#
                  -> State# s -> State# s
    copyUnboxedM## e msrc# o1# mbytes# o2# n# = copyMutableByteArray# msrc#
      (sizeof## e o1#) mbytes# (sizeof## e o2#) (sizeof## e n#)
    
    {- |
      @'hashUnboxedWith' e len# off# bytes# salt@ returns @bytes#@ @FNV-1@ hash,
      where @off#@ and @len#@ is offset and length (in elements).
      
      Note: the standard definition of this function is written in Haskell using
      low-level functions, but this implementation mayn't be as efficient as the
      foreign procedure in the @hashable@ package.
    -}
    hashUnboxedWith :: Proxy# e -> Int# -> Int# -> ByteArray# -> Int# -> Int#
    hashUnboxedWith e len# off# bytes# = go (sizeof## e off#) (sizeof## e len#)
      where
        go _  0# salt# = salt#
        go o# n# salt# = go (o# +# 1#) (n# -# 1#) (word2Int# hash#)
          where
            prod# = int2Word# (salt# *# 16777619#)
            elem# = indexWord8Array## bytes# o#
            hash# = prod# `xor#` elem#
    
    {- |
      @since 0.3
      
      Sort unboxed array.
    -}
    sortUnboxed# :: Proxy# e -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
    
    {- |
      @since 0.3
      
      Prepare mutable array for radix sort.
    -}
    fromOrdered# :: Proxy# e -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
    fromOrdered# _ _ _ _ = \ s# -> s#
    
    {- |
      @since 0.3
      
      Restore mutable array, prepared by 'fromOrdered#'.
    -}
    toOrdered# :: Proxy# e -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
    toOrdered# _ _ _ _ = \ s# -> s#
    
    {- |
      @since 0.3
      
      Fast 'sum'.
    -}
    sumUnboxed# :: Num e => ByteArray# -> Int# -> Int# -> e
    sumUnboxed# arr# =
      let
          go# !s 0#  _ = s
          go# !s n# o# = go# (s + (arr# !# o#)) (n# -# 1#) (o# +# 1#)
      in  \ n# o# -> case n# ># 0# of {1# -> go# 0 n# o#; _ -> 0}
    
    {- |
      @since 0.3
      
      Fast 'product'.
    -}
    productUnboxed# :: Num e => ByteArray# -> Int# -> Int# -> e
    productUnboxed# arr# =
      let
          go# !s 0#  _ = s
          go# !s n# o# = go# (s * (arr# !# o#)) (n# -# 1#) (o# +# 1#)
      in  \ n# o# -> case n# ># 0# of {1# -> go# 1 n# o#; _ -> 1}
    
    {- |
      @since 0.3
      
      Fast 'maximum'.
    -}
    maximumUnboxed# :: Ord e => ByteArray# -> Int# -> Int# -> e
    maximumUnboxed# arr# =
      let
          go# !s 0#  _ = s
          go# !s n# o# = go# (max s (arr# !# o#)) (n# -# 1#) (o# +# 1#)
      in  \ n# o# -> case n# ># 0# of
            1# -> go# (arr# !# 1#) n# o#
            _  -> error "TODO"
    
    {- |
      @since 0.3
      
      Fast 'minimum'.
    -}
    minimumUnboxed# :: Ord e => ByteArray# -> Int# -> Int# -> e
    minimumUnboxed# arr# =
      let
          go# !s 0#  _ = s
          go# !s n# o# = go# (max s (arr# !# o#)) (n# -# 1#) (o# +# 1#)
      in  \ n# o# -> case n# ># 0# of
            1# -> go# (arr# !# 1#) n# o#
            _  -> error "TODO"

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  @cloneUnboxed# e bytes# o# c#@ creates new @c#@-element length immutable slice
  of @bytes#@ beginning from @o#@-th element.
-}
cloneUnboxed## :: Unboxed e => Proxy# e -> ByteArray# -> Int# -> Int# -> ByteArray#
cloneUnboxed## e bytes# o# c# = unwrap $ runST $ ST $
  \ s1# -> case newUnboxed# e c# s1# of
    (# s2#, mbytes# #) -> case copyUnboxed## e bytes# o# mbytes# 0# c# s2# of
      s3# -> case unsafeFreezeByteArray# mbytes# s3# of
        (# s4#, bytes'# #) -> (# s4#, (Wrap bytes'#) #)

--------------------------------------------------------------------------------

peqUnboxed :: Unboxed e => proxy e -> ByteArray# -> Int#
           -> ByteArray# -> Int# -> Int# -> Bool
peqUnboxed e = eqUnboxed## (toProxy# e)

{- |
  @since 0.2
  
  Kind @(Type -> Type)@ proxy version of 'newUnboxed'.
-}
pnewUnboxed :: Unboxed e => proxy e -> Int# -> State# s
            -> (# State# s, MutableByteArray# s #)
pnewUnboxed =  newUnboxed . fromProxy

--------------------------------------------------------------------------------

psizeof## :: Unboxed e => Proxy# (proxy e) -> Int# -> Int#
psizeof## p# c# = sizeof## (unliftProxy## p#) c#

peqUnboxed# :: Unboxed e => Proxy# (proxy e) -> ByteArray# -> Int#
            -> ByteArray# -> Int# -> Int# -> Bool
peqUnboxed# e = eqUnboxed## (unliftProxy## e)

--------------------------------------------------------------------------------

{- Numeric instances. -}

instance Unboxed Int
  where
    filler = 0
    
    eqUnboxed##  = bytewiseEqUnboxed##
    sortUnboxed# = radixSortUnboxed#
    
    toOrdered# _  _  0# _  = \ s1# -> s1#
    toOrdered# e bs# i# o# = \ s1# -> case readWordArray# bs# o# s1# of
      (# s2#, w# #) -> case writeWordArray# bs# o# (shiftWI# w#) s2# of
        s3# -> toOrdered# e bs# (i# -# 1#) (o# +# 1#) s3#
    
    {-# INLINE fromOrdered# #-}
    fromOrdered# = toOrdered#
    
    {-# INLINE sizeof## #-}
    sizeof## _ n# = case n# ># 0# of {1# -> n# *# SIZEOF_HSWORD#; _ -> 0#}
    
    {-# INLINE chunkof## #-}
    chunkof## _ = (# SIZEOF_HSWORD#, 1# #)
    
    {-# INLINE offsetof## #-}
    offsetof## _ i# = case i# ># 0# of {1# -> quotInt# i# SIZEOF_HSWORD#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes# !# i# = I# (indexIntArray# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readIntArray# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, I# e# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (I# e#) = writeIntArray# mbytes# n# e#
    
    {-# INLINE newUnboxed #-}
    newUnboxed e = calloc## (toProxy## e)

instance Unboxed Int8
  where
    filler = 0
    
    eqUnboxed## = bytewiseEqUnboxed##
    
    sortUnboxed# _ bs# n# o# = \ s1# -> case radixSortIndexInt8# bs# n# o# s1# of
      (# s2#, idx# #) -> case readWordArray# idx# 0# s2# of
        (# s3#, c# #) ->
          let go# v# i# j#
                | 1# <-    i# ==# n# = \ s4# -> s4#
                | 0# <- word2Int# j# = \ s4# -> case readWordArray# idx# (v# +# 0x81#) s4# of
                  (# s5#, j'# #) -> go# (v# +# 1#) i# j'# s5#
                |        True        = \ s4# -> case writeInt8Array## bs# i# v# s4# of
                  s5# -> go# v# (i# +# 1#) (minusWord# j# ox01#) s5#
              
              ox01# = int2Word# 1#
          in  go# -0x80# 0# c# s3#
    
    {-# INLINE sizeof## #-}
    sizeof## _ n# = case n# ># 0# of {1# -> n#; _ -> 0#}
    
    {-# INLINE chunkof## #-}
    chunkof## _ = (# 1#, 1# #)
    
    {-# INLINE offsetof## #-}
    offsetof## _ i# = case i# ># 0# of {1# -> i#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes# !# i# = I8# (indexInt8Array# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readInt8Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, I8# e# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (I8# e#) = writeInt8Array# mbytes# n# e#
    
    {-# INLINE newUnboxed #-}
    newUnboxed e = calloc## (toProxy## e)

instance Unboxed Int16
  where
    filler = 0
    
    eqUnboxed##  = bytewiseEqUnboxed##
    sortUnboxed# = radixSortUnboxed#
    
    toOrdered# _  _  0# _  = \ s1# -> s1#
    toOrdered# e bs# i# o# = \ s1# -> case readWord16Array# bs# o# s1# of
      (# s2#, w# #) -> case writeWord16Array# bs# o# (shift16WI# w#) s2# of
        s3# -> toOrdered# e bs# (i# -# 1#) (o# +# 1#) s3#
    
    {-# INLINE fromOrdered# #-}
    fromOrdered# = toOrdered#
    
    {-# INLINE sizeof## #-}
    sizeof## _ n# = case n# ># 0# of {1# -> n# *# 2#; _ -> 0#}
    
    {-# INLINE chunkof## #-}
    chunkof## _ = (# 2#, 1# #)
    
    {-# INLINE offsetof## #-}
    offsetof## _ i# = case i# ># 0# of {1# -> quotInt# i# 2#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes# !# i# = I16# (indexInt16Array# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readInt16Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, I16# e# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (I16# e#) = writeInt16Array# mbytes# n# e#
    
    {-# INLINE newUnboxed #-}
    newUnboxed e = calloc## (toProxy## e)

instance Unboxed Int32
  where
    filler = 0
    
    eqUnboxed##  = bytewiseEqUnboxed##
    sortUnboxed# = radixSortUnboxed#
    
    toOrdered# _  _  0# _  = \ s1# -> s1#
    toOrdered# e bs# i# o# = \ s1# -> case readWord32Array# bs# o# s1# of
      (# s2#, w# #) -> case writeWord32Array# bs# o# (shift32WI# w#) s2# of
        s3# -> toOrdered# e bs# (i# -# 1#) (o# +# 1#) s3#
    
    {-# INLINE fromOrdered# #-}
    fromOrdered# = toOrdered#
    
    {-# INLINE sizeof## #-}
    sizeof## _ n# = case n# ># 0# of {1# -> n# *# 4#; _ -> 0#}
    
    {-# INLINE chunkof## #-}
    chunkof## _ = (# 4#, 1# #)
    
    {-# INLINE offsetof## #-}
    offsetof## _ i# = case i# ># 0# of {1# -> quotInt# i# 4#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes# !# i# = I32# (indexInt32Array# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readInt32Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, I32# e# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (I32# e#) = writeInt32Array# mbytes# n# e#
    
    {-# INLINE newUnboxed #-}
    newUnboxed e = calloc## (toProxy## e)

instance Unboxed Int64
  where
    filler = 0
    
    eqUnboxed##  = bytewiseEqUnboxed##
    sortUnboxed# = radixSortUnboxed#
    
    toOrdered# _  _  0# _  = \ s1# -> s1#
    toOrdered# e bs# i# o# = \ s1# -> case readWord64Array# bs# o# s1# of
      (# s2#, w# #) -> case writeWord64Array# bs# o# (shift64WI# w#) s2# of
        s3# -> toOrdered# e bs# (i# -# 1#) (o# +# 1#) s3#
    
    {-# INLINE fromOrdered# #-}
    fromOrdered# = toOrdered#
    
    {-# INLINE sizeof## #-}
    sizeof## _ n# = case n# ># 0# of {1# -> n# *# 8#; _ -> 0#}
    
    {-# INLINE chunkof## #-}
    chunkof## _ = (# 8#, 1# #)
    
    {-# INLINE offsetof## #-}
    offsetof## _ i# = case i# ># 0# of {1# -> quotInt# i# 8#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes# !# i# = I64# (indexInt64Array# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readInt64Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, I64# e# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (I64# e#) = writeInt64Array# mbytes# n# e#
    
    {-# INLINE newUnboxed #-}
    newUnboxed e = calloc## (toProxy## e)

instance Unboxed Word
  where
    filler = 0
    
    eqUnboxed##  = bytewiseEqUnboxed##
    sortUnboxed# = radixSortUnboxed#
    
    {-# INLINE sizeof## #-}
    sizeof## _ n# = case n# ># 0# of {1# -> n# *# SIZEOF_HSWORD#; _ -> 0#}
    
    {-# INLINE chunkof## #-}
    chunkof## _ = (# SIZEOF_HSWORD#, 1# #)
    
    {-# INLINE offsetof## #-}
    offsetof## _ i# = case i# ># 0# of {1# -> quotInt# i# SIZEOF_HSWORD#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes# !# i# = W# (indexWordArray# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readWordArray# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, W# e# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (W# e#) = writeWordArray# mbytes# n# e#
    
    {-# INLINE newUnboxed #-}
    newUnboxed e = calloc## (toProxy## e)

instance Unboxed Word8
  where
    filler = 0
    
    eqUnboxed## = bytewiseEqUnboxed##
    
    {-# INLINE sizeof## #-}
    sizeof## _ n# = case n# ># 0# of {1# -> n#; _ -> 0#}
    
    {-# INLINE chunkof## #-}
    chunkof## _ = (# 1#, 1# #)
    
    {-# INLINE (!#) #-}
    bytes# !# i# = W8# (indexWord8Array# bytes# i#)
    
    {-# INLINE offsetof## #-}
    offsetof## _ i# = case i# ># 0# of {1# -> i#; _ -> 0#}
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readWord8Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, W8# e# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (W8#  e#) = writeWord8Array# mbytes# n# e#
    
    {-# INLINE newUnboxed #-}
    newUnboxed e = calloc## (toProxy## e)
    
    sortUnboxed# _ bs# n# o# = \ s1# -> case radixSortIndex# bs# n# o# 1# s1# of
      (# s2#, idx# #) -> case readWordArray# idx# 0# s2# of
        (# s3#, c# #) ->
          let go# v# i# j#
                | 1# <-    i# ==# n# = \ s4# -> s4#
                | 0# <- word2Int# j# = \ s4# -> case readWordArray# idx# (word2Int# v# +# 1#) s4# of
                  (# s5#, j'# #) -> go# (plusWord# v# ox01#) i# j'# s5#
                |        True        = \ s4# -> case writeWord8Array## bs# i# v# s4# of
                  s5# -> go# v# (i# +# 1#) (minusWord# j# ox01#) s5#
              
              ox01# = int2Word# 1#
          in  go# (int2Word# 0#) 0# c# s3#

instance Unboxed Word16
  where
    filler = 0
    
    eqUnboxed##  = bytewiseEqUnboxed##
    sortUnboxed# = radixSortUnboxed#
    
    {-# INLINE sizeof## #-}
    sizeof## _ n# = case n# ># 0# of {1# -> n# *# 2#; _ -> 0#}
    
    {-# INLINE chunkof## #-}
    chunkof## _ = (# 2#, 1# #)
    
    {-# INLINE (!#) #-}
    bytes# !# i# = W16# (indexWord16Array# bytes# i#)
    
    {-# INLINE offsetof## #-}
    offsetof## _ i# = case i# ># 0# of {1# -> quotInt# i# 2#; _ -> 0#}
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readWord16Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, W16# e# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (W16# e#) = writeWord16Array# mbytes# n# e#
    
    {-# INLINE newUnboxed #-}
    newUnboxed e = calloc## (toProxy## e)

instance Unboxed Word32
  where
    filler = 0
    
    eqUnboxed##  = bytewiseEqUnboxed##
    sortUnboxed# = radixSortUnboxed#
    
    {-# INLINE sizeof## #-}
    sizeof## _ n# = case n# ># 0# of {1# -> n# *# 4#; _ -> 0#}
    
    {-# INLINE chunkof## #-}
    chunkof## _ = (# 4#, 1# #)
    
    {-# INLINE offsetof## #-}
    offsetof## _ i# = case i# ># 0# of {1# -> quotInt# i# 4#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes# !# i# = W32# (indexWord32Array# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readWord32Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, W32# e# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (W32# e#) = writeWord32Array# mbytes# n# e#
    
    {-# INLINE newUnboxed #-}
    newUnboxed e = calloc## (toProxy## e)

instance Unboxed Word64
  where
    filler = 0
    
    eqUnboxed##  = bytewiseEqUnboxed##
    sortUnboxed# = radixSortUnboxed#
    
    {-# INLINE sizeof## #-}
    sizeof## _ n# = case n# ># 0# of {1# -> n# *# 8#; _ -> 0#}
    
    {-# INLINE chunkof## #-}
    chunkof## _ = (# 8#, 1# #)
    
    {-# INLINE offsetof## #-}
    offsetof## _ i# = case i# ># 0# of {1# -> quotInt# i# 8#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes# !# i# = W64# (indexWord64Array# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readWord64Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, W64# e# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (W64# e#) = writeWord64Array# mbytes# n# e#
    
    {-# INLINE newUnboxed #-}
    newUnboxed e = calloc## (toProxy## e)

instance Unboxed Float
  where
    filler = 0
    
    eqUnboxed##  = bytewiseEqUnboxed##
    sortUnboxed# = radixSortUnboxed#
    
    {-# INLINE sizeof## #-}
    sizeof## _ n# = case n# ># 0# of {1# -> n# *# SIZEOF_HSFLOAT#; _ -> 0#}
    
    {-# INLINE chunkof## #-}
    chunkof## _ = (# SIZEOF_HSFLOAT#, 1# #)
    
    {-# INLINE offsetof## #-}
    offsetof## _ i# = case i# ># 0# of {1# -> quotInt# i# SIZEOF_HSFLOAT#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes# !# i# = F# (indexFloatArray# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readFloatArray# mbytes# i# s1# of
      (# s2#, f# #) -> (# s2#, F# f# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (F# e#) = writeFloatArray# mbytes# n# e#
    
    {-# INLINE newUnboxed #-}
    newUnboxed e = calloc## (toProxy## e)

instance Unboxed Double
  where
    filler = 0
    
    eqUnboxed##  = bytewiseEqUnboxed##
    sortUnboxed# = radixSortUnboxed#
    
    {-# INLINE sizeof## #-}
    sizeof## _ n# = case n# ># 0# of {1# -> n# *# SIZEOF_HSDOUBLE#; _ -> 0#}
    
    {-# INLINE chunkof## #-}
    chunkof## _ = (# SIZEOF_HSDOUBLE#, 1# #)
    
    {-# INLINE offsetof## #-}
    offsetof## _ i# = case i# ># 0# of {1# -> quotInt# i# SIZEOF_HSDOUBLE#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes# !# i# = D# (indexDoubleArray# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readDoubleArray# mbytes# i# s1# of
      (# s2#, d# #) -> (# s2#, D# d# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (D# e#) = writeDoubleArray# mbytes# n# e#
    
    {-# INLINE newUnboxed #-}
    newUnboxed e = calloc## (toProxy## e)

instance (Unboxed e, Integral e) => Unboxed (Ratio e)
  where
    filler = 0 :% 1
    
    eqUnboxed## = bytewiseEqUnboxed##
    
    sortUnboxed# e = radixSortUnboxed# (fromGValue# e)
      where
        {-
          @('Ratio' e)@ has limitations on stored values and involves additional
          overhead for checks when creating a value. Sorting for @('Complex' e)@
          and @('Ratio' e)@ works the same as for @('T2' e)@, but @('T2' e)@ has
          additional restrictions for typevar @e@.
        -}
        fromGValue# :: Proxy# (Ratio e) -> Proxy# (Complex e)
        fromGValue# =  \ _ -> proxy#
    
    {-# INLINE sizeof## #-}
    sizeof## e n# = 2# *# psizeof## e n#
    
    bytes# !# i# = bytes# !# i2# :% bytes# !# (i2# +# 1#) where i2# = 2# *# i#
    
    readUnboxed# mbytes# i# = let i2# = 2# *# i# in
      \ s1# -> case readUnboxed# mbytes# i2# s1# of
        (# s2#, n #) -> case readUnboxed# mbytes# (i2# +# 1#) s2# of
          (# s3#, d #) -> (# s3#, n :% d #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# i# (n :% d) = let i2# = 2# *# i# in
      \ s1# -> case writeUnboxed# mbytes# i2# n s1# of
        s2# -> writeUnboxed# mbytes# (i2# +# 1#) d s2#
    
    toOrdered# e bs# i# o# = toOrdered# (fromGValue# e) bs# (2# *# i#) (2# *# o#)
      where
        fromGValue# :: Proxy# (Ratio e) -> Proxy# e
        fromGValue# =  \ _ -> proxy#
    
    {-# INLINE fromOrdered# #-}
    fromOrdered# = toOrdered#

instance (Unboxed e, Num e) => Unboxed (Complex e)
  where
    filler = 0 :+ 0
    
    eqUnboxed##  = bytewiseEqUnboxed##
    sortUnboxed# = radixSortUnboxed#
    
    {-# INLINE sizeof## #-}
    sizeof## e n# = 2# *# psizeof## e n#
    
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
    
    toOrdered# e bs# i# o# = toOrdered# (fromGValue# e) bs# (2# *# i#) (2# *# o#)
      where
        fromGValue# :: Proxy# (Complex e) -> Proxy# e
        fromGValue# =  \ _ -> proxy#
    
    {-# INLINE fromOrdered# #-}
    fromOrdered# = toOrdered#

--------------------------------------------------------------------------------

{- Pointer instances. -}

instance Unboxed (Ptr e)
  where
    filler = NULL
    
    eqUnboxed##  = bytewiseEqUnboxed##
    sortUnboxed# = radixSortUnboxed#
    
    {-# INLINE sizeof## #-}
    sizeof## _ n# = case n# ># 0# of {1# -> n# *# SIZEOF_HSWORD#; _ -> 0#}
    
    {-# INLINE chunkof## #-}
    chunkof## _ = (# SIZEOF_HSWORD#, 1# #)
    
    {-# INLINE offsetof## #-}
    offsetof## _ i# = case i# ># 0# of {1# -> quotInt# i# SIZEOF_HSWORD#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes# !# i# = Ptr (indexAddrArray# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readAddrArray# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, Ptr e# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (Ptr e) = writeAddrArray# mbytes# n# e
    
    {-# INLINE newUnboxed #-}
    newUnboxed e = calloc## (toProxy## e)

instance Unboxed (FunPtr e)
  where
    filler = NULL
    
    eqUnboxed##  = bytewiseEqUnboxed##
    sortUnboxed# = radixSortUnboxed#
    
    {-# INLINE sizeof## #-}
    sizeof## _ n# = case n# ># 0# of {1# -> n# *# SIZEOF_HSWORD#; _ -> 0#}
    
    {-# INLINE chunkof## #-}
    chunkof## _ = (# SIZEOF_HSWORD#, 1# #)
    
    {-# INLINE offsetof## #-}
    offsetof## _ i# = case i# ># 0# of {1# -> quotInt# i# SIZEOF_HSWORD#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes#  !#  i# = FunPtr (indexAddrArray# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readAddrArray# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, FunPtr e# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (FunPtr e) = writeAddrArray# mbytes# n# e
    
    {-# INLINE newUnboxed #-}
    newUnboxed e = calloc## (toProxy## e)

instance Unboxed (StablePtr e)
  where
    filler = NULL
    
    eqUnboxed##  = bytewiseEqUnboxed##
    sortUnboxed# = radixSortUnboxed#
    
    {-# INLINE sizeof## #-}
    sizeof## _ n# = case n# ># 0# of {1# -> n# *# SIZEOF_HSWORD#; _ -> 0#}
    
    {-# INLINE chunkof## #-}
    chunkof## _ = (# SIZEOF_HSWORD#, 1# #)
    
    {-# INLINE offsetof## #-}
    offsetof## _ i# = case i# ># 0# of {1# -> quotInt# i# SIZEOF_HSWORD#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes# !# i# = StablePtr (indexStablePtrArray# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readStablePtrArray# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, StablePtr e# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (StablePtr e) = writeStablePtrArray# mbytes# n# e
    
    {-# INLINE newUnboxed #-}
    newUnboxed e = calloc## (toProxy## e)

--------------------------------------------------------------------------------

{- Foreign C instances. -}

#define deriving_instance_Unboxed(Type)\
instance Unboxed Type where\
{\
  filler      = Type filler;\
  eqUnboxed## = bytewiseEqUnboxed##;\
  arr# !# i# = Type ( arr# !# i# );\
  sizeof## _ = sizeof## (proxy# :: Proxy# Type);\
  \
  fillByteArray# marr# i# (Type e) = fillByteArray# marr# i# e;\
  writeUnboxed#  marr# i# (Type e) = writeUnboxed#  marr# i# e;\
  readUnboxed#   marr# i# = \ s1# -> case readUnboxed# marr# i# s1# of\
    {(# s2#, e #) -> (# s2#, Type e #)};\
  \
  newUnboxed# _ = newUnboxed# (proxy# :: Proxy# Type);\
  newUnboxed' (Type e) = newUnboxed' e;\
  \
  sortUnboxed# = radixSortUnboxed#;\
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
    
    {-# INLINE sizeof## #-}
    sizeof## _ n# = case n# ># 0# of
      1# -> case n# `quotRemInt#` 8# of {(# q#, r# #) -> q# +# (r# /=# 0#)}
      _  -> 0#
    
    {-# INLINE chunkof## #-}
    chunkof## _ = (# 1#, 8# #)
    
    {-# INLINE offsetof## #-}
    offsetof## _ i# = case i# ># 0# of {1# -> 8# *# i#; _ -> 0#}
    
    {-# INLINE newUnboxed #-}
    newUnboxed e = calloc## (toProxy## e)
    
    {-# INLINE (!#) #-}
    bytes# !# i# = isTrue# (b# `neWord#` int2Word# 0#)
      where
        b# = and# m# (indexWord8Array## bytes# n#)
        m# = uncheckedShiftL# (int2Word# 1#) bi#
        !(# n#, bi# #) = i# `quotRemInt#` 8#
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readWord8Array## mbytes# n# s1# of
        (# s2#, e# #) -> (# s2#, isTrue# (and# e# b# `neWord#` int2Word# 0#) #)
      where
        b# = uncheckedShiftL# (int2Word# 1#) bi#
        !(# n#, bi# #) = i# `quotRemInt#` 8#
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# e = \ s1# -> case readWord8Array## mbytes# c# s1# of
        (# s2#, old# #) -> writeWord8Array## mbytes# c# (upd# old#) s2#
      where
        upd# old# = if e then or# old# b# else old# `and#` not# b#
        b# = uncheckedShiftL# (int2Word# 1#) bi#
        !(# c#, bi# #) = n# `quotRemInt#` 8#
    
    fillByteArray# bs# c# e = \ s1# -> case setByteArray# bs# 0# n# val# s1# of
        s2# -> if isTrue# (b# ==# 0#) then s2# else
          case readWord8Array## bs# n# s2# of
            (# s3#, w8# #) -> writeWord8Array## bs# n#
              (if e then or# w8# (not# mask#) else and# w8# mask#) s3#
      where
        mask# = uncheckedShiftL# (int2Word# 0xff#) b#
        val#  = if e then 0xff# else 0x00#
        !(# n#, b# #) = quotRemInt# c# 8#
    
    fillByteArrayOff# bs# 0# n# e = fillByteArray# bs# n# e
    fillByteArrayOff# bs# o# n# e
        | 1# <- n# <# 1# = \ s1# -> s1#
        -- Normal byte array with whole byte offset
        | 0# <-      bo# = \ s1# -> case setByteArray# bs# no# nc# val# s1# of
          s2# -> if isTrue# (bc# ==# 0#) then s2# else
            case readWord8Array## bs# (no# +# nc#) s2# of
              (# s3#, w8# #) ->
                let mask# = uncheckedShiftL# oxff# bc#
                in  writeWord8Array## bs# (no# +# nc#)
                      (if e then w8# `or#` not# mask# else and# w8# mask#) s3#
        
        -- Single incomplete byte (e.g. [**110***])
        | 1# <- b# <# 8# = \ s1# -> case readWord8Array## bs# no# s1# of
            (# s2#, w# #) ->
              let val'# = and# mask# w# `or#` and# (not# mask#) (int2Word# val#)
                  mask# = not# (uncheckedShiftL# oxff# b#)
                         `or#` uncheckedShiftRL# oxff# bo#
              in  writeWord8Array## bs# no# val'# s2#
        
        -- Incomplete byte with optional n-byte tail (e.g. [*****000][...tail])
        |      True      = \ s1# -> case readWord8Array## bs# no# s1# of
          (# s2#, w# #) ->
            let val'# = and# w# (not# mask#) `or#` and# (int2Word# val#) mask#
                mask# = uncheckedShiftL# oxff# bo#
            in  case writeWord8Array## bs# no# val'# s2# of
                  s3# -> fillByteArrayOff# bs# (o# +# ob#) (n# -# ob#) e s3#
      where
        !(# no#, bo# #) = quotRemInt# o# 8#; val#  = if e then 0xff# else 0x00#
        !(# nc#, bc# #) = quotRemInt# n# 8#; oxff# = int2Word# 0xff#
        ob# = 8# -# bo#; b# = bo# +# n#
    
    copyUnboxed## e bytes# o1# mbytes# o2# c# = if isTrue# (c# <# 1#) then \ s1# -> s1# else
      \ s1# -> case writeUnboxed# mbytes# o2# (asProxy## e (bytes# !# o1#)) s1# of
        s2# -> copyUnboxed## e bytes# (o1# +# 1#) mbytes# (o2# +# 1#) (c# -# 1#) s2#
    
    copyUnboxedM## e src# o1# mbytes# o2# n# = if isTrue# (n# <# 1#) then \ s1# -> s1# else
      \ s1# -> case readUnboxed# src# o1# s1# of
        (# s2#, x #) -> case writeUnboxed# mbytes# o2# (asProxy## e x) s2# of
          s3# -> copyUnboxedM## e src# (o1# +# 1#) mbytes# (o2# +# 1#) (n# -# 1#) s3#
    
    sortUnboxed# _ bs# n# o# = case n# <# 2# of
      1# -> \ s1# -> s1#
      _  -> \ s1# -> case boolCountM# bs# n# o# s1# of
        (# s2#, c# #) ->
          let c'# = n# -# c#
          in  case fillByteArrayOff# bs# o# c'# False s2# of
                s3# -> fillByteArrayOff# bs# (o# +# c'#) c'# True s3#
    
    hashUnboxedWith e len# off# bytes#
        | 1# <- len# <# 1# = \ salt# -> salt#
        | 1# <- off# <# 0# = hashUnboxedWith e len# 0# bytes#
        | 0# <-        bi# = go0 byte_cnt# c#
        |             True = goo byte_cnt# (c# +# 1#) (indexWord8Array## bytes# c#)
      where
        go0 0# _  salt# = salt#
        go0 1# o# salt# = hash# salt# (indexWord8Array## bytes# o# `and#` mask#)
        go0 n# o# salt# = go0 (n# -# 1#) (o# +# 1#)
          (salt# `hash#` indexWord8Array## bytes# o#)
        
        goo 0# _    _   salt# = salt#
        goo 1# _  temp# salt# = hash# salt# (shiftRL# temp# bi# `and#` mask#)
        goo n# o# temp# salt# = goo (n# -# 1#) (o# +# 1#) byte# (hash# salt# curr#)
          where
            curr# = shiftRL# temp# bi# `or#` shiftL# byte# (8# -# bi#)
            byte# = indexWord8Array## bytes# o#
        
        hash# = \ s# v# -> word2Int# (int2Word# (s# *# 16777619#) `xor#` v#)
        mask# = int2Word# 0xff# `shiftRL#` bit_rest#
        
        !(# c#, bi# #) = off# `quotRemInt#` 8#
        
        bit_rest# = 8# -# remInt# len# 8#
        byte_cnt# = sizeof## e len#

instance Unboxed Char
  where
    filler = '\0'
    
    eqUnboxed##  = bytewiseEqUnboxed##
    sortUnboxed# = radixSortUnboxed#
    
    {-# INLINE sizeof## #-}
    sizeof## _ n# = case n# ># 0# of {1# -> n# *# 4#; _ -> 0#}
    
    {-# INLINE chunkof## #-}
    chunkof## _ = (# 4#, 1# #)
    
    {-# INLINE offsetof## #-}
    offsetof## _ i# = case i# ># 0# of {1# -> quotInt# i# 4#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes# !# i# = C# (indexWideCharArray# bytes# i#)
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readWideCharArray# mbytes# i# s1# of
      (# s2#, c# #) -> (# s2#, C# c# #)
    
    {-# INLINE writeUnboxed# #-}
    writeUnboxed# mbytes# n# (C# e#) = writeWideCharArray# mbytes# n# e#
    
    {-# INLINE newUnboxed #-}
    newUnboxed e = calloc## (toProxy## e)

--------------------------------------------------------------------------------

instance Unboxed E
  where
    filler = E
    
    eqUnboxed## _ _ _ _ _ _ = True
    
    {-# INLINE sizeof## #-}
    sizeof## _ _ = 0#
    
    {-# INLINE offsetof## #-}
    offsetof## _ _ = 0#
    
    {-# INLINE (!#) #-}
    readUnboxed# = \ _ _ s# -> (# s#, E #)
    (!#)  = \ _ _ -> E
    
    newUnboxed  _ _ = newByteArray# 0#
    newUnboxed' _ _ = newByteArray# 0#
    
    writeUnboxed#   _ _ = \ _ s# -> s#
    fillByteArray#  _ _ = \ _ s# -> s#
    
    sortUnboxed# _ _ _ _ = \ s# -> s#

instance (Unboxed e, Rank1 e) => Unboxed (I1 e)
  where
    filler = E :& filler
    
    eqUnboxed## = peqUnboxed#
    
    sortUnboxed# e = sortUnboxed# (fromGValue# e)
      where
        fromGValue# :: Proxy# e -> Proxy# (E :& e)
        fromGValue# =  \ _ -> proxy#
    
    sizeof## = psizeof##
    
    bytes# !# i# = E :& (bytes# !# i#)
    
    readUnboxed# bytes# i# = \ s1# -> case readUnboxed# bytes# i# s1# of
      (# s2#, e #) -> (# s2#, E :& e #)
    
    writeUnboxed#  bytes# n# (E :& e) = writeUnboxed#  bytes# n# e
    fillByteArray# bytes# n# (E :& e) = fillByteArray# bytes# n# e
    
    newUnboxed' = \ (E :& i) -> newUnboxed i
    newUnboxed  = pnewUnboxed

instance (Enum e, Shape e, Bounded e, Rank1 e, Unboxed e, Shape (e' :& e), Unboxed (e' :& e))
      => Unboxed (e' :& e :& e)
  where
    filler = filler :& filler
    
    eqUnboxed## e xs# o1# ys# o2# n# = let r# = rank## e in peqUnboxed# e xs#
      (o1# *# r#) ys# (o2# *# r#) (n# *# r#)
    
    sizeof## e n# = psizeof## e (rank## e *# n#)
    
    bytes# !# i# = go proxy#
      where
        go p# =
          let r# = rank## p#; o# = i#*#r# +# i#
          in  (asProxy## p# (bytes# !# o#)) :& (bytes# !# (o# +# r#))
    
    readUnboxed# bytes# i# = go proxy#
      where
        go p# = let r# = rank## p#; o# = i#*#r# +# i# in
          \ s1# -> case readUnboxed# bytes# o# s1# of
            (# s2#, es #) -> case readUnboxed# bytes# (o# +# r#) s2# of
              (# s3#, e #) -> (# s3#, asProxy## p# es :& e #)
    
    writeUnboxed# bytes# i# (es :& e) = let r# = rank# es; o# = i#*#r# +# i# in
      \ s1# -> case writeUnboxed# bytes# o# es s1# of
        s2# -> writeUnboxed# bytes# (o# +# r#) e s2#
    
    newUnboxed e n# = pnewUnboxed e (rank# e *# n#)
    
    sortUnboxed# = radixSortUnboxed#
    
    toOrdered# e bs# n# o# = toOrdered# (fromGValue e) bs# (r# *# n#) (r# *# o#)
      where
        fromGValue :: Proxy# (es :& e) -> Proxy# e
        fromGValue =  \ _ -> proxy#
        
        r# = rank## e
    
    fromOrdered# = toOrdered#

--------------------------------------------------------------------------------

{- Tuple instances. -}

instance Unboxed ()
  where
    filler = ()
    
    eqUnboxed## _ _ _ _ _ _ = True
    
    {-# INLINE sizeof## #-}
    sizeof## _ _ = 0#
    
    {-# INLINE (!#) #-}
    readUnboxed# = \ _ _ s# -> (# s#, () #)
    (!#)  = \ _ _ -> ()
    
    newUnboxed  _ _ = newByteArray# 0#
    newUnboxed' _ _ = newByteArray# 0#
    
    writeUnboxed#  _ _ = \ _ s# -> s#
    fillByteArray# _ _ = \ _ s# -> s#
    
    sortUnboxed# _ _ _ _ = \ s# -> s#

instance (Enum e, Shape e, Bounded e, Rank1 e, Unboxed e) => Unboxed (T2 e)
  where
    sizeof## e n# = psizeof## e (2# *# n#)
    
    eqUnboxed## e xs# o1# ys# o2# n# = peqUnboxed# e xs# (o1# *# 2#) ys# (o2# *# 2#) (n# *# 2#)
    
    bytes# !# n# = let o# = 2# *# n# in (bytes# !# o#, bytes# !# (o#+#1#))
    
    readUnboxed# bytes# n# = let o# = 2# *# n# in
      \ s1# -> case readUnboxed# bytes# o# s1# of
        (# s2#, e1 #) -> case readUnboxed# bytes# (o# +# 1#) s2# of
          (# s3#, e2 #) -> (# s3#, (e1,e2) #)
    
    writeUnboxed# mbytes# n# (e1,e2) = let o# = 2# *# n# in
      \ s1# -> case writeUnboxed# mbytes# o# e1 s1# of
        s2# -> writeUnboxed# mbytes# (o# +# 1#) e2 s2#
    
    newUnboxed e n# = pnewUnboxed e (2# *# n#)
    
    sortUnboxed# = radixSortUnboxed#
    
    toOrdered# e = toOrdered# (fromGValue# e)
      where
        fromGValue# :: Proxy# (T2 e) -> Proxy# (I2 e)
        fromGValue# =  \ _ -> proxy#
    
    fromOrdered# = toOrdered#

instance (Enum e, Shape e, Bounded e, Rank1 e, Unboxed e) => Unboxed (T3 e)
  where
    sizeof## e n# = psizeof## e (3# *# n#)
    
    eqUnboxed## e xs# o1# ys# o2# n# = peqUnboxed# e xs# (o1# *# 3#) ys# (o2# *# 3#) (n# *# 3#)
    
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
    
    sortUnboxed# = radixSortUnboxed#
    
    toOrdered# e = toOrdered# (fromGValue# e)
      where
        fromGValue# :: Proxy# (T3 e) -> Proxy# (I3 e)
        fromGValue# =  \ _ -> proxy#
    
    fromOrdered# = toOrdered#

instance (Enum e, Shape e, Bounded e, Rank1 e, Unboxed e) => Unboxed (T4 e)
  where
    sizeof## e n# = psizeof## e (4# *# n#)
    
    eqUnboxed## e xs# o1# ys# o2# n# = peqUnboxed# e xs# (o1# *# 4#) ys# (o2# *# 4#) (n# *# 4#)
    
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
    
    sortUnboxed# = radixSortUnboxed#
    
    toOrdered# e = toOrdered# (fromGValue# e)
      where
        fromGValue# :: Proxy# (T4 e) -> Proxy# (I4 e)
        fromGValue# =  \ _ -> proxy#
    
    fromOrdered# = toOrdered#

instance (Enum e, Shape e, Bounded e, Rank1 e, Unboxed e) => Unboxed (T5 e)
  where
    sizeof## e n# = psizeof## e (5# *# n#)
    
    eqUnboxed## e xs# o1# ys# o2# n# = peqUnboxed# e xs# (o1# *# 5#) ys# (o2# *# 5#) (n# *# 5#)
    
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
    
    sortUnboxed# = radixSortUnboxed#
    
    toOrdered# e = toOrdered# (fromGValue# e)
      where
        fromGValue# :: Proxy# (T5 e) -> Proxy# (I5 e)
        fromGValue# =  \ _ -> proxy#
    
    fromOrdered# = toOrdered#

instance (Enum e, Shape e, Bounded e, Rank1 e, Unboxed e) => Unboxed (T6 e)
  where
    sizeof## e n# = psizeof## e (6# *# n#)
    
    eqUnboxed## e xs# o1# ys# o2# n# = peqUnboxed# e xs# (o1# *# 6#) ys# (o2# *# 6#) (n# *# 6#)
    
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
    
    sortUnboxed# = radixSortUnboxed#
    
    toOrdered# e = toOrdered# (fromGValue# e)
      where
        fromGValue# :: Proxy# (T6 e) -> Proxy# (I6 e)
        fromGValue# =  \ _ -> proxy#
    
    fromOrdered# = toOrdered#

instance (Enum e, Shape e, Bounded e, Rank1 e, Unboxed e) => Unboxed (T7 e)
  where
    sizeof## e n# = psizeof## e (7# *# n#)
    
    eqUnboxed## e xs# o1# ys# o2# n# = peqUnboxed# e xs# (o1# *# 7#) ys# (o2# *# 7#) (n# *# 7#)
    
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
    
    sortUnboxed# = radixSortUnboxed#
    
    toOrdered# e = toOrdered# (fromGValue# e)
      where
        fromGValue# :: Proxy# (T7 e) -> Proxy# (I7 e)
        fromGValue# =  \ _ -> proxy#
    
    fromOrdered# = toOrdered#

instance (Enum e, Shape e, Bounded e, Rank1 e, Unboxed e) => Unboxed (T8 e)
  where
    sizeof## e n# = psizeof## e (8# *# n#)
    
    eqUnboxed## e xs# o1# ys# o2# n# = peqUnboxed# e xs# (o1# *# 8#) ys# (o2# *# 8#) (n# *# 8#)
    
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
    
    sortUnboxed# = radixSortUnboxed#
    
    toOrdered# e = toOrdered# (fromGValue# e)
      where
        fromGValue# :: Proxy# (T8 e) -> Proxy# (I8 e)
        fromGValue# =  \ _ -> proxy#
    
    fromOrdered# = toOrdered#

instance (Enum e, Shape e, Bounded e, Rank1 e, Unboxed e) => Unboxed (T9 e)
  where
    sizeof## e n# = psizeof## e (9# *# n#)
    
    eqUnboxed## e xs# o1# ys# o2# n# = peqUnboxed# e xs# (o1# *# 9#) ys# (o2# *# 9#) (n# *# 9#)
    
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
    
    sortUnboxed# = radixSortUnboxed#
    
    toOrdered# e = toOrdered# (fromGValue# e)
      where
        fromGValue# :: Proxy# (T9 e) -> Proxy# (I9 e)
        fromGValue# =  \ _ -> proxy#
    
    fromOrdered# = toOrdered#

instance (Enum e, Shape e, Bounded e, Rank1 e, Unboxed e) => Unboxed (T10 e)
  where
    sizeof## e n# = psizeof## e (10# *# n#)
    
    eqUnboxed## e xs# o1# ys# o2# n# = peqUnboxed# e xs# (o1# *# 10#) ys# (o2# *# 10#) (n# *# 10#)
    
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
    
    sortUnboxed# = radixSortUnboxed#
    
    toOrdered# e = toOrdered# (fromGValue# e)
      where
        fromGValue# :: Proxy# (T10 e) -> Proxy# (I10 e)
        fromGValue# =  \ _ -> proxy#
    
    fromOrdered# = toOrdered#

instance (Enum e, Shape e, Bounded e, Rank1 e, Unboxed e) => Unboxed (T11 e)
  where
    sizeof## e n# = psizeof## e (11# *# n#)
    
    eqUnboxed## e xs# o1# ys# o2# n# = peqUnboxed# e xs# (o1# *# 11#) ys# (o2# *# 11#) (n# *# 11#)
    
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
    
    sortUnboxed# = radixSortUnboxed#
    
    toOrdered# e = toOrdered# (fromGValue# e)
      where
        fromGValue# :: Proxy# (T11 e) -> Proxy# (I11 e)
        fromGValue# =  \ _ -> proxy#
    
    fromOrdered# = toOrdered#

instance (Enum e, Shape e, Bounded e, Rank1 e, Unboxed e) => Unboxed (T12 e)
  where
    sizeof## e n# = psizeof## e (12# *# n#)
    
    eqUnboxed## e xs# o1# ys# o2# n# = peqUnboxed# e xs# (o1# *# 12#) ys# (o2# *# 12#) (n# *# 12#)
    
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
    
    sortUnboxed# = radixSortUnboxed#
    
    toOrdered# e = toOrdered# (fromGValue# e)
      where
        fromGValue# :: Proxy# (T12 e) -> Proxy# (I12 e)
        fromGValue# =  \ _ -> proxy#
    
    fromOrdered# = toOrdered#

instance (Enum e, Shape e, Bounded e, Rank1 e, Unboxed e) => Unboxed (T13 e)
  where
    sizeof## e n# = psizeof## e (13# *# n#)
    
    eqUnboxed## e xs# o1# ys# o2# n# = peqUnboxed# e xs# (o1# *# 13#) ys# (o2# *# 13#) (n# *# 13#)
    
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
    
    sortUnboxed# = radixSortUnboxed#
    
    toOrdered# e = toOrdered# (fromGValue# e)
      where
        fromGValue# :: Proxy# (T13 e) -> Proxy# (I13 e)
        fromGValue# =  \ _ -> proxy#
    
    fromOrdered# = toOrdered#

instance (Enum e, Shape e, Bounded e, Rank1 e, Unboxed e) => Unboxed (T14 e)
  where
    sizeof## e n# = psizeof## e (14# *# n#)
    
    eqUnboxed## e xs# o1# ys# o2# n# = peqUnboxed# e xs# (o1# *# 14#) ys# (o2# *# 14#) (n# *# 14#)
    
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
    
    sortUnboxed# = radixSortUnboxed#
    
    toOrdered# e = toOrdered# (fromGValue# e)
      where
        fromGValue# :: Proxy# (T14 e) -> Proxy# (I14 e)
        fromGValue# =  \ _ -> proxy#
    
    fromOrdered# = toOrdered#

instance (Enum e, Shape e, Bounded e, Rank1 e, Unboxed e) => Unboxed (T15 e)
  where
    sizeof## e n# = psizeof## e (15# *# n#)
    
    eqUnboxed## e xs# o1# ys# o2# n# = peqUnboxed# e xs# (o1# *# 15#) ys# (o2# *# 15#) (n# *# 15#)
    
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
    
    sortUnboxed# = radixSortUnboxed#
    
    toOrdered# e = toOrdered# (fromGValue# e)
      where
        fromGValue# :: Proxy# (T15 e) -> Proxy# (I15 e)
        fromGValue# =  \ _ -> proxy#
    
    fromOrdered# = toOrdered#

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Default 'fillByteArrayOff#' implementation.
-}
{-# INLINE defaultFillByteArrayOff# #-}
defaultFillByteArrayOff# :: Unboxed e => MutableByteArray# s -> Int# -> Int#
                         -> e -> State# s -> State# s
defaultFillByteArrayOff# bs# o# c# e = go# o#
  where
    go# i# = \ s1# -> if isTrue# (i# >=# n#) then s1# else
      go# (i# +# 1#) (writeByteArray# bs# i# e s1#)
    
    n# = c# +# o#

{- |
  @since 0.3
  
  Recommended 'eqUnboxed##' implementation.
-}
bytewiseEqUnboxed## :: Unboxed e => Proxy# e -> ByteArray# -> Int#
                    -> ByteArray# -> Int# -> Int# -> Bool
bytewiseEqUnboxed## e xs# o1# ys# o2# c# = case c# <# 0# of
  1# -> True
  _  -> let i1# = sizeof## e o1#; i2# = sizeof## e o2#; n# = sizeof## e c#
        in  isTrue# (compareByteArrays# xs# i1# ys# i2# n# ==# 0#)

{- |
  @since 0.3
  
  Default 'eqUnboxed##' implemetation.
-}
defaulteqUnboxed## :: Unboxed e => Proxy# e -> ByteArray# -> Int#
                   -> ByteArray# -> Int# -> Int# -> Bool
defaulteqUnboxed## e xs# xi# ys# yi# n# = case n# ># 0# of {1# -> go xi# yi# n# 1#; _ -> True}
  where
    go  _   _  _  0# = False
    go  _   _  0# b# = isTrue# b#
    go xo# yo# i# b# = go (xo# +# 1#) (yo# +# 1#) (i# -# 1#)
      (case asProxy## e (xs# !# xo#) == (ys# !# yo#) of {True -> b#; _ -> 0#})

--------------------------------------------------------------------------------

radixSortUnboxed# :: Unboxed e => Proxy# e -> MutableByteArray# s -> Int# -> Int#
                  -> State# s -> State# s
radixSortUnboxed# e bs# n# o# = \ s1# -> case toOrdered# e bs# n# o# s1# of
  s2# -> case radixSortX8# e bs# n# o# s2# of
    s3# -> fromOrdered# e bs# n# o# s3#

radixSortX8# :: Unboxed e => Proxy# e -> MutableByteArray# s -> Int# -> Int#
             -> State# s -> State# s
radixSortX8# e bs# n# o# = let s# = sizeof## e 1# in
  \ s1# -> case radixSortIndex# bs# n# o# s# s1# of
    (# s2#, idx# #) -> case indexToOffset# idx# s# s2# of
      s3# -> case calloc## e (sizeof## e n#) s3# of
        (# s4#, tmp# #) -> writeRadixSort# e bs# tmp# idx# n# o# s4#

writeRadixSort# :: Unboxed e => Proxy# e -> MutableByteArray# s -> MutableByteArray# s
                -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
writeRadixSort# e bs# tmp# idx# n# o# = h# bs# o# tmp# 0# s# 0#
  where
    h#  _  _   _  _  0#  _ = \ s1# -> s1#
    h# xs# i# ys# j# c# k# = \ s1# -> case go# xs# i# ys# j# n# k# (256# *# k#) s1# of
      s2# -> h# ys# j# xs# i# (c# -# 1#) (k# +# 1#) s2#
    
    go#  _   _   _   _  0#  _    _ = \ s1# -> s1#
    go# xs# ix# ys# iy# i# bx# io# = \ s1# -> case readWord8Array## xs# bx# s1# of
      (# s2#, w# #) -> let wo# = word2Int# w# +# io# in case readWordArray# idx# wo# s2# of
        (# s3#, off# #) -> case writeWordArray# idx# wo# (off# `plusWord#` ox01#) s3# of
          s4# -> case readUnboxed# xs# ix# s4# of
            (# s5#, v #) -> case writeUnboxed# ys# (word2Int# off#) (asProxy## e v) s5# of
              s6# -> go# xs# (ix# +# 1#) ys# (iy# +# 1#) (i# -# 1#) (bx# +# s#) io# s6#
    
    ox01# = int2Word# 1#
    s#    = sizeof## e 1#

--------------------------------------------------------------------------------

-- | 'sizeof#' is unboxed 'sizeof'.
{-# INLINE sizeof# #-}
sizeof# :: Unboxed e => e -> Int# -> Int#
sizeof# e c# = sizeof## (toProxy## e) c#

{-# INLINE offsetof# #-}
offsetof# :: Unboxed e => e -> Int# -> Int#
offsetof# e i# = offsetof## (toProxy## e) i#

{- |
  @since 0.2.1
  
  Concatenation of two 'Unboxed' arrays.
-}
concat# :: Unboxed e => e -> ByteArray# -> Int# -> Int#
        -> ByteArray# -> Int# -> Int# -> State# s
        -> (# State# s, Int#, MutableByteArray# s #)
concat# e = concat## (toProxy## e)

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Concatenation of two 'Unboxed' arrays.
-}
concat## :: Unboxed e => Proxy# e -> ByteArray# -> Int# -> Int#
         -> ByteArray# -> Int# -> Int# -> State# s
         -> (# State# s, Int#, MutableByteArray# s #)
concat## e arr1# n1# o1# arr2# n2# o2# = \ s1# -> case newUnboxed# e n# s1# of
    (# s2#, marr# #) -> case copyUnboxed## e arr1# o1# marr# 0# n1# s2# of
      s3# -> case copyUnboxed## e arr2# o2# marr# n1# n2# s3# of
        s4# -> (# s4#, n#, marr# #)
  where
    n# = n1# +# n2#

{- |
  @since 0.3
  
  Allocate new mutable byte array and fill it by zero. Note that 'calloc#' use
  'setByteArray#', not 'fillByteArrayOff#' or 'fillByteArray#'.
-}
calloc## :: Unboxed e => Proxy# e -> Int# -> State# s
         -> (# State# s, MutableByteArray# s #)
calloc## e n# = let c# = sizeof## e n# in \ s1# -> case newByteArray# c# s1# of
  (# s2#, mbytes# #) -> case setByteArray# mbytes# 0# c# 0# s2# of
    s3# -> (# s3#, mbytes# #)



