{-# LANGUAGE Trustworthy, CPP, MagicHash, UnboxedTuples, BangPatterns #-}

{- |
    Module      :  SDP.Unboxed.Utils
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Unboxed.Utils" provide service definitions for 'Unboxed' class.
-}
module SDP.Unboxed.Utils
(
  -- * Shifts
  shiftWI#, shift16WI#, shift32WI#, shift64WI#,
  
  -- * 'ByteOrder'
  ByteOrder (..), compareByteArrays#, targetByteOrder,
  
  -- * 'ByteArray#' accessors
  indexWord8Array##, readWord8Array##, writeWord8Array##, writeInt8Array##,
  
  -- * Count and radix sort
  boolCountM#, indexToOffset#, radixSortIndex#, writeRadixIndex#,
  radixSortIndexInt8#,
  
  -- * Commons
  gcd#
)
where

#if MIN_VERSION_base(4,11,0)
import GHC.ByteOrder
#endif

import GHC.Exts

#include <ghcautoconf.h>
#include "MachDeps.h"

default ()

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Convert an 'Int' value to a representation suitable for radix sort.
  Note that this function is designed to read and write using 'Unboxed',
  'Foreign.Storable.Storable' or another compatible API.
-}
{-# INLINE shiftWI# #-}
shiftWI# :: Word# -> Word#
#if SIZEOF_HSWORD == 4
shiftWI# =  xor# (int2Word# 0x01# `shiftL#` 31#)
#else
shiftWI# =  xor# (int2Word# 0x01# `shiftL#` 63#)
#endif

{- |
  @since 0.3
  
  Convert an 'Int16' value to a representation suitable for radix sort.
  Note that this function is designed to read and write using 'Unboxed',
  'Foreign.Storable.Storable' or another compatible API.
-}
{-# INLINE shift16WI# #-}
#if MIN_VERSION_base(4,16,0)
shift16WI# :: Word16# -> Word16#
shift16WI# =  xorWord16# (wordToWord16# (int2Word# 0x8000#))
#else
shift16WI# :: Word# -> Word#
shift16WI# =  xor# (int2Word# 0x8000#)
#endif

{- |
  @since 0.3
  
  Convert an 'Int32' value to a representation suitable for radix sort.
  Note that this function is designed to read and write using 'Unboxed',
  'Foreign.Storable.Storable' or another compatible API.
-}
{-# INLINE shift32WI# #-}
#if MIN_VERSION_base(4,16,0)
shift32WI# :: Word32# -> Word32#
shift32WI# =  xorWord32# (wordToWord32# (int2Word# 0x01# `shiftL#` 31#))
#else
shift32WI# :: Word# -> Word#
shift32WI# =  xor# (int2Word# 0x01# `shiftL#` 31#)
#endif

{- |
  @since 0.3
  
  Convert an 'Int64' value to a representation suitable for radix sort.
  Note that this function is designed to read and write using 'Unboxed',
  'Foreign.Storable.Storable' or another compatible API.
-}
{-# INLINE shift64WI# #-}
shift64WI# :: Word# -> Word#
shift64WI# = xor# (int2Word# 0x01# `shiftL#` 63#)

--------------------------------------------------------------------------------

-- HINT: compareByteArrays# and GHC.ByteOrder is available since base 4.11.0.0

#if !MIN_VERSION_base(4,11,0)

{-
  NOTE: compareByteArrays# here is used only for equality check, so I don't need
  to worry about endianness and the encoding of signed and unsigned integers.
-}
compareByteArrays# :: ByteArray# -> Int# -> ByteArray# -> Int# -> Int# -> Int#
compareByteArrays# xs# ox# ys# oy# n# = go# 0#
  where
    go# i#
        | 1# <-     i# >=# n# = 0#
        | 1# <- eqWord# x# y# = go# (i# +# 1#)
        | 1# <- ltWord# x# y# = -1#
        |                True =  1#
      where
        x# = indexWord8Array# xs# (ox# +# i#)
        y# = indexWord8Array# ys# (oy# +# i#)

-- | Byte ordering.
data ByteOrder = BigEndian | LittleEndian deriving ( Eq, Ord, Bounded, Enum, Read, Show )

one# :: Int
one# =  runST $ ST $ \ s1# -> case newByteArray# 2# s1# of
  (# s2#, marr# #) -> case writeWord16Array# marr# 0# (int2Word# 0x0001#) s2# of
    s3# -> case readWord8Array# marr# 0# s3# of
      (# s4#, i# #) -> (# s4#, I# (word2Int# i#) #)

-- | The byte ordering of the target machine.
targetByteOrder :: ByteOrder
#if defined(WORDS_BIGENDIAN)
targetByteOrder =  BigEndian
#else
targetByteOrder =  if one# == 1 then LittleEndian else BigEndian
#endif

#endif

{-# INLINE indexWord8Array## #-}
indexWord8Array## :: ByteArray# -> Int# -> Word#
#if MIN_VERSION_base(4,16,0)
indexWord8Array## bs# i# = word8ToWord# (indexWord8Array# bs# i#)
#else
indexWord8Array## =  indexWord8Array#
#endif

{-# INLINE readWord8Array## #-}
readWord8Array## :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
#if MIN_VERSION_base(4,16,0)
readWord8Array## bs# i# = \ s1# -> case readWord8Array# bs# i# s1# of
  (# s2#, w8# #) -> (# s2#, word8ToWord# w8# #)
#else
readWord8Array## =  readWord8Array#
#endif

{-# INLINE writeWord8Array## #-}
writeWord8Array## :: MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
#if MIN_VERSION_base(4,16,0)
writeWord8Array## bs# i# v# = writeWord8Array# bs# i# (wordToWord8# v#)
#else
writeWord8Array## =  writeWord8Array#
#endif

{-# INLINE writeInt8Array## #-}
writeInt8Array## :: MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
#if MIN_VERSION_base(4,16,0)
writeInt8Array## bs# i# v# = writeInt8Array# bs# i# (intToInt8# v#)
#else
writeInt8Array## =  writeInt8Array#
#endif

--------------------------------------------------------------------------------

boolCountM# :: MutableByteArray# s -> Int# -> Int#
            -> State# s -> (# State# s, Int# #)
boolCountM# bs# n# o#
    | 1# <- n# <# 1# = \ s1# -> (# s1#, 0# #)
    | 1# <- o# <# 0# = \ s1# -> (# s1#, 0# #)
    | 0# <-      bo# = \ s1# -> case byteCount# (cn# -# 1#) 0# s1# of
      (# s2#, c# #)
        | 0# <- bn# -> (# s2#, c# #)
        |   True    -> case readWord8Array## bs# (cn# +# co#) s2# of
          (# s3#, w# #) ->
            let w'# = w# `and#` not# (uncheckedShiftL# (int2Word# 0xff#) bn#)
            in  (# s3#, c# +# word2Int# (popCnt# w'#) #)
    
    | 1# <- b# <# 8# = \ s1# -> case readWord8Array## bs# co# s1# of
      (# s2#, w# #) ->
        let w'# = w# `and#` not# (uncheckedShiftL# oxff# b#)
                     `and#` uncheckedShiftRL# oxff# bo#
        in  (# s2#, word2Int# (popCnt# w'#) #)
    
    |      True      = \ s1# -> case readWord8Array## bs# co# s1# of
      (# s2#, w# #) ->
        let w'# = w# `and#` uncheckedShiftL# oxff# bo#; b'# = 8# -# bo#
        in  case boolCountM# bs# (n# -# b'#) (o# +# b'#) s2# of
              (# s3#, c# #) -> (# s3#, c# +# word2Int# (popCnt# w'#) #)
  where
    !(# cn#, bn# #) = quotRemInt# n# 8#; oxff# = int2Word# 0xff#
    !(# co#, bo# #) = quotRemInt# o# 8#; b#    = bo# +# n#
    
    byteCount# -1# c# = \ s1# -> (# s1#, c# #)
    byteCount#  i# c# = \ s1# -> case readWord8Array## bs# (i# +# co#) s1# of
      (# s2#, w# #) -> byteCount# (i# -# 1#) (c# +# word2Int# (popCnt# w#)) s2#

--------------------------------------------------------------------------------

indexToOffset# :: MutableByteArray# s -> Int# -> State# s -> State# s
indexToOffset# idx# c# = go# c# 0#
  where
    go# 0#  _ = \ s1# -> s1#
    go# i# j# = \ s1# -> case h# 256# j# (int2Word# 0#) s1# of
        s2# -> go# (i# -# 1#) (j# +# 256#) s2#
    
    h# 0# _  _  = \ s1# -> s1#
    h# e# k# w# = \ s1# -> case readWordArray# idx# k# s1# of
      (# s2#, v# #) -> case writeWordArray# idx# k# w# s2# of
        s3# -> h# (e# -# 1#) (k# +# 1#) (plusWord# v# w#) s3#

{- |
  @radixSortIndex# bs# n# o# s#@ creates a 256-element unpacked array (of
  numbers of type 'Word'), where the value @c@ with key @k@ corresponds to the
  number of elements of the @n@-element array @bs@, the @(o# +# i# *# s#)th@
  byte of which is equal to @k@. In other words, @radixSortIndex#@ iterates
  through all the bytes in the @bs@ array, starting at @o@, with step @s@, and
  up to @(o + n*s)@.
  
  Example:
  @radixSortIndex# bs# n# 7# 8#@ counts the number of elements with the
  corresponding last (if target byte order is little endian, then high) bytes of
  8-byte (64-bit) machine words ('Word64' numbers).
-}
radixSortIndex# :: MutableByteArray# s -> Int# -> Int# -> Int#
                -> State# s -> (# State# s, MutableByteArray# s #)
radixSortIndex# bs# n# o# s# =
  let go# _    -1# = \ s1# -> s1#
      go# idx#  i# = \ s1# -> case writeRadixIndex# bs# n# (o# +# i#) s# idx# (i# *# 256#) s1# of
          s2# -> go# idx# (i# -# 1#) s2#
  
  in  \ s1# -> case calloc# SIZEOF_HSWORD# (256# *# s#) s1# of
        (# s2#, idx# #) -> case go# idx# (s# -# 1#) s2# of
          s3# -> (# s3#, idx# #)

writeRadixIndex# :: MutableByteArray# s -> Int# -> Int# -> Int#
                 -> MutableByteArray# s -> Int# -> State# s -> State# s
writeRadixIndex# bs# n# o# s# idx# io# = case orI# (n# <# 1#) (o# <# 0#) of
    0# -> \ s1# -> go# o# n# s1#
    _  -> \ s1# -> s1#
  where
    ox01# = int2Word# 0x01#
    
    go# _  0# = \ s1# -> s1#
    go# i# j# = \ s1# -> case readWord8Array## bs# i# s1# of
      (# s2#, w# #) ->
        let k# = word2Int# w# +# io#
        in  case readWordArray# idx# k# s2# of
              (# s3#, l# #) -> case writeWordArray# idx# k# (plusWord# l# ox01#) s3# of
                s4# -> go# (i# +# s#) (j# -# 1#) s4#

--------------------------------------------------------------------------------

radixSortIndexInt8# :: MutableByteArray# s -> Int# -> Int# -> State# s
                    -> (# State# s, MutableByteArray# s #)
radixSortIndexInt8# bs# n# o# = \ s1'# -> case calloc# SIZEOF_HSWORD# 256# s1'# of
  (# s2'#, idx# #) -> case orI# (n# <# 1#) (o# <# 0#) of
    1# -> (# s2'#, idx# #)
    _  -> let h# -1# = \ s1# -> s1#
              h#  i# = \ s1# -> case go# n# (i# *# 256#) (o# +# i#) s1# of
                s2# -> h# (i# -# 1#) s2#
              
              go# 0#  _  _  = \ s1# -> s1#
              go# j# i# io# = \ s1# -> case readWord8Array## bs# i# s1# of
                (# s2#, w# #) ->
                  let k# = io# +# andI# 255# (word2Int# w# +# 0x80#) -- Int8 to Word8 range
                  in  case readWordArray# idx# k# s2# of
                        (# s3#, l# #) -> case writeWordArray# idx# k# (plusWord# l# ox01#) s3# of
                          s4# -> go# (j# -# 1#) (i# +# 1#) io# s4#
              
              ox01# = int2Word# 0x01#
          
          in  case h# 0# s2'# of s3'# -> (# s3'#, idx# #)

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Allocate new mutable byte array and fill it by zero. Note that 'calloc#' use
  'setByteArray#', not 'fillByteArrayOff#' or 'fillByteArray#'.
-}
calloc# :: Int# -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
calloc# e# n# = let c# = e# *# n# in \ s1# -> case newByteArray# c# s1# of
  (# s2#, mbytes# #) -> case setByteArray# mbytes# 0# c# 0# s2# of
    s3# -> (# s3#, mbytes# #)

{-# INLINE gcd# #-}
gcd# :: Int# -> Int# -> Int#
gcd# a# 0# = a#
gcd# a# b# = gcd# b# (remInt# a# b#)

