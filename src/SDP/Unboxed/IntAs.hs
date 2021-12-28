{-# LANGUAGE CPP, MagicHash, UnboxedTuples, Trustworthy, GeneralizedNewtypeDeriving #-}

{- |
    Module      :  SDP.Unboxed.IntAs
    Copyright   :  (c) Andrey Mulik 2020-2021
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Unboxed.IntAs" provides additional 'Int' instances for 'Unboxed'.
-}
module SDP.Unboxed.IntAs
(
  IntAs8 (..), IntAs16 (..), IntAs32 (..), IntAs64 (..)
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Unboxed
import SDP.Index

import GHC.Base
import GHC.Exts

import Foreign.Storable

import Text.Read

#include "MachDeps.h"

default ()

--------------------------------------------------------------------------------

-- | Int value as 1 byte.
newtype IntAs8  = IntAs8  Int
  deriving ( Eq, Ord, Enum, Bounded, Num, Real, Integral )

-- | Int value as 2 bytes.
newtype IntAs16 = IntAs16 Int
  deriving ( Eq, Ord, Enum, Bounded, Num, Real, Integral )

-- | Int value as 4 bytes.
newtype IntAs32 = IntAs32 Int
  deriving ( Eq, Ord, Enum, Bounded, Num, Real, Integral )

-- | Int value as 8 bytes.
newtype IntAs64 = IntAs64 Int
  deriving ( Eq, Ord, Enum, Bounded, Num, Real, Integral )

--------------------------------------------------------------------------------

instance Shape IntAs8
instance Shape IntAs16
instance Shape IntAs32
instance Shape IntAs64

instance Index IntAs8  where offset = offsetIntegral
instance Index IntAs16 where offset = offsetIntegral
instance Index IntAs32 where offset = offsetIntegral
instance Index IntAs64 where offset = offsetIntegral

instance Show IntAs8  where show (IntAs8  x) = show x
instance Show IntAs16 where show (IntAs16 x) = show x
instance Show IntAs32 where show (IntAs32 x) = show x
instance Show IntAs64 where show (IntAs64 x) = show x

instance Read IntAs8  where readPrec = IntAs8  <$> readPrec
instance Read IntAs16 where readPrec = IntAs16 <$> readPrec
instance Read IntAs32 where readPrec = IntAs32 <$> readPrec
instance Read IntAs64 where readPrec = IntAs64 <$> readPrec

instance Unboxed IntAs8
  where
    filler = 0
    
    {-# INLINE newUnboxed #-}
    newUnboxed = calloc#
    
    {-# INLINE eqUnboxed# #-}
    eqUnboxed# = bytewiseEqUnboxed#
    
    {-# INLINE sizeof #-}
    sizeof _ n = max 0 n
    
    {-# INLINE (!#) #-}
    bytes#  !#  i# = IntAs8 (I# (indexInt8Array# bytes# i#))
    
    {-# INLINE offsetof# #-}
    offsetof# _ i# = case i# ># 0# of {1# -> quotInt# i# 8#; _ -> 0#}
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readInt8Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, IntAs8 (I# e#) #)
    
    writeByteArray# mbytes# n# (IntAs8 (I# e#)) = writeInt8Array# mbytes# n# e#

instance Unboxed IntAs16
  where
    filler = 0
    
    {-# INLINE newUnboxed #-}
    newUnboxed = calloc#
    
    {-# INLINE eqUnboxed# #-}
    eqUnboxed# = bytewiseEqUnboxed#
    
    {-# INLINE sizeof #-}
    sizeof _ n = max 0 n * 2
    
    {-# INLINE (!#) #-}
    bytes#  !#  i# = IntAs16 (I# (indexInt16Array# bytes# i#))
    
    {-# INLINE offsetof# #-}
    offsetof# _ i# = case i# ># 0# of {1# -> quotInt# i# 4#; _ -> 0#}
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readInt16Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, IntAs16 (I# e#) #)
    
    writeByteArray# mbytes# n# (IntAs16 (I# e#)) = writeInt16Array# mbytes# n# e#

instance Unboxed IntAs32
  where
    filler = 0
    
    {-# INLINE newUnboxed #-}
    newUnboxed = calloc#
    
    {-# INLINE eqUnboxed# #-}
    eqUnboxed# = bytewiseEqUnboxed#
    
    {-# INLINE sizeof #-}
    sizeof _ n = max 0 n * 4
    
    {-# INLINE (!#) #-}
    bytes#  !#  i# = IntAs32 (I# (indexInt32Array# bytes# i#))
    
    {-# INLINE offsetof# #-}
    offsetof# _ i# = case i# ># 0# of {1# -> quotInt# i# 2#; _ -> 0#}
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readInt32Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, IntAs32 (I# e#) #)
    
    writeByteArray# mbytes# n# (IntAs32 (I# e#)) = writeInt32Array# mbytes# n# e#

instance Unboxed IntAs64
  where
    filler = 0
    
    {-# INLINE newUnboxed #-}
    newUnboxed = calloc#
    
    {-# INLINE eqUnboxed# #-}
    eqUnboxed# = bytewiseEqUnboxed#
    
    {-# INLINE sizeof #-}
    sizeof _ n = max 0 n * 8
    
    {-# INLINE (!#) #-}
    bytes#  !#  i# = IntAs64 (I# (indexInt64Array# bytes# i#))
    
    {-# INLINE offsetof# #-}
    offsetof# _ i# = case i# ># 0# of {1# -> i#; _ -> 0#}
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readInt64Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, IntAs64 (I# e#) #)
    
    writeByteArray# mbytes# n# (IntAs64 (I# e#)) = writeInt64Array# mbytes# n# e#

--------------------------------------------------------------------------------

instance Storable IntAs8
  where
    sizeOf _ = SIZEOF_INT8; alignment _ = ALIGNMENT_INT8
    
    peekElemOff (Ptr p#) (I# i#) = IO $
      \ s1# -> case readInt8OffAddr# p# i# s1# of
        (# s2#, x #) -> (# s2#, IntAs8 (I# x) #)
    
    pokeElemOff (Ptr p#) (I# i#) (IntAs8 (I# x)) = IO $
      \ s1# -> case writeInt8OffAddr# p# i# x s1# of s2 -> (# s2, () #)

instance Storable IntAs16
  where
    sizeOf _ = SIZEOF_INT16; alignment _ = ALIGNMENT_INT16
    
    peekElemOff (Ptr p#) (I# i#) = IO $
      \ s1# -> case readInt16OffAddr# p# i# s1# of
        (# s2, x #) -> (# s2, IntAs16 (I# x) #)
    
    pokeElemOff (Ptr p#) (I# i#) (IntAs16 (I# x)) = IO $
      \ s1# -> case writeInt16OffAddr# p# i# x s1# of s2 -> (# s2, () #)

instance Storable IntAs32
  where
    sizeOf _ = SIZEOF_INT32; alignment _ = ALIGNMENT_INT32
    
    peekElemOff (Ptr p#) (I# i#) = IO $
      \ s1# -> case readInt32OffAddr# p# i# s1# of
        (# s2, x #) -> (# s2, IntAs32 (I# x) #)
    
    pokeElemOff (Ptr p#) (I# i#) (IntAs32 (I# x)) = IO $
      \ s1# -> case writeInt32OffAddr# p# i# x s1# of s2 -> (# s2, () #)

instance Storable IntAs64
  where
    sizeOf _ = SIZEOF_INT64; alignment _ = ALIGNMENT_INT64
    
    peekElemOff (Ptr p#) (I# i#) = IO $
      \ s1# -> case readInt64OffAddr# p# i# s1# of
        (# s2, x #) -> (# s2, IntAs64 (I# x) #)
    
    pokeElemOff (Ptr p#) (I# i#) (IntAs64 (I# x)) = IO $
      \ s1# -> case writeInt64OffAddr# p# i# x s1# of s2 -> (# s2, () #)


