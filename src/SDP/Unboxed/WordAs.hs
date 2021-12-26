{-# LANGUAGE CPP, MagicHash, UnboxedTuples, Trustworthy, GeneralizedNewtypeDeriving #-}

{- |
    Module      :  SDP.Unboxed.WordAs
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Unboxed.WordAs" provides additional 'Word' instances for 'Unboxed'.
-}
module SDP.Unboxed.WordAs
(
  WordAs8 (..), WordAs16 (..), WordAs32 (..), WordAs64 (..)
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

-- | Word value as 1 byte.
newtype WordAs8  = WordAs8  Word
  deriving ( Eq, Ord, Enum, Bounded, Num, Real, Integral )

-- | Word value as 2 bytes.
newtype WordAs16 = WordAs16 Word
  deriving ( Eq, Ord, Enum, Bounded, Num, Real, Integral )

-- | Word value as 4 bytes.
newtype WordAs32 = WordAs32 Word
  deriving ( Eq, Ord, Enum, Bounded, Num, Real, Integral )

-- | Word value as 8 bytes.
newtype WordAs64 = WordAs64 Word
  deriving ( Eq, Ord, Enum, Bounded, Num, Real, Integral )

--------------------------------------------------------------------------------

instance Shape WordAs8
instance Shape WordAs16
instance Shape WordAs32
instance Shape WordAs64

instance Index WordAs8  where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign
instance Index WordAs16 where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign
instance Index WordAs32 where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign
instance Index WordAs64 where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign

instance Show WordAs8  where show (WordAs8  x) = show x
instance Show WordAs16 where show (WordAs16 x) = show x
instance Show WordAs32 where show (WordAs32 x) = show x
instance Show WordAs64 where show (WordAs64 x) = show x

instance Read WordAs8  where readPrec = WordAs8  <$> readPrec
instance Read WordAs16 where readPrec = WordAs16 <$> readPrec
instance Read WordAs32 where readPrec = WordAs32 <$> readPrec
instance Read WordAs64 where readPrec = WordAs64 <$> readPrec

instance Unboxed WordAs8
  where
    filler = 0
    
    {-# INLINE newUnboxed #-}
    newUnboxed = calloc#
    
    {-# INLINE sizeof #-}
    sizeof _ n = max 0 n
    
    {-# INLINE offsetof# #-}
    offsetof# _ i# = case i# ># 0# of {1# -> quotInt# i# 8#; _ -> 0#}
    
    {-# INLINE (!#) #-}
    bytes#  !#  i# = WordAs8 (W# (indexWord8Array# bytes# i#))
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readWord8Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, WordAs8 (W# e#) #)
    
    writeByteArray# mbytes# n# (WordAs8 (W# e#)) = writeWord8Array# mbytes# n# e#

instance Unboxed WordAs16
  where
    filler = 0
    
    {-# INLINE newUnboxed #-}
    newUnboxed = calloc#
    
    {-# INLINE sizeof #-}
    sizeof _ n = max 0 n * 2
    
    {-# INLINE (!#) #-}
    bytes#  !#  i# = WordAs16 (W# (indexWord16Array# bytes# i#))
    
    {-# INLINE offsetof# #-}
    offsetof# _ i# = case i# ># 0# of {1# -> quotInt# i# 4#; _ -> 0#}
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readWord16Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, WordAs16 (W# e#) #)
    
    writeByteArray# mbytes# n# (WordAs16 (W# e#)) = writeWord16Array# mbytes# n# e#

instance Unboxed WordAs32
  where
    filler = 0
    
    {-# INLINE newUnboxed #-}
    newUnboxed = calloc#
    
    {-# INLINE sizeof #-}
    sizeof _ n = max 0 n * 4
    
    {-# INLINE (!#) #-}
    bytes#  !#  i# = WordAs32 (W# (indexWord32Array# bytes# i#))
    
    {-# INLINE offsetof# #-}
    offsetof# _ i# = case i# ># 0# of {1# -> quotInt# i# 4#; _ -> 0#}
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readWord32Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, WordAs32 (W# e#) #)
    
    writeByteArray# mbytes# n# (WordAs32 (W# e#)) = writeWord32Array# mbytes# n# e#

instance Unboxed WordAs64
  where
    filler = 0
    
    {-# INLINE newUnboxed #-}
    newUnboxed = calloc#
    
    {-# INLINE sizeof #-}
    sizeof _ n = max 0 n * 8
    
    {-# INLINE (!#) #-}
    bytes#  !#  i# = WordAs64 (W# (indexWord64Array# bytes# i#))
    
    {-# INLINE offsetof# #-}
    offsetof# _ i# = case i# ># 0# of {1# -> i#; _ -> 0#}
    
    {-# INLINE readUnboxed# #-}
    readUnboxed# mbytes# i# = \ s1# -> case readWord64Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, WordAs64 (W# e#) #)
    
    writeByteArray# mbytes# n# (WordAs64 (W# e#)) = writeWord64Array# mbytes# n# e#

--------------------------------------------------------------------------------

instance Storable WordAs8
  where
    sizeOf _ = SIZEOF_WORD8; alignment _ = ALIGNMENT_WORD8
    
    peekElemOff (Ptr p#) (I# i#) = IO $
      \ s1# -> case readWord8OffAddr# p# i# s1# of
        (# s2#, x #) -> (# s2#, WordAs8 (W# x) #)
    
    pokeElemOff (Ptr p#) (I# i#) (WordAs8 (W# x)) = IO $
      \ s1# -> case writeWord8OffAddr# p# i# x s1# of s2 -> (# s2, () #)

instance Storable WordAs16
  where
    sizeOf _ = SIZEOF_WORD16; alignment _ = ALIGNMENT_WORD16
    
    peekElemOff (Ptr p#) (I# i#) = IO $
      \ s1# -> case readWord16OffAddr# p# i# s1# of
        (# s2, x #) -> (# s2, WordAs16 (W# x) #)
    
    pokeElemOff (Ptr p#) (I# i#) (WordAs16 (W# x)) = IO $
      \ s1# -> case writeWord16OffAddr# p# i# x s1# of s2 -> (# s2, () #)

instance Storable WordAs32
  where
    sizeOf _ = SIZEOF_WORD32; alignment _ = ALIGNMENT_WORD32
    
    peekElemOff (Ptr p#) (I# i#) = IO $
      \ s1# -> case readWord32OffAddr# p# i# s1# of
        (# s2, x #) -> (# s2, WordAs32 (W# x) #)
    
    pokeElemOff (Ptr p#) (I# i#) (WordAs32 (W# x)) = IO $
      \ s1# -> case writeWord32OffAddr# p# i# x s1# of s2 -> (# s2, () #)

instance Storable WordAs64
  where
    sizeOf _ = SIZEOF_WORD64; alignment _ = ALIGNMENT_WORD64
    
    peekElemOff (Ptr p#) (I# i#) = IO $
      \ s1# -> case readWord64OffAddr# p# i# s1# of
        (# s2, x #) -> (# s2, WordAs64 (W# x) #)
    
    pokeElemOff (Ptr p#) (I# i#) (WordAs64 (W# x)) = IO $
      \ s1# -> case writeWord64OffAddr# p# i# x s1# of s2 -> (# s2, () #)




