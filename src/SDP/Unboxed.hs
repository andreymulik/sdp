{-# LANGUAGE Trustworthy, MagicHash, UnboxedTuples, BangPatterns #-}

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
  -- * Export
  module SDP.Proxy,
  
  -- * Unboxed
  Unboxed (..), cloneUnboxed#, cloneUnboxedM#, thawUnboxed#, freezeUnboxed#,
  bytewiseEqUnboxed##, radixSortUnboxed#, copyUnboxed#, copyUnboxedM#,
  sizeof, offsetof, chunkof,
  
  -- ** Kind @(Type -> Type)@ proxies
  psizeof#, psizeof, pchunkof, pchunkof#, poffsetof#, poffsetof,
  pnewUnboxed, pcopyUnboxed, pcopyUnboxedM, pcloneUnboxed, pcloneUnboxedM,
  pthawUnboxed, pfreezeUnboxed, cloneUnboxed1#, peqUnboxed,
  
  -- ** Kind @(Type -> Type -> Type)@ proxies
  pnewUnboxed1, pcloneUnboxed1, pcopyUnboxed1, pcopyUnboxedM1, pcloneUnboxedM1,
  
  -- * Wrap helpers
  Wrap (..), lzero#, single#, fromList#, fromFoldable#, fromListN#,
  newLinear#, newLinearN#, fromFoldableM#, concat#, pconcat,
  
  -- * Byte order
  ByteOrder (..), targetByteOrder
)
where

import SDP.Unboxed.Class
import SDP.Unboxed.Utils
import SDP.Proxy

import GHC.Exts
import GHC.ST

default ()

--------------------------------------------------------------------------------

-- | See 'sizeof##'.
{-# INLINE sizeof #-}
sizeof :: Unboxed e => e -> Int -> Int
sizeof e (I# c#) = I# (sizeof# e c#)

{- |
  @since 0.3
  
  See 'chunkof##'.
-}
{-# INLINE chunkof #-}
chunkof :: Unboxed e => e -> (Int, Int)
chunkof e = case chunkof# e of (# l#, c# #) -> (I# l#, I# c#)

{- |
  @since 0.3
  
  See 'offsetof##'.
-}
{-# INLINE offsetof #-}
offsetof :: Unboxed e => e -> Int -> Int
offsetof e (I# i#) = I# (offsetof# e i#)

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  The size of the minimal block of memory (in bytes) and the maximum number
  of values in it for a given type.
  
  @
    -- 16 bytes (8 bytes for each component) and one value
    chunkof## (proxy# :: Proxy# Ratio Int64) === (# 16#, 1# #)
    
    -- 8 bytes, one value
    chunkof## (proxy# :: Proxy# Int64) === (# 8#, 1# #)
    
    -- 4 bytes, one value
    chunkof## (proxy# :: Proxy# Int32) === (# 4#, 1# #)
    chunkof## (proxy# :: Proxy#  Char) === (# 4#, 1# #)
    
    -- 1 byte, 8 values
    chunkof## (proxy# :: Proxy# Bool) === (# 1#, 8# #)
  @
-}
chunkof# :: Unboxed e => e -> (# Int#, Int# #)
chunkof# e = chunkof## (toProxy## e)

--------------------------------------------------------------------------------

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

poffsetof :: Unboxed e => proxy e -> Int -> Int
poffsetof =  offsetof . fromProxy

--------------------------------------------------------------------------------

{- |
  @since 0.2.1
  
  'psizeof#' is proxy version of 'sizeof##'.
-}
psizeof# :: Unboxed e => proxy e -> Int# -> Int#
psizeof# =  sizeof# . fromProxy
{- |
  @since 0.3
  
  'pchunkof#' is proxy version of 'chunkof##'.
-}
pchunkof# :: Unboxed e => proxy e -> (# Int#, Int# #)
pchunkof# e = chunkof# (fromProxy e)

poffsetof# :: Unboxed e => proxy e -> Int# -> Int#
poffsetof# e = offsetof# (fromProxy e)

--------------------------------------------------------------------------------

{- |
  @since 0.2
  
  Kind @(Type -> Type)@ proxy version if 'copyUnboxed#'.
-}
pcopyUnboxed :: Unboxed e => proxy e -> ByteArray# -> Int#
             -> MutableByteArray# s -> Int# -> Int#
             -> State# s -> State# s
pcopyUnboxed =  copyUnboxed# . fromProxy

{- |
  @since 0.2
  
  Kind @(Type -> Type)@ proxy version if 'copyUnboxedM#'.
-}
pcopyUnboxedM :: Unboxed e => proxy e -> MutableByteArray# s -> Int#
              -> MutableByteArray# s -> Int# -> Int#
              -> State# s -> State# s
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
  @since 0.2
  
  @'cloneUnboxed#' e bytes# o# c#@ creates new @c#@-element length immutable
  slice of @bytes#@ beginning from @o#@-th element.
-}
cloneUnboxed# :: Unboxed e => e -> ByteArray# -> Int# -> Int# -> ByteArray#
cloneUnboxed# e = cloneUnboxed## (toProxy## e)

{- |
  @since 0.2.1
  
  Kind @(Type -> Type)@ proxy version of 'cloneUnboxed#'.
-}
pcloneUnboxedM :: Unboxed e => proxy e -> MutableByteArray# s -> Int# -> Int#
               -> State# s -> (# State# s, MutableByteArray# s #)
pcloneUnboxedM =  cloneUnboxedM# . fromProxy

{- |
  @since 0.2.1
  
  Kind @(Type -> Type)@ proxy version of 'thawUnboxed#'.
-}
pthawUnboxed :: Unboxed e => proxy e -> ByteArray# -> Int#
             -> State# s -> (# State# s, MutableByteArray# s #)
pthawUnboxed =  thawUnboxed# . fromProxy

{- |
  @since 0.2.1
  
  Kind @(Type -> Type)@ proxy version of 'pfreezeUnboxed'.
-}
pfreezeUnboxed :: Unboxed e => proxy e -> MutableByteArray# s -> Int#
               -> State# s -> (# State# s, ByteArray# #)
pfreezeUnboxed =  freezeUnboxed# . fromProxy

--------------------------------------------------------------------------------

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
newLinear# :: Unboxed e => [e] -> State# s
           -> (# State# s, MutableByteArray# s #)
newLinear# es = let !(I# n#) = length es in newLinearN# n# es

{- |
  @since 0.2.1
  
  Create mutable 'Unboxed' array from known size list.
-}
newLinearN# :: Unboxed e => Int# -> [e] -> State# s
            -> (# State# s, MutableByteArray# s #)
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
  
  Create mutable 'Unboxed' array from 'Foldable' stream.
-}
fromFoldableM# :: (Foldable f, Unboxed e) => f e -> State# s
               -> (# State# s, Int, MutableByteArray# s #)
fromFoldableM# es = \ s1# -> case pnewUnboxed es n# s1# of
    (# s2#, marr# #) ->
      let
        go y r = \ i# s4# -> case writeUnboxed# marr# i# y s4# of
          s5# -> if isTrue# (i# ==# n# -# 1#) then s5# else r (i# +# 1#) s5#
      in case if n == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2# of
          s3# -> (# s3#, n, marr# #)
  where
    !n@(I# n#) = length es

--------------------------------------------------------------------------------

{- |
  @since 0.2.1
  
  @'cloneUnboxedM#' e mbytes# o# c#@ creates new @c#@-element length mutable
  slice of @bytes#@ beginning from @o#@-th element.
-}
cloneUnboxedM# :: Unboxed e => e -> MutableByteArray# s -> Int# -> Int#
               -> State# s -> (# State# s, MutableByteArray# s #)
cloneUnboxedM# e mbytes# o# n# = \ s1# -> case newByteArray# (sizeof# e n#) s1# of
  (# s2#, copy# #) -> case copyUnboxedM# e mbytes# o# copy# 0# n# s2# of
    s3# -> (# s3#, copy# #)

--------------------------------------------------------------------------------

{- |
  @since 0.2.1
  
  @'thawUnboxed#' e bytes# c#@ creates new @sizeof# e c#@ bytes length
  'MutableByteArray#' and copy @bytes#@ to it.
-}
thawUnboxed# :: Unboxed e => e -> ByteArray# -> Int#
             -> State# s -> (# State# s, MutableByteArray# s #)
thawUnboxed# e bytes# c# = let n# = sizeof# e c# in \ s1# -> case newByteArray# n# s1# of
  (# s2#, mbytes# #) -> case copyByteArray# bytes# 0# mbytes# 0# n# s2# of
    s3# -> (# s3#, mbytes# #)

{- |
  @since 0.2.1
  
  @'freezeUnboxed#' e mbytes# c#@ creates new @sizeof# e c#@ bytes length
  'ByteArray#' and copy @mbytes#@ to it.
-}
freezeUnboxed# :: Unboxed e => e -> MutableByteArray# s -> Int#
               -> State# s -> (# State# s, ByteArray# #)
freezeUnboxed# e mbytes# n# = \ s1# -> case cloneUnboxedM# e mbytes# 0# n# s1# of
  (# s2#, copy# #) -> unsafeFreezeByteArray# copy# s2#

{- |
  @'copyUnboxed#' e bytes\# o1\# mbytes\# o2\# n\#@ unsafely writes elements
  from @bytes\#@ to @mbytes\#@, where o1\# and o2\# - offsets (element
  count), @n\#@ - count of elements to copy.
-}
copyUnboxed# :: Unboxed e => e -> ByteArray# -> Int# -> MutableByteArray# s
             -> Int# -> Int# -> State# s -> State# s
copyUnboxed# e = copyUnboxed## (toProxy## e)

{- |
  @'copyUnboxedM#' e msrc\# o1\# mbytes\# o2\# n\#@ unsafely writes elements
  from @msrc\#@ to @mbytes\#@, where o1\# and o2\# - offsets (element
  count), @n\#@ - count of elements to copy.
-}
copyUnboxedM# :: Unboxed e => e -> MutableByteArray# s -> Int#
              -> MutableByteArray# s -> Int# -> Int#
              -> State# s -> State# s
copyUnboxedM# e = copyUnboxedM## (toProxy## e)

--------------------------------------------------------------------------------

-- | Proxy concatenation of two byte arrays representing 'Unboxed' structures.
pconcat :: Unboxed e => proxy e -> ByteArray# -> Int# -> Int#
        -> ByteArray# -> Int# -> Int# -> State# s
        -> (# State# s, Int#, MutableByteArray# s #)
pconcat = concat# . fromProxy

--------------------------------------------------------------------------------

{- |
  @since 0.2
  
  Kind @(Type -> Type -> Type)@ proxy version of 'newUnboxed'.
-}
pnewUnboxed1 :: Unboxed e => p (proxy e) -> Int# -> State# s
             -> (# State# s, MutableByteArray# s #)
pnewUnboxed1 =  newUnboxed . fromProxy1

{- |
  @since 0.2
  
  Kind @(Type -> Type -> Type)@ proxy version of 'copyUnboxed#'.
-}
pcopyUnboxed1 :: Unboxed e => p (proxy e) -> ByteArray# -> Int#
              -> MutableByteArray# s -> Int# -> Int#
              -> State# s -> State# s
pcopyUnboxed1 =  copyUnboxed# . fromProxy1

{- |
  @since 0.2.1
  
  Kind @(Type -> Type -> Type)@ proxy version of 'copyUnboxedM#'.
-}
pcopyUnboxedM1 :: Unboxed e => p (proxy e) -> MutableByteArray# s -> Int#
               -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
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
pcloneUnboxedM1 :: Unboxed e => p (proxy e) -> MutableByteArray# s -> Int# -> Int#
                -> State# s -> (# State# s, MutableByteArray# s #)
pcloneUnboxedM1 =  cloneUnboxedM# . fromProxy1



