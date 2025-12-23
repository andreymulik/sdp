{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, GADTs #-}
{-# LANGUAGE Trustworthy, MagicHash, UnboxedTuples, KindSignatures #-}

{- |
    Module      :  SDP.Prim.SBytes.IO
    Copyright   :  (c) Andrey Mulik 2025
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Prim.SBytes.IO" provides boxed pseudo-primitive safe array.
-}
module SDP.Prim.SBytes.IO
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * Pseudo-primitive types
  MIOBytes#, IOBytes#, fromMIOBytes#, unpackMIOBytes#, offsetMIOBytes#,
  packMIOBytes#, coerceMIOBytes#,
  
  unsafeCoerceMIOBytes#, unsafeSBytesToPtr#, unsafePtrToSBytes#
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Prim.SBytes.ST
import SDP.Prim.SBytes
import SDP.IndexedM
import SDP.SortM

import GHC.Exts ( MutableByteArray#, State#, Int# )

import qualified Foreign as F

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

-- | 'MIOBytes#' is mutable preudo-primitive 'Int'-indexed lazy boxed array.
data MIOBytes# io e
  where
    MIOBytes# :: (MonadIO io, Unboxed e) => STBytes# RealWorld e -> MIOBytes# io e

-- | 'IOBytes#' is mutable preudo-primitive 'Int'-indexed lazy boxed array.
type IOBytes# = MIOBytes# IO

--------------------------------------------------------------------------------

instance Eq (MIOBytes# io e)
  where
    MIOBytes# xs# == MIOBytes# ys# = xs# == ys#

--------------------------------------------------------------------------------

{- NullableM instance. -}

instance (MonadIO io, Unboxed e) => NullableM io (MIOBytes# io e)
  where
    newNull = pack newNull
    isNullM = stToMIO . isNullM . unpack

--------------------------------------------------------------------------------

{- Estimate and EstimateM instances. -}

instance Estimate (MIOBytes# io e)
  where
    sizeHint = sizeHint . unpack
    sizeOf   = sizeOf . unpack
    
    (<==>) = on (<=>) sizeOf
    (<.=>) = (<=>) . sizeOf
    
    (.==.) = on (==) sizeOf
    (./=.) = on (/=) sizeOf
    (.<=.) = on (<=) sizeOf
    (.>=.) = on (>=) sizeOf
    (.>.)  = on (>)  sizeOf
    (.<.)  = on (<)  sizeOf
    
    (.==) = (==) . sizeOf
    (./=) = (/=) . sizeOf
    (.>=) = (>=) . sizeOf
    (.<=) = (<=) . sizeOf
    (.>)  = (>)  . sizeOf
    (.<)  = (<)  . sizeOf
    
    shrinkTo n (MIOBytes# es) = MIOBytes# (shrinkTo n es)

instance MonadIO io => EstimateM io (MIOBytes# io e)
  where
    getSizeHint = getSizeHint . unpack
    getSizeOf   = getSizeOf . unpack
    
    estimateMGE = pure ... (.>=.)
    estimateMLE = pure ... (.<=.)
    estimateMGT = pure ... (.>.)
    estimateMLT = pure ... (.<.)
    estimateMNE = pure ... (./=.)
    estimateMEQ = pure ... (.==.)
    
    notShorterThanM = pure ... (.>=)
    noLongerThanM   = pure ... (.<=)
    longerThanM     = pure ... (.>)
    shorterThanM    = pure ... (.<)
    otherLengthM    = pure ... (./=)
    hasLengthM      = pure ... (.==)
    
    (<<=>>) = pure ... (<==>)
    (<=>>)  = pure ... (<.=>)

--------------------------------------------------------------------------------

{- Bordered and BorderedM instances. -}

instance Bordered (MIOBytes# io e) Int
  where
    lower    = const 0
    upper    = upper . unpack
    bounds   = bounds . unpack
    indices  = indices . unpack
    indexOf  = indexOf . unpack
    indexIn  = indexIn . unpack
    offsetOf = offsetOf . unpack
    
    eitherViewOf bnds (MIOBytes# es) = MIOBytes# <$> eitherViewOf bnds es

instance MonadIO io => BorderedM io (MIOBytes# io e) Int
  where
    getIndexOf = pure ... indexOf . unpack
    getIndices = pure . indices . unpack
    getBounds  = pure . bounds . unpack
    getUpper   = pure . upper . unpack
    getLower _ = pure 0
    
    getEitherViewOf = pure ... eitherViewOf

--------------------------------------------------------------------------------

{- ForceableM instance. -}

instance (MonadIO io, Unboxed e) => ForceableM io (MIOBytes# io e)
  where
    copied (MIOBytes# es) = pack (copied es)

--------------------------------------------------------------------------------

{- ConcatM instance. -}

instance (MonadIO io, Unboxed e) => ConcatM io (MIOBytes# io e)
  where
    MIOBytes# xs <~> MIOBytes# ys = MIOBytes# <$> stToMIO (xs <~> ys)
    
    concatM = concatMapM pure
    
    concatMapM f ess = do
      -- create empty buffer
      buff <- newNull
      
      -- write all structures to buffer
      foldr (\ es go -> do appendBufferM buff =<< f es; go) (pure ()) ess
      
      -- Since we don't use the buffer any further, we can turn it into STBytes#
      unsafeFromBufferM buff

--------------------------------------------------------------------------------

{- SequenceM and LinearM instances. -}

instance (MonadIO io, Unboxed e) => SequenceM io (MIOBytes# io e) e
  where
    foldrM f base es = go 0 (sizeOf es)
      where
        go i n = i >= n ? pure base $ do
          e   <- unsafeReadByOff es i
          acc <- go (i + 1) n
          f e acc
    
    foldlM f base es = go (sizeOf es) 0
      where
        go i n = i < n ? pure base $ do
          e   <- unsafeReadByOff es i
          acc <- go (i - 1) n
          f acc e
    
    ofoldrM f base es = go 0 (sizeOf es)
      where
        go i n = i >= n ? pure base $ do
          e   <- unsafeReadByOff es i
          acc <- go (i + 1) n
          f i e acc
    
    ofoldlM f base es = go (sizeOf es) 0
      where
        go i n = i < n ? pure base $ do
          e   <- unsafeReadByOff es i
          acc <- go (i - 1) n
          f i acc e
    
    getLeft  = stToMIO . getLeft  . unpack
    getRight = stToMIO . getRight . unpack

instance (Unboxed e, MonadIO io) => LinearM io (MIOBytes# io e) e
  where
    prepend e es = pack $ prepend e (unpack es)
    append  es e = pack $ append (unpack es) e
    
    unconsM' = fmap (second MIOBytes# <$>) . stToMIO . unconsM' . unpack
    unsnocM' = fmap (first  MIOBytes# <$>) . stToMIO . unsnocM' . unpack
    
    headM' = stToMIO . headM' . unpack
    lastM' = stToMIO . lastM' . unpack
    
    tailM' = fmap (MIOBytes# <$>) . stToMIO . tailM' . unpack
    initM' = fmap (MIOBytes# <$>) . stToMIO . initM' . unpack
    
    takeM n = pack . takeM n . unpack
    dropM n = pack . dropM n . unpack
    keepM n = pack . keepM n . unpack
    sansM n = pack . sansM n . unpack
    
    singleM = pack . singleM
    
    newLinear     = pack . newLinear
    newLinearN    = pack ... newLinearN
    fromFoldableM = pack . fromFoldableM
    
    reverseM = pack . reverseM . unpack
    reversed = stToMIO . reversed . unpack
    
    (!*) = unsafeReadByOff
    
    unsafeReadByOff es i = stToMIO $ unsafeReadByOff (unpack es) i
    
    writeM = writeM'
    
    unsafeWriteM es i e = stToMIO $ unsafeWriteM (unpack es) i e
    
    unsafeCopyTo src so trg to = stToMIO . unsafeCopyTo (unpack src) so (unpack trg) to
    
    unsafeCopyM es = pack ... unsafeCopyM (unpack es)

--------------------------------------------------------------------------------

{- MapM and IndexedM instances. -}

instance (Unboxed e, MonadIO io) => MapM io (MIOBytes# io e) Int e
  where
    newMap' e ascs =
      let bnds = rangeBounds (fsts ascs)
      in  isNull ascs ? newNull $ fromAssocs' bnds e ascs
    
    writeM' es = stToMIO ... writeM' (unpack es)
    
    unsafeReadMByKey = stToMIO ... unsafeReadMByKey . unpack
    
    overwrite = stToMIO ... overwrite . unpack
    kfoldrM   = ofoldrM
    kfoldlM   = ofoldlM

instance (Unboxed e, MonadIO io) => IndexedM io (MIOBytes# io e) Int e
  where
    fromAssocs  bnds = pack  .  fromAssocs  bnds
    fromAssocs' bnds = pack ... fromAssocs' bnds
    
    fromIndexed' = pack . fromIndexed'
    
    fromIndexedM es = do
      copy <- flip mreplicate (unreachEx "fromIndexedM") =<< getSizeOf es
      copy <$ ofoldrM (\ i e _ -> unsafeWriteM copy i e) () es

--------------------------------------------------------------------------------

{- SortM instance. -}

instance (Unboxed e, MonadIO io) => SortM io (MIOBytes# io e) e
  where
    sortedMBy f = stToMIO . sortedMBy f . unpack
    sortMBy   f = stToMIO .  sortMBy f  . unpack

--------------------------------------------------------------------------------

{- Thaw and Freeze instances. -}

instance (MonadIO io, Unboxed e) => Thaw io (SBytes# e) (MIOBytes# io e)
  where
    unsafeThaw = pack . unsafeThaw
    thaw       = pack . thaw

instance (MonadIO io, Unboxed e) => Freeze io (MIOBytes# io e) (SBytes# e)
  where
    unsafeFreeze = stToMIO . unsafeFreeze . unpack
    freeze       = stToMIO . freeze . unpack

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  'unpackMIOBytes#' returns 'MutableByteArray#' field of 'STBytes#' or fails.
-}
unpackMIOBytes# :: MIOBytes# io e -> MutableByteArray# RealWorld
unpackMIOBytes# =  \ (MIOBytes# es) -> unpackSTBytes# es

{- |
  @since 0.3
  
  'offsetMIOBytes#' returns 'STBytes#' offset in elements.
-}
offsetMIOBytes# :: MIOBytes# io e -> Int#
offsetMIOBytes# =  \ (MIOBytes# es) -> offsetSTBytes# es

{- |
  @since 0.3
  
  'packMIOBytes#' creates new 'MIOBytes#' from sized 'MutableArray#'.
-}
packMIOBytes# :: (MonadIO io, Unboxed e) => Int -> Int -> MutableByteArray# RealWorld -> MIOBytes# io e
packMIOBytes# n o marr# = MIOBytes# (packSTBytes# n o marr#)

{- |
  @since 0.3
  
  'fromMIOBytes#' returns new 'MutableByteArray#'.
-}
fromMIOBytes# :: MIOBytes# io e -> State# RealWorld -> (# State# RealWorld, MutableByteArray# RealWorld #)
fromMIOBytes# =  \ (MIOBytes# es) -> fromSTBytes# es

{- |
  @since 0.3
  
  'coerceMIOBytes#' is 'coerce' alias.
-}
coerceMIOBytes# :: Unboxed b => MIOBytes# io a -> MIOBytes# io b
coerceMIOBytes# =  \ (MIOBytes# es) -> MIOBytes# (coerceSTBytes# es)

--------------------------------------------------------------------------------

{- |
  'unsafeCoerceMIOBytes#' is unsafe low-lowel coerce of an mutable array with
  recounting the number of elements and offset (with possible rounding).
-}
unsafeCoerceMIOBytes# :: Unboxed b => MIOBytes# io a -> MIOBytes# io b
unsafeCoerceMIOBytes# (MIOBytes# es) = MIOBytes# (coerceSTBytes# es)

{- |
  @'unsafeSBytesToPtr#' es@ byte-wise stores 'SBytes#' content to 'F.Ptr'.
  Returns the number of overwritten elements and a pointer to
  @psizeof es (sizeOf es)@ bytes of allocated memory.
-}
unsafeSBytesToPtr# :: SBytes# e -> IO (Int, F.Ptr e)
unsafeSBytesToPtr# es = do
  let es' = unsafeCoerceSBytes# es :: SBytes# Word8
  
  let n = sizeOf es'
  ptr <- F.mallocBytes n
  ofoldr (\ i e go -> do F.pokeByteOff ptr i e; go) (pure (n, ptr)) es'

{- |
  @'unsafePtrToSBytes#' n ptr@ byte-wise stores @n@ elements of 'F.Ptr' @ptr@ to
  'SBytes#'.
-}
unsafePtrToSBytes# :: Unboxed e => (Int, F.Ptr e) -> IO (SBytes# e)
unsafePtrToSBytes# (c, ptr) = do
  let
    mreplicate' :: Unboxed e => proxy e -> e -> IO (IOBytes# e)
    mreplicate' =  const $ mreplicate (max 0 c)
  
  es <- mreplicate' ptr (unreachEx "unsafePtrToSBytes#")
  
  let es' = unsafeCoerceMIOBytes# es :: IOBytes# Word8
  void $ mupdate es' (const . F.peekByteOff ptr)
  unsafeFreeze es

--------------------------------------------------------------------------------

newtype MIOBuffer# e = MIOBuffer# (BufferForM (ST RealWorld) (STBytes# RealWorld e))

instance (MonadIO io, Unboxed e) => NullableM io (MIOBuffer# e)
  where
    isNullM = \ (MIOBuffer# buff) -> stToMIO (isNullM buff)
    newNull = stToMIO (MIOBuffer# <$> newNull)

instance (MonadIO io, Unboxed e) => BufferM io (MIOBytes# io e)
  where
    type BufferForM io (MIOBytes# io e) = MIOBuffer# e
    
    unsafeFromBufferM (MIOBuffer# buff) = stToMIO (MIOBytes# <$> unsafeFromBufferM buff)
    
    fromBufferM (MIOBuffer# buff) = stToMIO (MIOBytes# <$> fromBufferM buff)
    
    appendBufferM (MIOBuffer# buff) (MIOBytes# mbytes) =
      stToMIO (appendBufferM buff mbytes)

--------------------------------------------------------------------------------

{-# INLINE unpack #-}
unpack :: MIOBytes# io e -> STBytes# RealWorld e
unpack =  \ (MIOBytes# es) -> es

{-# INLINE pack #-}
pack :: (MonadIO io, Unboxed e) => ST RealWorld (STBytes# RealWorld e) -> io (MIOBytes# io e)
pack =  stToMIO . fmap MIOBytes#

unreachEx :: String -> a
unreachEx =  throw . UnreachableException . showString "in SDP.Prim.SBytes.IO."




