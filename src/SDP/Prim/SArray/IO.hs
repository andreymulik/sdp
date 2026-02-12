{-# LANGUAGE Trustworthy, MagicHash, UnboxedTuples, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{- |
    Module      :  SDP.Prim.SArray.IO
    Copyright   :  (c) Andrey Mulik 2026
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Prim.SArray.IO" provides boxed pseudo-primitive safe array.
-}
module SDP.Prim.SArray.IO
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  module SDP.Sort,
  
  -- * Pseudo-primitive types
  MIOArray#, IOArray#, fromMIOArray#, unpackMIOArray#, offsetMIOArray#,
  packMIOArray#, coerceMIOArray#
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Prim.SArray.ST
import SDP.Prim.SArray
import SDP.IndexedM
import SDP.SortM
import SDP.Sort

import GHC.Types
import GHC.Exts ( MutableArray#, State#, Int# )
import GHC.ST   ( ST (..) )

import Data.Coerce

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

-- | 'MIOArray#' is mutable preudo-primitive 'Int'-indexed lazy boxed array.
newtype MIOArray# (io :: Type -> Type) e = MIOArray# (STArray# RealWorld e)
  deriving ( Eq )

-- | 'IOArray#' is mutable preudo-primitive 'Int'-indexed lazy boxed array.
type IOArray# = MIOArray# IO

--------------------------------------------------------------------------------

{- NullableM instance. -}

instance MonadIO io => NullableM io (MIOArray# io e)
  where
    newNull = pack newNull
    isNullM = stToMIO . isNullM . unpack

--------------------------------------------------------------------------------

{- Estimate and EstimateM instances. -}

instance Estimate (MIOArray# io e)
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

instance MonadIO io => EstimateM io (MIOArray# io e)
  where
    getSizeHint = getSizeHint . unpack
    getSizeOf   = getSizeOf . unpack
    
    estimateMGE = pure ... (.>=.)
    estimateMLE = pure ... (.<=.)
    estimateMGT = pure ... (.>.)
    estimateMLT = pure ... (.<.)
    estimateMNE = pure ... (./=.)
    estimateMEQ = pure ... (.==.)
    
    noShorterThanM = pure ... (.>=)
    noLongerThanM  = pure ... (.<=)
    longerThanM    = pure ... (.>)
    shorterThanM   = pure ... (.<)
    otherLengthM   = pure ... (./=)
    hasLengthM     = pure ... (.==)
    
    (<<=>>) = pure ... (<==>)
    (<<=>)  = pure ... (<.=>)

--------------------------------------------------------------------------------

{- Bordered and BorderedM instances. -}

instance Bordered (MIOArray# io e) Int
  where
    lower    = const 0
    upper    = upper . unpack
    bounds   = bounds . unpack
    indices  = indices . unpack
    indexOf  = indexOf . unpack
    indexIn  = indexIn . unpack
    offsetOf = offsetOf . unpack
    
    eitherViewOf bnds = fmap MIOArray# . eitherViewOf bnds . unpack

instance MonadIO io => BorderedM io (MIOArray# io e) Int
  where
    getIndexOf = pure ... indexOf . unpack
    getIndices = pure . indices . unpack
    getBounds  = pure . bounds . unpack
    getUpper   = pure . upper . unpack
    getLower _ = pure 0
    
    getEitherViewOf = pure ... eitherViewOf

--------------------------------------------------------------------------------

{- ForceableM instance. -}

instance MonadIO io => ForceableM io (MIOArray# io e)
  where
    copied = pack . copied . unpack

--------------------------------------------------------------------------------

{- ConcatM instance. -}

instance MonadIO io => ConcatM io (MIOArray# io e)
  where
    (<~>) = fmap MIOArray# . stToMIO ... on (<~>) unpack

--------------------------------------------------------------------------------

{- SequenceM and LinearM instances. -}

instance MonadIO io => SequenceM io (MIOArray# io e) e
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

instance MonadIO io => LinearM io (MIOArray# io e) e
  where
    prepend e es = pack $ prepend e (unpack es)
    append  es e = pack $ append (unpack es) e
    
    unconsM' = fmap (second MIOArray# <$>) . stToMIO . unconsM' . unpack
    unsnocM' = fmap (first  MIOArray# <$>) . stToMIO . unsnocM' . unpack
    
    headM' = stToMIO . headM' . unpack
    lastM' = stToMIO . lastM' . unpack
    
    tailM' = fmap (MIOArray# <$>) . stToMIO . tailM' . unpack
    initM' = fmap (MIOArray# <$>) . stToMIO . initM' . unpack
    
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

instance MonadIO io => MapM io (MIOArray# io e) Int e
  where
    newMap' e ascs =
      let bnds = rangeBounds (fsts ascs)
      in  isNull ascs ? newNull $ fromAssocs' bnds e ascs
    
    writeM' es = stToMIO ... writeM' (unpack es)
    
    unsafeReadMByKey es key = stToMIO $ unsafeReadMByKey (unpack es) key
    
    overwrite = stToMIO ... overwrite . unpack
    kfoldrM   = ofoldrM
    kfoldlM   = ofoldlM

instance MonadIO io => IndexedM io (MIOArray# io e) Int e
  where
    fromAssocs  bnds = pack  .  fromAssocs  bnds
    fromAssocs' bnds = pack ... fromAssocs' bnds
    
    fromIndexed' = pack . fromIndexed'
    
    fromIndexedM es = do
      copy <- flip mreplicate (unreachEx "fromIndexedM") =<< getSizeOf es
      copy <$ ofoldrM (\ i e _ -> unsafeWriteM copy i e) () es

--------------------------------------------------------------------------------

{- SortM instance. -}

instance MonadIO io => SortM io (MIOArray# io e) e
  where
    sortedMBy f = stToMIO . sortedMBy f . unpack
    sortMBy   f = stToMIO .  sortMBy f  . unpack

--------------------------------------------------------------------------------

{- Thaw and Freeze instances. -}

instance MonadIO io => Thaw io (SArray# e) (MIOArray# io e)
  where
    unsafeThaw = pack . unsafeThaw
    thaw       = pack . thaw

instance MonadIO io => Freeze io (MIOArray# io e) (SArray# e)
  where
    unsafeFreeze = stToMIO . unsafeFreeze . unpack
    freeze       = stToMIO . freeze . unpack

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  'unpackMIOArray#' returns 'MutableArray#' field of 'STArray#' or fails.
-}
unpackMIOArray# :: MIOArray# io e -> MutableArray# RealWorld e
unpackMIOArray# =  \ (MIOArray# es) -> unpackSTArray# es

{- |
  @since 0.3
  
  'offsetMIOArray#' returns 'STArray#' offset in elements.
-}
offsetMIOArray# :: MIOArray# io e -> Int#
offsetMIOArray# =  \ (MIOArray# es) -> offsetSTArray# es

{- |
  @since 0.3
  
  'packMIOArray#' creates new 'MIOArray#' from sized 'MutableArray#'.
-}
packMIOArray# :: Int -> Int -> MutableArray# RealWorld e -> MIOArray# io e
packMIOArray# n o marr# = MIOArray# (packSTArray# n o marr#)

{- |
  @since 0.3
  
  'fromMIOArray#' returns new 'MutableArray#'.
-}
fromMIOArray# :: MIOArray# io e -> State# RealWorld -> (# State# RealWorld, MutableArray# RealWorld e #)
fromMIOArray# =  \ (MIOArray# es) -> fromSTArray# es

{- |
  @since 0.3
  
  'coerceMIOArray#' is 'coerce' alias.
-}
coerceMIOArray# :: Coercible a b => MIOArray# io a -> MIOArray# io b
coerceMIOArray# =  coerce

--------------------------------------------------------------------------------

{-# INLINE unpack #-}
unpack :: MIOArray# io e -> STArray# RealWorld e
unpack =  coerce

{-# INLINE pack #-}
pack :: (MonadIO io) => ST RealWorld (STArray# RealWorld e) -> io (MIOArray# io e)
pack =  stToMIO . coerce

unreachEx :: String -> a
unreachEx =  throw . UnreachableException . showString "in SDP.Prim.SArray.IO."

