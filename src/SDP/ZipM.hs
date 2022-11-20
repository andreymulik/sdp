{-# LANGUAGE Safe, MultiParamTypeClasses, FunctionalDependencies #-}

{- |
    Module      :  SDP.ZipM
    Copyright   :  (c) Andrey Mulik 2021-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    @since 0.2.1
    "SDP.ZipM" provides 'ZipM' - class of "Control.Applicative.ZipList"-like
    structures.
-}
module SDP.ZipM
(
  -- * Export
  module SDP.Zip,
  
  -- * ZipM
  ZipM (..)
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Zip

default ()

--------------------------------------------------------------------------------

-- | 'ZipM' is monadic version of 'SDP.Zip.Zip'.
class (Monad m) => ZipM m z | z -> m
  where
    {-# MINIMAL zipWithM #-}
    
    -- | Monadic 'zip'.
    zipM  :: z a -> z b -> m (z (a, b))
    zipM  =  mzipWith  (,)
    
    -- | Monadic 'zip3'.
    zipM3 :: z a -> z b -> z c -> m (z (a, b, c))
    zipM3 =  mzipWith3 (,,)
    
    -- | Monadic 'zip4'.
    zipM4 :: z a -> z b -> z c -> z d -> m (z (a, b, c, d))
    zipM4 =  mzipWith4 (,,,)
    
    -- | Monadic 'zip5'.
    zipM5 :: z a -> z b -> z c -> z d -> z e -> m (z (a, b, c, d, e))
    zipM5 =  mzipWith5 (,,,,)
    
    -- | Monadic 'zip6'.
    zipM6 :: z a -> z b -> z c -> z d -> z e -> z f -> m (z (a, b, c, d, e, f))
    zipM6 =  mzipWith6 (,,,,,)
    
    -- | Pure to monadic lifted 'zipWith'.
    mzipWith  :: (a -> b -> r) -> z a -> z b -> m (z r)
    mzipWith  g = zipWithM (\ a b -> return (g a b))
    
    -- | Pure to monadic lifted 'zipWith3'.
    mzipWith3 :: (a -> b -> c -> r) -> z a -> z b -> z c -> m (z r)
    mzipWith3 g = zipWithM3 (\ a b c -> return (g a b c))
    
    -- | Pure to monadic lifted 'zipWith4'.
    mzipWith4 :: (a -> b -> c -> d -> r) -> z a -> z b -> z c -> z d -> m (z r)
    mzipWith4 g = zipWithM4 (\ a b c d -> return (g a b c d))
    
    -- | Pure to monadic lifted 'zipWith5'.
    mzipWith5 :: (a -> b -> c -> d -> e -> r) -> z a -> z b -> z c -> z d -> z e -> m (z r)
    mzipWith5 g = zipWithM5 (\ a b c d e -> return (g a b c d e))
    
    -- | Pure to monadic lifted 'zipWith6'.
    mzipWith6 :: (a -> b -> c -> d -> e -> f -> r) -> z a -> z b -> z c -> z d -> z e -> z f -> m (z r)
    mzipWith6 g = zipWithM6 (\ a b c d e f -> return (g a b c d e f))
    
    -- | Monadic 'zipWith'.
    zipWithM :: (a -> b -> m r) -> z a -> z b -> m (z r)
    
    -- | Monadic 'zipWith3'.
    zipWithM3 :: (a -> b -> c -> m r) -> z a -> z b -> z c -> m (z r)
    zipWithM3 f za zb zc = zipWithM ($) `flip` zc =<< mzipWith f za zb
    
    -- | Monadic 'zipWith4'.
    zipWithM4 :: (a -> b -> c -> d -> m r) -> z a -> z b -> z c -> z d -> m (z r)
    zipWithM4 f za zb zc zd = zipWithM ($) `flip` zd =<< mzipWith3 f za zb zc
    
    -- | Monadic 'zipWith5'.
    zipWithM5 :: (a -> b -> c -> d -> e -> m r) -> z a -> z b -> z c -> z d -> z e -> m (z r)
    zipWithM5 f za zb zc zd ze = zipWithM ($) `flip` ze =<< mzipWith4 f za zb zc zd
    
    -- | Monadic 'zipWith6'.
    zipWithM6 :: (a -> b -> c -> d -> e -> f -> m r) -> z a -> z b -> z c -> z d -> z e -> z f -> m (z r)
    zipWithM6 f za zb zc zd ze zf = zipWithM ($) `flip` zf =<< mzipWith5 f za zb zc zd ze

