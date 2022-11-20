{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE Safe, CPP #-}

{- |
    Module      :  SDP.SafePrelude
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.SafePrelude" module re-exports common "Prelude" definitions except
    those overridden in this library and its extensions (e.g. @sdp-io@).
    
    In addition, this module re-exports the most common definitions from other
    @base@ and @sdp@ modules ("Control.Applicative", "Data.Bifunctor",
    "SDP.Estimate", etc.) and some useful combinators that were used in this
    library and may be useful to its users.
    
    Import "Prelude" without conflicting functions, may require additional
    imports for functions overridden in other modules:
    
    > import Prelude ()
    > import SDP.SafePrelude
-}
module SDP.SafePrelude
(
  -- * Exports
  module Control.Applicative, liftA4, liftA5, liftA6,
  joinM1, joinM2, joinM3, joinM4, joinM5, joinM6,
  
  module Control.Monad.IO.Class, stToMIO,
  module Control.Monad.ST,
  module Control.Monad, liftM6,
  
  module Data.Functor.Classes,
  module Data.Bifunctor,
  module Data.Foldable,
  
  module SDP.Comparing,
  module SDP.Estimate,
  
  module Prelude,
  
#if !MIN_VERSION_base(4,11,0)
  Semigroup (..),
#endif
  
  -- * Combinators
  (.), id, on, (?), (?+), (?-), (?^), (?:), (+?), (...),
  (<=<<), (>>=>), (>>=<<), whenJust
)
where

import Prelude hiding
  (
    -- defined in Control.Category
    (.), id,
    
    -- defined in SDP.Zip and Data.List
    zip, zip3, zipWith, zipWith3,
    
    -- defined in SDP.Scan and Data.List
    scanl, scanr, scanl1, scanr1,
    
    -- defined in SDP.Linear and Data.List
    head, tail, init, last, take, drop, (!!), (++), reverse, filter, lookup,
    concat, concatMap, replicate, takeWhile, dropWhile, iterate,
    
    -- defined in System.IO.Handle, System.IO.Classes (@sdp-io@) and System.IO
    readFile, writeFile, appendFile, getContents,
    getChar, putChar, getLine, putStr, putStrLn
  )

import SDP.Internal.Utils
import SDP.Comparing
import SDP.Estimate

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ( Semigroup (..) ) -- For base >= 4.9 && < 4.11
#endif

import Data.Functor.Classes

import Data.Bifunctor
import Data.Foldable hiding ( foldrM, foldlM, concat, concatMap )

import Control.Applicative
import Control.Category

import Control.Monad.IO.Class
import Control.Monad.ST
import Control.Monad hiding ( zipWithM, mfilter )

default ()

--------------------------------------------------------------------------------

-- | Very useful combinator.
liftA4 :: Applicative t => (a -> b -> c -> d -> e)
       -> t a -> t b -> t c -> t d -> t e
liftA4 g as bs cs ds = g <$> as <*> bs <*> cs <*> ds

-- | Very very useful combinator
liftA5 :: Applicative t => (a -> b -> c -> d -> e -> f)
       -> t a -> t b -> t c -> t d -> t e -> t f
liftA5 g as bs cs ds es = g <$> as <*> bs <*> cs <*> ds <*> es

-- | An even more useful combinator.
liftA6 :: Applicative t => (a -> b -> c -> d -> e -> f -> g)
       -> t a -> t b -> t c -> t d -> t e -> t f -> t g
liftA6 g as bs cs ds es fs = g <$> as <*> bs <*> cs <*> ds <*> es <*> fs

-- | See 'liftA6'.
liftM6 :: Monad m => (a -> b -> c -> d -> e -> f -> g)
       -> m a -> m b -> m c -> m d -> m e -> m f -> m g
liftM6 g as bs cs ds es fs = do
  a <- as; b <- bs; c <- cs; d <- ds; e <- es; f <- fs
  return $ g a b c d e f

--------------------------------------------------------------------------------

-- | Lift 'Applicative' to 'Monad'.
joinM1 :: Monad m => m (a -> m b) -> a -> m b
joinM1 go a = do h <- go; h a

-- | Lift 'Applicative' to 'Monad'.
joinM2 :: Monad m => m (a -> b -> m c) -> a -> b -> m c
joinM2 go a b = do h <- go; h a b

-- | Lift 'Applicative' to 'Monad'.
joinM3 :: Monad m => m (a -> b -> c -> m d) -> a -> b -> c -> m d
joinM3 go a b c = do h <- go; h a b c

-- | Lift 'Applicative' to 'Monad'.
joinM4 :: Monad m => m (a -> b -> c -> d -> m e)
       -> a -> b -> c -> d -> m e
joinM4 go a b c d = do h <- go; h a b c d

-- | Lift 'Applicative' to 'Monad'.
joinM5 :: Monad m => m (a -> b -> c -> d -> e -> m f)
       -> a -> b -> c -> d -> e -> m f
joinM5 go a b c d e = do h <- go; h a b c d e

-- | Lift 'Applicative' to 'Monad'.
joinM6 :: Monad m => m (a -> b -> c -> d -> e -> f -> m g)
       -> a -> b -> c -> d -> e -> f -> m g
joinM6 go a b c d e f = do h <- go; h a b c d e f

--------------------------------------------------------------------------------

-- | Perform action if 'Just', do nothing if 'Nothing'.
whenJust :: Monad m => (a -> m ()) -> Maybe a -> m ()
whenJust =  maybe (return ())

-- | 'stToMIO' is just @'liftIO' . 'stToIO'@.
stToMIO :: MonadIO io => ST RealWorld e -> io e
stToMIO =  liftIO . stToIO



