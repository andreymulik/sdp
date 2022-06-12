{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE Safe, CPP #-}

{- |
    Module      :  SDP.SafePrelude
    Copyright   :  (c) Andrey Mulik 2019-2021
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

import SDP.Comparing
import SDP.Estimate

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ( Semigroup (..) ) -- For base >= 4.9 && < 4.11
#endif

import Data.Functor.Classes

import Data.Bifunctor
import Data.Foldable hiding ( foldrM, foldlM, concat, concatMap )
import Data.Function ( on )

import Control.Applicative
import Control.Category

import Control.Monad.IO.Class
import Control.Monad.ST
import Control.Monad hiding ( zipWithM )

infixl 8 ?+, ?-
infixr 1 ?,  ?^, <=<<, >>=>
infixr 0 ...

default ()

--------------------------------------------------------------------------------

{- |
  Ternary operator.
  
  > (odd 1 ? "is True" $ "is False") == "is True"
-}
{-# INLINE (?) #-}
(?) :: Bool -> a -> a -> a
(?) =  \ p t e -> if p then t else e

-- | @p ?+ f $ a@ returns @'Just' (f a)@ if @(p a)@ and 'Nothing' otherwise.
{-# INLINE (?+) #-}
(?+) :: (a -> Bool) -> (a -> b) -> a -> Maybe b
(?+) =  \ p f a -> p a ? Just (f a) $ Nothing

-- | @p ?- f $ a@ returns 'Nothing' if @(p a)@ and @'Just' (f a)@ otherwise.
{-# INLINE (?-) #-}
(?-) :: (a -> Bool) -> (a -> b) -> a -> Maybe b
(?-) =  \ p f a -> p a ? Nothing $ Just (f a)

-- | Prepends 'Maybe' to list.
{-# INLINE (?:) #-}
(?:) :: Maybe a -> [a] -> [a]
(?:) =  \ mx xs -> case mx of {(Just x) -> x : xs; _ -> xs}

-- | Short version of 'Data.Maybe.fromMaybe'.
{-# INLINE (+?) #-}
(+?) :: a -> Maybe a -> a
_ +? Just x = x
x +?      _ = x

-- | @(...) = (.) . (.)@.
{-# INLINE (...) #-}
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) =  (.) . (.)

--------------------------------------------------------------------------------

{-# INLINE (?^) #-}
-- | Lifted @('?')@.
(?^) :: Monad m => m Bool -> m a -> m a -> m a
(?^) =  \ mb mt me -> do b <- mb; if b then mt else me

-- | Monadic version of @('...')@.
(<=<<) :: Monad m => (c -> m d) -> (a -> b -> m c) -> (a -> b -> m d)
(<=<<) =  \ mg mf a b -> mf a b >>= mg

-- | Monadic vesion of @('...')@ with reversed arguments.
(>>=>) :: Monad m => (a -> b -> m c) -> (c -> m d) -> (a -> b -> m d)
(>>=>) =  \ mf mg a b -> mf a b >>= mg

-- | @ma >>=<< mb@ is composition of 'join' and 'liftM2'.
{-# INLINE (>>=<<) #-}
(>>=<<) :: Monad m => m a -> m b -> (a -> b -> m c) -> m c
(>>=<<) = \ ma mb f -> join $ liftM2 f ma mb

--------------------------------------------------------------------------------

-- | Very useful combinator.
liftA4 :: Applicative t => (a -> b -> c -> d -> e) -> t a -> t b -> t c -> t d -> t e
liftA4 g as bs cs ds = g <$> as <*> bs <*> cs <*> ds

-- | Very very useful combinator
liftA5 :: Applicative t => (a -> b -> c -> d -> e -> f) -> t a -> t b -> t c -> t d -> t e -> t f
liftA5 g as bs cs ds es = g <$> as <*> bs <*> cs <*> ds <*> es

-- | An even more useful combinator.
liftA6 :: Applicative t => (a -> b -> c -> d -> e -> f -> g) -> t a -> t b -> t c -> t d -> t e -> t f -> t g
liftA6 g as bs cs ds es fs = g <$> as <*> bs <*> cs <*> ds <*> es <*> fs

-- | See 'liftA6'.
liftM6 :: Monad m => (a -> b -> c -> d -> e -> f -> g) -> m a -> m b -> m c -> m d -> m e -> m f -> m g
liftM6 g as bs cs ds es fs = do a <- as; b <- bs; c <- cs; d <- ds; e <- es; f <- fs; return $ g a b c d e f

--------------------------------------------------------------------------------

joinM1 :: Monad m => m (a -> m b) -> a -> m b
joinM1 go a = do h <- go; h a

joinM2 :: Monad m => m (a -> b -> m c) -> a -> b -> m c
joinM2 go a b = do h <- go; h a b

joinM3 :: Monad m => m (a -> b -> c -> m d) -> a -> b -> c -> m d
joinM3 go a b c = do h <- go; h a b c

joinM4 :: Monad m => m (a -> b -> c -> d -> m e) -> a -> b -> c -> d -> m e
joinM4 go a b c d = do h <- go; h a b c d

joinM5 :: Monad m => m (a -> b -> c -> d -> e -> m f) -> a -> b -> c -> d -> e -> m f
joinM5 go a b c d e = do h <- go; h a b c d e

joinM6 :: Monad m => m (a -> b -> c -> d -> e -> f -> m g) -> a -> b -> c -> d -> e -> f -> m g
joinM6 go a b c d e f = do h <- go; h a b c d e f

--------------------------------------------------------------------------------

whenJust :: Monad m => (a -> m ()) -> Maybe a -> m ()
whenJust =  maybe (return ())

-- | 'stToMIO' is just @'liftIO' . 'stToIO'@.
stToMIO :: MonadIO io => ST RealWorld e -> io e
stToMIO =  liftIO . stToIO



