{-# LANGUAGE MultiParamTypeClasses #-}

{- |
    Module      :  SDP.Cat
    Copyright   :  (c) Andrey Mulik 2024
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable

    "SDP.Cat" is a module that provides 'Cat' class.
-}
module SDP.Cat
(
  -- * Concat
  Concat (..)
)
where

default ()

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Helper class for concatenating mutable structures.
-}
class Monad m => Concat m c
  where
    -- | Concatenation of two mutable structures, returning a new structure.
    cat :: c -> c -> m c



