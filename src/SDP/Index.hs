{-# LANGUAGE Safe #-}

{- |
    Module      :  SDP.Index
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    The 'Index' class is an alternative to 'Data.Ix.Ix' with a richer interface,
    generalized indexes and more convenient function names.
-}
module SDP.Index
(
  -- * Exports
  module SDP.Nullable,
  module SDP.Shape,
  
  -- * Shapes
  (:|:), SubIndex, takeDim, dropDim, joinDim, splitDim,
  
  -- * Indices
  Index (..),
  
  -- ** Helpers
  InBounds (..), offsetIntegral, defaultBoundsUnsign
)
where

import SDP.Index.Class
import SDP.Shape

import SDP.Nullable


