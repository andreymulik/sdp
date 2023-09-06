{-# LANGUAGE Safe, TypeOperators #-}

{- |
    Module      :  SDP.Index
    Copyright   :  (c) Andrey Mulik 2019-2023
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
  (C.:|:), C.SubIndex, takeDim, dropDim, joinDim, C.splitDim,
  
  -- * Indices
  C.Index (..), C.SizesOf,
  
  -- ** Helpers
  C.InBounds (..), C.offsetIntegral, C.defaultBoundsUnsign
)
where

import qualified SDP.Index.Class as C
import SDP.Shape

import SDP.Nullable

default ()

--------------------------------------------------------------------------------

{- |
  Take some dimensions.
  
  @
  takeDim ([1,2,3,4] :: I4 Int) === [1] :: I1 Int
  takeDim ([1,2,3,4] :: I4 Int) === E@
-}
takeDim :: C.SubIndex i j => i -> j
takeDim =  C.takeDim

{- |
  Drop some dimensions (second argument used as type variable).
  
  @
  dropDim ([1,2,3,4] :: I4 Int)    ([] :: E)      === [1,2,3,4]
  dropDim ([1,2,3,4] :: I4 Int) ([1,2] :: I2 Int) ===   [3,4]@
-}
dropDim :: C.SubIndex i j => i -> j -> i C.:|: j
dropDim =  C.dropDim

{- |
  Join some dimensions.
  
  @
  joinDim ([1,2] :: I2 Int)  [3]  ===  [1,2,3]  :: I3 Int
  joinDim ([1,2] :: I2 Int) [3,4] === [1,2,3,4] :: I4 Int@
-}
joinDim :: C.SubIndex i j => j -> (i C.:|: j) -> i
joinDim =  C.joinDim



