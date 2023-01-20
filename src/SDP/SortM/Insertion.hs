{-# LANGUAGE Safe #-}

{- |
    Module      :  SDP.SortM.Insertion
    Copyright   :  (c) Andrey Mulik 2019-2023
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.SortM.Insertion" provides insertion sort - simple sorting algorithm.
-}
module SDP.SortM.Insertion
(
  -- * Insertion Sort
  insertionSort, insertionSortBy, insertionSortOn, unsafeInsertionSort
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM

default ()

--------------------------------------------------------------------------------

-- | 'insertionSort' is just synonym for @'insertionSortBy' 'compare'@.
{-# INLINE insertionSort #-}
insertionSort :: (LinearM m v e, BorderedM m v i, Ord e) => v -> m ()
insertionSort =  insertionSortBy compare

{- |
  'insertionSortOn' is a version of 'insertionSortBy' that uses a cast function
  to 'compare' elements.
-}
{-# INLINE insertionSortOn #-}
insertionSortOn :: (LinearM m v e, BorderedM m v i, Ord o) => (e -> o) -> v -> m ()
insertionSortOn =  insertionSortBy . comparing

{- |
  'insertionSortBy' is naive service sorting procedure, that have @O(n^2)@
  complexity in all cases.
-}
{-# INLINE insertionSortBy #-}
insertionSortBy :: (LinearM m v e, BorderedM m v i) => Compare e -> v -> m ()
insertionSortBy cmp es = do n <- getSizeOf es; unsafeInsertionSort cmp es 0 0 (n - 1)

--------------------------------------------------------------------------------

{- |
  @'unsafeInsertionSort' cmp es b s e@ is internal sorting procedure, where
  @cmp@ - compare function, @es@ - data structure, @[b .. s]@ - sorted range,
  @[b .. e]@ - sortable range.
-}
unsafeInsertionSort :: (LinearM m v e) => Compare e -> v -> Int -> Int -> Int -> m ()
unsafeInsertionSort cmp es b s e = forM_ [s + 1 .. e] $ \ i -> do
  ei <- es !#> i
  let
    next' l u j = l > u ? return j $ do
      let c = (l + u) `div` 2
      ec <- es !#> c
      case ei `cmp` ec of
        GT -> next' (c + 1) u j
        LT -> next' l (c - 1) c
        EQ -> return (c + 1)
  p <- next' b (i - 1) i
  lshiftM es p i



