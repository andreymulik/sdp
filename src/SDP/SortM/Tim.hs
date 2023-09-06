{-# LANGUAGE Safe #-}

{- |
    Module      :  SDP.SortM.Tim
    Copyright   :  (c) Andrey Mulik 2019-2021
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (requires non-portable modules)
    
    "SDP.SortM.Tim" provides @InsertionSort@ and @TimSort@ algorithms.
-}
module SDP.SortM.Tim
(
  -- * TimSort
  timSort, timSortBy, timSortOn, minrunTS
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM

import SDP.SortM.Insertion

import Data.Bits

default ()

--------------------------------------------------------------------------------

-- | 'timSort' is just synonym for @'timSortBy' 'compare'@.
{-# INLINE timSort #-}
timSort :: (LinearM m v e, BorderedM m v i, Ord e) => v -> m ()
timSort =  timSortBy compare

{- |
  'timSortOn' is a version of 'timSortBy' that uses a conversion function to
  compare elements.
-}
{-# INLINE timSortOn #-}
timSortOn :: (LinearM m v e, BorderedM m v i, Ord o) => (e -> o) -> v -> m ()
timSortOn =  timSortBy . comparing

{- |
  'timSortBy' is a sorting procedure for mutable random access data structures
  using any comparison function and having @O(nlogn)@ complexity in the worst
  case.
-}
{-# INLINE timSortBy #-}
timSortBy :: (LinearM m v e, BorderedM m v i) => Compare e -> v -> m ()
timSortBy cmp es = sort' =<< getSizeOf es
  where
    gt = \ x y -> case cmp x y of {GT -> True; _ -> False}
    
    sort' n
      |  n < 0  = return ()
      | n <= 64 = unsafeInsertionSort cmp es 0 0 (n - 1)
      |   True  = iteratePreN (3 :: Int) 0 >>= go
        where
          go [sx, sy, sz] = do
            nxt <- iteratePreN (1 :: Int) (sx + sy + sz)
            if (sx > sy + sz && sy > sz) || sz <= sx
              then do merge sx sy sz; go ([sx, sy + sz] ++ nxt)
              else do merge 0  sx sy; go ([sx + sy, sz] ++ nxt)
          go [sx, sy] = merge 0 sx sy
          go     _    = return ()
          
          iteratePreN 0 _ = return []
          iteratePreN j o = case n - o of
              0 -> return []
              1 -> return [1]
              2 -> do
                e0 <- es !#> o
                e1 <- es !#> o + 1
                when (e0 `gt` e1) $ swapM es o (o + 1)
                return [2]
              _ -> do
                end <- normalized =<< actual
                end == 0 ? return [end - o] $ (end - o :) <$> iteratePreN (j - 1) end
            where
              actual = (es !#> o) >>=<< (es !#> o + 1) $ \ e0 e1 ->
                  e0 `gt` e1 ? desc e1 (o + 2) $ asc e1 (o + 2)
                where
                  desc p i = do c <- es !#> i; c `gt` p ? rev' o i $ i /= n - 1 ? desc c (i + 1) $ rev' o (i + 1)
                  asc  p i = do c <- es !#> i; p `gt` c ? return i $ i /= n - 1 ? asc  c (i + 1) $ return (i + 1)
                  rev  f l = when (f < l) $ do swapM es f l; rev (f + 1) (l - 1)
                  rev' f l = do rev f (l - 1); return l
              
              normalized s = do
                let ex = min n (o + minrunTS n) -- minimal expected ending
                when (ex > s) $ unsafeInsertionSort cmp es o (s - 1) (ex - 1)
                return (ex `max` s)
    
    merge o sx sy = copied' es o sx >>= mergeGo o 0 (o + sx)
      where
        mergeGo ic il ir left
          | il >= lb = return () -- at least left is empty, merge is completed.
          | ir >= rb = copyTo left il es ic (lb - il)
          |   True   = (left !#> il) >>=<< (es !#> ir) $
            \ l r -> if r `gt` l
              then writeM es ic l >> mergeGo (ic + 1) (il + 1) ir left
              else writeM es ic r >> mergeGo (ic + 1) il (ir + 1) left
        rb = o + sx + sy
        lb = sx

--------------------------------------------------------------------------------

{-# INLINE minrunTS #-}
-- | 'minrunTS' returns @TimSort@ chunk size by given length.
minrunTS :: Int -> Int
minrunTS i = mr i 0 where mr n r = n >= 64 ? mr (shiftR n 1) (n .&. 1) $ n + r




