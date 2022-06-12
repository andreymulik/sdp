{-# LANGUAGE Safe, MagicHash, MultiParamTypeClasses, FlexibleInstances #-}

{- |
    Module      :  SDP.Unrolled.Unlist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Unrolled.Unlist" provides 'Unlist' - immutable lazy boxed unrolled
    linked list.
-}
module SDP.Unrolled.Unlist
(
  -- * Exports
  module SDP.Indexed,
  module SDP.Sort,
  module SDP.Scan,
  module SDP.Set,
  
  -- * Unlist
  Unlist
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Indexed
import SDP.Sort
import SDP.Scan
import SDP.Set

import SDP.Templates.AnyChunks
import SDP.Prim.SArray
import SDP.SortM.Tim

default ()

--------------------------------------------------------------------------------

-- | 'Unlist' is unrolled linked list of boxed values.
type Unlist = AnyChunks SArray#

--------------------------------------------------------------------------------

{- Eq1 and Ord1 instances. -}

instance Eq1 Unlist
  where
    liftEq f xs ys
        | isNull xs && isNull ys = True
        | isNull xs || isNull ys = False
        |         n1 > n2        = liftEq f (take n2 x) y && liftEq f (drop n2 xs) (fromChunks ys')
        |          True          = liftEq f x (take n1 y) && liftEq f (fromChunks xs') (drop n1 ys)
      where
        (x, xs') = uncons (toChunks xs); n1 = sizeOf x
        (y, ys') = uncons (toChunks ys); n2 = sizeOf y

instance Ord1 Unlist
  where
    liftCompare _ Z   Z = EQ
    liftCompare _ Z   _ = LT
    liftCompare _ _   Z = GT
    liftCompare f xs ys = if n1 > n2
        then liftCompare f (take n2 x) y <> liftCompare f (drop n2 xs) (fromChunks ys')
        else liftCompare f x (take n1 y) <> liftCompare f (fromChunks xs') (drop n1 ys)
      where
        (x, xs') = uncons (toChunks xs); n1 = sizeOf x
        (y, ys') = uncons (toChunks ys); n2 = sizeOf y

instance Zip Unlist
  where
    all2 f as bs             = all2 f (listL as) (listL bs)
    any2 f as bs             = any2 f (listL as) (listL bs)
    all3 f as bs cs          = all3 f (listL as) (listL bs) (listL cs)
    any3 f as bs cs          = any3 f (listL as) (listL bs) (listL cs)
    all4 f as bs cs ds       = all4 f (listL as) (listL bs) (listL cs) (listL ds)
    any4 f as bs cs ds       = any4 f (listL as) (listL bs) (listL cs) (listL ds)
    all5 f as bs cs ds es    = all5 f (listL as) (listL bs) (listL cs) (listL ds) (listL es)
    any5 f as bs cs ds es    = any5 f (listL as) (listL bs) (listL cs) (listL ds) (listL es)
    all6 f as bs cs ds es fs = all6 f (listL as) (listL bs) (listL cs) (listL ds) (listL es) (listL fs)
    any6 f as bs cs ds es fs = any6 f (listL as) (listL bs) (listL cs) (listL ds) (listL es) (listL fs)
    
    zipWith  f as bs             = fromList $ zipWith  f (listL as) (listL bs)
    zipWith3 f as bs cs          = fromList $ zipWith3 f (listL as) (listL bs) (listL cs)
    zipWith4 f as bs cs ds       = fromList $ zipWith4 f (listL as) (listL bs) (listL cs) (listL ds)
    zipWith5 f as bs cs ds es    = fromList $ zipWith5 f (listL as) (listL bs) (listL cs) (listL ds) (listL es)
    zipWith6 f as bs cs ds es fs = fromList $ zipWith6 f (listL as) (listL bs) (listL cs) (listL ds) (listL es) (listL fs)

instance Sort (Unlist e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'
    sortedBy    f = go . toChunks
      where
        go (x1 : x2 : xs) = sortedBy f x1 && last x1 `f` head x2 && go (x2 : xs)
        go      [x1]      = sortedBy f x1
        go       []       = True

--------------------------------------------------------------------------------

{-# INLINE done #-}
done :: STArray# s e -> ST s (Unlist e)
done =  unsafeFreeze


