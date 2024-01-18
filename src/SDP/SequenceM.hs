{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds, DefaultSignatures, RankNTypes #-}
{-# LANGUAGE Safe, CPP, BangPatterns #-}

#ifdef SDP_QUALIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{- |
    Module      :  SDP.SequenceM
    Copyright   :  (c) Andrey Mulik 2023
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)

    "SDP.SequenceM" is a module that provides 'SequenceM' class.
-}
module SDP.SequenceM
(
  -- * Export
  module SDP.Cat,

  -- * SequenceM class
  SequenceM (..), SequenceM1, SequenceM2, prepend, append,

#ifdef SDP_QUALIFIED_CONSTRAINTS
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  SequenceM', SequenceM''
#endif
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Sequence
import SDP.Cat
import SDP.Map

import Control.Exception.SDP

default ()

infixl 5 !#>

--------------------------------------------------------------------------------

{- |
  @since 0.3

  'SequenceM' is 'Sequence' for mutable structures.
-}
class (Monad m, ForceableM m seq, Concat m seq, EstimateM m seq)
    => SequenceM m seq e | seq -> m, seq -> e
  where
    {-# MINIMAL (unconsM|getHead,getTail|unconsM'),
                (unsnocM|getInit,getLast|unsnocM'),
                (!#>), writeM #-}

    -- | Monadic 'single'.
    singleM :: e -> m seq
    singleM e = do es <- newNull; e += es
    default singleM :: NullableM m seq => e -> m seq

    {- |
      Prepends new element to the start of the structure (monadic 'toHead').
      Like most size-changing operations, @prepend@ doesn't guarantee the
      correctness of the original structure after conversion.
    -}
    (+=) :: e -> seq -> m seq
    e += es = do e' <- singleM e; e' `cat` es

    {- |
      Appends new element to the end of the structure (monadic 'toLast').
      Like most size-changing operations, @append@ doesn't guarantee the
      correctness of the original structure after conversion.
    -}
    (=+) :: seq -> e -> m seq
    es =+ e = do e' <- singleM e; es `cat` e'

    default unconsM' :: NullableM m seq => seq -> m (Maybe (e, seq))
    unconsM' :: seq -> m (Maybe (e, seq))
    unconsM' =  nowNull ^- unconsM

    default unsnocM' :: NullableM m seq => seq -> m (Maybe (seq, e))
    unsnocM' :: seq -> m (Maybe (seq, e))
    unsnocM' =  nowNull ^- unsnocM

    unconsM :: seq -> m (e, seq)
    unconsM es = liftA2 (,) (getHead es) (getTail es)

    unsnocM :: seq -> m (seq, e)
    unsnocM es = liftA2 (,) (getInit es) (getLast es)

    {- |
      'getHead' is monadic version of 'head'. This procedure mustn't modify the
      source structure or return references to its mutable fields.
    -}
    getHead :: seq -> m e
    getHead =  fmap head . getLeft

    {- |
      'getLast' is monadic version of 'last'. This procedure mustn't modify the
      source structure or return references to its mutable fields.
    -}
    getLast :: seq -> m e
    getLast =  fmap head . getRight

    {- |
      'getTail' is monadic version of 'head'. This procedure mustn't modify the
      source structure or return references to its mutable fields.
    -}
    getTail :: seq -> m seq
    getTail =  newLinear . tail <=< getLeft

    {- |
      'getInit' is monadic version of 'last'. This procedure mustn't modify the
      source structure or return references to its mutable fields.
    -}
    getInit :: seq -> m seq
    getInit =  newLinear . init <=< getLeft

    -- | Monadic 'fromList'.
    {-# INLINE newLinear #-}
    newLinear :: [e] -> m seq
    newLinear =  fromFoldableM

    -- | Monadic 'fromFoldable'.
    {-# INLINE fromFoldableM #-}
    fromFoldableM :: Foldable f => f e -> m seq
    fromFoldableM =  foldr (\ x xs' -> do xs <- xs'; x += xs) newNull
    default fromFoldableM :: NullableM m seq => Foldable f => f e -> m seq

    -- | Left view of line.
    {-# INLINE getLeft #-}
    getLeft :: seq -> m [e]
    getLeft =  maybe (return []) (\ (x, es) -> do
        xs <- getLeft es
        return (x : xs)
      ) <=< unconsM'

    -- | Right view of line.
    {-# INLINE getRight #-}
    getRight :: seq -> m [e]
    getRight =  fmap reverse . getLeft

    -- | Monadic 'reverse', returns new structure.
    {-# INLINE reversed #-}
    reversed :: seq -> m seq
    reversed =  newLinear <=< getRight

    -- | 'foldrM' is just 'ofoldrM' in 'Linear' context.
    foldrM :: (e -> r -> m r) -> r -> seq -> m r
    foldrM =  ofoldrM . const

    -- | 'foldlM' is just 'ofoldlM' in 'Linear' context.
    foldlM :: (r -> e -> m r) -> r -> seq -> m r
    foldlM =  ofoldlM . const

    -- | 'foldrM'' is strict version of 'foldrM'.
    foldrM' :: (e -> r -> m r) -> r -> seq -> m r
    foldrM' f = foldrM (\ e !r -> f e r)

    -- | 'foldlM'' is strict version of 'foldlM'.
    foldlM' :: (r -> e -> m r) -> r -> seq -> m r
    foldlM' f = foldlM (\ !r e -> f r e)

    -- | 'foldrM1' is 'foldrM' version with 'last' element as base.
    foldrM1 :: (e -> e -> m e) -> seq -> m e
    foldrM1 f = getLeft >=> \ es' -> case es' of
      (es :< e) -> foldr ((=<<) . f) (pure e) es
      _         -> emptyEx "foldrM1: must be non-empty"

    -- | 'foldlM1' is 'foldlM' version with 'head' element as base.
    foldlM1 :: (e -> e -> m e) -> seq -> m e
    foldlM1 f = getLeft >=> \ es' -> case es' of
      (e :> es) -> foldl (flip $ (=<<) . flip f) (pure e) es
      _         -> emptyEx "foldlM1: must be non-empty"

    -- | 'ofoldrM' is right monadic fold with offset.
    ofoldrM :: (Int -> e -> r -> m r) -> r -> seq -> m r
    ofoldrM f base = foldr ((=<<) . uncurry f) (pure base) . assocs <=< getLeft

    -- | 'ofoldlM' is left monadic fold with offset.
    ofoldlM :: (Int -> r -> e -> m r) -> r -> seq -> m r
    ofoldlM f base es = foldl (flip $ uncurry ((=<<) ... flip . f)) (pure base)
                      . assocs =<< getLeft es

    -- | 'ofoldrM'' is strict version of 'ofoldrM'.
    ofoldrM' :: (Int -> e -> r -> m r) -> r -> seq -> m r
    ofoldrM' f = ofoldrM (\ !i e !r -> f i e r)

    -- | 'ofoldrM'' is strict version of 'ofoldrM'.
    ofoldlM' :: (Int -> r -> e -> m r) -> r -> seq -> m r
    ofoldlM' f = ofoldlM (\ !i !r e -> f i r e)

    -- | @prefixM p es@ returns the longest @es@ prefix size, satisfying @p@.
    prefixM :: (e -> Bool) -> seq -> m Int
    prefixM p = fmap (prefix p) . getLeft

    -- | @suffixM p es@ returns the longest @es@ suffix size, satisfying @p@.
    suffixM :: (e -> Bool) -> seq -> m Int
    suffixM p = fmap (suffix p) . getLeft

    {- |
      @infixesM inf es@ returns a list of @inf@ positions in @es@, without
      intersections.
    -}
    infixesM :: Eq e => seq -> seq -> m [Int]
    infixesM =  liftA2 infixes `on` getLeft

    -- | @mprefix p es@ returns the longest @es@ prefix size, satisfying @p@.
    mprefix :: (e -> m Bool) -> seq -> m Int
    mprefix p = foldr (\ e c -> do b <- p e; b ? succ <$> c $ pure 0) (pure 0)
            <=< getLeft

    -- | @msuffix p es@ returns the longest @es@ suffix size, satisfying @p@.
    msuffix :: (e -> m Bool) -> seq -> m Int
    msuffix p = foldl (\ c e -> do b <- p e; b ? succ <$> c $ pure 0) (pure 0)
            <=< getLeft

    {- |
      @since 0.3

      Removes duplicate elements, returns a new structure.
    -}
    nubM :: Eq e => seq -> m seq
    nubM =  nubByM (==)

    {- |
      @since 0.3

      Removes duplicate elements, returns a new structure.
    -}
    nubByM :: Equal e -> seq -> m seq
    nubByM f = newLinear . nubBy f <=< getLeft

    -- | @('!#>')@ is unsafe monadic offset-based reader.
    (!#>) :: seq -> Int -> m e

    -- | Unsafe monadic offset-based writer.
    writeM :: seq -> Int -> e -> m ()

    {- |
      @since 0.3

      @nowSubseqOf xs ys@ check if @xs@ is subsequence of @ys@.
    -}
    nowSubseqOf :: Eq e => seq -> seq -> m Bool
    nowSubseqOf =  liftA2 isSubseqOf `on` getLeft

    {- |
      @since 0.3

      @nowPrefixOf xs ys@ check if @xs@ is prefix of @ys@.
    -}
    nowPrefixOf :: Eq e => seq -> seq -> m Bool
    nowPrefixOf =  liftA2 isPrefixOf `on` getLeft

    {- |
      @since 0.3

      @nowSuffixOf xs ys@ check if @xs@ is suffix of @ys@.
    -}
    nowSuffixOf :: Eq e => seq -> seq -> m Bool
    nowSuffixOf =  liftA2 isSuffixOf `on` getLeft

    {- |
      @since 0.3

      @nowInfixOf xs ys@ check if @xs@ is infix of @ys@.
    -}
    nowInfixOf :: Eq e => seq -> seq -> m Bool
    nowInfixOf =  liftA2 isInfixOf `on` getLeft

    selectM :: (e -> Maybe a) -> seq -> m [a]
    selectM =  selectM' . (pure .)

    extractM :: (e -> Maybe a) -> seq -> m ([a], [e])
    extractM =  extractM' . (pure .)

    selectM' :: (e -> m (Maybe a)) -> seq -> m [a]
    selectM' f = foldrM (\ x es -> maybe es (: es) <$> f x) []

    extractM' :: (e -> m (Maybe a)) -> seq -> m ([a], [e])
    extractM' f = foldrM (\ x (es, xs) ->
        let fits e = (e : es, xs)
        in  maybe (es, x : xs) fits <$> f x
      ) ([], [])

--------------------------------------------------------------------------------

-- | 'SequenceM' contraint for @(Type -> Type)@-kind types.
type SequenceM1 m l e = SequenceM m (l e) e

-- | 'SequenceM' contraint for @(Type -> Type -> Type)@-kind types.
type SequenceM2 m l i e = SequenceM m (l i e) e

#ifdef SDP_QUALIFIED_CONSTRAINTS
-- | 'SequenceM' contraint for @(Type -> Type)@-kind types.
type SequenceM' m l = forall e . SequenceM m (l e) e

-- | 'SequenceM' contraint for @(Type -> Type -> Type)@-kind types.
type SequenceM'' m l = forall i e . SequenceM m (l i e) e
#endif

--------------------------------------------------------------------------------

{-# DEPRECATED prepend "will be removed in sdp-0.4" #-}
{-# DEPRECATED append  "will be removed in sdp-0.4" #-}

{- |
  @since 0.2

  Same as @('+=')@, moved from 'SDP.LinearM.LinearM'.
-}
prepend :: SequenceM m seq e => e -> seq -> m seq
prepend =  (+=)

{- |
  @since 0.2

  Same as @('+=')@, moved from 'SDP.LinearM.LinearM'.
-}
append :: SequenceM m seq e => seq -> e -> m seq
append =  (=+)

--------------------------------------------------------------------------------

{-# NOINLINE emptyEx #-}
emptyEx :: String -> a
emptyEx =  throw . PatternMatchFail . showString "in SDP.SequenceM."


