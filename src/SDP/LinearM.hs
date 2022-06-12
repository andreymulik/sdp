{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds, DefaultSignatures, FlexibleContexts, RankNTypes #-}
{-# LANGUAGE Safe, CPP, BangPatterns, GADTs, ViewPatterns, PatternSynonyms #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{- |
    Module      :  SDP.LinearM
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.LinearM" is a module that provides 'BorderedM' and 'LinearM' classes.
-}
module SDP.LinearM
(
  -- * Exports
  module SDP.NullableM,
  module SDP.Linear,
  
  -- * BorderedM class
  module SDP.BorderedM,
  
  -- * LinearM class
  LinearM (..), LinearM1, LinearM2,
  
  -- ** LinearM field
  -- $fmrNotes
  pattern (:+=), pattern (:=+), pattern (:~=),
  
#if MIN_VERSION_fmr(0,3,0)
  (=+:), (+=:), (~=:),
#endif
  
#if __GLASGOW_HASKELL__ >= 806
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  LinearM', LinearM''
#endif
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.BorderedM
import SDP.NullableM
import SDP.Linear
import SDP.Map

import Data.Property hiding ( set )
import Data.Typeable

default ()

infixl 5 !#>

--------------------------------------------------------------------------------

{- |
  'LinearM' is 'Linear' version for mutable data structures. This class is
  designed with the possibility of in-place implementation, so many operations
  from 'Linear' have no analogues here.
-}
class (Monad m, NullableM m l) => LinearM m l e | l -> m, l -> e
  where
    {-# MINIMAL (newLinear|fromFoldableM), (takeM|sansM), (dropM|keepM),
        (getLeft|getRight), (!#>), writeM, copyTo #-}
    
    -- | Monadic 'single'.
    singleM :: e -> m l
    singleM =  newLinear . single
    
    {- |
      'getHead' is monadic version of 'head'. This procedure mustn't modify the
      source structure or return references to its mutable fields.
    -}
    getHead :: l -> m e
    getHead =  fmap head . getLeft
    
    {- |
      'getLast' is monadic version of 'last'. This procedure mustn't modify the
      source structure or return references to its mutable fields.
    -}
    getLast :: l -> m e
    getLast =  fmap head . getRight
    
    {- |
      Prepends new element to the start of the structure (monadic 'toHead').
      Like most size-changing operations, @prepend@ doesn't guarantee the
      correctness of the original structure after conversion.
    -}
    prepend :: e -> l -> m l
    prepend e es = newLinear . (e :) =<< getLeft es
    
    {- |
      Appends new element to the end of the structure (monadic 'toLast').
      Like most size-changing operations, @append@ doesn't guarantee the
      correctness of the original structure after conversion.
    -}
    append :: l -> e -> m l
    append es e = newLinear . (:< e) =<< getLeft es
    
    -- | Monadic 'fromList'.
    {-# INLINE newLinear #-}
    newLinear :: [e] -> m l
    newLinear =  fromFoldableM
    
    -- | Monadic 'fromListN'.
    {-# INLINE newLinearN #-}
    newLinearN :: Int -> [e] -> m l
    newLinearN =  newLinear ... take
    
    -- | Monadic 'fromFoldable'.
    {-# INLINE fromFoldableM #-}
    fromFoldableM :: (Foldable f) => f e -> m l
    fromFoldableM =  newLinear . toList
    
    -- | Left view of line.
    {-# INLINE getLeft #-}
    getLeft :: l -> m [e]
    getLeft =  fmap reverse . getRight
    
    -- | Right view of line.
    {-# INLINE getRight #-}
    getRight :: l -> m [e]
    getRight =  fmap reverse . getLeft
    
    -- | @('!#>')@ is unsafe monadic offset-based reader.
    (!#>) :: l -> Int -> m e
    
    -- | Unsafe monadic offset-based writer.
    writeM :: l -> Int -> e -> m ()
    
    -- | Create copy.
    {-# INLINE copied #-}
    copied :: l -> m l
    copied =  getLeft >=> newLinear
    
    -- | @copied' es l n@ returns the slice of @es@ from @l@ of length @n@.
    {-# INLINE copied' #-}
    copied' :: l -> Int -> Int -> m l
    copied' es l n = getLeft es >>= newLinearN n . drop l
    
    -- | Monadic 'reverse', returns new structure.
    {-# INLINE reversed #-}
    reversed :: l -> m l
    reversed =  newLinear <=< getRight
    
    {- |
      @since 0.2.1
      Monadic in-place 'reverse', reverse elements of given structure.
    -}
    reversed' :: l -> m ()
    reversed' es = do
      xs <- getRight es
      assocs xs `forM_` (uncurry $ writeM es)
    
    -- | Monadic 'concat'.
    merged :: (Foldable f) => f l -> m l
    merged =  newLinear . concat <=< sequence . foldr ((:) . getLeft) []
    
    -- | Monadic version of 'replicate'.
    {-# INLINE filled #-}
    filled :: Int -> e -> m l
    filled n = newLinearN n . replicate n
    
    -- | @'removed' n es@ removes element with offset @n@ from @es@.
    removed :: Int -> l -> m l
    removed n es = newLinear . remove n =<< getLeft es
    
    {- |
      @since 0.2.1
      
      @'lshiftM' es i j@ cyclically shifts the elements with offsets between @i@
      and @j@ @(i < j)@ one position to the left (the @j@-th element is in the
      @i@-th position, the @i@-th in the @(i+1)@th, etc.) If @i >= j@, does
      nothing.
    -}
    lshiftM :: l -> Int -> Int -> m ()
    lshiftM es i j =
      let go k ej = when (k <= j) $ do ek <- es !#> k; writeM es k ej; go (k + 1) ek
      in  when (i < j) $ go i =<< (es !#> j)
    
    {- |
      @copyTo source soff target toff count@ writes @count@ elements of @source@
      from @soff@ to @target@ starting with @toff@.
    -}
    copyTo :: l -> Int -> l -> Int -> Int -> m ()
    
    -- | 'ofoldrM' is right monadic fold with offset.
    ofoldrM :: (Int -> e -> r -> m r) -> r -> l -> m r
    ofoldrM f base = foldr ((=<<) . uncurry f) (pure base) . assocs <=< getLeft
    
    -- | 'ofoldlM' is left monadic fold with offset.
    ofoldlM :: (Int -> r -> e -> m r) -> r -> l -> m r
    ofoldlM f base es = foldl (flip $ uncurry ((=<<) ... flip . f)) (pure base)
                      . assocs =<< getLeft es
    
    -- | 'ofoldrM'' is strict version of 'ofoldrM'.
    ofoldrM' :: (Int -> e -> r -> m r) -> r -> l -> m r
    ofoldrM' f = ofoldrM (\ !i e !r -> f i e r)
    
    -- | 'ofoldrM'' is strict version of 'ofoldrM'.
    ofoldlM' :: (Int -> r -> e -> m r) -> r -> l -> m r
    ofoldlM' f = ofoldlM (\ !i !r e -> f i r e)
    
    -- | 'foldrM' is just 'ofoldrM' in 'Linear' context.
    foldrM :: (e -> r -> m r) -> r -> l -> m r
    foldrM =  ofoldrM . const
    
    -- | 'foldlM' is just 'ofoldlM' in 'Linear' context.
    foldlM :: (r -> e -> m r) -> r -> l -> m r
    foldlM =  ofoldlM . const
    
    -- | 'foldrM'' is strict version of 'foldrM'.
    foldrM' :: (e -> r -> m r) -> r -> l -> m r
    foldrM' f = foldrM (\ e !r -> f e r)
    
    -- | 'foldlM'' is strict version of 'foldlM'.
    foldlM' :: (r -> e -> m r) -> r -> l -> m r
    foldlM' f = foldlM (\ !r e -> f r e)
    
    -- | 'foldrM1' is 'foldrM' version with 'last' element as base.
    foldrM1 :: (e -> e -> m e) -> l -> m e
    foldrM1 f = getLeft >=> \ (es :< e) -> foldr ((=<<) . f) (pure e) es
    
    -- | 'foldlM1' is 'foldlM' version with 'head' element as base.
    foldlM1 :: (e -> e -> m e) -> l -> m e
    foldlM1 f = getLeft >=> \ (e :> es) -> foldl (flip $ (=<<) . flip f) (pure e) es
    
    -- | Just swap two elements.
    swapM :: l -> Int -> Int -> m ()
    swapM es i j = do ei <- es !#> i; writeM es i =<< es !#> j; writeM es j ei
    
    {- |
      @since 0.3
      
      Mutable version of 'iterate'.
    -}
    iterateM :: Int -> (e -> m e) -> e -> m l
    iterateM n go e = newLinearN n =<< iterate' n e id
      where
        iterate' 0 _ xs' = return (xs' [])
        iterate' i x xs' = do x' <- go x; iterate' (i - 1) x' (xs' . (x :))
    
    {- |
      @since 0.3
      
      'iterate' for mutable structures.
    -}
    miterate :: Int -> (e -> e) -> e -> m l
    miterate n = newLinearN n ... iterate n
    
    {- |
      @takeM n es@ returns a reference to the @es@, keeping first @n@ elements.
      Changes in the source and result must be synchronous.
    -}
    takeM :: Int -> l -> m l
    default takeM :: (BorderedM m l i) => Int -> l -> m l
    takeM n es = do s <- getSizeOf es; sansM (s - n) es
    
    {- |
      @dropM n es@ returns a reference to the @es@, discarding first @n@ elements.
      Changes in the source and result must be synchronous.
    -}
    dropM :: Int -> l -> m l
    default dropM :: (BorderedM m l i) => Int -> l -> m l
    dropM n es = do s <- getSizeOf es; keepM (s - n) es
    
    {- |
      @keepM n es@ returns a reference to the @es@, keeping last @n@ elements.
      Changes in the source and result must be synchronous.
    -}
    keepM :: Int -> l -> m l
    default keepM :: (BorderedM m l i) => Int -> l -> m l
    keepM n es = do s <- getSizeOf es; dropM (s - n) es
    
    {- |
      @sansM n es@ returns a reference to the @es@, discarding last @n@ elements.
      Changes in the source and result must be synchronous.
    -}
    sansM :: Int -> l -> m l
    default sansM :: (BorderedM m l i) => Int -> l -> m l
    sansM n es = do s <- getSizeOf es; takeM (s - n) es
    
    {- |
      @splitM n es@ returns pair of references to the @es@: keeping and
      discarding first @n@ elements. Changes in the source and result must be
      synchronous.
    -}
    splitM :: Int -> l -> m (l, l)
    splitM n es = liftA2 (,) (takeM n es) (dropM n es)
    
    {- |
      @divideM n es@ returns pair of references to the @es@: discarding and
      keeping last @n@ elements. Changes in the source and results must be
      synchronous.
    -}
    divideM :: Int -> l -> m (l, l)
    divideM n es = liftA2 (,) (sansM n es) (keepM n es)
    
    {- |
      @splitM ns es@ returns the sequence of @es@ prefix references of length
      @n <- ns@. Changes in the source and results must be synchronous.
    -}
    splitsM :: (Foldable f) => f Int -> l -> m [l]
    splitsM ns es =
      let f ds' n = do ds <- ds'; (d,d') <- splitM n (head ds); pure (d':d:ds)
      in  reverse <$> foldl f (pure [es]) ns
    
    {- |
      @dividesM ns es@ returns the sequence of @es@ suffix references of length
      @n <- ns@. Changes in the source and results must be synchronous.
    -}
    dividesM :: (Foldable f) => f Int -> l -> m [l]
    dividesM ns es =
      let f n ds' = do ds <- ds'; (d, d') <- divideM n (head ds); pure (d':d:ds)
      in  foldr f (pure [es]) ns
    
    {- |
      @partsM n es@ returns the sequence of @es@ prefix references, splitted by
      offsets in @es@. Changes in the source and results must be synchronous.
    -}
    partsM :: (Foldable f) => f Int -> l -> m [l]
    partsM =  splitsM . go . toList where go is = zipWith (-) is (0 : is)
    
    {- |
      @chunksM n es@ returns the sequence of @es@ prefix references of length
      @n@. Changes in the source and results must be synchronous.
    -}
    chunksM :: Int -> l -> m [l]
    chunksM n es = do (t, d) <- splitM n es; nowNull d ?^ pure [t] $ (t :) <$> chunksM n d
    
    {- |
      @eachM n es@ returns new sequence of @es@ elements with step @n@. eachM
      shouldn't return references to @es@.
    -}
    eachM :: Int -> l -> m l
    eachM n = newLinearN n . each n <=< getLeft
    
    -- | @prefixM p es@ returns the longest @es@ prefix size, satisfying @p@.
    prefixM :: (e -> Bool) -> l -> m Int
    prefixM p = fmap (prefix p) . getLeft
    
    -- | @suffixM p es@ returns the longest @es@ suffix size, satisfying @p@.
    suffixM :: (e -> Bool) -> l -> m Int
    suffixM p = fmap (suffix p) . getLeft
    
    -- | @mprefix p es@ returns the longest @es@ prefix size, satisfying @p@.
    mprefix :: (e -> m Bool) -> l -> m Int
    mprefix p = foldr (\ e c -> do b <- p e; b ? succ <$> c $ pure 0) (pure 0) <=< getLeft
    
    -- | @msuffix p es@ returns the longest @es@ suffix size, satisfying @p@.
    msuffix :: (e -> m Bool) -> l -> m Int
    msuffix p = foldl (\ c e -> do b <- p e; b ? succ <$> c $ pure 0) (pure 0) <=< getLeft

--------------------------------------------------------------------------------

{- fmr append, prepend and delete fields. -}

{- $fmrNotes
  Note that @fmr-0.3@ support starts with @sdp-0.2.1.2@ patch - previous
  versions only work with @fmr-0.2@.
  
  @fmr-0.3@ introduces new field extensions, so the patterns @(:+=)@, @(:=+)@
  and @(:~=)@ will be deprecated in @sdp-0.3@ and will be removed in @sdp-0.4@.
  @sdp-0.3@ will provide new combinators to do the same.
-}

#if !MIN_VERSION_fmr(0,3,0)
-- | 'FieldLinearM' is a service type used to prepend, append or remove element.
data FieldLinearM l e m field record
  where
    Prepend :: (LinearM m l e, FieldGet field, FieldSet field)
            => e -> field m record l -> FieldLinearM l e m field record
    Append  :: (LinearM m l e, FieldGet field, FieldSet field)
            => field m record l -> e -> FieldLinearM l e m field record
    Delete  :: (LinearM m l e, FieldGet field, FieldSet field)
            => Int -> field m record l -> FieldLinearM l e m field record
  deriving ( Typeable )

instance IsProp (FieldLinearM l e)
  where
    performProp record (Append  field e) = setRecord field record =<<
                         flip append e =<< getRecord field record
    
    performProp record (Delete  n field) = setRecord field record =<<
                             removed n =<< getRecord field record
    
    performProp record (Prepend e field) = setRecord field record =<<
                             prepend e =<< getRecord field record

{- |
  @since 0.2.1
  
  @(':+=')@ is @fmr@-compatible 'prepend' element pattern for 'LinearM' fields.
-}
pattern (:+=) ::
  (
    Typeable record, Typeable field, Typeable m, Typeable l, Typeable e,
    LinearM m l e, FieldGet field, FieldSet field
  ) => e -> field m record l -> Prop m record
pattern e :+= field <- (cast' -> Just (Prepend e field)) where (:+=) = Prop ... Prepend

{- |
  @since 0.2.1
  
  @(':=+')@ is @fmr@-compatible 'append' element pattern for 'LinearM' fields.
-}
pattern (:=+) ::
  (
    Typeable record, Typeable field, Typeable m, Typeable l, Typeable e,
    LinearM m l e, FieldGet field, FieldSet field
  ) => field m record l -> e -> Prop m record
pattern field :=+ e <- (cast' -> Just (Append field e)) where (:=+) = Prop ... Append

{- |
  @since 0.2.1
  
  @(':~=')@ is @fmr@-compatible delete element pattern for 'LinearM' fields, see
  'removed'.
-}
pattern (:~=) ::
  (
    Typeable record, Typeable field, Typeable m, Typeable l, Typeable e,
    LinearM m l e, FieldGet field, FieldSet field
  ) => Int -> field m record l -> Prop m record
pattern n :~= field <- (cast' -> Just (Delete n field)) where (:~=) = Prop ... Delete

-- | 'cast'' is just service function for 'Prop' data extraction.
cast' ::
  (
    Typeable record, Typeable field, Typeable m, Typeable l, Typeable e,
    LinearM m l e, FieldGet field, FieldSet field
  ) => Prop m record -> Maybe (FieldLinearM l e m record)
cast' =  cast
#else
-- | 'FieldLinearM' is a service type used to prepend, append or remove element.
data FieldLinearM m record
  where
    Prepend :: (FieldModifyM field m record l, LinearM m l e)
            => !e -> !field -> FieldLinearM m record
    
    Append  :: (FieldModifyM field m record l, LinearM m l e)
            => !field -> !e -> FieldLinearM m record
    
    Delete  :: (FieldModifyM field m record l, LinearM m l e)
            => !Int -> !field -> FieldLinearM m record
  deriving ( Typeable )

instance IsProp FieldLinearM
  where
    performProp record (Prepend e field) = () <$ modifyRecordM field record (prepend  e)
    performProp record (Append  field e) = () <$ modifyRecordM field record (`append` e)
    performProp record (Delete  n field) = () <$ modifyRecordM field record (removed  n)

{- |
  @since 0.2.1
  
  @(':+=')@ is @fmr@-compatible 'prepend' element pattern for 'LinearM' fields.
-}
pattern (:+=) :: (Typeable m, Typeable record, Monad m)
              => (FieldModifyM field m record l, LinearM m l e)
              => e -> field -> Prop m record
pattern e :+= field = Prop (Prepend e field)

{- |
  @since 0.2.1
  
  @(':=+')@ is @fmr@-compatible 'append' element pattern for 'LinearM' fields.
-}
pattern (:=+) :: (Typeable m, Typeable record, Monad m)
              => (FieldModifyM field m record l, LinearM m l e)
              => field -> e -> Prop m record
pattern field :=+ e = Prop (Append field e)

{- |
  @since 0.2.1
  
  @(':~=')@ is @fmr@-compatible delete element pattern for 'LinearM' fields, see
  'removed'.
-}
pattern (:~=) :: (Typeable m, Typeable record, Monad m)
              => (FieldModifyM field m record l, LinearM m l e)
              => Int -> field -> Prop m record
pattern n :~= field = Prop (Delete n field)

(=+:) ::
  (
    LinearM m l e, FieldModifyM field m record l,
    Typeable field, Typeable m, Typeable record
  ) => field -> e -> Prop m record
field =+: e = Prop (Append field e)

(+=:) ::
  (
    LinearM m l e, FieldModifyM field m record l,
    Typeable field, Typeable m, Typeable record
  ) => e -> field -> Prop m record
field +=: e = Prop (Prepend field e)

(~=:) ::
  (
    LinearM m l e, FieldModifyM field m record l,
    Typeable m, Typeable record, Typeable field
  ) => Int -> field -> Prop m record
n ~=: field = Prop (Delete n field)

#endif

--------------------------------------------------------------------------------

-- | 'LinearM' contraint for @(Type -> Type)@-kind types.
type LinearM1 m l e = LinearM m (l e) e

-- | 'LinearM' contraint for @(Type -> Type -> Type)@-kind types.
type LinearM2 m l i e = LinearM m (l i e) e

#if __GLASGOW_HASKELL__ >= 806
-- | 'LinearM' contraint for @(Type -> Type)@-kind types.
type LinearM' m l = forall e . LinearM m (l e) e

-- | 'LinearM' contraint for @(Type -> Type -> Type)@-kind types.
type LinearM'' m l = forall i e . LinearM m (l i e) e
#endif


