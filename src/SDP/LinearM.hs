{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, BangPatterns #-}
{-# LANGUAGE Safe, ConstraintKinds, DefaultSignatures #-}

{- |
    Module      :  SDP.LinearM
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.LinearM" is a module that provides 'BorderedM' and 'LinearM' classes.
-}
module SDP.LinearM
(
  -- * Exports
  module SDP.Linear,
  
  -- * BorderedM class
  BorderedM (..), BorderedM1, BorderedM2,
  
  -- * LinearM class
  LinearM (..), LinearM1,
  
  -- * SplitM class
  SplitM (..), SplitM1
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Linear
import SDP.Map

default ()

infixl 5 !#>

--------------------------------------------------------------------------------

-- | 'BorderedM' is 'Bordered' version for mutable data structures.
class (Monad m, Index i) => BorderedM m b i | b -> m, b -> i
  where
    {-# MINIMAL (getBounds|getLower, getUpper) #-}
    
    -- | 'getBounds' returns 'bounds' of mutable data structure.
    getBounds :: b -> m (i, i)
    getBounds es = liftA2 (,) (getLower es) (getUpper es)
    
    -- | 'getLower' returns 'lower' bound of mutable data structure.
    getLower :: b -> m i
    getLower =  fsts . getBounds
    
    -- | 'getUpper' returns 'upper' bound of mutable data structure.
    getUpper :: b -> m i
    getUpper =  snds . getBounds
    
    -- | 'getSizeOf' returns 'size' of mutable data structure.
    getSizeOf :: b -> m Int
    getSizeOf =  fmap size . getBounds
    
    -- | 'getSizesOf' returns 'sizes' of mutable data structure.
    getSizesOf :: b -> m [Int]
    getSizesOf =  fmap sizes . getBounds
    
    -- | 'nowIndexIn' is 'indexIn' version for mutable structures.
    nowIndexIn :: b -> i -> m Bool
    nowIndexIn es i = flip inRange i <$> getBounds es
    
    -- | 'getOffsetOf' is 'offsetOf' version for mutable structures.
    getOffsetOf :: b -> i -> m Int
    getOffsetOf es i = flip offset i <$> getBounds es
    
    -- | 'getIndexOf' is 'indexOf' version for mutable structures.
    getIndexOf :: b -> Int -> m i
    getIndexOf es i = flip index i <$> getBounds es
    
    -- | 'getIndices' returns 'indices' of mutable data structure.
    getIndices :: b -> m [i]
    getIndices =  fmap range . getBounds

--------------------------------------------------------------------------------

{- |
  'LinearM' is 'Linear' version for mutable data structures. This class is
  designed with the possibility of in-place implementation, so many operations
  from 'Linear' have no analogues here.
-}
class (Monad m) => LinearM m l e | l -> m, l -> e
  where
    {-# MINIMAL (newLinear|fromFoldableM), (getLeft|getRight), (!#>), writeM, copyTo #-}
    
    -- | Monadic 'single'.
    newNull :: m l
    newNull =  newLinear []
    
    -- | Monadic 'isNull'.
    nowNull :: l -> m Bool
    nowNull =  fmap isNull . getLeft
    
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
    
    -- | (!#>) is unsafe monadic offset-based reader.
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
    
    -- | Monadic 'reverse'.
    {-# INLINE reversed #-}
    reversed :: l -> m l
    reversed =  newLinear <=< getRight
    
    -- | Monadic 'concat'.
    merged :: (Foldable f) => f l -> m l
    merged =  newLinear . concat <=< sequence . foldr ((:) . getLeft) []
    
    -- | Monadic version of 'replicate'.
    {-# INLINE filled #-}
    filled :: Int -> e -> m l
    filled n = newLinearN n . replicate n
    
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
    
    -- | Just swap two elements.
    swapM :: l -> Int -> Int -> m ()
    swapM es i j = do ei <- es !#> i; writeM es i =<< es !#> j; writeM es j ei

--------------------------------------------------------------------------------

{- |
  'SplitM' is 'Split' version for mutable data structures. This class is
  designed with the possibility of in-place implementation, so many operations
  from 'Split' have no analogues here.
-}
class (LinearM m s e) => SplitM m s e
  where
    {-# MINIMAL (takeM|sansM), (dropM|keepM) #-}
    
    {- |
      @takeM n es@ returns a reference to the @es@, keeping first @n@ elements.
      Changes in the source and result must be synchronous.
    -}
    takeM :: Int -> s -> m s
    default takeM :: (BorderedM m s i) => Int -> s -> m s
    takeM n es = do s <- getSizeOf es; sansM (s - n) es
    
    {- |
      @dropM n es@ returns a reference to the @es@, discarding first @n@ elements.
      Changes in the source and result must be synchronous.
    -}
    dropM :: Int -> s -> m s
    default dropM :: (BorderedM m s i) => Int -> s -> m s
    dropM n es = do s <- getSizeOf es; keepM (s - n) es
    
    {- |
      @keepM n es@ returns a reference to the @es@, keeping last @n@ elements.
      Changes in the source and result must be synchronous.
    -}
    keepM :: Int -> s -> m s
    default keepM :: (BorderedM m s i) => Int -> s -> m s
    keepM n es = do s <- getSizeOf es; dropM (s - n) es
    
    {- |
      @sansM n es@ returns a reference to the @es@, discarding last @n@ elements.
      Changes in the source and result must be synchronous.
    -}
    sansM :: Int -> s -> m s
    default sansM :: (BorderedM m s i) => Int -> s -> m s
    sansM n es = do s <- getSizeOf es; takeM (s - n) es
    
    {- |
      @splitM n es@ returns pair of references to the @es@: keeping and
      discarding first @n@ elements. Changes in the source and result must be
      synchronous.
    -}
    splitM  :: Int -> s -> m (s, s)
    splitM n es = liftA2 (,) (takeM n es) (dropM n es)
    
    {- |
      @divideM n es@ returns pair of references to the @es@: discarding and
      keeping last @n@ elements. Changes in the source and results must be
      synchronous.
    -}
    divideM :: Int -> s -> m (s, s)
    divideM n es = liftA2 (,) (sansM n es) (keepM n es)
    
    {- |
      @splitM ns es@ returns the sequence of @es@ prefix references of length
      @n <- ns@. Changes in the source and results must be synchronous.
    -}
    splitsM :: (Foldable f) => f Int -> s -> m [s]
    splitsM ns es =
      let f ds' n = do ds <- ds'; (d,d') <- splitM n (head ds); pure (d':d:ds)
      in  reverse <$> foldl f (pure [es]) ns
    
    {- |
      @dividesM ns es@ returns the sequence of @es@ suffix references of length
      @n <- ns@. Changes in the source and results must be synchronous.
    -}
    dividesM :: (Foldable f) => f Int -> s -> m [s]
    dividesM ns es =
      let f n ds' = do ds <- ds'; (d, d') <- divideM n (head ds); pure (d':d:ds)
      in  foldr f (pure [es]) ns
    
    -- TODO: comboM :: (e -> e -> Bool) -> s -> m Int
    
    {- |
      @partsM n es@ returns the sequence of @es@ prefix references, splitted by
      offsets in @es@. Changes in the source and results must be synchronous.
    -}
    partsM :: (Foldable f) => f Int -> s -> m [s]
    partsM =  splitsM . go . toList where go is = zipWith (-) is (0 : is)
    
    {- |
      @chunksM n es@ returns the sequence of @es@ prefix references of length
      @n@. Changes in the source and results must be synchronous.
    -}
    chunksM :: Int -> s -> m [s]
    chunksM n es = do (t, d) <- splitM n es; nowNull d ?^ pure [t] $ (t :) <$> chunksM n d
    
    {- |
      @eachM n es@ returns new sequence of @es@ elements with step @n@. eachM
      shouldn't return references to @es@.
    -}
    eachM :: Int -> s -> m s
    eachM n = newLinearN n . each n <=< getLeft
    
    -- | @prefixM p es@ returns the longest @es@ prefix size, satisfying @p@.
    prefixM :: (e -> Bool) -> s -> m Int
    prefixM p = fmap (prefix p) . getLeft
    
    -- | @suffixM p es@ returns the longest @es@ suffix size, satisfying @p@.
    suffixM :: (e -> Bool) -> s -> m Int
    suffixM p = fmap (suffix p) . getLeft
    
    -- | @mprefix p es@ returns the longest @es@ prefix size, satisfying @p@.
    mprefix :: (e -> m Bool) -> s -> m Int
    mprefix p = foldr (\ e c -> do b <- p e; b ? succ <$> c $ pure 0) (pure 0) <=< getLeft
    
    -- | @msuffix p es@ returns the longest @es@ suffix size, satisfying @p@.
    msuffix :: (e -> m Bool) -> s -> m Int
    msuffix p = foldl (\ c e -> do b <- p e; b ? succ <$> c $ pure 0) (pure 0) <=< getLeft

--------------------------------------------------------------------------------

-- | Kind @(* -> *)@ 'SplitM' structure.
type SplitM1 m l e = SplitM m (l e) e

-- | Kind @(* -> *)@ 'LinearM' structure.
type LinearM1 m l e = LinearM m (l e) e

-- | Kind @(* -> *)@ 'BorderedM' structure.
type BorderedM1 m l i e = BorderedM m (l e) i

-- | Kind @(* -> * -> *)@ 'BorderedM' structure.
type BorderedM2 m l i e = BorderedM m (l i e) i


