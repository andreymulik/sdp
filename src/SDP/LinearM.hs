{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds, DefaultSignatures, FlexibleContexts, RankNTypes #-}
{-# LANGUAGE Safe, CPP, BangPatterns, GADTs, ViewPatterns, PatternSynonyms #-}

#if MIN_VERSION_fmr(0,3,0)
{-# LANGUAGE TypeFamilies, DataKinds #-}
#endif

#ifdef SDP_QUALIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{- |
    Module      :  SDP.LinearM
    Copyright   :  (c) Andrey Mulik 2019-2023
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)

    "SDP.LinearM" is a module that provides 'BorderedM' and 'LinearM' classes.
-}
module SDP.LinearM
(
  -- * Exports
  module SDP.SequenceM,
  module SDP.Linear,

  -- * LinearM class
  LinearM (..), LinearM1, LinearM2, exceptM, (~=), eachM, eachFromM,
  splitsM, dividesM, partsM, chunksM, intersperseM, intercalateM,

  -- ** LinearM field
  -- $fmrNotes
  pattern (:+=), pattern (:=+), pattern (:~=),

#if MIN_VERSION_fmr(0,3,0)
  (=+:), (+=:), (~=:),
#endif

#ifdef SDP_QUALIFIED_CONSTRAINTS
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  LinearM', LinearM''
#endif
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.SequenceM
import SDP.Linear

#if MIN_VERSION_fmr(0,3,0)
import Data.Field
import Data.Proxy
#else
import Data.Typeable
import Data.Property hiding ( set )
#endif

default ()

--------------------------------------------------------------------------------

{- |
  'LinearM' is 'Linear' version for mutable data structures. This class is
  designed with the possibility of in-place implementation, so many operations
  from 'Linear' have no analogues here.
-}
class (SequenceM m l e, NullableM m l) => LinearM m l e | l -> m, l -> e
  where
    {-# MINIMAL (takeM|sansM), (dropM|keepM), copyTo #-}

    -- | Monadic version of 'replicate'.
    {-# INLINE filled #-}
    filled :: Int -> e -> m l
    filled n = newLinearN n . replicate n

    {- |
      @since 0.3

      Monadic 'fromList''.
    -}
    newLinear' :: Maybe SizeHint -> [e] -> m l
    newLinear' =  const newLinear

    -- | Monadic 'fromListN'.
    {-# INLINE newLinearN #-}
    newLinearN :: Int -> [e] -> m l
    newLinearN =  newLinear ... take

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

    filterM :: (e -> m Bool) -> l -> m l
    filterM go = newLinear <=< foldrM (\ e xs ->
        do b <- go e; return (b ? e : xs $ xs)
      ) []

    -- | Monadic 'concat'.
    merged :: Foldable f => f l -> m l
    merged =  newLinear . concat <=< sequence . foldr ((:) . getLeft) []

    {- |
      @takeM n es@ returns a reference to the @es@, keeping first @n@ elements.
      Changes in the source and result must be synchronous.
    -}
    takeM :: Int -> l -> m l
    takeM n es = do s <- getSizeOf es; sansM (s - n) es

    {- |
      @dropM n es@ returns a reference to the @es@, discarding first @n@ elements.
      Changes in the source and result must be synchronous.
    -}
    dropM :: Int -> l -> m l
    dropM n es = do s <- getSizeOf es; keepM (s - n) es

    {- |
      @keepM n es@ returns a reference to the @es@, keeping last @n@ elements.
      Changes in the source and result must be synchronous.
    -}
    keepM :: Int -> l -> m l
    keepM n es = do s <- getSizeOf es; dropM (s - n) es

    {- |
      @sansM n es@ returns a reference to the @es@, discarding last @n@ elements.
      Changes in the source and result must be synchronous.
    -}
    sansM :: Int -> l -> m l
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
      @since 0.3

      @splitsMOn sub line@ splits @line@ by @sub@. Returns new structure.
    -}
    splitsMOn :: Eq e => l -> l -> m [l]
    splitsMOn sub line = do
      sz <- getSizeOf sub
      is <- infixesM sub line
      mapM (dropM sz) =<< partsM is line

    {-
      @since 0.3

      Monadic 'takeWhile'.
    -}
    takeWhile' :: (e -> Bool) -> l -> m l
    takeWhile' f es = do c <- prefixM f es; takeM c es

    {-
      @since 0.3

      Monadic 'dropWhile'.
    -}
    dropWhile' :: (e -> Bool) -> l -> m l
    dropWhile' f es = do c <- prefixM f es; dropM c es

    {-
      @since 0.3

      Monadic 'takeEnd'.
    -}
    takeEnd' :: (e -> Bool) -> l -> m l
    takeEnd' f es = do c <- suffixM f es; keepM c es

    {-
      @since 0.3

      Monadic 'dropEnd'.
    -}
    dropEnd' :: (e -> Bool) -> l -> m l
    dropEnd' f es = do c <- suffixM f es; sansM c es

    {-
      @since 0.3

      Monadic 'takeWhile'.
    -}
    takeWhileM :: (e -> m Bool) -> l -> m l
    takeWhileM f es = do c <- mprefix f es; takeM c es

    {-
      @since 0.3

      Monadic 'dropWhile'.
    -}
    dropWhileM :: (e -> m Bool) -> l -> m l
    dropWhileM f es = do c <- mprefix f es; dropM c es

    {-
      @since 0.3

      Monadic 'takeEnd'.
    -}
    takeEndM :: (e -> m Bool) -> l -> m l
    takeEndM f es = do c <- msuffix f es; keepM c es

    {-
      @since 0.3

      Monadic 'dropEnd'.
    -}
    dropEndM :: (e -> m Bool) -> l -> m l
    dropEndM f es = do c <- msuffix f es; sansM c es

    {- |
      @since 0.3

      @replaceMBy sub new line@ replace every non-overlapping occurrence of @sub@
      in @line@ with @new@. Returns new structure.
    -}
    replaceMBy :: Eq e => l -> l -> l -> m l
    replaceMBy sub new = intercalateM new <=< splitsMOn sub

    {- |
      @since 0.3

      @removeMAll sub line@ removes every non-overlapping occurrence of @sub@ in
      @line@. Same as @do new <- newNull replaceMBy sub new line@.
    -}
    removeMAll :: Eq e => l -> l -> m l
    removeMAll =  merged <=<< splitsMOn

    -- | @'removed' n es@ removes element with offset @n@ from @es@.
    removed :: Int -> l -> m l
    removed n es = newLinear . remove n =<< getLeft es

    {-# INLINE copied' #-}
    -- | @copied' es l n@ returns the slice of @es@ from @l@ of length @n@.
    copied' :: l -> Int -> Int -> m l
    copied' es l n = getLeft es >>= newLinearN n . drop l

    {- |
      @since 0.2.1
      Monadic in-place 'reverse', reverse elements of given structure.
    -}
    reversed' :: l -> m ()
    reversed' es = ofoldr (\ i e go -> do writeM es i e; go) (return ()) =<< getRight es

    {- |
      @copyTo source soff target toff count@ writes @count@ elements of @source@
      from @soff@ to @target@ starting with @toff@.
    -}
    copyTo :: l -> Int -> l -> Int -> Int -> m ()

    {- Special operations. -}

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

    -- | Just swap two elements.
    swapM :: l -> Int -> Int -> m ()
    swapM es i j = do ei <- es !#> i; writeM es i =<< es !#> j; writeM es j ei

--------------------------------------------------------------------------------

{- |
  @since 0.3

  'exceptM' is inverted 'filterM'.
-}
exceptM :: LinearM m l e => (e -> m Bool) -> l -> m l
exceptM  = filterM . (fmap not .)

{- |
  @eachM n es@ returns new sequence of @es@ elements with step @n@. eachM
  shouldn't return references to @es@.
-}
eachM :: LinearM m l e => Int -> l -> m l
eachM n =
  let hint = SizeHint 0 (max 0 n)
  in  newLinear' (Just hint) . each n <=< getLeft

{- |
  @since 0.3

  @eachFromM n es@ returns each nth element of structure, beginning from o.
-}
eachFromM :: LinearM m l e => Int -> Int -> l -> m l
eachFromM n o =
  let hint = SizeHint 0 (max 0 (n - o))
  in  newLinear' (Just hint) . eachFrom n o <=< getLeft

{- |
  @splitM ns es@ returns the sequence of @es@ prefix references of length
  @n <- ns@. Changes in the source and results must be synchronous.
-}
splitsM :: (LinearM m l e, Foldable f) => f Int -> l -> m [l]
splitsM ns es =
  let f ds' n = do ds <- ds'; (d, d') <- splitM n (head ds); pure (d':d:ds)
  in  reverse <$> foldl f (pure [es]) ns

{- |
  @dividesM ns es@ returns the sequence of @es@ suffix references of length
  @n <- ns@. Changes in the source and results must be synchronous.
-}
dividesM :: (LinearM m l e, Foldable f) => f Int -> l -> m [l]
dividesM ns es =
  let f n ds' = do ds <- ds'; (d, d') <- divideM n (head ds); pure (d':d:ds)
  in  foldr f (pure [es]) ns

{- |
  @partsM n es@ returns the sequence of @es@ prefix references, splitted by
  offsets in @es@. Changes in the source and results must be synchronous.
-}
partsM :: (LinearM m l e, Foldable f) => f Int -> l -> m [l]
partsM =  splitsM . go . toList where go is = zipWith (-) is (0 : is)

{- |
  @chunksM n es@ returns the sequence of @es@ prefix references of length
  @n@. Changes in the source and results must be synchronous.
-}
chunksM :: LinearM m l e => Int -> l -> m [l]
chunksM n es = do (t, d) <- splitM n es; nowNull d ?^ pure [t] $ (t :) <$> chunksM n d

-- | Monadic 'intersperse'.
intersperseM :: LinearM m l e => e -> l -> m l
intersperseM e es = do
  xs <- drop 1 <$> foldrM (pure . (e :) ... (:)) [] es
  xs .< 2 ? pure es $ newLinear xs

-- | Monadic 'intercalate'.
intercalateM :: (Foldable f, LinearM m l e) => l -> f l -> m l
intercalateM e es = do
  xs <- drop 1 <$> foldr (\ x xs' -> do xs <- xs'; pure (e : x : xs)) (pure []) es
  merged (xs .< 2 ? [] $ xs)

{- |
  @since 0.3

  Same as 'removed'
-}
(~=) :: LinearM m l e => Int -> l -> m l
(~=) =  removed

--------------------------------------------------------------------------------

{- fmr append, prepend and delete fields. -}

{- $fmrNotes
  Note that @fmr-0.3@ support starts with @sdp-0.2.1.2@ patch - previous
  versions only work with @fmr-0.2@.

  Note that @fmr-0.3@ provides new field extension mechanism, therefore @(:+=)@,
  @(:=+)@ and @(:~=)@ patterns in @sdp-0.3@ implemented differently and have
  different types. @fmr-0.2@ and old style patterns support will be dropped
  in @sdp-0.4@.
-}

#if !MIN_VERSION_fmr(0,3,0)

{-# WARNING FieldLinearM "deprecated, fmr-0.2 support will be dropped in sdp-0.4" #-}

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
    performProp record (Append field e) = setRecord field record
                           =<< (=+ e) =<< getRecord field record

    performProp record (Delete n field) = setRecord field record
                        =<< removed n =<< getRecord field record

    performProp record (Prepend e field) = setRecord field record
                            =<< (e +=) =<< getRecord field record

{-# WARNING (:+=) "different for fmr-0.2 and fmr-0.3" #-}

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

{-# WARNING (:=+) "different for fmr-0.2 and fmr-0.3" #-}

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

{-# WARNING (:~=) "different for fmr-0.2 and fmr-0.3" #-}

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
data instance AccessRep "linear" "" m l e

newtype instance AccessRep "linear" "head" m l e = AccessHead {accessHead :: AccessUse "linear" "head" m l e}
newtype instance AccessRep "linear" "last" m l e = AccessLast {accessLast :: AccessUse "linear" "last" m l e}

newtype instance AccessRep "linear" "left"  m l e = AccessLeft  {accessLeft  :: AccessUse "linear"  "left" m l e}
newtype instance AccessRep "linear" "right" m l e = AccessRight {accessRight :: AccessUse "linear" "right" m l e}

newtype instance AccessRep "linear" "prepend" m l e = AccessPrepend {accessPrepend :: AccessUse "linear" "prepend" m l e}
newtype instance AccessRep "linear" "append"  m l e = AccessAppend  {accessAppend  :: AccessUse "linear"  "append" m l e}
newtype instance AccessRep "linear" "remove"  m l e = AccessRemove  {accessRemove  :: AccessUse "linear"  "remove" m l e}

--------------------------------------------------------------------------------

type instance AccessUse "linear" "" m l e = ()

type instance AccessUse "linear" "left"  m l e = l -> m [e]
type instance AccessUse "linear" "right" m l e = l -> m [e]

type instance AccessUse "linear" "head" m l e = l -> m e
type instance AccessUse "linear" "last" m l e = l -> m e

type instance AccessUse "linear" "prepend" m l e = e -> l -> m l
type instance AccessUse "linear" "append"  m l e = l -> e -> m l
type instance AccessUse "linear" "remove"  m l e = Int -> l -> m l

--------------------------------------------------------------------------------

instance Attribute "linear" "" m l e where attribute = undefined

instance LinearM m l e => Attribute "linear" "left" m l e
  where
    attribute = AccessLeft getLeft

instance LinearM m l e => Attribute "linear" "right" m l e
  where
    attribute = AccessRight getRight

instance LinearM m l e => Attribute "linear" "head" m l e
  where
    attribute = AccessHead getHead

instance LinearM m l e => Attribute "linear" "last" m l e
  where
    attribute = AccessLast getLast

instance LinearM m l e => Attribute "linear" "append" m l e
  where
    attribute = AccessAppend (=+)

instance LinearM m l e => Attribute "linear" "prepend" m l e
  where
    attribute = AccessPrepend (+=)

instance LinearM m l e => Attribute "linear" "remove" m l e
  where
    attribute = AccessRemove removed

--------------------------------------------------------------------------------

instance IsMVar m var => UseAttribute var "linear" "" m l e
  where
    useAttr _ _ = ()

instance IsMVar m var => UseAttribute var "linear" "head" m l e
  where
    useAttr attr _ rep = do
      l <- fromMRef attr
      accessHead l rep

instance IsMVar m var => UseAttribute var "linear" "last" m l e
  where
    useAttr attr _ rep = do
      l <- fromMRef attr
      accessLast l rep

instance IsMVar m var => UseAttribute var "linear" "left" m l e
  where
    useAttr attr _ rep = do
      l <- fromMRef attr
      accessLeft l rep

instance IsMVar m var => UseAttribute var "linear" "right" m l e
  where
    useAttr attr _ rep = do
      l <- fromMRef attr
      accessRight l rep

instance IsMVar m var => UseAttribute var "linear" "append" m l e
  where
    useAttr attr _ rep e = do
      l <- fromMRef attr
      accessAppend l rep e

instance IsMVar m var => UseAttribute var "linear" "prepend" m l e
  where
    useAttr attr _ e rep = do
      l <- fromMRef attr
      accessPrepend l e rep

instance IsMVar m var => UseAttribute var "linear" "remove" m l e
  where
    useAttr attr _ rep n = do
      l <- fromMRef attr
      accessRemove l rep n

pattern (:+=) :: UseField "linear" "append" api
              => FieldT m api l e -> e -> l -> m l

pattern fld :+= val <- (const Nothing -> Just (fld, val)) where (:+=) = flip (=+:)

pattern (:=+) :: UseField "linear" "prepend" api
              => FieldT m api l e -> e -> l -> m l

pattern fld :=+ val <- (const Nothing -> Just (fld, val)) where (:=+) = flip (+=:)

pattern (:~=) :: UseField "linear" "remove" api
              => FieldT m api l e -> Int -> l -> m l

pattern fld :~= n <- (const Nothing -> Just (fld, n)) where (:~=) = flip (~=:)

--------------------------------------------------------------------------------

linear' :: UseField "linear" sub api => Proxy sub -> FieldT m api rep a
        -> AccessUse "linear" sub m rep a
linear' =  use (Proxy :: Proxy "linear")

-- | Same as @(':=+')@ but doesn't require 'Typeable'.
(=+:) :: UseField "linear" "append" api
      => e -> FieldT m api l e -> l -> m l
(=+:) =  \ e fld l -> linear' (Proxy :: Proxy "append") fld l e

-- | Same as @(':=+')@ but doesn't require 'Typeable'.
(+=:) :: UseField "linear" "prepend" api
      => e -> FieldT m api l e -> l -> m l
(+=:) =  \ e fld l -> linear' (Proxy :: Proxy "prepend") fld e l

-- | Same as @(':=+')@ but doesn't require 'Typeable'.
(~=:) :: UseField "linear" "remove" api
      => Int -> FieldT m api l e -> l -> m l
(~=:) =  \ n fld l -> linear' (Proxy :: Proxy "remove") fld n l
#endif

--------------------------------------------------------------------------------

-- | 'LinearM' contraint for @(Type -> Type)@-kind types.
type LinearM1 m l e = LinearM m (l e) e

-- | 'LinearM' contraint for @(Type -> Type -> Type)@-kind types.
type LinearM2 m l i e = LinearM m (l i e) e

#ifdef SDP_QUALIFIED_CONSTRAINTS
-- | 'LinearM' contraint for @(Type -> Type)@-kind types.
type LinearM' m l = forall e . LinearM m (l e) e

-- | 'LinearM' contraint for @(Type -> Type -> Type)@-kind types.
type LinearM'' m l = forall i e . LinearM m (l i e) e
#endif

