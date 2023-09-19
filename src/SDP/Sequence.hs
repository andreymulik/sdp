{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Trustworthy, CPP, TypeFamilies, ConstraintKinds, TypeOperators #-}
{-# LANGUAGE DefaultSignatures, PatternSynonyms, ViewPatterns, BangPatterns #-}

#ifdef SDP_LINEAR_EXTRAS
{-# LANGUAGE FlexibleContexts #-}
#endif

#ifdef SDP_QUALIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.Sequence
    Copyright   :  (c) Andrey Mulik 2023
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)

    "SDP.Sequence" is a module that provides several convenient interfaces for
    working with various sequences.
-}
module SDP.Sequence
(
  -- * Export
  module SDP.Forceable,
  module SDP.Nullable,

  -- * Sequence class
  Sequence (..), Sequence1, Sequence2, pattern (:>), pattern (:<),

#ifdef SDP_QUALIFIED_CONSTRAINTS
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  Sequence', Sequence'',
#endif

  -- ** Legacy
  o_foldr1, o_foldl1, o_foldr1', o_foldl1',
  o_foldr, o_foldl, o_foldr', o_foldl'
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Forceable
import SDP.Nullable

import qualified Data.List as L
import Data.Maybe

#ifdef SDP_LINEAR_EXTRAS
import qualified GHC.Exts as L
#endif

import Control.Exception.SDP

default ()

infixr 5 :>, ++
infixl 5 :<
infixl 9 !^

--------------------------------------------------------------------------------

{-# RULES
  "select/Just"  select  Just = listL;
  #-}

{- |
  'Sequence' is a type class similar to 'Foldable' with three important differences:

  * 'Sequence' allows monomorphic instances, for example: instance Sequence ByteString Char
  * 'Sequence' implements not only sequence reduction, but also their creation
  * 'Sequence' explicitly implements basic convolution operations

  __NOTE:__ Sequence allows non-empty sequences as a last resort, but not more
  complex size restrictions. For example, a type with a valid number of elements
  @3n + 4@ is not a valid sequence instance (but can be 'Foldable').

  Before using 'head', 'tail', 'init' and 'last', you must ensure that there
  is at least one element in the argument structure.

  You will receive an error when you try to:

  * use 'fromList' and 'fromFoldable' with an empty sequence
  * extract the 'tail' or 'init' from single-value sequence
  * extract the 'head', 'tail', 'init' or 'last' of empty sequence

  'Sequence' structures must follow some rules:

  @
    mappend === (<>) === (++)

    isNull  (single e)   === False
    isNull (toHead x xs) === False
    isNull (toLast xs x) === False

    sfoldr  === ofoldr . const
    sfoldl  === ofoldl . const
    sfoldr1 === ofoldr1 . const
    sfoldl1 === ofoldl1 . const

    reverse . reverse === id
    listL === reverse . listR === listR . reverse
    listR === reverse . listL === listL . reverse

    fromList === fromFoldable
    fromFoldable === foldr toHead Z
    select f = sfoldr (\ x es -> case f x of {Just e -> e : es; _ -> es}) []

    -- For Map instances
    ofoldr f base xs === sfoldr (uncurry f) base (assocs xs)

    -- For 'Foldable' instances:
    toList === listL
    length === sizeOf
    length (toHead x xs) === length xs + 1
    length (toLast xs x) === length xs + 1

    isNull xs === length xs == 0
    length xs === length (listL xs) === length (listR xs)

    sfoldr  === foldr
    sfoldl  === foldl
    sfoldr1 === foldr1
    sfoldl1 === foldl1
  @
-}
class
  (
#ifdef SDP_LINEAR_EXTRAS
    L.IsList seq, L.Item seq ~ e,
#endif
    Forceable seq,
    Semigroup seq,
    Estimate seq
  ) => Sequence seq e | seq -> e
  where
    {-# MINIMAL (uncons|(head,last)|uncons'), (unsnoc|(init,last)|unsnoc') #-}

    {- Item-level operations. -}

    -- | Just singleton.
    single :: e -> seq
    single =  flip toHead Z
    default single :: Nullable seq => e -> seq

    -- | Prepends element to line, constructor for ':>' pattern.
    toHead :: e -> seq -> seq
    toHead e es = single e ++ es

    -- | Appends element to line, constructor for ':<' pattern.
    toLast :: seq -> e -> seq
    toLast es e = es ++ single e

    -- | Same as @'isNull' '?-' 'uncons'@
    uncons' :: seq -> Maybe (e, seq)
    uncons' =  isNull ?- \ xs -> (head xs, tail xs)
    default uncons' :: Nullable seq => seq -> Maybe (e, seq)

    -- | Same as @'isNull' '?-' 'unsnoc'@
    unsnoc' :: seq -> Maybe (seq, e)
    unsnoc' =  isNull ?- \ xs -> (init xs, last xs)
    default unsnoc' :: Nullable seq => seq -> Maybe (seq, e)

    -- | Separates line to 'head' and 'tail', deconstructor for ':>' pattern.
    uncons :: seq -> (e, seq)
    uncons xs = fromMaybe (pfailEx "(:>)") (uncons' xs)

    -- | Separates line to 'init' and 'last', deconstructor for ':<' pattern.
    unsnoc :: seq -> (seq, e)
    unsnoc xs = fromMaybe (pfailEx "(:<)") (unsnoc' xs)

    -- | Returns first element of line, may fail.
    head :: seq -> e
    head =  fst . uncons

    -- | Returns line except first, may fail.
    tail :: seq -> seq
    tail =  snd . uncons

    -- | Returns line except 'last' element, may fail.
    init :: seq -> seq
    init =  fst . unsnoc

    -- | Returns last element, may fail.
    last :: seq -> e
    last =  snd . unsnoc

    {- Item(s) from/to sequence. -}

    -- | Creates line from list.
    fromList :: [e] -> seq
    fromList =  fromFoldable

    -- | Creates line from list.
    default fromFoldable :: (Nullable seq, Foldable f) => f e -> seq
    fromFoldable :: Foldable f => f e -> seq
    fromFoldable =  foldr toHead lzero

    -- | Left to right view of line, same to 'toList'.
    listL :: seq -> [e]
    listL =  L.unfoldr uncons'

    -- | Right to left view of line.
    listR :: seq -> [e]
    listR =  L.reverse . listL

    -- | Returns sequence with reversed element order.
    reverse :: seq -> seq
    reverse =  fromList . listR

    {- Folds. -}

    -- | 'sfoldr' is just 'foldr' in 'Sequence' context.
    sfoldr :: (e -> b -> b) -> b -> seq -> b
    sfoldr =  ofoldr . const

    -- | 'sfoldl' is just 'foldl' in 'Sequence' context.
    sfoldl :: (b -> e -> b) -> b -> seq -> b
    sfoldl =  ofoldl . const

    -- | 'sfoldr'' is just 'foldr'' in 'Sequence' context.
    sfoldr' :: (e -> b -> b) -> b -> seq -> b
    sfoldr' =  ofoldr' . const

    -- | 'sfoldl'' is just 'foldl'' in 'Sequence' context.
    sfoldl' :: (b -> e -> b) -> b -> seq -> b
    sfoldl' =  ofoldl' . const

    -- | 'sfoldr1' is just 'Data.Foldable.foldr1' in 'Sequence' context.
    sfoldr1 :: (e -> e -> e) -> seq -> e
    sfoldr1 f = \ es' -> case unsnoc' es' of
      Just (es, e) -> sfoldr f e es
      _            -> pfailEx "sfoldr1"

    -- | 'sfoldl1' is just 'Data.Foldable.foldl1' in 'Sequence' context.
    sfoldl1 :: (e -> e -> e) -> seq -> e
    sfoldl1 f = \ es' -> case uncons' es' of
      Just (e, es) -> sfoldl f e es
      _            -> pfailEx "sfoldl1"

    -- | 'sfoldr1'' is just strict 'Data.Foldable.foldr1' in 'Sequence' context.
    sfoldr1' :: (e -> e -> e) -> seq -> e
    sfoldr1' f = \ es' -> case unsnoc' es' of
      Just (es, e) -> sfoldr' f e es
      _            -> pfailEx "sfoldr1'"

    -- | 'sfoldl1'' is just 'Data.Foldable.foldl1'' in 'Sequence' context.
    sfoldl1' :: (e -> e -> e) -> seq -> e
    sfoldl1' f = \ es' -> case uncons' es' of
      Just (e, es) -> sfoldl' f e es
      _            -> pfailEx "sfoldl1'"

    {- Folds with offset. -}

    -- | 'ofoldr' is right fold with offset.
    ofoldr :: (Int -> e -> b -> b) -> b -> seq -> b
    ofoldr f base = ofoldr f base . listL

    -- | 'ofoldl' is left fold with offset.
    ofoldl :: (Int -> b -> e -> b) -> b -> seq -> b
    ofoldl f base = ofoldl f base . listL

    -- | 'ofoldr'' is strict version of 'ofoldr'.
    ofoldr' :: (Int -> e -> b -> b) -> b -> seq -> b
    ofoldr' f = ofoldr (\ !i e !b -> f i e b)

    -- | 'ofoldl'' is strict version of 'ofoldl'.
    ofoldl' :: (Int -> b -> e -> b) -> b -> seq -> b
    ofoldl' f = ofoldl (\ !i !b e -> f i b e)

    {- Concatenation. -}

    -- | Concatenation of two lines.
    (++) :: seq -> seq -> seq
    (++) =  (<>)

    -- | prefix gives length of init, satisfying preducate.
    prefix :: (e -> Bool) -> seq -> Int
    prefix p = sfoldr' (\ e c -> p e ? succ c $ 0) 0

    -- | suffix gives length of tail, satisfying predicate.
    suffix :: (e -> Bool) -> seq -> Int
    suffix p = sfoldl' (\ c e -> p e ? succ c $ 0) 0

    {- |
      @infixes inf es@ returns a list of @inf@ positions in @es@, without
      intersections.

      > "" `infixes` es = []
      > "abba" `infixes` "baababba" == [4]
      > "abab" `infixes` "baababab" == [2]
      > "aaaa" `infixes` "aaaaaaaa" == [0, 4]
    -}
    infixes :: Eq e => seq -> seq -> [Int]
    infixes =  on infixes listL

    {- Deduplication. -}

    -- | Same as @nubBy ('==')@.
    nub :: Eq e => seq -> seq
    nub =  nubBy (==)

    -- | Generalization of nubBy.
    nubBy :: Equal e -> seq -> seq
    nubBy f = fromList . nubBy f . listL

    {- Subsequences. -}

    {- |
      The @isSubseqOf xs ys@ checks if all the elements of the @xs@ occur,
      in order, in the @ys@. The elements don't have to occur consecutively.
    -}
    isSubseqOf :: Eq e => seq -> seq -> Bool
    isSubseqOf =  L.isSubsequenceOf `on` listL

    -- | @sub `'isPrefixOf'` es@ checks if @sub@ is beginning of @es@.
    isPrefixOf :: Eq e => seq -> seq -> Bool
    isPrefixOf =  isPrefixOf `on` listL

    -- | @sub `'isSuffixOf'` es@ checks if @sub@ is ending of @es@.
    isSuffixOf :: Eq e => seq -> seq -> Bool
    isSuffixOf =  isSuffixOf `on` listL

    -- | isInfixOf checks whether the first line is the substring of the second
    isInfixOf :: Eq e => seq -> seq -> Bool
    isInfixOf =  isInfixOf `on` listL

    {- Selections. -}

    -- | @select f es@ is selective map of @es@ elements to new list.
    select :: (e -> Maybe a) -> seq -> [a]
    select f = sfoldr (\ x es -> maybe es (: es) (f x)) []

    {- |
      @extract f es@ returns a selective map of @es@ elements to new list and
      the remaining elements of the line.
    -}
    extract :: (e -> Maybe a) -> seq -> ([a], [e])
    extract f =
      let g = \ b -> second (b :) `maybe` (first . (:)) $ f b
      in  sfoldr' g ([], [])

    {- |
      @selectWhile f es@ selects results of applying @f@ to @es@ (left to right)
      untill first fail.
    -}
    selectWhile :: (e -> Maybe a) -> seq -> [a]
    selectWhile f =
      let go e xs = case f e of {Just x -> x : xs; _ -> []}
      in  sfoldr go []

    {- |
      @selectEnd f es@ selects results of applying @f@ to @es@ (right to left)
      untill first fail.
    -}
    selectEnd :: (e -> Maybe a) -> seq -> [a]
    selectEnd f =
      let go xs e = case f e of {Just x -> x : xs; _ -> []}
      in  reverse . sfoldl go []

    {- |
      @since 0.3

      Monadic version of 'select'.
    -}
    mselect :: Applicative t => (e -> t (Maybe a)) -> seq -> t [a]
    mselect go = sfoldr (liftA2 (\ x xs -> maybe xs (: xs) x) . go) (pure [])

    {- |
      @since 0.3

      Monadic version of 'extract'.
    -}
    mextract :: Applicative t => (e -> t (Maybe a)) -> seq -> t ([a], [e])
    mextract go = sfoldr (\ e -> liftA2 (\ x (xs, es) ->
        maybe (xs, e : es) (\ x' -> (x' : xs, es)) x) (go e)
      ) (pure ([], []))

    {- |
      @since 0.3

      'traverse' for 'Sequence'.
    -}
    otraverse :: Applicative t => (Int -> e -> t e) -> seq -> t seq
    otraverse f = fmap fromList . ofoldr (\ o e xs ->
        liftA2 (:) (f o e) xs
      ) (pure [])

    {- Operations with elements. -}

    {- |
      Returns the element of a sequence by offset, may be completely unsafe.
      This is an optimistic read function and shouldn't perform checks for
      efficiency reasons.

      If you need safety, use (!) or (!?). The generalization of this function
      by index type (.!).

      > es !^ i = listL es !! i
    -}
    (!^) :: seq -> Int -> e
    (!^) =  (L.!!) . listL

    {- |
      @write es n e@ writes value @e@ in position @n@ (offset), returns new
      structure. If @n@ is out of range, returns equal structure (@es@ or copy).
    -}
    write :: seq -> Int -> e -> seq
    write es = fromList ... write (listL es)

    {- |
      @since 0.2.1

      @'before' es i e@ insert @e@ to @es@ before element with offset @i@. If
      @i@ goes beyond the lower or upper bounds, @e@ is prepended or appended to
      @es@ respectively.

      > before [0 .. 5] (-1) 7 == [7,0,1,2,3,4,5]
      > before [0 .. 5]   0  7 == [7,0,1,2,3,4,5]
      > before [0 .. 5]   3  7 == [0,1,2,7,3,4,5]
      > before [0 .. 5]   5  7 == [0,1,2,3,4,7,5]
      > before [0 .. 5]   6  7 == [0,1,2,3,4,5,7]
      > before [0 .. 5]  19  7 == [0,1,2,3,4,5,7]
    -}
    before :: seq -> Int -> e -> seq
    before es = fromList ... before (listL es)

--------------------------------------------------------------------------------

-- | Pattern @(':>')@ is left-size view of line. Same as 'uncons' and 'toHead'.
pattern  (:>)   :: Sequence seq e => e -> seq -> seq
pattern x :> xs <- (uncons' -> Just (x, xs)) where (:>) = toHead

-- | Pattern @(':<')@ is right-size view of line. Same as 'unsnoc' and 'toLast'.
pattern   (:<)  :: Sequence seq e => seq -> e -> seq
pattern xs :< x <- (unsnoc' -> Just (xs, x)) where (:<) = toLast

--------------------------------------------------------------------------------

-- | 'Sequence' contraint for @(Type -> Type)@-kind types.
type Sequence1 l e = Sequence (l e) e

-- | 'Sequence' contraint for @(Type -> Type -> Type)@-kind types.
type Sequence2 l i e = Sequence (l i e) e

#ifdef SDP_QUALIFIED_CONSTRAINTS
-- | 'Sequence' contraint for @(Type -> Type)@-kind types.
type Sequence' l = forall e . Sequence (l e) e

-- | 'Sequence' contraint for @(Type -> Type -> Type)@-kind types.
type Sequence'' l = forall i e . Sequence (l i e) e
#endif

--------------------------------------------------------------------------------

{-# COMPLETE [], (:>) #-}
{-# COMPLETE [], (:<) #-}

instance Sequence [e] e
  where
    toHead = (:)
    toLast = flip (foldr' (:) . pure)

    uncons' = isNull ?- uncons
    unsnoc' = isNull ?- unsnoc

    uncons    []    = pfailEx "(:>)"
    uncons (e : es) = (e, es)

    unsnoc   [ ]    = pfailEx "(:<)"
    unsnoc   [e]    = ([], e)
    unsnoc (e : es) = let (es', e') = unsnoc es in (e : es', e')

    head  = L.head
    tail  = L.tail
    init  = L.init
    last  = L.last
    listL = toList
    listR = L.reverse

    ofoldr f base =
      let go !i es = case es of {x : xs -> f i x $ go (i + 1) xs; _ -> base}
      in  go 0

    ofoldl f =
      let go !i base es = case es of {x : xs -> go (i + 1) (f i base x) xs; _ -> base}
      in  go 0

    single       = pure
    fromList     = id
    fromFoldable = toList

    (++)   = (L.++)
    (!^)   = (L.!!)

    write es n e = n < 0 ? es $ go n es
      where
        go i (x : xs) = i == 0 ? e : xs $ x : go (i - 1) xs
        go _ _ = []

    sfoldr'    = foldr'
    sfoldl'    = foldl'
    sfoldr     = foldr
    sfoldl     = foldl
    nubBy      = L.nubBy
    reverse    = L.reverse
    isSubseqOf = L.isSubsequenceOf

    before es i e = go (max 0 i) es
      where
        go 0    xs    = e : xs
        go n (x : xs) = x : go (n - 1) xs
        go _    []    = [e]

    infixes  Z  = const []
    infixes sub = go 0
      where
        go _ [] = []
        go i es = sub `isPrefixOf` es ? i : go (i + n) (L.drop n es) $ go (i + 1) (tail es)

        n = sizeOf sub

    isPrefixOf = L.isPrefixOf
    isSuffixOf = L.isSuffixOf
    isInfixOf  = L.isInfixOf

    selectWhile _    []    = []
    selectWhile f (x : xs) = case f x of {(Just e) -> e : select f xs; _ -> []}

    selectEnd f = reverse . selectWhile f . reverse

--------------------------------------------------------------------------------

{- Legacy. -}

{-# DEPRECATED o_foldr  "in favour 'sfoldr'"  #-}
{-# DEPRECATED o_foldl  "in favour 'sfoldl'"  #-}
{-# DEPRECATED o_foldr' "in favour 'sfoldr''" #-}
{-# DEPRECATED o_foldl' "in favour 'sfoldl''" #-}

-- | Same as 'sfoldr'.
o_foldr :: Sequence seq e => (e -> b -> b) -> b -> seq -> b
o_foldr =  sfoldr

-- | Same as 'sfoldl'.
o_foldl :: Sequence seq e => (b -> e -> b) -> b -> seq -> b
o_foldl =  sfoldl

-- | Same as 'sfoldr''.
o_foldr' :: Sequence seq e => (e -> b -> b) -> b -> seq -> b
o_foldr' =  sfoldr'

-- | Same as 'sfoldl''.
o_foldl' :: Sequence seq e => (b -> e -> b) -> b -> seq -> b
o_foldl' =  sfoldl'

--------------------------------------------------------------------------------

{-# DEPRECATED o_foldr1  "in favour 'sfoldr1'"  #-}
{-# DEPRECATED o_foldl1  "in favour 'sfoldl1'"  #-}
{-# DEPRECATED o_foldr1' "in favour 'sfoldr1''" #-}
{-# DEPRECATED o_foldl1' "in favour 'sfoldl1''" #-}

-- | Same as 'sfoldr1'.
o_foldr1 :: Sequence seq e => (e -> e -> e) -> seq -> e
o_foldr1 =  sfoldr1

-- | Same as 'sfoldl1'.
o_foldl1 :: Sequence seq e => (e -> e -> e) -> seq -> e
o_foldl1 =  sfoldl1

-- | Same as 'sfoldr1''.
o_foldr1' :: Sequence seq e => (e -> e -> e) -> seq -> e
o_foldr1' =  sfoldr1'

-- | Same as 'sfoldl1''.
o_foldl1' :: Sequence seq e => (e -> e -> e) -> seq -> e
o_foldl1' =  sfoldl1'

--------------------------------------------------------------------------------

{-# NOINLINE pfailEx #-}
pfailEx :: String -> a
pfailEx =  throw . PatternMatchFail . showString "in SDP.Sequence."


