{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Trustworthy, CPP, TypeFamilies, ConstraintKinds, TypeOperators #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns, BangPatterns #-}

#ifdef SDP_LINEAR_EXTRAS
{-# LANGUAGE FlexibleContexts #-}
#endif

#ifdef SDP_QUALIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.Linear
    Copyright   :  (c) Andrey Mulik 2019-2023
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)

    "SDP.Linear" is a module that provides several convenient interfaces for
    working with various linear data structures.
-}
module SDP.Linear
(
  -- * Exports
  module SDP.Forceable,
  module SDP.Nullable,
  module SDP.Sequence,
  module SDP.Index,
  module SDP.Sort,
  module SDP.Zip,

  -- * Bordered class
  module SDP.Bordered,

  -- * Linear class
  Linear (..), Linear1, Linear2,

#ifdef SDP_QUALIFIED_CONSTRAINTS
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  Linear', Linear'',
#endif

  -- * Related functions
  splitBy, divideBy, splits, divides, tails, inits, parts, chunks, save, skip,
  partitions, subsequences, intersperse, intercalate, except, mexcept,
  spanl, breakl, spanr, breakr,
  
  selectWhile', selectEnd', extractWhile', extractEnd', dropWhileEnd,
  stripPrefix, stripSuffix, stripPrefix', stripSuffix',
  each, eachFrom, combo, ascending
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Forceable
import SDP.Nullable
import SDP.Sequence
import SDP.Bordered
import SDP.Index
import SDP.Sort
import SDP.Zip

import qualified Data.List as L

import Control.Exception.SDP

default ()

infix 8 `filter`, `except`

--------------------------------------------------------------------------------

{-# RULES "select'/Just" select' Just = id #-}

{- |
  'Linear' is one of the main SDP classes, a class of linear data structures.

  A structure of type @l@ must be able to contain an arbitrary ordered dataset
  of type @e@, with arbitrary length. The ordering implies that each @e@ element
  in @l@ can be associated with a non-negative number.

  Structures that cannot store, for example, a zero number of elements (e.g.
  'Data.List.NonEmpty.NonEmpty'), or have some static size, aren't 'Linear'.

  Of course, the @l@ structure can also have a greater dimension, for example,
  a matrix (of scalar elements) can be 'Linear' if some order is defined for its
  elements. The standard SDP structures use the 'Index' class to define this
  order. You may also want to think of the table as a linear structure
  containing rows/columns, or use a different ordering method such as Gray code.

  'Linear' structures must follow some rules:

  @
    mconcat === fold
    mempty  === lzero

    concat === fold
    concatMap === foldMap
    fromList === fromFoldable
    fromFoldable === foldr toHead Z

    filter p = fromList . sfoldr (\ x xs -> p x ? x : xs $ xs) []

    -- For 'Foldable' instances:
    length (replicate n e) === n

    isNull xs === length xs == 0
  @
-}
class (Monoid l, Nullable l, Sequence l e) => Linear l e | l -> e
  where
    {-# MINIMAL (take|sans), (drop|keep) #-}

    {- Item(s) from/to sequence. -}

    -- | @replicate n e@ returns a line of @n@ repetitions of the element @e@.
    replicate :: Int -> e -> l
    replicate n = fromListN n . replicate n

    {- |
      @since 0.3

      Creates line from list.
    -}
    fromList' :: Maybe SizeHint -> [e] -> l
    fromList' =  const fromList

    -- | Create finite line from (possibly infinite) list.
    fromListN :: Int -> [e] -> l
    fromListN n es = Just (SizeHintEQ n) `fromList'` L.take n es

    {- |
      @since 0.3

      @unfoldr gen x@ creates new structure using producing function @gen@ and
      initial value @x@. @gen@ takes the element and returns 'Nothing' if it is
      done producing the linear structure or returns @Just (e, a)@, in which
      case, @e@ is a prepended to the structure and @a@ is used as the next
      element in a recursive call.
    -}
    unfoldr :: (b -> Maybe (e, b)) -> b -> l
    unfoldr =  fromList ... L.unfoldr

    {- Folds with offset. -}

    {- |
      @iterate n f x@ returns sequence of @n@ applications of @f@ to @x@.

      Note that @iterate@ returns finite sequence, instead "Prelude" prototype.
    -}
    iterate :: Int -> (e -> e) -> e -> l
    iterate n = fromListN n ... iterate n

    {- Filtering operations. -}

    -- | Generalized filter.
    filter :: (e -> Bool) -> l -> l
    filter p = fromList . sfoldr (\ x xs -> p x ? x : xs $ xs) []

    mfilter :: Monad m => (e -> m Bool) -> l -> m l
    mfilter go = fmap fromList . mselect (\ e -> do b <- go e; return (b ? Just e $ Z))

    -- | Generalization of partition.
    partition :: (e -> Bool) -> l -> (l, l)
    partition p es = (filter p es, except p es)

    -- | Monadic version of 'partition'.
    mpartition :: Monad m => (e -> m Bool) -> l -> m (l, l)
    mpartition p es = liftA2 (,) (mfilter p es) (mexcept p es)

    {- Concatenation. -}

    -- | Generalized concat.
    concat :: Foldable f => f l -> l
    concat =  fold

    -- | Generalized concatMap.
    concatMap :: Foldable f => (a -> l) -> f a -> l
    concatMap =  foldMap

    {- Splits. -}

    -- | @take n es@ takes first @n@ elements of @es@.
    take :: Int -> l -> l
    take n es = sans (sizeOf es - n) es

    -- | @drop n es@ drops first @n@ elements of @es@.
    drop :: Int -> l -> l
    drop n es = keep (sizeOf es - n) es

    -- | @keep n es@ takes last @n@ elements of @es@.
    keep :: Int -> l -> l
    keep n es = drop (sizeOf es - n) es

    -- | @sans n es@ drops last @n@ elements of @es@.
    sans :: Int -> l -> l
    sans n es = take (sizeOf es - n) es

    -- | @split n es@ is same to @(take n es, drop n es)@.
    split :: Int -> l -> (l, l)
    split n es = (take n es, drop n es)

    -- | @divide n es@ is same to @(sans n es, keep n es)@.
    divide :: Int -> l -> (l, l)
    divide n es = (sans n es, keep n es)

    -- | Splits line by separation elements.
    splitsBy :: (e -> Bool) -> l -> [l]
    splitsBy e = map fromList . splitsBy e . listL

    {- |
      @splitsOn sub line@ splits @line@ by @sub@.

      > splitsOn "fo" "foobar bazfoobar1" == ["","obar baz","obar1"]
    -}
    splitsOn :: Eq e => l -> l -> [l]
    splitsOn sub line = drop (sizeOf sub) <$> parts (infixes sub line) line

    -- | Takes the longest 'prefix' by predicate.
    takeWhile :: (e -> Bool) -> l -> l
    takeWhile p es = take (prefix p es) es

    -- | Takes the longest 'suffix' by predicate.
    takeEnd :: (e -> Bool) -> l -> l
    takeEnd p es = keep (suffix p es) es

    -- | Drops the longest 'prefix' by predicate.
    dropWhile :: (e -> Bool) -> l -> l
    dropWhile p es = drop (prefix p es) es

    -- | Drops the longest 'suffix' by predicate.
    dropEnd :: (e -> Bool) -> l -> l
    dropEnd p es = sans (suffix p es) es

    {- Pad/remove/replace. -}

    {- |
      @since 0.3

      @'padL' n e es@ returns n-element line. Prepend missing elements to @es@ if
      @sizeOf es < n@, otherwise 'keep' @n@.
    -}
    padL :: Int -> e -> l -> l
    padL n e = keep n . (replicate n e ++)

    {- |
      @since 0.3

      @'padR' n e es@ returns n-element line. Append missing elements to @es@ if
      @sizeOf es < n@, otherwise 'take' @n@.
    -}
    padR :: Int -> e -> l -> l
    padR n e = take n . (++ replicate n e)

    {- |
      @replaceBy sub new line@ replace every non-overlapping occurrence of @sub@
      in @line@ with @new@.

      > replaceBy "foo" "bar" "foobafoorbaz" == "barbabarrbaz"
    -}
    replaceBy :: Eq e => l -> l -> l -> l
    replaceBy sub new = intercalate new . splitsOn sub

    {- |
      Removes every non-overlapping occurrence of @sub@ with 'Z'.

      > removeAll = concat ... splitsOn
      > (`replaceBy` Z) = removeAll
    -}
    removeAll :: Eq e => l -> l -> l
    removeAll =  concat ... splitsOn

    {- Selections. -}

    -- | @select' f es@ is selective map of @es@ elements to new line.
    select' :: (e -> Maybe e) -> l -> l
    select' =  fromList ... select

    {- |
      @extract' f es@ returns a selective map of @es@ elements to new line and
      the remaining elements of the line.
    -}
    extract' :: (e -> Maybe e) -> l -> (l, l)
    extract' =  both fromList ... extract

    {- |
      @extractWhile f es@ selects results of applying @f@ to @es@ (left to
      right) untill first fail. Returns selected results and rest of line.
    -}
    extractWhile :: (e -> Maybe a) -> l -> ([a], l)
    extractWhile f es =
      let go e xs = case f e of {Just x -> x : xs; _ -> []}
      in  second (`drop` es) . swap $ csfoldr' go [] es

    {- |
      @extractEnd f es@ selects results of applying @f@ to @es@ (right to left)
      untill first fail. Returns rest of line and selected results.
    -}
    extractEnd :: (e -> Maybe a) -> l -> (l, [a])
    extractEnd f es =
      let go xs e = case f e of {Just x -> x : xs; _ -> []}
      in  bimap (`sans` es) reverse $ csfoldl' go [] es

    {- |
      @since 0.3

      Monadic version of 'select''.
    -}
    mselect' :: Applicative t => (e -> t (Maybe e)) -> l -> t l
    mselect' =  fmap fromList ... mselect

    {- |
      @since 0.3

      Monadic version of 'extract''.
    -}
    mextract' :: Applicative t => (e -> t (Maybe e)) -> l -> t (l, l)
    mextract' =  fmap (both fromList) ... mextract

    {- Operations with elements. -}

    {- |
      @since 0.2.1

      @'remove' es i@ delete element with offset @i@ from @es@.

      > remove (-1) [0 .. 5] == [0 .. 5]
      > remove   6  [0 .. 5] == [0 .. 5]
      > remove   0  [0 .. 5] == [1,2,3,4,5]
      > remove   3  [0 .. 5] == [0,1,2,4,5]
      > remove   5  [0 .. 5] == [0,1,2,3,4]
    -}
    remove :: Int -> l -> l
    remove n = fromList . remove n . listL

--------------------------------------------------------------------------------

-- | 'Linear' contraint for @(Type -> Type)@-kind types.
type Linear1 l e = Linear (l e) e

-- | 'Linear' contraint for @(Type -> Type -> Type)@-kind types.
type Linear2 l i e = Linear (l i e) e

#ifdef SDP_QUALIFIED_CONSTRAINTS
-- | 'Linear' contraint for @(Type -> Type)@-kind types.
type Linear' l = forall e . Linear (l e) e

-- | 'Linear' contraint for @(Type -> Type -> Type)@-kind types.
type Linear'' l = forall i e . Linear (l i e) e
#endif

--------------------------------------------------------------------------------

{- |
  @save n es@ takes first @n@ elements of @es@ if @n > 0@ and last @-n@
  elements otherwise.
-}
save :: Linear l e => Int -> l -> l
save n = n > 0 ? take n $ keep (-n)

{- |
  @skip n es@ drops first @n@ elements of @es@ if @n > 0@ and last @-n@
  elements otherwise.
-}
skip :: Linear l e => Int -> l -> l
skip n = n > 0 ? drop n $ sans (-n)

{- |
  @since 0.3

  Splits structures into parts by given offsets.

  @
    parts [0,5,6,12,26] ['a'..'z'] = ["","abcde","f","ghijkl","mnopqrstuvwxyz",""]
    -- if previous offset is equal or greater, subline is empty and next
    -- begins from previous:
    parts [0, 5, 4, 12, 26] ['a' .. 'z'] = ["","abcde","","fghijklm","nopqrstuvwxyz",""]
  @
-}
parts :: (Linear l e, Foldable f) => f Int -> l -> [l]
parts =
  let go o is' = case is' of {i : is -> (i - o) : go i is; _ -> []}
  in  splits . go 0 . toList

{- |
  @since 0.3

  Splits structures into chunks of size @n@ and the rest.

  > chunks x [] = [] -- forall x
  > chunks 0 es = [] -- forall es

  > chunks 3 [1 .. 10] == [[1,2,3],[4,5,6],[7,8,9],[10]]
-}
chunks :: Linear l e => Int -> l -> [l]
chunks n es = isNull es || n < 1 ? [] $ let (x, xs) = split n es in x : chunks n xs

--------------------------------------------------------------------------------

-- | Left-side span.
spanl :: Linear l e => (e -> Bool) -> l -> (l, l)
spanl p es = (takeWhile p es, dropWhile p es)

-- | Left-side break.
breakl :: Linear l e => (e -> Bool) -> l -> (l, l)
breakl =  spanl . (not .)

-- | Right-side span.
spanr :: Linear l e => (e -> Bool) -> l -> (l, l)
spanr p es = (dropEnd p es, takeEnd p es)

-- | Right-side break.
breakr :: Linear l e => (e -> Bool) -> l -> (l, l)
breakr =  spanr . (not .)

{- |
  @since 0.3

  @dropWhileEnd f = dropWhile f . dropEnd f@.
-}
dropWhileEnd :: Linear l e => (e -> Bool) -> l -> l
dropWhileEnd f = dropWhile f . dropEnd f

--------------------------------------------------------------------------------

-- | @'except' p es = 'filter' (not . p) es@
except :: Linear l e => (e -> Bool) -> l -> l
except =  filter . (not .)

{- |
  @since 0.3

  Monadic version 'except'.
-}
mexcept :: (Monad m, Linear l e) => (e -> m Bool) -> l -> m l
mexcept =  mfilter . (fmap not .)

--------------------------------------------------------------------------------

-- | Generalization of partition, that select sublines by predicates.
partitions :: (Linear l e, Foldable f) => f (e -> Bool) -> l -> [l]
partitions ps es =
  let f = \ es' -> case es' of
        (x : xs) -> (\ (y, ys) -> (ys : y : xs)) . (`partition` x)
        _        -> unreachEx "partitions"
  in  L.reverse $ foldl f [es] ps

-- | Generalized 'intersperse'.
intersperse :: Linear l e => e -> l -> l
intersperse e es =
  let xs = drop 1 $ sfoldr ((e :) ... (:)) [] es
  in  es .< 2 ? es $ fromList xs

-- | Generalized 'subsequences'.
subsequences :: Linear l e => l -> [l]
subsequences =  map fromList . L.subsequences . listL

--------------------------------------------------------------------------------

-- | 'tails' returns sequence of @es@ 'tail'.
tails :: Linear l e => l -> [l]
tails Z  = [Z]
tails es = es : tails (tail es)

-- | 'inits' returns sequence of @es@  'init'.
inits :: Linear l e => l -> [l]
inits Z  = [Z]
inits es = es : inits (init es)

--------------------------------------------------------------------------------

{- |
  Split line by first (left) separation element. If there is no such
  element, @splitBy es = (es, Z)@.

  > splitBy (== '.') "foo" == ("foo","")
  > splitBy (== '.') "foo." == ("foo","")
  > splitBy (== '.') ".foo" == ("","foo")
  > splitBy (== '.') "foo.bar" == ("foo","bar")
  > splitBy (== '.') "foo.bar.baz" == ("foo","bar.baz")
-}
splitBy :: Linear l e => (e -> Bool) -> l -> (l, l)
splitBy =  second (drop 1) ... breakl

{- |
  Split line by last (right) separation element. If there is no such
  element, @divide es = (Z, es)@.

  > divideBy (== '.') "foo" == ("","foo")
  > divideBy (== '.') ".foo" == ("","foo")
  > divideBy (== '.') "foo." == ("foo","")
  > divideBy (== '.') "foo.bar" == ("foo","bar")
  > divideBy (== '.') "foo.bar.baz" == ("foo.bar","baz")
-}
divideBy :: Linear l e => (e -> Bool) -> l -> (l, l)
divideBy =  first (sans 1) ... breakr

--------------------------------------------------------------------------------

{- |
  Splits line into sequences of given sizes (left to right).

  > splits [5, 3, 12] ['a'..'z'] = ["abcde","fgh","ijklmnopqrst","uvwxyz"]
-}
splits :: (Linear l e, Foldable f) => f Int -> l -> [l]
splits ns es =
  let f = \ es' n -> case es' of
        (r : ds) -> let (d, r') = split n r in r' : d : ds
        _        -> unreachEx "splits: must be non-empty"
  in  reverse $ foldl f [es] ns

{- |
  Splits line into sequences of given sizes (right to left).

  > divides [5,3,12] ['a'..'z'] == ["abcdef","ghijk","lmn","opqrstuvwxyz"]
-}
divides :: (Linear l e, Foldable f) => f Int -> l -> [l]
divides ns es =
  let f = \ n es' -> case es' of
        (r : ds) -> let (r', d) = divide n r in r' : d : ds
        _        -> unreachEx "divides: must be non-empty"
  in  foldr f [es] ns

--------------------------------------------------------------------------------

-- | @selectWhile'@ is 'selectWhile' version for generalized structures.
selectWhile' :: Linear l e => (e -> Maybe e) -> l -> l
selectWhile' =  fromList ... selectWhile

-- | @selectEnd'@ is 'selectEnd' version for generalized structures.
selectEnd' :: Linear l e => (e -> Maybe e) -> l -> l
selectEnd' =  fromList ... selectEnd

-- | @extractWhile'@ is 'extractWhile' version for generalized structures.
extractWhile' :: Linear l e => (e -> Maybe e) -> l -> (l, l)
extractWhile' =  first fromList ... extractWhile

-- | @extractEnd'@ is 'extractEnd' version for generalized structures.
extractEnd' :: Linear l e => (e -> Maybe e) -> l -> (l, l)
extractEnd' =  second fromList ... extractEnd

--------------------------------------------------------------------------------

-- | @stripPrefix sub line@ strips prefix @sub@ of @line@ (if any).
stripPrefix :: (Linear l e, Eq e) => l -> l -> l
stripPrefix sub line = sub `isPrefixOf` line ? drop (sizeOf sub) line $ line

-- | @stripSuffix sub line@ strips suffix @sub@ of @line@ (if any).
stripSuffix :: (Linear l e, Eq e) => l -> l -> l
stripSuffix sub line = sub `isSuffixOf` line ? sans (sizeOf sub) line $ line

-- | @stripPrefix' sub line@ strips prefix @sub@ of @line@ or returns 'Nothing'.
stripPrefix' :: (Linear l e, Eq e) => l -> l -> Maybe l
stripPrefix' sub = isPrefixOf sub ?+ drop (sizeOf sub)

-- | @stripSuffix sub line@ strips suffix @sub@ of @line@ or returns 'Nothing'.
stripSuffix' :: (Linear l e, Eq e) => l -> l -> Maybe l
stripSuffix' sub = isSuffixOf sub ?+ sans (sizeOf sub)

--------------------------------------------------------------------------------

{- |
  @each n es@ returns each @n@-th element of structure.

  @
    each n [1 .. 5] = []
    each 1 [1 .. 5] = [1 .. 5]
    each 2 [1 .. 5] = [1, 3, 5]
  @

  If @n == 1@, returns @es@.
  If @n < 1@, returns 'Z'.
-}
each :: Linear l e => Int -> l -> l
each n es = case n <=> 1 of
  GT -> fromList $ ofoldr (\ i x xs -> mod i n == 0 ? x : xs $ xs) [] es
  EQ -> es
  LT -> Z

{- |
  @eachFrom o n es@ returns each nth element of structure, beginning from o.

  @
    eachFrom o n = each n . drop o
    eachFrom 0 2 [1 .. 20] == [2, 4 .. 20]
    eachFrom 1 2 [1 .. 20] == [3, 5 .. 19]
  @
-}
eachFrom :: Linear l e => Int -> Int -> l -> l
eachFrom o n = each n . drop o

{- |
  @combo f es@ returns the length of the @es@ subsequence (left to tight)
  whose elements are in order @f@.

  > combo (<) [] == 0
  > combo (<) [1] == 1
  > combo (<) [7, 4, 12] == 1
  > combo (<) [1, 7, 3, 12] == 2
-}
combo :: Linear l e => Equal e -> l -> Int
combo f = go . listL
  where
    go [ ] = 0
    go [_] = 1
    go (e1 : e2 : es) = f e1 e2 ? go' 2 e2 es $ 1
      where
        go' !i p (x : xs) = f p x ? go' (i + 1) x xs $ i
        go'  i _    _     = i

-- | intercalate is generalization of intercalate
intercalate :: (Foldable f, Linear l e) => l -> f l -> l
intercalate e es =
  let xs = 1 `drop` foldr ((e :) ... (:)) [] es
  in  concat (xs .< 2 ? [] $ xs)

{- |
  @ascending es lengths@ checks if the subsequences of @es@ of lengths @lengths@
  is sorted.
-}
ascending :: (Sort l e, Linear l e, Ord e) => l -> [Int] -> Bool
ascending =  all sorted ... flip splits

--------------------------------------------------------------------------------

instance Linear [e] e
  where
    fromListN = L.take
    replicate = L.replicate

    filter     = L.filter
    concat     = L.concat
    unfoldr    = L.unfoldr
    concatMap  = L.concatMap
    partition  = L.partition

    iterate n f e = n < 1 ? [] $ e : iterate (n - 1) f (f e)

    remove i es = i < 0 ? es $ go i es
      where
        go 0 (_ : xs) = xs
        go n (x : xs) = x : go (n - 1) xs
        go _    []    = []

    take  = L.take
    drop  = L.drop
    split = L.splitAt

    splitsBy f es = dropWhile f <$> L.findIndices f es `parts` es

--------------------------------------------------------------------------------

{-# NOINLINE unreachEx #-}
unreachEx :: String -> a
unreachEx =  throw . UnreachableException . showString "in SDP.Linear."

