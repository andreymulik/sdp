{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns, BangPatterns, DefaultSignatures #-}
{-# LANGUAGE Trustworthy, CPP, TypeFamilies, ConstraintKinds #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.Linear
    Copyright   :  (c) Andrey Mulik 2019-2021
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Linear" is a module that provides several convenient interfaces for
    working with various linear data structures.
-}
module SDP.Linear
(
  -- * Exports
  module SDP.Nullable,
  module SDP.Index,
  module SDP.Sort,
  module SDP.Zip,
  
  -- * Bordered class
  Bordered (..), Bordered1, Bordered2,
  
  -- * Linear class
  Linear (..), Linear1, Linear2, pattern (:>), pattern (:<),
  
#if __GLASGOW_HASKELL__ >= 806
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  Bordered', Bordered'', Linear', Linear'',
#endif
  
  -- ** Split class
  Split (..), Split1,
  
  -- * Related functions
  stripPrefix, stripSuffix, stripPrefix', stripSuffix',
  intercalate, tails, inits, ascending
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Nullable
import SDP.Index
import SDP.Sort
import SDP.Zip

import qualified Data.List as L

import Control.Exception.SDP

default ()

infix  8 `filter`, `except`
infixr 5 :>, ++
infixl 5 :<
infixl 9 !^

--------------------------------------------------------------------------------

-- | Class of bordered data structures.
class (Index i, Estimate b) => Bordered b i | b -> i
  where
    {-# MINIMAL (bounds|(lower, upper)) #-}
    
    {-# INLINE bounds #-}
    {- |
      Returns the exact 'upper' and 'lower' bounds of given structure. If the
      structure doesn't have explicitly defined boundaries (list, for example),
      use the @'defaultBounds' . 'sizeOf'@.
    -}
    bounds :: b -> (i, i)
    bounds es = (lower es, upper es)
    
    {-# INLINE lower #-}
    -- | Returns lower bound of structure
    lower :: b -> i
    lower =  fst . bounds
    
    {-# INLINE upper #-}
    -- | Returns upper bound of structure
    upper :: b -> i
    upper =  snd . bounds
    
    {-# INLINE sizeOf #-}
    -- | Returns actual size of structure.
    sizeOf :: b -> Int
    sizeOf =  size . bounds
    
    -- | Returns actual sizes of structure.
    sizesOf :: b -> [Int]
    sizesOf =  sizes . bounds
    
    {-# INLINE indexIn #-}
    -- | Checks if an index falls within the boundaries of the structure.
    indexIn :: b -> i -> Bool
    indexIn =  inRange . bounds
    
    {-# INLINE indices #-}
    -- | Returns index range list.
    indices :: b -> [i]
    indices =  range . bounds
    
    {-# INLINE indexOf #-}
    -- | Returns index by offset in structure.
    indexOf :: b -> Int -> i
    indexOf =  index . bounds
    
    {-# INLINE offsetOf #-}
    -- | Returns index offset in structure bounds.
    offsetOf :: b -> i -> Int
    offsetOf =  offset . bounds

--------------------------------------------------------------------------------

instance (Index i) => Bordered (i, i) i
  where
    bounds = id
    lower  = fst
    upper  = snd
    
    indices = range
    indexIn = inRange
    
    sizeOf   = size
    indexOf  = index
    offsetOf = offset

instance Bordered [e] Int
  where
    sizeOf = length
    lower  = const 0
    
    upper es = length es - 1

--------------------------------------------------------------------------------

{-# RULES
  "select/Just"  select  Just = listL;
  "select'/Just" select' Just = id;
  #-}

{- |
  Class of list-like data structures, which
  
  * can be converted to and from list
  * can be created from singleton or 'Foldable' stream
  * support filter operations, separation, concatenation and selection
  * It can be represented as 'head', 'tail', 'init' and 'last' elements and
  constructed from 'head' and 'tail' or 'init' and 'last'.
-}
class (Nullable l) => Linear l e | l -> e
  where
    {-# MINIMAL (listL|listR), (fromList|fromFoldable), (head,tail|uncons), (init,last|unsnoc) #-}
    
    -- | Separates line to 'head' and 'tail', deconstructor for ':>' pattern.
    uncons :: l -> (e, l)
    uncons xs = (head xs, tail xs)
    
    -- | Same as @'isNull' '?-' 'uncons'@
    uncons' :: l -> Maybe (e, l)
    uncons' =  isNull ?- uncons
    
    -- | Prepends element to line, constructor for ':>' pattern.
    toHead :: e -> l -> l
    toHead =  (++) . single
    
    -- | Returns first element of line, may fail.
    head :: l -> e
    head =  fst . uncons
    
    -- | Returns line except first, may fail.
    tail :: l -> l
    tail =  snd . uncons
    
    -- | Separates line to 'init' and 'last', deconstructor for ':<' pattern.
    unsnoc :: l -> (l, e)
    unsnoc xs = (init xs, last xs)
    
    -- | Same as @'isNull' '?-' 'unsnoc'@
    unsnoc' :: l -> Maybe (l, e)
    unsnoc' =  isNull ?- unsnoc
    
    -- | Appends element to line, constructor for ':<' pattern.
    toLast :: l -> e -> l
    toLast es =  (es ++) . single
    
    -- | Returns line except 'last' element, may fail.
    init :: l -> l
    init =  fst . unsnoc
    
    -- | Returns last element, may fail.
    last :: l -> e
    last =  snd . unsnoc
    
    -- | Just singleton.
    single :: e -> l
    single =  fromList . pure
    
    -- | Concatenation of two lines.
    (++) :: l -> l -> l
    (++) =  fromList ... on (++) listL
    
    -- | @replicate n e@ returns a line of @n@ repetitions of the element @e@.
    replicate :: Int -> e -> l
    replicate n = fromListN n . replicate n
    
    -- | Creates line from list.
    fromList :: [e] -> l
    fromList =  fromFoldable
    
    -- | Create finite line from (possibly infinite) list.
    fromListN :: Int -> [e] -> l
    fromListN =  fromList ... take
    
    -- | Right to left view of line.
    listR :: l -> [e]
    listR =  listL . reverse
    
    -- | Left to right view of line, same to 'toList'.
    listL :: l -> [e]
    listL =  reverse . listR
    
    -- | Generalized 'fromList'.
    fromFoldable :: (Foldable f) => f e -> l
    fromFoldable =  fromList . toList
    
    {- |
      Returns the element of a sequence by offset, may be completely unsafe.
      This is an optimistic read function and shouldn't perform checks for
      efficiency reasons.
      
      If you need safety, use (!) or (!?). The generalization of this function
      by index type (.!).
      
      > es !^ i = listL es !! i
    -}
    (!^) :: l -> Int -> e
    (!^) =  (L.!!) . listL
    
    {- |
      @write es n e@ writes value @e@ in position @n@ (offset), returns new
      structure. If @n@ is out of range, returns equal structure (@es@ or copy).
    -}
    write :: l -> Int -> e -> l
    write es = fromList ... write (listL es)
    
    -- | Generalized concat.
    concat :: (Foldable f) => f l -> l
    concat =  foldr (++) Z
    
    -- | Generalized concatMap.
    concatMap :: (Foldable f) => (a -> l) -> f a -> l
    concatMap f = concat . foldr ((:) . f) []
    
    -- | Generalized intersperse.
    intersperse :: e -> l -> l
    intersperse e = fromList . intersperse e . listL
    
    -- | Generalized filter.
    filter :: (e -> Bool) -> l -> l
    filter p = fromList . filter p . listL
    
    -- | Inverted filter.
    except :: (e -> Bool) -> l -> l
    except p = filter (not . p)
    
    -- | Generalization of partition.
    partition :: (e -> Bool) -> l -> (l, l)
    partition p es = (filter p es, except p es)
    
    -- | Generalization of partition, that select sublines by predicates.
    partitions :: (Foldable f) => f (e -> Bool) -> l -> [l]
    partitions ps es =
      let f = \ (x : xs) -> (\ (y, ys) -> (ys : y : xs)) . (`partition` x)
      in  reverse $ foldl f [es] ps
    
    -- | @select f es@ is selective map of @es@ elements to new list.
    select :: (e -> Maybe a) -> l -> [a]
    select f = foldr (\ x es -> case f x of {(Just e) -> e : es; _ -> es}) [] . listL
    
    -- | @select' f es@ is selective map of @es@ elements to new line.
    select' :: (t e ~ l, Linear1 t a) => (e -> Maybe a) -> l -> t a
    select' =  fromList ... select
    
    {- |
      @extract f es@ returns a selective map of @es@ elements to new list and
      the remaining elements of the line.
    -}
    extract :: (e -> Maybe a) -> l -> ([a], l)
    extract f =
      let g = \ b -> second (b :) `maybe` (first . (:)) $ f b
      in  fmap fromList . foldr g ([], []) . listL
    
    {- |
      @extract' f es@ returns a selective map of @es@ elements to new line and
      the remaining elements of the line.
    -}
    extract' :: (t e ~ l, Linear1 t a) => (e -> Maybe a) -> l -> (t a, l)
    extract' =  first fromList ... extract
    
    {- |
      @selects fs es@ sequentially applies the functions from @fs@ to the
      remainder of @es@, returns a list of selections and the remainder of the
      last selection.
    -}
    selects :: (Foldable f) => f (e -> Maybe a) -> l -> ([[a]], l)
    selects fs es =
      let g = \ as -> first (: as) ... flip extract
      in  foldl (uncurry g) ([], es) fs
    
    {- |
      @selects' fs es@ sequentially applies the functions from @fs@ to the
      remainder of @es@, returns a line of selections and the remainder of the
      last selection.
    -}
    selects' :: (Foldable f, t e ~ l, Linear1 t a) => f (e -> Maybe a) -> l -> ([t a], l)
    selects' =  first (map fromList) ... selects
    
    {- |
      The @isSubseqOf xs ys@ checks if all the elements of the @xs@ occur,
      in order, in the @ys@. The elements don't have to occur consecutively.
    -}
    isSubseqOf :: (Eq e) => l -> l -> Bool
    isSubseqOf =  isSubseqOf `on` listL
    
    -- | Generalized reverse.
    reverse :: l -> l
    reverse =  fromList . listR
    
    -- | O(1) 'force', same as 'id'.
    force :: l -> l
    force =  fromList . listL
    
    {- |
      @since 0.2.1
      
      @'before' es i e@ insert @e@ to @es@ before element with offset @i@. If
      @i@ goes beyond the lower or upper bounds, @e@ is prepended or appended to
      @es@ respectively.
      
      > before [0 .. 5] (-1) 7 == [7,0,1,2,3,4,5]
      > before [0 .. 5]   0  7 == [7,0,1,2,3,4,5]
      > before [0 .. 5]   3  7 == [0,1,2,7,3,4,5]
      > before [0 .. 5]   5  7 == [0,1,2,3,4,7,5]
      > before [0 .. 5]  19  7 == [0,1,2,3,4,5,7]
    -}
    before :: l -> Int -> e -> l
    before es = fromList ... before (listL es)
    
    {- |
      @since 0.2.1
      
      @'after' es i e@ insert @e@ to @es@ after element with offset @i@.
      
      > after es i e == before es (i + 1) e
    -}
    after :: l -> Int -> e -> l
    after es i e = before es (i + 1) e
    
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
    
    -- | Generalized 'subsequences'.
    subsequences :: l -> [l]
    subsequences =  (Z :) . go
      where
        go (x :> xs) = single x : foldr (\ ys r -> ys : (x :> ys) : r) [] (go xs)
        go     _     = Z
    
    {- |
      @iterate n f x@ returns sequence of @n@ applications of @f@ to @x@.
      
      Note that @iterate@ returns finite sequence, instead "Prelude" prototype.
    -}
    iterate :: Int -> (e -> e) -> e -> l
    iterate n = fromListN n ... iterate n
    
    -- | Same as @nubBy ('==')@.
    nub :: (Eq e) => l -> l
    nub =  nubBy (==)
    
    -- | Generalization of nubBy.
    nubBy :: Equal e -> l -> l
    nubBy f = fromList . nubBy f . listL
    
    {- Folds with offset. -}
    
    -- | 'ofoldr' is right fold with offset.
    ofoldr :: (Int -> e -> b -> b) -> b -> l -> b
    ofoldr f base = ofoldr f base . listL
    
    -- | 'ofoldl' is left fold with offset.
    ofoldl :: (Int -> b -> e -> b) -> b -> l -> b
    ofoldl f base = ofoldl f base . listL
    
    -- | 'ofoldr'' is strict version of 'ofoldr'.
    ofoldr' :: (Int -> e -> b -> b) -> b -> l -> b
    ofoldr' f = ofoldr (\ !i e !b -> f i e b)
    
    -- | 'ofoldl'' is strict version of 'ofoldl'.
    ofoldl' :: (Int -> b -> e -> b) -> b -> l -> b
    ofoldl' f = ofoldl (\ !i !b e -> f i b e)
    
    {- 'Foldable' crutches. -}
    
    -- | 'o_foldr' is just 'foldr' in 'Linear' context.
    o_foldr :: (e -> b -> b) -> b -> l -> b
    o_foldr =  ofoldr . const
    
    -- | 'o_foldl' is just 'foldl' in 'Linear' context.
    o_foldl :: (b -> e -> b) -> b -> l -> b
    o_foldl =  ofoldl . const
    
    -- | 'o_foldr'' is just 'foldr'' in 'Linear' context.
    o_foldr' :: (e -> b -> b) -> b -> l -> b
    o_foldr' =  ofoldr' . const
    
    -- | 'o_foldl'' is just 'foldl'' in 'Linear' context.
    o_foldl' :: (b -> e -> b) -> b -> l -> b
    o_foldl' =  ofoldl' . const
    
    {- |
      @since 0.2.1
      
      'o_foldr1' is just 'Data.Foldable.foldr1' in 'Linear' context.
    -}
    o_foldr1 :: (e -> e -> e) -> l -> e
    o_foldr1 f = \ (es :< e) -> o_foldr f e es
    
    {- |
      @since 0.2.1
      
      'o_foldl1' is just 'Data.Foldable.foldl1' in 'Linear' context.
    -}
    o_foldl1 :: (e -> e -> e) -> l -> e
    o_foldl1 f = \ (e :> es) -> o_foldl f e es
    
    {- |
      @since 0.2.1
      
      'o_foldr1'' is just strict 'Data.Foldable.foldr1' in 'Linear' context.
    -}
    o_foldr1' :: (e -> e -> e) -> l -> e
    o_foldr1' f = \ (es :< e) -> o_foldr' f e es
    
    {- |
      @since 0.2.1
      
      'o_foldl1'' is just 'Data.Foldable.foldl1'' in 'Linear' context.
    -}
    o_foldl1' :: (e -> e -> e) -> l -> e
    o_foldl1' f = \ (e :> es) -> o_foldl' f e es

--------------------------------------------------------------------------------

-- | Split - class of splittable data structures.
class (Linear s e) => Split s e | s -> e
  where
    {-# MINIMAL (take|sans), (drop|keep) #-}
    
    -- | @take n es@ takes first @n@ elements of @es@.
    take :: Int -> s -> s
    default take :: (Bordered s i) => Int -> s -> s
    take n es = sans (sizeOf es - n) es
    
    -- | @drop n es@ drops first @n@ elements of @es@.
    drop :: Int -> s -> s
    default drop :: (Bordered s i) => Int -> s -> s
    drop n es = keep (sizeOf es - n) es
    
    -- | @keep n es@ takes last @n@ elements of @es@.
    keep :: Int -> s -> s
    default keep :: (Bordered s i) => Int -> s -> s
    keep n es = drop (sizeOf es - n) es
    
    -- | @sans n es@ drops last @n@ elements of @es@.
    sans :: Int -> s -> s
    default sans :: (Bordered s i) => Int -> s -> s
    sans n es = take (sizeOf es - n) es
    
    {- |
      @save n es@ takes first @n@ elements of @es@ if @n > 0@ and last @-n@
      elements otherwise.
    -}
    save :: Int -> s -> s
    save n = n > 0 ? take n $ keep (-n)
    
    {- |
      @skip n es@ drops first @n@ elements of @es@ if @n > 0@ and last @-n@
      elements otherwise.
    -}
    skip :: Int -> s -> s
    skip n = n > 0 ? drop n $ sans (-n)
    
    -- | @split n es@ is same to @(take n es, drop n es)@.
    split :: Int -> s -> (s, s)
    split n es = (take n es, drop n es)
    
    -- | @divide n es@ is same to @(sans n es, keep n es)@.
    divide :: Int -> s -> (s, s)
    divide n es = (sans n es, keep n es)
    
    {- |
      Splits line into sequences of given sizes (left to right).
      
      > splits [5, 3, 12] ['a'..'z'] = ["abcde","fgh","ijklmnopqrst","uvwxyz"]
    -}
    splits :: (Foldable f) => f Int -> s -> [s]
    splits ns es =
      let f = \ (r : ds) n -> let (d, r') = split n r in r' : d : ds
      in  reverse $ foldl f [es] ns
    
    {- |
      Splits line into sequences of given sizes (right to left).
      
      > divides [5,3,12] ['a'..'z'] == ["abcdef","ghijk","lmn","opqrstuvwxyz"]
    -}
    divides :: (Foldable f) => f Int -> s -> [s]
    divides ns es =
      let f = \ n (r : ds) -> let (r', d) = divide n r in r' : d : ds
      in  foldr f [es] ns
    
    {- |
      Splits structures into parts by given offsets.
      
      > parts [0,5,6,12,26] ['a'..'z'] = ["","abcde","f","ghijkl","mnopqrstuvwxyz",""]
      > -- if previous offset is equal or greater, subline is empty and next
      > begins from previous:
      > parts [0, 5, 4, 12, 26] ['a' .. 'z'] = ["","abcde","","fghijklm","nopqrstuvwxyz",""]
    -}
    parts :: (Foldable f) => f Int -> s -> [s]
    parts =  splits . go . toList where go is = zipWith (-) is (0 : is)
    
    {- |
      Splits structures into chunks of size @n@ and the rest.
      
      > chunks x [] = [] -- forall x
      > chunks 0 es = [] -- forall es
      
      > chunks 3 [1 .. 10] == [[1,2,3],[4,5,6],[7,8,9],[10]]
    -}
    chunks :: Int -> s -> [s]
    chunks _  Z = []
    chunks n es = n < 1 ? [] $ let (x, xs) = split n es in x : chunks n xs
    
    {- |
      Split line by first (left) separation element. If there is no such
      element, @splitBy es = (es, Z)@.
      
      > splitBy (== '.') "foo" == ("foo","")
      > splitBy (== '.') "foo." == ("foo","")
      > splitBy (== '.') ".foo" == ("","foo")
      > splitBy (== '.') "foo.bar" == ("foo","bar")
      > splitBy (== '.') "foo.bar.baz" == ("foo","bar.baz")
    -}
    splitBy :: (e -> Bool) -> s -> (s, s)
    splitBy f = bimap fromList fromList . splitBy f . listL
    
    {- |
      Split line by last (right) separation element. If there is no such
      element, @divide es = (Z, es)@.
      
      > divideBy (== '.') "foo" == ("","foo")
      > divideBy (== '.') ".foo" == ("","foo")
      > divideBy (== '.') "foo." == ("foo","")
      > divideBy (== '.') "foo.bar" == ("foo","bar")
      > divideBy (== '.') "foo.bar.baz" == ("foo.bar","baz")
    -}
    divideBy :: (e -> Bool) -> s -> (s, s)
    divideBy f = bimap fromList fromList . divideBy f . listL
    
    -- | Splits line by separation elements.
    splitsBy :: (e -> Bool) -> s -> [s]
    splitsBy e = map fromList . splitsBy e . listL
    
    {- |
      @splitsOn sub line@ splits @line@ by @sub@.
      
      > splitsOn "fo" "foobar bazfoobar1" == ["","obar baz","obar1"]
    -}
    splitsOn :: (Eq e) => s -> s -> [s]
#if __GLASGOW_HASKELL__ >= 820
    default splitsOn :: (Eq e, Bordered s i) => s -> s -> [s]
    splitsOn sub line = drop (sizeOf sub) <$> parts (infixes sub line) line
    -- ghc-8.0.1 has bug in default signatures, so this can be used with it
#else
    {-
      Not tested, but should be significantly slower than the definitions below.
      If you plan to support ghc-8.0.1, override splitsOn in all your instances.
    -}
    splitsOn sub line = fromList <$> splitsOn (listL sub) (listL line)
#endif
    
    {- |
      @replaceBy sub new line@ replace every non-overlapping occurrence of @sub@
      in @line@ with @new@.
      
      > replaceBy "foo" "bar" "foobafoorbaz" == "barbabarrbaz"
    -}
    replaceBy :: (Eq e) => s -> s -> s -> s
    replaceBy sub new = intercalate new . splitsOn sub
    
    {- |
      Removes every non-overlapping occurrence of @sub@ with 'Z'.
      
      > removeAll = concat ... splitsOn
      > (`replaceBy` Z) = removeAll
    -}
    removeAll :: (Eq e) => s -> s -> s
    removeAll =  concat ... splitsOn
    
    {- |
      @combo f es@ returns the length of the @es@ subsequence (left to tight)
      whose elements are in order @f@.
      
      > combo (<) [] == 0
      > combo (<) [1] == 1
      > combo (<) [7, 4, 12] == 1
      > combo (<) [1, 7, 3, 12] == 2
    -}
    combo :: Equal e -> s -> Int
    combo f = combo f . listL
    
    {- |
      @justifyL n e es@ appends @e@ elements if the @es@ is shorter than @n@,
      @justifyL n e es@ prepends @(n - sizeOf es)@ elements @e@ to @es@ from the
      takes @n@ elements if longer.
      left side if @(sizeOf es < n)@. Otherwise returns the first @n@ elements
      of @es@, like @'take' n es@ do.
    -}
    justifyL :: Int -> e -> s -> s
    justifyL n e = take n . (++ replicate n e)
    
    {- |
      @justifyR n e es@ appends @(n - sizeOf es)@ elements @e@ to @es@ from the
      right side if @(sizeOf es < n)@. Otherwise returns the first @n@ elements
      of @es@, like @'keep' n es@ do.
    -}
    justifyR :: Int -> e -> s -> s
    justifyR n e = keep n . (replicate n e ++)
    
    {- |
      @each n es@ returns each @n@-th element of structure.
      If @n == 1@, returns @es@.
      If @n < 1@, returns 'Z'.
    -}
    each :: Int -> s -> s
    each n = fromList . each n . listL
    
    {- |
      @eachFrom o n es@ returns each nth element of structure, beginning from o.
      
      > eachFrom o n = each n . drop o
      
      > eachFrom 0 2 [1 .. 20] == [2, 4 .. 20]
      > eachFrom 1 2 [1 .. 20] == [3, 5 .. 19]
    -}
    eachFrom :: Int -> Int -> s -> s
    eachFrom o n = each n . drop o
    
    -- | @sub `'isPrefixOf'` es@ checks if @sub@ is beginning of @es@.
    isPrefixOf :: (Eq e) => s -> s -> Bool
    isPrefixOf (x :> xs) (y :> ys) = x == y && xs `isPrefixOf` ys
    isPrefixOf xs               ys = isNull xs && isNull ys
    
    -- | @sub `'isSuffixOf'` es@ checks if @sub@ is ending of @es@.
    isSuffixOf :: (Eq e) => s -> s -> Bool
    isSuffixOf (xs :< x) (ys :< y) = x == y && xs `isSuffixOf` ys
    isSuffixOf xs               ys = isNull xs && isNull ys
    
    -- | isInfixOf checks whether the first line is the substring of the second
    isInfixOf  :: (Eq e) => s -> s -> Bool
    isInfixOf _   Z = False
    isInfixOf xs ys = xs `isPrefixOf` ys || xs `isInfixOf` tail ys
    
    -- | prefix gives length of init, satisfying preducate.
    prefix :: (e -> Bool) -> s -> Int
    prefix p = o_foldr' (\ e c -> p e ? succ c $ 0) 0
    
    -- | suffix gives length of tail, satisfying predicate.
    suffix :: (e -> Bool) -> s -> Int
    suffix p = o_foldl' (\ c e -> p e ? succ c $ 0) 0
    
    {- |
      @infixes inf es@ returns a list of @inf@ positions in @es@, without
      intersections.
      
      > "" `infixes` es = []
      > "abba" `infixes` "baababba" == [4]
      > "abab" `infixes` "baababab" == [2]
      > "aaaa" `infixes` "aaaaaaaa" == [0, 4]
    -}
    infixes :: (Eq e) => s -> s -> [Int]
    infixes =  on infixes listL
    
    -- | @dropSide f = dropWhile f . dropEnd f@.
    dropSide :: (e -> Bool) -> s -> s
    dropSide f = dropWhile f . dropEnd f
    
    -- | Takes the longest init by predicate.
    takeWhile :: (e -> Bool) -> s -> s
    takeWhile p es = take (prefix p es) es
    
    -- | Drops the longest init by predicate.
    dropWhile :: (e -> Bool) -> s -> s
    dropWhile p es = drop (prefix p es) es
    
    -- | Takes the longest suffix by predicate.
    takeEnd :: (e -> Bool) -> s -> s
    takeEnd p es = keep (suffix p es) es
    
    -- | Drops the longest prefix by predicate.
    dropEnd :: (e -> Bool) -> s -> s
    dropEnd p es = sans (suffix p es) es
    
    -- | Left-side span.
    spanl :: (e -> Bool) -> s -> (s, s)
    spanl p es = (takeWhile p es, dropWhile p es)
    
    -- | Left-side break.
    breakl :: (e -> Bool) -> s -> (s, s)
    breakl p es = (takeWhile (not . p) es, dropWhile (not . p) es)
    
    -- | Right-side span.
    spanr :: (e -> Bool) -> s -> (s, s)
    spanr p es = (dropEnd p es, takeEnd p es)
    
    -- | Right-side break.
    breakr :: (e -> Bool) -> s -> (s, s)
    breakr p es = (dropEnd (not . p) es, takeEnd (not . p) es)
    
    {- |
      @selectWhile f es@ selects results of applying @f@ to @es@ (left to right)
      untill first fail.
    -}
    selectWhile :: (e -> Maybe a) -> s -> [a]
    selectWhile f = selectWhile f . listL
    
    {- |
      @selectEnd f es@ selects results of applying @f@ to @es@ (right to left)
      untill first fail.
    -}
    selectEnd :: (e -> Maybe a) -> s -> [a]
    selectEnd f = selectEnd f . listL
    
    {- |
      @extractWhile f es@ selects results of applying @f@ to @es@ (left to
      right) untill first fail. Returns selected results and rest of line.
    -}
    extractWhile :: (e -> Maybe a) -> s -> ([a], s)
    extractWhile f es = let as = selectWhile f es in (as, length as `drop` es)
    
    {- |
      @extractEnd f es@ selects results of applying @f@ to @es@ (right to left)
      untill first fail. Returns rest of line and selected results.
    -}
    extractEnd :: (e -> Maybe a) -> s -> (s, [a])
    extractEnd f es = let as = selectEnd f es in (length as `sans` es, as)
    
    -- | @selectWhile'@ is 'selectWhile' version for generalized structures.
    selectWhile' :: (t e ~ l, Split1 t a) => (e -> Maybe a) -> s -> t a
    selectWhile' =  fromList ... selectWhile
    
    -- | @selectEnd'@ is 'selectEnd' version for generalized structures.
    selectEnd' :: (t e ~ l, Split1 t a) => (e -> Maybe a) -> s -> t a
    selectEnd' =  fromList ... selectEnd
    
    -- | @extractWhile'@ is 'extractWhile' version for generalized structures.
    extractWhile' :: (t e ~ l, Split1 t a) => (e -> Maybe a) -> s -> (t a, s)
    extractWhile' =  first fromList ... extractWhile
    
    -- | @extractEnd'@ is 'extractEnd' version for generalized structures.
    extractEnd' :: (t e ~ l, Split1 t a) => (e -> Maybe a) -> s -> (s, t a)
    extractEnd' =  second fromList ... extractEnd

--------------------------------------------------------------------------------

-- | Pattern @(':>')@ is left-size view of line. Same as 'uncons' and 'toHead'.
pattern  (:>)   :: (Linear l e) => e -> l -> l
pattern x :> xs <- (uncons' -> Just (x, xs)) where (:>) = toHead

-- | Pattern @(':<')@ is right-size view of line. Same as 'unsnoc' and 'toLast'.
pattern   (:<)  :: (Linear l e) => l -> e -> l
pattern xs :< x <- (unsnoc' -> Just (xs, x)) where (:<) = toLast

-- | 'Linear' contraint for @(Type -> Type)@-kind types.
type Linear1 l e = Linear (l e) e

-- | 'Linear' contraint for @(Type -> Type -> Type)@-kind types.
type Linear2 l i e = Linear (l i e) e

-- | 'Bordered' contraint for @(Type -> Type)@-kind types.
type Bordered1 l i e = Bordered (l e) i

-- | 'Bordered' contraint for @(Type -> Type -> Type)@-kind types.
type Bordered2 l i e = Bordered (l i e) i

-- | Kind @(Type -> Type)@ 'Split' structure.
type Split1 s e = Split (s e) e

#if __GLASGOW_HASKELL__ >= 806
-- | 'Linear' contraint for @(Type -> Type)@-kind types.
type Linear' l = forall e . Linear (l e) e

-- | 'Linear' contraint for @(Type -> Type -> Type)@-kind types.
type Linear'' l = forall i e . Linear (l i e) e

-- | 'Bordered' contraint for @(Type -> Type)@-kind types.
type Bordered' l i = forall e . Bordered (l e) i

-- | 'Bordered' contraint for @(Type -> Type -> Type)@-kind types.
type Bordered'' l = forall i e . Bordered (l i e) i
#endif

--------------------------------------------------------------------------------

{-# COMPLETE [], (:>) #-}
{-# COMPLETE [], (:<) #-}

instance Linear [e] e
  where
    toHead = (:)
    single = pure
    (++)   = (L.++)
    (!^)   = (L.!!)
    
    fromList     = id
    fromListN    = take
    fromFoldable = toList
    
    write es n e = n < 0 ? es $ go n es
      where
        go i (x : xs) = i == 0 ? e : xs $ x : go (i - 1) xs
        go _ _ = []
    
    toLast = flip (foldr' (:) . pure)
    listR  = L.reverse
    listL  = toList
    nubBy  = L.nubBy
    
    -- | O(1) force, same as 'id'.
    force = id
    
    uncons    []    = throw $ PatternMatchFail "in SDP.Linear.(:>)"
    uncons (e : es) = (e, es)
    
    unsnoc   [ ]    = throw $ PatternMatchFail "in SDP.Linear.(:<)"
    unsnoc   [e]    = ([], e)
    unsnoc (e : es) = let (es', e') = unsnoc es in (e : es', e')
    
    filter      = L.filter
    reverse     = L.reverse
    replicate   = L.replicate
    partition   = L.partition
    concatMap   = L.concatMap
    intersperse = L.intersperse
    isSubseqOf  = L.isSubsequenceOf
    
    before es i e = go (max 0 i) es
      where
        go 0    xs    = e : xs
        go n (x : xs) = x : go (n - 1) xs
        go _    []    = [e]
    
    remove i es = i < 0 ? es $ go i es
      where
        go 0 (_ : xs) = xs
        go n (x : xs) = x : go (n - 1) xs
        go _    []    = []
    
    iterate n f e = n < 1 ? [] $ e : iterate (n - 1) f (f e)
    
    ofoldr f base =
      let go !i es = case es of {(x : xs) -> f i x $ go (i + 1) xs; _ -> base}
      in  go 0
    
    ofoldl f =
      let go !i base es = case es of {(x : xs) -> go (i + 1) (f i base x) xs; _ -> base}
      in  go 0
    
    o_foldr' = foldr'
    o_foldl' = foldl'
    o_foldr  = foldr
    o_foldl  = foldl

instance Split [e] e
  where
    take  = L.take
    drop  = L.drop
    split = L.splitAt
    
    each n = case n <=> 1 of {LT -> const []; EQ -> id; GT -> go n}
      where
        go i (x : xs) = i == 1 ? x : go n xs $ go (i - 1) xs
        go _ _ = []
    
    infixes  Z  = const []
    infixes sub = go 0
      where
        go _ [] = []
        go i es = sub `isPrefixOf` es ? i : go (i + n) (drop n es) $ go (i + 1) (tail es)
        
        n = sizeOf sub
    
    combo _ [ ] = 0
    combo _ [_] = 1
    combo f (e1 : e2 : es) = e1 `f` e2 ? go 2 e2 es $ 1
      where
        go !i p (x : xs) = p `f` x ? go (i + 1) x xs $ i
        go  i _    _     = i
    
    splitBy  f es = let (as, bs) = breakl f es in isNull bs ? (es, []) $ (as, tail bs)
    divideBy f es = let (as, bs) = breakr f es in isNull as ? ([], es) $ (init as, bs)
    splitsBy f es = dropWhile f <$> L.findIndices f es `parts` es
    
    isPrefixOf = L.isPrefixOf
    isSuffixOf = L.isSuffixOf
    isInfixOf  = L.isInfixOf
    
    breakl = L.break
    spanl  = L.span
    
    selectWhile _    []    = []
    selectWhile f (x : xs) = case f x of {(Just e) -> e : select f xs; _ -> []}
    
    selectEnd f = reverse . selectWhile f . reverse

--------------------------------------------------------------------------------

-- | @stripPrefix sub line@ strips prefix @sub@ of @line@ (if any).
stripPrefix :: (Split s e, Bordered s i, Eq e) => s -> s -> s
stripPrefix sub line = sub `isPrefixOf` line ? drop (sizeOf sub) line $ line

-- | @stripSuffix sub line@ strips suffix @sub@ of @line@ (if any).
stripSuffix :: (Split s e, Bordered s i, Eq e) => s -> s -> s
stripSuffix sub line = sub `isSuffixOf` line ? sans (sizeOf sub) line $ line

-- | @stripPrefix' sub line@ strips prefix @sub@ of @line@ or returns 'Nothing'.
stripPrefix' :: (Split s e, Bordered s i, Eq e) => s -> s -> Maybe s
stripPrefix' sub = isPrefixOf sub ?+ drop (sizeOf sub)

-- | @stripSuffix sub line@ strips suffix @sub@ of @line@ or returns 'Nothing'.
stripSuffix' :: (Split s e, Bordered s i, Eq e) => s -> s -> Maybe s
stripSuffix' sub = isSuffixOf sub ?+ sans (sizeOf sub)

-- | intercalate is generalization of intercalate
intercalate :: (Foldable f, Linear1 f l, Linear l e) => l -> f l -> l
intercalate =  concat ... intersperse

-- | @tails es@ returns sequence of @es@ tails.
tails :: (Linear l e) => l -> [l]
tails Z  = [Z]
tails es = es : tails (tail es)

-- | tails is generalization of inits.
inits :: (Linear l e) => l -> [l]
inits Z  = [Z]
inits es = es : inits (init es)

{- |
  @ascending es lengths@ checks if the subsequences of @es@ of lengths @lengths@
  is sorted.
-}
ascending :: (Split s e, Sort s e, Ord e) => s -> [Int] -> Bool
ascending =  all sorted ... flip splits

