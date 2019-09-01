{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}

{- |
    Module      :  SDP.Indexed
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
  
  Indexed is one of the main classes of SDP, designed to read and write immutable
  indexable data structures.
-}

module SDP.Indexed
(
  module SDP.Linear,
  
  Indexed (..),
  
  (>/>)
)
where

import SDP.SafePrelude
import Prelude ()

import Data.List ( findIndex, findIndices )

import SDP.Linear
import SDP.Set

import SDP.Simple

default ()

infixl 9 !^, .!, !, !?

--------------------------------------------------------------------------------

-- | Class of indexed data structures.
class (Linear v e, Index i) => Indexed v i e | v -> i, v -> e
  where
    {-# MINIMAL assoc', fromIndexed, (//), ((!)|(!?)), (*$) #-}
    
    {- Global operations. -}
    
    {- |
      assoc creates new structure from list of associations [(index, element)],
      where default element is IndexException (UndefinedValue).
    -}
    assoc :: (i, i) -> [(i, e)] -> v
    assoc bnds = assoc' bnds (undEx "assoc (default)")
    
    -- | assoc' is safe version of assoc.
    assoc' :: (i, i) -> e -> [(i, e)] -> v
    
    -- | fromIndexed converts indexed structure to another one.
    fromIndexed :: (Bordered v' j e, Indexed v' j e) => v' -> v
    
    -- | imap applies function to indices.
    imap :: (Indexed v' j e) => (i, i) -> v' -> (i -> j) -> v
    imap bnds es f = assoc bnds [ (i, es ! f i) | i <- range bnds ]
    
    -- | accum creates a new structure from the old and the assocs list.
    default accum :: (Bordered v i e) => (e -> e' -> e) -> v -> [(i, e')] -> v
    accum :: (e -> e' -> e) -> v -> [(i, e')] -> v
    accum f es ies = bounds es `assoc` [ (i, es .! i `f` e') | (i, e') <- ies ]
    
    -- | Writes elements to immutable structure (by copying).
    (//) :: v -> [(i, e)] -> v
    
    -- | Update function. Uses (!) and may throw IndexException.
    (/>) :: v -> [i] -> (i -> e -> e) -> v
    (/>) es is f = es // [ (i, f i (es ! i)) | i <- is ]
    
    {- Elementwise operations. -}
    
    -- | (!^) is completely unsafe reader. Must work as fast, as possible.
    default (!^) :: (Bordered v i e) => v -> Int -> e
    (!^) :: v -> Int -> e
    es !^ i = es .! index (bounds es) i
    
    {- |
      (.!) is unsafe reader, but on bit faster version of (!). Use (.!) only if
      you are really sure that you will not go beyond the bounds. E.g. after
      testing with (!).
    -}
    {-# INLINE (.!) #-}
    (.!) :: v -> i -> e
    (.!) = (!)
    
    -- | (!) is pretty safe reader. Must throw IndexException.
    {-# INLINE (!) #-}
    (!)  :: v -> i -> e
    (!) dat = fromMaybe (undEx "(!)") . (dat !?)
    
    -- | (!?) is completely safe, but very boring function.
    default (!?) :: (Bordered v i e) => v -> i -> Maybe e
    (!?) :: v -> i -> Maybe e
    (!?) dat = (not . indexOf dat) ?: (dat .!)
    
    -- |  Write one element to structure.
    default write_ :: (Bordered v i e) => v -> Int -> e -> v
    write_ :: v -> Int -> e -> v
    write_ es i e = es // [(index (bounds es) i, e)]
    
    -- |  Write one element to structure.
    write :: v -> i -> e -> v
    write es i e = es // [(i, e)]
    
    -- | Searches the index of first matching element.
    (.$) :: (e -> Bool) -> v -> Maybe i
    (.$) f es = null ?: head $ f *$ es
    
    -- | Searches the indices of all matching elements.
    default (*$) :: (Bordered v i e) => (e -> Bool) -> v -> [i]
    (*$) :: (e -> Bool) -> v -> [i]
    (*$) f = fsts . filter (f . snd) . assocs

--------------------------------------------------------------------------------

instance Indexed [e] Int e
  where
    assoc' bnds e = toResultList . normalAssocs
      where
        toResultList = fromListN (size bnds) . snds
        normalAssocs = fill . setWith cmpfst . filter (inRange bnds . fst)
        
        fill (ie1@(i1, _) : ie2@(i2, _) : xs) = ie1 : fill rest
          where
            rest = nx /= i2 ? (nx, e) : ie2 : xs $ ie2 : xs
            nx   = next bnds i1
        fill xs  = xs
    
    (!^) = (.!)
    
    []       .! _ = error "in SDP.Indexed.(.!)"
    (x : xs) .! n = n == 0 ? x $ xs .! (n - 1)
    
    [] ! _ = throw $ EmptyRange "in SDP.Indexed.(!)"
    es ! n = n >= 0 ? es !# n $ throw $ IndexUnderflow "in SDP.Indexed.(!)"
      where
        []       !# _  = throw $ IndexOverflow "in SDP.Indexed.(!)"
        (x : xs) !# n' = n' == 0 ? x $ xs !# (n' - 1)
    
    []       !? _ = Nothing
    (x : xs) !? n = case n <=> 0 of {LT -> Nothing; EQ -> Just x; GT -> xs !? (n - 1)}
    
    fromIndexed es = (es !) <$> indices es
    
    {-# INLINE (//) #-}
    xs // es = snds $ unionWith cmpfst (assocs xs) (setWith cmpfst es)
    
    {-# INLINE (.$) #-}
    (.$) = findIndex
    
    {-# INLINE (*$) #-}
    (*$) = findIndices

-- | Update one element in structure.
(>/>) :: (Indexed v i e) => v -> [i] -> (e -> e) -> v
(>/>) es is = (es /> is) . const

--------------------------------------------------------------------------------

undEx :: String -> a
undEx msg = throw . UndefinedValue $ "in SDP.Indexed." ++ msg


