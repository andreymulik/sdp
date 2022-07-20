{-# LANGUAGE Safe #-}

{- |
    Module      :  Text.Show.SDP
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "Text.Show.SDP" provides common 'ShowS' stuff.
-}
module Text.Show.SDP
(
  -- * Common show templates
  assocsPrec, showsRaw, showsRawLinear
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Indexed

import GHC.Show ( appPrec )

default ()

--------------------------------------------------------------------------------

-- | 'assocsPrec' is 'showsPrec' template.
assocsPrec :: (Indexed v i e, Show i, Show e) => String -> Int -> v -> ShowS
assocsPrec name = \ p es -> showParen (p > appPrec) $ showString name
                                                    . shows (bounds es)
                                                    . showChar ' '
                                                    . shows (assocs es)

{- |
  'showsRaw' is a primitive list-to-string conversion pattern.
  
  Note that attempting to parse the resulting string with standard @ReadS@-based
  functions will cause an error (ambiguous parse). To properly parse a string,
  use the @readRawSequence@ function from the "SDP.Text.Read" module.
-}
showsRaw :: (Show e) => Int -> [e] -> ShowS
showsRaw _    []    = id
showsRaw p (x : xs) =
  let stream = shows x . foldr (\ e rest -> showChar ' ' . shows e . rest) id xs
  in  showParen (p > appPrec) stream

-- | Just 'showsRaw' version for 'Linear'.
showsRawLinear :: (Linear l e, Show e) => Int -> l -> ShowS
showsRawLinear =  (. listL) . showsRaw




