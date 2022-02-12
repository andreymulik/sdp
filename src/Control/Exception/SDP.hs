{-# LANGUAGE Safe #-}

{- |
    Module      :  Control.Exception.SDP
    Copyright   :  (c) Andrey Mulik 2019-2021
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
  
  "Control.Exception.SDP" - service module that provide some useful exceptions.
  Note that "SDP.SafePrelude" doesn't export this module.
-}
module Control.Exception.SDP
(
  -- * Exports
  module Control.Exception,
  
  -- * Exceptions
  UnreachableException (..), IndexException (..)
)
where

import Prelude ( Eq (..), Show (..), String, (++) )

import Control.Exception
import Data.Typeable

default ()

--------------------------------------------------------------------------------

{- |
  'IndexException' replaces the less informative 'ArrayException' and has more
  neutral names.
  
  * 'UnacceptableExpansion' - occurs when performing the (safe) rebound
  operation with unacceptable target range (example: an attempt to convert
  structure with bounds @(2, 5)@ to structure with bounds @('\\0', '\\255')@ is
  invalid because available size is smaller than required)
  * 'UnexpectedRank' - occurs when trying to convert one representation of an
  index to another, if their dimensions doesn't match (example: trying to
  convert a list @[1, 2, 3]@ of type @[Int]@ to an index of type @(T4 Int)@)
  * 'UndefinedValue' - occurs when referring to a non-existent or undefined
  element; some unsafe structures and operations can lead to the possibility of
  untracked reading of invalid or undefined values
  * 'EmptyRange' - occurs when accessing the contents of an empty structure
  * 'IndexOverflow' - occurs when going beyond the upper boundary of the
  structure (overflow)
  * 'IndexUnderflow' - occurs when going beyond the lower boundary of the
  structure (underflow)
  
  If the error type may depend on the check order, it should be indicated in the
  documentation. For example: overflow is checked first, and then underflow. But
  if an overflow is detected, underflow may not be noticed. 'IndexException'
  constructor order is the recommended order.
-}
data IndexException = UnacceptableExpansion String
                    | UndefinedValue        String
                    | UnexpectedRank        String
                    | IndexUnderflow        String
                    | IndexOverflow         String
                    | EmptyRange            String
  deriving ( Eq, Typeable )

instance Show IndexException
  where
    show (UnacceptableExpansion s) = "unacceptable expansion "   ++ s
    show (UndefinedValue        s) = "undefined element "        ++ s
    show (UnexpectedRank        s) = "unexpected rank "          ++ s
    show (IndexUnderflow        s) = "index out of lower bound " ++ s
    show (IndexOverflow         s) = "index out of upper bound " ++ s
    show (EmptyRange            s) = "empty range "              ++ s

instance Exception IndexException

--------------------------------------------------------------------------------

{- |
  A 'UnreachableException' is used as an exception that should never be thrown.
  UnreachableException is a more specific version of 'undefined' (or just
  unconditional 'AssertionFailed').
  
  Example: 'Data.Map.Map.lookupLT'' functions whose default definition use
  'UnreachableException' as the second element of the default tuple that
  compared with other tuples by 'cmpfst' and will never be returned.
-}
newtype UnreachableException = UnreachableException String
  deriving ( Eq, Typeable )

instance Show UnreachableException
  where
    show (UnreachableException s) = "unreachable exception " ++ s

instance Exception UnreachableException

