{-# LANGUAGE Trustworthy, MagicHash #-}

{- |
    Module      :  SDP.Proxy
    Copyright   :  (c) Andrey Mulik 2023
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Proxy" module provides 'Proxy' and 'Proxy#' helpers.
-}
module SDP.Proxy
(
  -- * Proxy
  fromProxy, toProxy, fromProxy1, toProxy1, fromProxy#,
  
  -- * Proxy\#
  toProxy##, fromProxy##, toProxy#, asProxy##, unliftProxy##
)
where

import GHC.Exts

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Returns 'UnreachableException' (with function name for debug) of suitable type.
-}
toProxy :: e -> proxy e
toProxy =  \ _ -> unreachEx "toProxy: inappropriate use, (toProxy e)\
                           \ should never be evaluated."

{- |
  @since 0.2
  
  Returns 'undefined' (sdp < 0.3) or 'UnreachableException' (with function name
  for debug, since @sdp-0.3@) of suitable type.
-}
fromProxy :: proxy e -> e
fromProxy =  \ _ -> unreachEx "fromProxy: inappropriate use, (fromProxy e)\
                              \ should never be evaluated."

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Returns 'proxy#'.
-}
toProxy# :: proxy e -> Proxy# e
toProxy# =  \ _ -> proxy#

{- |
  @since 0.3
  
  Returns 'UnreachableException' (with function name for debug) of suitable type.
-}
fromProxy# :: Proxy# e -> proxy e
fromProxy# =  \ _ -> unreachEx "fromProxy#: inappropriate use, (fromProxy# e)\
                              \ should never be evaluated."

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Returns 'proxy#'.
-}
toProxy## :: e -> Proxy# e
toProxy## =  \ _ -> proxy#

{- |
  @since 0.3
  
  Returns 'UnreachableException' (with function name for debug) of suitable type.
-}
fromProxy## :: Proxy# e -> e
fromProxy## =  \ _ -> unreachEx "fromProxy##: inappropriate use, (fromProxy## e)\
                              \ should never be evaluated."

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Returns second argument.
-}
asProxy## :: Proxy# e -> e -> e
asProxy## =  \ _ x -> x

{- |
  Since 0.3
  
  Retuns 'proxy#'.
-}
unliftProxy## :: Proxy# (proxy e) -> Proxy# e
unliftProxy## =  \ _ -> proxy#

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Returns 'UnreachableException' (with function name for debug) of suitable type.
-}
toProxy1 :: e -> m (proxy e)
toProxy1 =  \ _ -> unreachEx "toProxy1: inappropriate use, (toProxy1 e)\
                            \ should never be evaluated."

{- |
  @since 0.2
  
  Returns 'undefined' (sdp < 0.3) or 'UnreachableException' (with function name
  for debug, since @sdp-0.3@) of suitable type.
-}
fromProxy1 :: m (proxy e) -> e
fromProxy1 =  \ _ -> unreachEx "fromProxy1: inappropriate use, (fromProxy1 e)\
                              \ should never be evaluated."

--------------------------------------------------------------------------------

unreachEx :: String -> a
unreachEx =  throw . UnreachableException . showString "SDP.Proxy."

