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
  asProxy#, toProxy#, fromProxy#, fromProxy, fromProxy##, fromProxy1
)
where

import GHC.Exts

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

{- |
  Returns 'undefined' (sdp < 0.3) or 'UnreachableException' (with function name
  for debug, since @sdp-0.3@) of suitable type.
-}
fromProxy :: proxy e -> e
fromProxy =  \ _ -> unreachEx "fromProxy: inappropriate use, (fromProxy e)\
                              \ should never be evaluated."

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Returns second argument.
-}
asProxy# :: Proxy# e -> e -> e
asProxy# =  \ _ x -> x

{- |
  @since 0.3
  
  Returns 'proxy#'.
-}
toProxy# :: e -> Proxy# e
toProxy# =  \ _ -> proxy#

{- |
  @since 0.3
  
  Returns 'UnreachableException' (with function name for debug) of suitable type.
-}
fromProxy# :: Proxy# e -> e
fromProxy# =  \ _ -> unreachEx "fromProxy#: inappropriate use, (fromProxy# e)\
                              \ should never be evaluated."

--------------------------------------------------------------------------------

{- |
  Since 0.3
  
  Retuns 'proxy#'.
-}
fromProxy## :: Proxy# (proxy e) -> Proxy# e
fromProxy## =  \ _ -> proxy#

--------------------------------------------------------------------------------

{- |
  @since 0.2
  
  Returns 'undefined' (sdp < 0.3) or 'UnreachableException' (with function name
  for debug, since @sdp-0.3@) of suitable type.
-}
fromProxy1 :: m (proxy e) -> e
fromProxy1 =  \ _ -> unreachEx "fromProxy1: inappropriate use of the @fromProxy1@\
                              \ function: the value should never be used."

--------------------------------------------------------------------------------

unreachEx :: String -> a
unreachEx =  throw . UnreachableException . showString "SDP.Proxy."




