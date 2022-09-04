{-# LANGUAGE Safe, CPP, ConstraintKinds #-}

#ifdef SDP_QUALIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.Forceable
    Copyright   :  (c) Andrey Mulik 2021-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Forceable" provides 'Forceable' class.
    
    @since 0.3
-}
module SDP.Forceable
(
  -- * Forceable
  Forceable (..), Forceable1, Forceable2,
  
#ifdef SDP_QUALIFIED_CONSTRAINTS
  Forceable', Forceable'',
#endif
)
where

default ()

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Many data structures allow you to represent almost equivalent values in many
  ways. For example, an array with an optional @offset@ field allows you to
  slice subarray quickly and with minimal memory overhead. But original array
  with all of its contents remains in memory and there is no easy way to
  determine which part of the memory in use now (or potentially), and which part
  can be freed. In this case, 'force' allows you to use fast split and slice
  operations, but you can slow down these functions if necessary to improve the
  efficiency of your code.
  
  Note that there is no universal set of rules that a 'force' implementation
  must follow, for example, it can always create a new structure, or sometimes
  refer to an old one (part of it), so you cannot use @unsafeThaw@ and other
  similar operations after 'force' if you are working with generic types.
-}
class Forceable f
  where
    -- | Force the value, default: @force = id@.
    force :: f -> f
    force =  id

{- |
  @since 0.3
  
  'Forceable' contraint for @(Type -> Type)@-kind types.
-}
type Forceable1 f e = Forceable (f e)

{- |
  @since 0.3
  
  'Forceable' contraint for @(Type -> Type -> Type)@-kind types.
-}
type Forceable2 f i e = Forceable (f i e)

#ifdef SDP_QUALIFIED_CONSTRAINTS
{- |
  @since 0.3
  
  'Forceable' contraint for @(Type -> Type@-kind types.
-}
type Forceable' f = forall e . Forceable (f e)

{- |
  @since 0.3
  
  'Forceable' contraint for @(Type -> Type -> Type)@-kind types.
-}
type Forceable'' f = forall i e . Forceable (f i e)
#endif

--------------------------------------------------------------------------------

{- |
  The list doesn't imply variations in value representation and don't cause
  memory, so 'force' is just 'id'.
-}
instance Forceable [a]



