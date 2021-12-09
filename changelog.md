# Version 0.2.1.1

`sdp-0.2.1.1` patch includes macOS `sdp.cabal` fix by flurie.

Relaxes `ghc-prim` version restrictions: `ghc-prim-0.7` and `ghc-prim-0.8` are
now also supported.

# Version 0.2.1

`sdp-0.2.1` is the first minor version of `sdp-0.2` that fixes some bugs,
extends classes and adds new operations.

## Fixed bugs

* The `reversed` description has been aligned with what the function actually
does. Added procedure `reversed'`, which does what `reversed` was originally
intended to do.
* The documentation has become more readable and accurate, and the changelog is
finally up to date with the state of the library at the time of publication.
* Added `Control.Concurrent.*` modules, `Control.Concurent.*` is marked as typo

## Remaining bugs and issues
* Common index range implementation is still broken

## Changes

* In `SDP.Linear.Linear`: added `after`, `before` and `remote` functions, added
new folds
* In `SDP.LinearM.LinearM`: added `lshiftM` procedure, added new folds
* In `SDP.Nullable`: added new `Nullable` instances, added default signatures
for `lzero` and `isNull`, `Z` pattern now defined here
* In `SDP.Unboxed`: added a lot of new operations, new instances, more readeable
documentation
* For most classes: added constraint synonyms and quantified synonyms
`since GHC 8.6.1`.

# Version 0.2.0.1

`sdp-0.2.0.1` is the first patch of `sdp-0.2`, fixing some of the bugs known at
the time of publication.

## Fixed bugs and issues

* Support for older `base`, `ghc-prim` versions has been checked and extended,
`sdp` has been tested with many versions of its dependencies.

## Remaining bugs and issues

* `reversed` implementation is still incorrect for backward compatibility
* Documentation is still written by a crooked-handed programmer, but I'm working
on it.
* Common index range implementation is still broken
* Typo in word **concurrent** is still present

# Version 0.2

`sdp-0.2` is the first major release of `sdp` and introduces many new
functions, classes and types.

## Known bugs:

* Inaccurate version limitations of supported packages - this `sdp` version is
very fragile
* Incorrect behavior of the `reversed` function, which does not correspond to
its description
* An implementation of the smallest common index range is generally incorrect
for nonlinear shapes
* Typo in word **concurrent**: in names of modules `Control.Concurent.TArray`,
`Control.Concurent.TUnlist` and `Control.Concurent.TUnrolled`.

## Known issues:
* Problems related to the inability to build a library with previous versions of
`base` and `ghc-prim`
* The `Index` implementation for `Word` may fail test with some versions of
`QuickCheck`. This isn't a bug.
* Problems with the clarity and accuracy of the documentation, associated with
the need to proofread a large amount of text in a non-native language.

# Versions < 0.2

Versions below `sdp-0.2` are terrible, so they aren't published and exist only
in the repository. All changes are documented starting from version `sdp-0.2`,
`@since` tags - from version `sdp-0.2.1`.


