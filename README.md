# Simple Data Processing

It is a library for simple data processing. `sdp` is inspired by `array`,
`vector`, `bytestring`, `containers` and `repa`. `sdp` is focused on efficiency,
openness and simplicity.

## Motivation

Originally `sdp` was created as `array` replacement. Now its main purpose was to
provide the most comfortable interface for working with various data structures:
eliminate name conflicts with other libraries and between them, simplify their
interaction, make code even more versatile and reusable.

### Structures

All predefined structures are based on **pseudo-primitive** types `SArray#`,
`SBytes#`, `STArray#`, `STBytes#`, `MIOArray#`, `MIOBytes#` that encapsulate
real primitives with correct size and offset.

`sdp` uses **templates** to define more complex structures:
* `AnyBorder` adds explicit boundaries of arbitrary type
* `AnyChunks` defines an unrolled list of given elements

Based on pseudo-primitives and templates, the following are defined:
* immutable arrays with immutable explicit immutable boundaries: `Array` and
`Bytes`
* mutable arrays with immutable explicit immutable boundaries: `STArray` and
`STBytes`, `MIOArray` (`IOArray`) and `MIOBytes` (`IOBytes`)
* immutable unrolled lists with immutable implicit boundaries: `Unlist` and
`Ublist`
* mutable unrolled lists with immutable implicit boundaries: `STUnlist` and
`STUblist`, `MIOUnlist` (`IOUnlist`) and `MIOUblist` (`IOUblist`)
* immutable unrolled lists with explicit immutable boundaries: `Unrolled` and
`ByteList`
* mutable unrolled lists with explicit immutable boundaries `STUnrolled` and
`STByteList`, `MIOUnrolled` (`IOUnrolled`) and `MIOByteList` (`IOByteList`)

### Classes

`sdp` generalize the most popular operations on linear (list-like) and
associative data structures including selection, splitting and sorting. With
`sdp` list functions aren't overlap their counterparts for other structures.

* `Nullable` is a service class of structures with null values.
* `Bordered` is a class of structures with borders and finite number of elements.
* `Estimate` is a service class for efficiently (at least, in finite time)
estimating the length of a structure or compare pair of structures by length (in
finite time if at least one of them is finite). Allows to express such
conditions as:
```
xs .<. ys -- length xs < length ys
es .== 10 -- length es == 10
es .>   5 -- length es > 5
```
* `Unboxed` is a service class that simplifies interacting with data stored in
`ByteArray#` and `MutableArray#`. Used in containers that based on `SBytes#`,
`STBytes#` or `IOBytes#`.
* `Shape` is a service class for dimension operations and finite-dimensional
index transformations. `Index` is a service class that generalizes `Enum` to
interval operations, replaces `Ix`.
* `Linear` is a class of linear structures that generalizes the standard list
functions. `Split` is `Linear` extension, which implements additional list-like
operations like `split(At)` `takeWhile`, `isPrefixOf`, etc.
* `Indexed` is a class of indexed structures that generalizes read and modify
operations immutable structures.
* `Shaped` is a class of operations on structures generalized by the type of
index. Provides safe change of range and index type, fast extraction of
subsequences.
* `Map` is a class of operations on associative arrays (dictionaries).
* `Set` is a class of operations on sets.
* `Zip` is a class that generalizes zipping operations (see `zip`, `zipWith`,
`ZipList`, etc.).
* `Scan` is a class of convolutions with intermediate values.
* `Sort` is a sorting class for immutable structures.
* `BorderedM`, `LinearM`, `SplitM`, `IndexedM`, `SortM` - classes of operations
on mutable containers.

## Versions

`sdp` follow [Haskell Package Versioning Policy](https://pvp.haskell.org). To
simplify the search for extensions, I also recommend the following rules:
* The `MAJOR` version of the derivative must match the smallest `MAJOR` version
of `sdp` with which it's compatible.
* Extensions should be called `sdp-%extensionname%` (e.g. `sdp-quickcheck`).
* Wrappers should be called `sdp4%libraryname%` (e.g., `sdp4text`).

## Using the SDP category

The `SDP` category is intended for `sdp` classes and primitives, as for
structures whose names are already taken in the `Data` category. It shouldn't be
used instead of `System`, `Control`, `Foreign`, etc.

## Contributing
For details of the process for submitting pull requests, please read
[CONTRIBUTING.md](https://github.com/andreymulik/sdp/blob/master/CONTRIBUTING.md).

## License
`sdp` is FOSS (free and open source software), you can redistribute it and/or
modify it under the terms of the BSD3 license. `sdp` is distributed in the hope
that it will be useful, but without any warranty, without even the implied
warranty of merchantability or fitness for a particular purpose. See the LICENSE
file for more details.

