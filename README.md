# monoidal-containers
[![Build status](https://github.com/bgamari/monoidal-containers/actions/workflows/Cabal.yml/badge.svg)](https://github.com/bgamari/monoidal-containers/actions/workflows/Cabal.yml)

Often when working with the containers offered by the `containers` and 
`unordered-containers` packages one would prefer the monoidal structure 
of the values to be used when resolving conflicts between keys when merging 
structures. Sadly, these are not the semantics offered by the provided
instances. This package provides `newtypes` with an appropriate set of
instances and utility functions to make them usable.

## Important Note

This library is designed to be a drop-in replacement for `Data.Map` and similar. Absolutely no other semantic changes are introduced in functions like `fromList` or `insert`. They are still left-biased like `Data.Map`'s. The only difference is in instances for `Semigroup`.
