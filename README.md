# monoidal-containers
[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![Hackage](https://img.shields.io/hackage/v/monoidal-containers.svg)](https://hackage.haskell.org/package/monoidal-containers) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/monoidal-containers/badge)](https://matrix.hackage.haskell.org/#/package/monoidal-containers) [![Github CI](https://github.com/bgamari/monoidal-containers/workflows/Cabal/badge.svg)](https://github.com/bgamari/monoidal-containers/actions) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/bgamari/monoidal-containers/blob/master/LICENSE)



Often when working with the containers offered by the `containers` and 
`unordered-containers` packages one would prefer the monoidal structure 
of the values to be used when resolving conflicts between keys when merging 
structures. Sadly, these are not the semantics offered by the provided
instances. This package provides `newtypes` with an appropriate set of
instances and utility functions to make them usable.

## Important Note

This library is designed to be a drop-in replacement for `Data.Map` and similar. Absolutely no other semantic changes are introduced in functions like `fromList` or `insert`. They are still left-biased like `Data.Map`'s. The only difference is in instances for `Semigroup`.
