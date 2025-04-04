# Monoidal containers

# 0.6.6.0

* [Support ghc 9.12](https://github.com/bgamari/monoidal-containers/pull/108)
* [Allow hashable 1.5](https://github.com/bgamari/monoidal-containers/pull/107]

# 0.6.5.0

* [Support for GHC 9.10](https://github.com/bgamari/monoidal-containers/pull/105)

# 0.6.4.0

* [Bump upper bound of `these`](https://github.com/bgamari/monoidal-containers/pull/95)
* [Bump upper bound of `semialign`](https://github.com/bgamari/monoidal-containers/pull/96)
* [Build with GHC 9.6](https://github.com/bgamari/monoidal-containers/pull/97)

# 0.6.3.0

  * [Bump upper bounds of aeson, base, and lens](https://github.com/bgamari/monoidal-containers/pull/92)

# 0.6.2.0

  * [Remove cyclic definition of `!?`](https://github.com/bgamari/monoidal-containers/pull/88)
  * [Bump lens and hashable version bounds](https://github.com/bgamari/monoidal-containers/pull/87)

# 0.6.1.0

  * Drop official support for GHC 7.8.4
  * Require containers 0.5.9 or greater.
  * Support lens-5, semialign-1.2, and aeson-2
  * MonoidalMap and MonoidalMap.Strict: Add several functions added to Data.Map since 0.5 up to 0.5.9:
    `take`, `drop`, `splitAt`, `lookupMin`, `lookupMax`, `restrictKeys`,
    `withoutKeys`, `fromDescList`,  `fromDescListWith`, `fromDescListWithKey`,
    `fromDistinctDescList`, `takeWhileAntitone`, `dropWhileAntitone`,
    `spanAntitone`, and `traverseMaybeWithKey`
  * Add instances of `Filterable` and `Witherable` from the `witherable` package
  * Add `Control.Lens.Wrapped.Rewrapped` instances.

# 0.6.0.1

  * Support semialign-1.1

# 0.6

  * Deprecates 0.5.* and reverts behavior of fromList, insert, mapKeys, etc. to match behavior in Data.Map. The only difference in behavior between Data.Map.Monoidal.MonoidalMap and Data.Map.Map is now the semigroup and monoid instances (as was the case in 0.4 and earlier).
  * Fix the argument order of Data.HashMap.Monoidal.insert
  * Remove Data.HashMap.Monoidal.insertOrReplace as it is now identical to Data.HashMap.Monoidal.insert
  * Added Data.HashMap.Monoidal.insertWith and Data.HashMap.Monoidal.fromListWith

# 0.5.0.1

  * Add a flag, `split-these`, to select whether to use the new "these"/"semialign" packages or the older combined "these" package.
  * Add default.nix to make it easier to hack on this package in nix

# 0.5.0.0

  * Added Data.IntMap.Monoidal and Data.IntMap.Monoidal.Strict, corresponding to Data.IntMap and Data.IntMap.Strict
  * Make `fromList`, `insert`, and `mapKeys` from `Data.Map.Monoidal` and `Data.Map.Monoidal.Strict` require `Semigroup` on values to properly capture monoidal behavior instead of reverting to the left-biased semantics of `Data.Map`.
  * Add Align instances and, for sufficiently recent versions of `these`, Semialign instances
  * Support `these` 0.8.0

# 0.4.0.0

General changes:

 * Added support for `unordered-containers < 0.2.8`
 * Added many more functions in `Data.Map.[Strict.]Monoid`

Weakened `Monoid` constraints to `Semigroup` whenever possible as enabled by the
[Semigroup-Monoid
proposal](https://prime.haskell.org/wiki/Libraries/Proposals/SemigroupMonoid).
This includes,

 * the `Monoid` instance of `MonoidalHashMap` and `MonoidalMap`
 * the `IsList` instance of `MonoidalHashMap` and `MonoidalMap`
 * the `modifyDef` and `mapKeys` functions of `MonoidalHashMap`


# 0.3 and earlier

Pre-history
