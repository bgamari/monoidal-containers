# Monoidal containers

# Next

  * Support `these` 1.0.1.
  * Add `SemialignWithIndex` instances.

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
