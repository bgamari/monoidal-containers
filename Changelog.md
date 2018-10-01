# Monoidal containers

# 0.4.1.0

* Added support for `unordered-containers < 0.2.8`
* Added many more functions in `Data.Map.[Strict.]Monoid`

# 0.4.0.0

Weakened `Monoid` constraints to `Semigroup` whenever possible as enabled by the
[Semigroup-Monoid
proposal](https://prime.haskell.org/wiki/Libraries/Proposals/SemigroupMonoid).
This includes,

 * the `Monoid` instance of `MonoidalHashMap` and `MonoidalMap`
 * the `IsList` instance of `MonoidalHashMap` and `MonoidalMap`
 * the `modifyDef` and `mapKeys` functions of `MonoidalHashMap`
 

# 0.3 and earlier

Pre-history
