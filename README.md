# monoidal-containers

Often when working with the containers offered by the `containers` and 
`unordered-containers` packages one would prefer the monoidal structure 
of the values to be used when resolving conflicts between keys when merging 
structures. Sadly, these are not the semantics offered by the provided
instances. This package provides `newtypes` with an appropriate set of
instances and utility functions to make them usable.
