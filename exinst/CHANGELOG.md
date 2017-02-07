# Version HEAD

* Add `Dict0`.

* Re-export `Constraint` from `base`.

* Add `Dict{0,2,3,4}` instances for `Bool`.

* Add `GHC.Generics.Generic` support for `Some{1,2,3,4}`. This only works for
  indexes with `PEnum` and `PBounded` instances.


# Version 0.2

* Depend on `singletons-2.2`, which means `KProxy` is gone.

* Add `_Some{1,2,3,4}` prisms.

* Add `Dict1` instance for `Bool`.

* Add `some{1,2,3,4}SingRep`.

* Thanks to Sandy Maguire for having contributed to this release.


# Version 0.1.2

* Export the `Some{1,2,3,4}` constructors.

* Make all the fields in `Some{1,2,3,4}` strict.

* Relax upper bound on `base`, `singletons`, `constraints`.

* Re-export `Dict(Dict)` from `constraints`.


# Version 0.1.1

* Relax upper bound on `singletons` dependency.

* `withSome{1,2,3,4}Sing` brings `SingI` instances to scope.

* Generalized the `(* -> Constraint)` parameter in `Dict{1,2,3,4}`
  to `(k -> Constraint)`.


# Version 0.1

* Initial release.
