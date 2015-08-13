# exinst

> See the [BSD3 LICENSE](https://github.com/k0001/exinst/blob/master/exinst/LICENSE.txt)
> file to learn about the legal terms and conditions for this library.

Exinst is a library providing you with tools to automatically derive instances for
type-indexed types whose type-indexes have been existentialized. Currently it only
supports using [`singleton`](https://hackage.haskell.org/package/singletons) types as
type-indexes.

> TODO: Support for non-singleton-types types with kind `*` using `Typeable` should
> be possible, but I haven't worked on that yet. It's on the roadmap.

In short, what `exinst` currently gives you is: For any type ``t :: k -> *``,
if `k` is a singleton type and `c (t k) :: Constraint` is satisfied, then you can
existentialize away the `k` parameter with `Some1 t`, and have `c (Some1 t)`
automatically satisfied. Currently, up to 4 type indexes can be existentialized
using `Some1`, `Some2`, `Some3` and `Some4` respectively.

> NOTE: This tutorial asumes some familiarity with singleton types as implemented
> by the [`singleton`](https://hackage.haskell.org/package/singletons) library.
> A singleton type is, in very rough terms, a type inhabited by a single term,
> which allows one to go from its term-level representation to its type-level
> representation and back without much trouble. A bit like the term `()`, which
> is of type `()`: whenever you have the type `()` you know what that its
> term-level representation must be `()`, and whenever you have the term `()`
> you know that its type must be `()`.

## Motivation

As a motivation, let's consider the following example:

> TODO: check language extensions needed for the following example.

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

data Size = Big | Small

data Receptacle (a :: Size) :: * where
  Vase :: Receptacle 'Small
  Glass :: Receptacle 'Small
  Barrel :: Receptacle 'Big

deriving instance Show (Receptacle a)
```

`Receptacle` can describe three types of receptacles (`Vase`, `Glass` and
`Barrel`), while at the same time being able to indicate, at the type level,
whether the size of the receptacle is `Big` or `Small`. Additionally, we've
provided `Show` instances for `Receptacle` (which could have been derived
automatically, too).

Now, if we want to put `Receptacle`s in a container, for example in `[]`, we can
do so only as long as the `Receptacle` type is fully applied. That is, we can
have `[Receptacle 'Small]` and `[Receptacle 'Big]`, but we can't have
`[Receptacle]`. So, if we want to have `Receptacle`s of different sizes in a
container like `[]`, we need a different solution.

At this point we need to ask ourselves why we need to put `Receptacle`s of
different sizes in a same container. If the answer is something like “because we
want to show all of them, no matter what size they are”, then we should realize
that what we are actually asking for is that no matter what `Size` our
`Receptable` has, we need to be able to find a `Show` instance for that
`Receptacle`. In Haskell, we can express just that using existential types
and constraints hidden behind a data constructor.

```haskell
--We need to add these language extensions to the ones in the previous example
--{-# LANGUAGE ExistentialQuantification #-}
--{-# LANGUAGE FlexibleContexts #-}

data ReceptacleOfAnySizeThatCanBeShown
  = forall a. (Show (Receptacle a))
      => MkReceptacleOfAnySizeThatCanBeShown (Receptacle a)
```

We can construct values of type `ReceptacleOfAnySizeThatCanBeShown` only as long
as there exist a `Show` instance for the `Receptacle a` we give to the
`MkReceptacleOfAnySizeThatCanBeShown` constructor. In our case, both `Receptacle
'Small` and `Receptacle 'Big` have `Show` instances, so all of `Vase`, `Glass` and
`Barrel` can be used successfully with `MkReceptacleOfAnySizeThatCanBeShown`.

Now, `ReceptacleOfAnySizeThatCanBeShown` on itself doesn't yet have a `Show`
instance, and we can't derive one automatically using the `deriving` mechanism,
but we can give an explicit `Show` instance that just forwards the work to the
`Show` instance of the underlying `Receptacle a`.

```haskell
instance Show ReceptacleOfAnySizeThatCanBeShown where
  show (MkReceptacleOfAnySizeThatCanBeShown a) = show a
```

That works as intended:

```
> show (MkReceptacleOfAnySizeThatCanBeShown Vase)
"Vase"
> show (MkReceptacleOfAnySizeThatCanBeShown Barrel)
"Barrel"
```

And now, as we wanted, we can put `Receptacle`s of different sizes in a `[]` and
show them (as long as we wrap each of them as
`ReceptacleOfAnySizeThatCanBeShown`, that is).

```
> map show [MkReceptacleOfAnySizeThatCanBeShown Vase, MkReceptacleOfAnySizeThatCanBeShown Barrel]
["Vase", "Barrel"]
```

However, the above solution is unsatisfying for various reasons: For one, the
`Show` instance for `ReceptacleOfAnySizeThatCanBeShown` works only as long as
the `ReceptacleOfAnySizeThatCanBeShown` itself carries a witness that the `Show`
constraint for `Receptacle a` is satisfied, which means that if we want to write
yet another instance for `ReceptacleOfAnySizeThatCanBeShown` that simply forwards
its implementation to the underlying `Receptacle a`, say `Eq`, then the
`MkReceptacleOfAnySizeThatCanBeShown` constructor would need to be modified to witness
the `Eq (Receptacle a)` instance too:

```haskell
data ReceptacleOfAnySizeThatCanBeShown
  = forall a. (Show (Receptacle a), Eq (Receptacle a))
      => MkReceptacleOfAnySizeThatCanBeShown (Receptacle a)
```

With that in place we can provide an `Eq` instance for
`ReceptacleOfAnySizeThatCanBeShown` as we did for `Show` before, but if we pay
close attention, we can see how the implementation of
`ReceptacleOfAnySizeThatCanBeShown` starts to become a bottleneck: Every
instance we want to provide for `ReceptacleOfAnySizeThatCanBeShown` that simply
forwards its work to the underlying `Receptacle a` needs to be witnessed by
`MkReceptacleOfAnySizeThatCanBeShown` itself, it is not enough that there exists
an instance for `Receptacle a`. Moreover, even the name
`ReceptacleOfAnySizeThatCanBeShown` that we chose before isn't completely
accurate anymore, and will become less and less accurate as we continue adding
constraints to `MkReceptacleOfAnySizeThatCanBeShown`.

Additionally, everywhere we use the `MkReceptacleOfAnySizeThatCanBeShown`
constructor we need to witness that the existentialized `Receptacle a` satisfies
all the required constraints, which means that, if the `Receptacle a` we pass to
`MkReceptacleOfAnySizeThatCanBeShown` is being received, say, as a parameter to
a function, then the type of that function will also require that its caller
satisfies all of the same constraints, even though it is obvious to us,
statically, that the instances exist. We can now see how all of this becomes
unmanegeable, or at least very *boilerplatey*, as those constraints start to
propagate through our code base.

What we need is a way for instances such as the `Show` instance for
`ReceptacleOfAnySizeThatCanBeShown` to find the `Show` instance for `Receptacle
a` without it being explicitely witnessed by the
`MkReceptacleOfAnySizeThatCanBeShown` constructor. That is exactly the problem
that `exinst` solves: allowing *exi*stentials to find their *inst*ances.


## Usage

Given the code for `Size`, `Receptacle` and its `Show` instances above, we can
achieve the same functionality as our initial `ReceptacleOfAnySizeThatCanBeShown` by
existentializing the type indexes of `Receptacle 'Small` and `Receptacle 'Big`
as `Some1 Receptacle`. In order to do that, we must first ensure that `Size` and its
constructors can be used as singleton types (as supported by the `singletons` library),
for which we can use some TH provided by `Data.Singletons.TH`:

```haskell
import Data.Singletons.TH

Data.Singletons.TH.genSingletons [''Size]
```

And we'll also need a `Show` instance for `Size` for reasons that will become
apparent later:

```haskell
deriving instance Show Size
```

Now we can construct a `Show1 Size` and `show` achieving the same results as we
did with `ReceptacleOfAnySizeThatCanBeShown` before.

Note: this code won't work yet. Keep reading.

```
> import Exinst.Singletons (some1)
> import Exinst.Instances.Base ()
> :t some1 Glass
:t some1 Glass :: Some1 Receptacle
> show (some1 Glass)
"Some1 Small Glass"
```

Well, actually, the default `Show` instance for `Some1` shows a bit more of
information, as it permits this string to be `Read` back into a `Some1
Receptacle` if needed, but displaying just `"Glass"` would be possible too, if
desired.

> TODO: Implement said `Read` instance.

The important thing to notice in the example above is that `some1` does not
require us to satisfy a `Show (Receptacle 'Small)` constraint, it just requires
that the type index for the type-indexed type we give it as argument is a
singleton type:

```haskell
some1 :: forall (f1 :: k1 -> *) (a1 :: k1). SingI a1 => f1 a1 -> Some1 f1
```

It is the application of `show` to `some1 Glass` which will fail to compile if
there isn't a `Show` instance for `Receptacle 'Small`, complaining that a `Show`
instance for `Some1 Receptable` can't be found. The reason for this is that even
if `Show` instances for `Some1` are derived for free, they are only derived for
`Some1 (t :: k1 -> *)` where a `Show (t a)` for a specific but statically
unknown `a` can be found at runtime (mostly, there are other minor requirements too).
The mechanism through which instances are found at runtime relies on `Dict` from the
[`constraints`](https://hackage.haskell.org/package/constraints) library, which
`exinst` wraps in a `Dict1` typeclass to be instantiated once per singleton
type.

```haskell
-- The Exinst.Singletons.Dict1 class
class Dict1 (c :: * -> Constraint) (f1 :: k1 -> *) where
  dict1 :: Sing (a1 :: k1) -> Dict (c (f1 a1))
```

What `Dict1` says is that: for a type-indexed type `f1`, given a term-level
representation of the singleton type that indexes said `f1`, we can obtain a
witness that the constraint `c` is satisfied by `f1` applied to the singleton
type.

That class seems to be a bit too abstract, but the instances we as users need to
write for it are quite silly and straightforward. Even *boilerplatey* if you
will; they could even be generated using TH

> TODO: Write the TH for deriving the `Dict{1,2,3,4}` implementation.

Here's an example of how to provide `Show` support for `Some1 Receptacle` via
`Dict1`:

```
instance (Show (Receptacle 'Small), Show (Receptacle 'Big)) => Dict1 Show Receptacle where
  dict1 = \x -> case x of
    SSmall -> Dict
    SBig -> Dict
```

The implementation of `dict1` looks quite silly, but it has to look like that as
it is only by pattern-matching on each of the `Sing Size` constructors that we
learn about the type level representation of a singleton type, which we then use
to select the proper `Show` instance among all of those listed in the instance head.

Given this `Dict1` instance, we can proceed to excecute the REPL example mentioned before
and it will work just fine.

However, that `Dict1` instance is still a bit insatisfactory: If we wanted,
again, to provide `Eq` support for our `Some1 Receptacle` type, we would need to
write yet another `Dict1` instance like the one above, but mentioning `Eq`
instead of `Show`. We can do better.

The trick, when writing `Dict1` instances such as the one above, is to leave `c`
and `f1 :: k1 -> *` completely polymorphic, and instead only talk concretely
about the singleton type with kind `k1`. This might sound strange at first, as
`c` and `f1` are the only two type parameters to `Dict1`. But as it often happens
when working with singleton types, we are not particularly interested in the
types involved, but in their kinds instead. So, this is the `Dict1` instance
you often want to write:

```haskell
instance (c (f1 'Small), c (f1 'Big)) => Dict1 c f1 where
  dict1 = \x -> case x of
    SSmall -> Dict
    SBig -> Dict
```

That instance says that for any choice of `c` and `f1 :: Size -> *`, if an
instance for `c (f1 a)` exists for a specific choice of `a`, then, given a term
level representation for that `a` and the aid of `dict1`, said instance can be
looked up at runtime.

Notice that `Some1` itself doesn't have any requirements about `Dict1`, it's the
various instances for `Some1` who rely on `Dict1`. `Dict1` has nothing to do
with `Some1`, nor with the choice of `f` nor with the choice of `c`; it is only
related to the singleton type used as a type-index for `f`.

As of this writing, we can find some ready-made instances for `Some1`, `Some2`,
`Some3` and `Some4` in the following modules, which you need to import so as to
bring to scope the desired instances at their usage site:

* Package `exinst`, module `Exinst.Instances.Base`: Instances for various
  type-classes found in the `base` package: `Eq`, `Ord`, `Show`.

* Package `exinst-aeson`, module `Exinst.Instances.Aeson`: Instances for
  `FromJSON` and `ToJSON` from the `aeson` package.

* Package `exinst-bytes`, module `Exinst.Instances.Bytes`: Instances for
  `Serial` from the `bytes` package.

* Package `exinst-hashable`, module `Exinst.Instances.Hashable`: Instances for
  `Hashable` from the `hashable` package.

* Package `exinst-deepseq`, module `Exinst.Instances.DeepSeq`: Instances for
  `NFData` from the `deepseq` package.

You are invited to read the instance heads for said instances so as to understand
what you need to provide in order to get those instances “for free”. As a rule of
thumb, most instances will require this: If you expect to have an instance for
`class Y => Z a` satisfied for `Some1 (f :: k -> *)`, then make sure an instance
for `Z` is available for the `DemoteRep ('KProxy :: KProxy k)`, that a `Dict1 Z
(f :: k -> *)` or more general instance exists, and that the `Y` instance for
`Some1 (f :: k -> *)` exists too.

> TODO: Have something similar to `Dict1` and friends for working with
> non-singleton types, possibly integrating with 'Data.Constraint.Forall.ForallT'
> if it made sense to do so.

Here is the full code needed to have, say, the `Eq`, `Show`, `ToJSON` and
`FromJSON` instances available for `Some1 Receptacle`:

```haskell
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import qualified Data.Aeson as Ae
import           Data.Constraint (Dict(Dict))
import qualified Data.Singletons.TH
import           Exinst.Singletons (Dict1(dict1))

-----

data Size = Big | Small
  deriving (Eq, Show)

Data.Singletons.TH.genSingletons [''Size]
Data.Singletons.TH.singDecideInstances [''Size]

instance Ae.ToJSON Size where
  toJSON = \x -> case x of
    Small -> Ae.toJSON ("Small" :: String)
    Big -> Ae.toJSON ("Big" :: String)

instance Ae.FromJSON Size where
  parseJSON = Ae.withText "Size" $ \t -> case t of
     "Big" -> return Big
     "Small" -> return Small
     _ -> fail "Unknown"


instance (c (f 'Big), c (f 'Small)) => Dict1 c f where
  dict1 = \x -> case x of
    SBig -> Dict
    SSmall -> Dict

-----

data Receptacle (a :: Size) :: * where
  Vase :: Receptacle 'Small
  Glass :: Receptacle 'Small
  Barrel :: Receptacle 'Big

deriving instance Eq (Receptacle a)
deriving instance Show (Receptacle a)

instance Ae.ToJSON (Receptacle a) where
  toJSON = \x -> case x of
    Vase -> Ae.toJSON ("Vase" :: String)
    Glass -> Ae.toJSON ("Glass" :: String)
    Barrel -> Ae.toJSON ("Barrel" :: String)

instance Ae.FromJSON (Receptacle 'Small) where
  parseJSON = Ae.withText "Receptacle 'Small" $ \t -> case t of
     "Vase" -> return Vase
     "Glass" -> return Glass
     _ -> fail "Unknown"

instance Ae.FromJSON (Receptacle 'Big) where
  parseJSON = Ae.withText "Receptacle 'Big" $ \t -> case t of
     "Barrel" -> return Barrel
     _ -> fail "Unknown"
```

Now, provided that we import `Exinst.Instances.Base` and
`Exinst.Instances.Aeson`, `Some1 Receptacle` will have `Eq`, `Show`, `FromJSON`
and `FromJSON` instances:

```
> import Exinst.Instances.Base ()
> import Exinst.Instances.Aeson ()

> -- Trying `fromSome1`.
> fromSome1 (some1 Vase) == Just Vase
True
> fromSome1 (some1 Vase) == Just Glass
False
> fromSome1 (some1 Vase) == Just Barrel
False

> -- Trying `withSome1`
> withSome1 (some1 Vase) show
"Vase"
> withSome1 (some1 Vase) (== Vase)    -- This will fail, use `fromSome1`
                                      -- if you know you are expecting
                                      -- a `Receptacle 'Small`

> -- Trying the `Eq` instance.
> some1 Vase == some1 Vase
True
> some1 Vase == some1 Glass
False
> some1 Vase == some1 Barrel
False

> -- Trying the `Show` instance.
> show (some1 Vase)
"Some1 Small Vase"
> map show [some1 Vase, some1 Glass, some1 Barrel]
["Some1 Small Vase","Some1 Small Glass","Some1 Big Barrel"]

> -- Trying the `ToJSON` and `FromJSON` instances.
> Ae.encode (some1 Vase)
"[\"Small\",\"Vase\"]"  -- Just like in Show, the ToJSON adds some information
                        -- about the Size type-index. That's why we require
                        -- Size to provide a ToJSON instance too.
> Ae.decode (Ae.encode (some1 Vase)) == Just (some1 Vase)
True
> Ae.decode (Ae.encode (some1 Vase)) == Just (some1 Glass)
False
```


## About `Some2`, `Some3` and `Some4`.

Just like `Some1` hides the last singleton type index from fully applied
type-indexed type, `Some2` hides the last two type indexes, `Some3` hides the
last three, and `Some3` hides the last four. They can be used in the same way as
`Some1`.

Like as most instances for `Some1` require `Dict1` instances to be present for
their singleton type-index, most instances for `Some2`, `Some3` and `Some4` will
require that `Dict2`, `Dict3` or `Dict4` instances exist, respectively. Writing
these instances is very straightforward. Supposing you have a type `X :: T4 ->
T3 -> T2 -> T1 -> *` and want to existentialize all of the four type indexes yet
be able to continue using all of its instances, we can write something like
this:

```haskell
instance (c (f1 'T1a), c (f1 'T1b)) => Dict1 c (f1 :: T1 -> *) where
  dict1 = \x -> case x of { ST1a -> Dict; ST1b -> Dict }
instance (Dict1 c (f2 'T2a), Dict1 c (f2 'T2b)) => Dict2 c (f2 :: T2 -> k1 -> *) where
  dict2 = \x -> case x of { ST2a -> dict1; ST2b -> dict1 }
instance (Dict2 c (f3 'T3a), Dict2 c (f3 'T3b)) => Dict3 c (f3 :: T3 -> k2 -> k1 -> *) where
  dict3 = \x -> case x of { ST3a -> dict2; ST3b -> dict2 }
instance (Dict3 c (f4 'T4a), Dict3 c (f4 'T4b)) => Dict4 c (f4 :: T4 -> k3 -> k2 -> k1 -> *) where
  dict4 = \x -> case x of { ST4a -> dict3; ST4b -> dict3 }
```

That is, assuming the following `T1`, `T2`, `T3` and `T4`:

```haskell
data T4 = T4a | T4b
data T3 = T3a | T3b
data T2 = T2a | T2b
data T1 = T1a | T1b
```

Effectively, we wrote just one instance per singleton type per type-index
position, each of them promoting a term-level representation of a singleton
type to its type-level representation and forwarding the rest of the work to
a “smaller” dict. That is, `dict4` reifies the type of the fourth-to-last
type-index of `X` and then calls `dict3` to do the same for the third-to-last
type-index of `X` and so on. Notice, however, how we didn't need to mention `X`
in none of the instances above: As we said before, these instances are
intended to work for any choice of `c`, `f4`, `f3`, `f2` and `f1`.

> TODO: See if instead of having `Some1`, `Some2`, `Some3`, `Some4`, and their
> respective `Dict1`, `Dict2`, `Dict3` and `Dict4`, etc., we can have a single
> `SomeN` and a single `DictN` working out the number of parameters using
> type-level natural numbers.

## Converting `Some1 (f :: k -> *)` to `f (a :: k)`.

If you have a `Some1 (f :: k -> *)` and you know, statically, that you need an
specific `f (a :: k)`, then you can use `fromSome1` which will give you an
`f (a :: k)` only if `a` was the type that was existentialized by `Some1`.
Using `fromSome1` requires that the singleton type-index implements
`Data.Singletons.Decide.SDecide`, which can be derived mechanically with TH by
means of `Data.Singletons.TH.singInstance`.

If you don't know, statically, the type of `f (a :: k)`, then you can use
`withSome1Sing` or `withSome1` to work with `f (a :: k)` as long as `a` never
leaves their scope (don't worry, the compiler will yell at you if you try to do
that).


# Library implementors: Writing instances for `Some1` and friends.

Instances for `Some1` seem to come out of thin air, but the truth is that they
need to be written at least once by library authors so that, provided all its
requirements are satisfied, they are made available.

When we imported `Exinst.Instances.Base` before, we brought to scope, among
other things, the `Show` instance for `Some1`, which is defined as this:

```haskell
-- Internal wrapper so that we don't have to write the string manipulation parts
-- in the 'Show' instance by hand.
data Some1'Show r1 x = Some1 r1 x deriving (Show)

instance forall (f1 :: k1 -> *)
  . ( SingKind ('KProxy :: KProxy k1)
    , Show (DemoteRep ('KProxy :: KProxy k1))
    , Dict1 Show f1
    ) => Show (Some1 f1)
  where
    showsPrec n = \some1 -> withSome1Sing some1 $ \sa1 (x :: f1 a1) ->
       case dict1 sa1 :: Dict (Show (f1 a1)) of
          Dict -> showsPrec n (Some1 (fromSing sa1) x)
```

This code should be relatively straightforward if you are familiar with uses of
the `singletons` and `constraints` libraries. We are simply reifying singleton
types from their term-level representation to their type-level representation,
and afterwards using the `Dict1` mechanism to lookup the required instances
during runtime. Additionaly, this instance requires that the term level
representation of the singleton type implements `Show` too, as, like we saw in a
previous example, the type index itself is shown in this `Show` implementation,
in the hope that it can be later recovered and reified to the type level when
using `Read`.


# Related work on Generic instances for GADTs

One of the most appealing applications of `exinst` is to reduce the boilerplate
associated with manually writing instances for existentialized GADTs. However,
quite often, writing instances for said GADTs on its own is very cumbersome
due to the lack of generic instance deriving mechanisms for GADTs. There exists,
however, at the time of this writing, at least one library able to derive
generic representations for some GADTs using TH:
[`instant-generics`](https://hackage.haskell.org/package/instant-generics).

Combining [`instant-generics`](https://hackage.haskell.org/package/instant-generics) (and
[`instant-aeson`](https://hackage.haskell.org/package/instant-aeson),
[`instant-hashable`](https://hackage.haskell.org/package/instant-hashable),
[`instant-bytes`](https://hackage.haskell.org/package/instant-bytes) and
[`instant-deepseq`](https://hackage.haskell.org/package/instant-deepseq))
with [`exinst-generics`](https://hackage.haskell.org/package/exinst-generics) (and
[`exinst-aeson`](https://hackage.haskell.org/package/exinst-aeson),
[`exinst-hashable`](https://hackage.haskell.org/package/exinst-hashable),
[`exinst-bytes`](https://hackage.haskell.org/package/exinst-bytes) and
[`exinst-deepseq`](https://hackage.haskell.org/package/exinst-deepseq)),
you can reduce a lot of the boilerplate associated with working with GADTs, in
particular when it comes to the serialization and deserialization of them (i.e.,
`Show` and `Read`, or `ToJSON` and `FromJSON`) or puting GADTs in monomorphic
containers (i.e., `[]` or `HashMap`), which become straightforward things to do
once you are able able to both generically derive the instances for your GADT
and then existentialize away the type-index while keeping the underlying
instances available.
