# InterfaceSpecs

## Background

Recently there's been a lot of interest in attempting to add more static guarantees
or checking to julia as a language. I think this interest is coming from a number
of different angles, but I've heard at least the following:

1. Some people would like a more formalized way to document the expectations of
   various interfaces that provide extension points. E.g. what exactly does it
   mean to be iterable, to be an array, a random number generator, etc.

2. Some people would like like better tooling to automatically determine whether
   an interface has been implemented correctly or not. E.g. if somebody implements
   a custom lattice for the compiler, it would be nice to make sure that it satisfies
   the basic interface.

3. Some people would like more static guarantees on abstract signatures for optimization
   reasons (e.g. to prevent invalidations)

I think the solutions to these kinds of things fall in roughly two general categories:

1. *Semantic* Solutions: These are changes to the language semantics, for example
    by being able to restrict and check the return types of abstract signatures
    and enforce the implementation.

2. *Non-Semantic* Solutions: In this model, interface specifications never **change** the semantics of a program, but interfaces are useful for the developer as documentation and can potentially be checked at test time (or a separate check stage).

## This package

This package is a playground for my thinking around this topic. I hesitate to even
call it a proposal, because I'm not sure it's going anywhere, but I think there's
some neat ideas that I'd like to explore.

The core idea of this package is that I'd like to provide the ability for users
to write down specifications for the interfaces in a natural, but machine-readable
way and then use these specifications to provide a smooth path from property-based
testing of these specifications to a more formal-methods/type-checking based
approach to software verification.

## Quick Tour

This is a quick tour of the kind of thing you might do with this package. The
examples in this section have been fully implemented and are working, but this
is a thin-thread exercise, and anything other than these example is likely to
not work.

Let's start with a somewhat complicated example. Let's supposed we'd like to
write down what it means to be iterable:

```
@spec ValidIterType(x) begin
    @check isa(x, Union{NTuple{2, Any}, Nothing})
end

@spec Iteration(T::Type) begin
    @∀ x::T->begin
        @check ValidIterType(iterate(x))
        let r = iterate(x)
            # TODO: This is written loop-peeled this way to play nice with the compiler's
            # termination analysis to make sure it can prove this for singleton
            # cases. It would be nice if that wasn't required.
            r = iterate(x, r[2])
            while true
                @check ValidIterType(r)
                r === nothing && break
                r = iterate(x, r[2])
            end
        end
    end
end
```

There's a few things going on there, but this spec basically says the following:

For a type `T` to be iterable, it needs to satisfy the following for all values
of the type:
    - `iterate(x::T)` must exist, have a valid return type, must not throw (nothrow is implicit, see below for explicit error handling support)
    - The same must be true for `iterate(x, s)` where `s` is any iteration state for this type.

Now, what might we be able to do with a spec like this. The simplest thing we could
perhaps do is exhaustively check it. Semantically, this means replacing the
`@∀` macro by a loop over all the instances of type and the `@check` macro by
some sort of `@test` variant. Of course, it is not possible to enumerate all the
values of a type in most cases (either for semantic or for performance reasons),
but let's start there. For example, let's check that the `Bool` type (which is
exhaustively enumerable) correctly implements iteration:

```
julia> compute(Iteration(Bool))
InterfaceSpecs.Fact{0x00000000000082c8}(Iteration(Bool))
```

The returned object indicates the interface `Iteration` is satisfied on `Bool`
(in the world age where this was evaluated). Or said another way, we have proven
the proposition that `Iteration(Bool)` is satisfied (in the given world age) by
computation.

This is all fine and good, but obviously most types can't be exhaustively checked,
as we see when we try the same thing on Int:
```
julia> compute(Iteration(Int))
ERROR: No exhaustive checking for type Int64. Please provide a proof engine.
Stacktrace:
 [1] error(s::String)
   @ Base ./error.jl:35
 [2] forall(prop::var"#11#16", T::Type)
   @ InterfaceSpecs ~/.julia/dev/InterfaceSpecs/src/runtime.jl:54
```

However, in many cases all we want is some confidence that our code is correct,
in which case it might suffice to check a few of the common corner cases. This
kind of approach is often referred to as property-based testing and we can use it
here:
```
julia> check(PropertyCheck(), Iteration(Int))
true
```

This API uses fixed (but user-extensible through multiple dispatch for custom types
for course) lists of examples to check against the interface.

Here's an example to see what happens if we don't satisfy the interface:
```
julia> struct NonIterable; x::Int; end

julia> InterfaceSpecs.testinstances(::Type{NonIterable}) = (NonIterable(1),)

julia> check(PropertyCheck(), Iteration(NonIterable))
ERROR: MethodError: no method matching iterate(::NonIterable)

Closest candidates are:
  iterate(::Union{LinRange, StepRangeLen})
   @ Base range.jl:880
  iterate(::Union{LinRange, StepRangeLen}, ::Integer)
   @ Base range.jl:880
  iterate(::T) where T<:Union{Base.KeySet{<:Any, <:Dict}, Base.ValueIterator{<:Dict}}
   @ Base dict.jl:698
  ...

```

Up to this point, there's really no magic here, just a somewhat fancy way of
writing pre-canned tests for a particular interface that a package author might
use in their own test suite. And that's sort of the point. I think 90% of the
value here is just standardizing what it looks like to write these kinds of
specs for interfaces and giving people the tools to easily check them on a few
cases. Everything else is just bonus fairy dust, but since it's cool let's take
a look at it anyway.

As people many people know, the julia compiler has a fairly strong static model
of what it means to be julia code, so it is natural that we can use it to prove
things about Julia code. For example, in this case, the julia compiler is strong
enough to prove that `Int` satisfies the iteration interface:

```
julia> check(InferenceEngine(), Iteration(Int))
InterfaceSpecs.Fact{0x00000000000082ce}(Iteration(Int64))

julia> check(InferenceEngine(), Iteration(NonIterable))
ERROR: Inference was unable to prove termination for prop #11
```

Note that here `check` returned a `Fact` (as `compute` did in the first example),
because the proposition was proven correct. For the two property-based tests,
it just returned `true`, because even though it did not fail, it did not actually
prove that there are no failing cases.

This latest example, also introduces the concept of `engines`, which can be used
to either test or prove props. One major design point of this package is that I
do not really want to be in the business of telling people how to write their
test or proof packages, I just want to provide an integration point that serves
as the clearing house between the spec authors and the people working on
test/verification packages (of course in practice things might turn out messier,
but let's start there). Software verification is a notoriously hard thing to do
and it would be unsurprising to need to use different engines to prove different
kinds of propositions (or even parts of the same proposition), which this
interface is intended to provide.

Of particular note is that these specs do not operate solely on the type domain
(though of course if you expect to use julia inference to prove things, they
better be mostly type-domain queries). For example, here is a spec that might say
something about the correctness of the iseven function:

```
@spec MultIsEven(T::Type) begin
    @∀ x::T->iseven(x * 2)
end
```

As before, we can establish this for `Bool`, but not `Int` by direct computation:
```
julia> compute(MultIsEven(Bool))
InterfaceSpecs.Fact{0x00000000000082d7}(MultIsEven(Bool))

julia> compute(MultIsEven(Int))
ERROR: No exhaustive checking for type Int64. Please provide a proof engine.
Stacktrace:
 [1] error(s::String)
   @ Base ./error.jl:35
 [2] forall(prop::var"#31#36", T::Type)
   @ InterfaceSpecs ~/.julia/dev/InterfaceSpecs/src/runtime.jl:54
```

However, in this case, Julia's type inference cannot prove the proposition,
because the julia inference lattice does not model these properties in the
value domain:

```
check(InferenceEngine(), MultIsEven(Int))
ERROR: Inference was unable to prove nothrow for prop #5
Stacktrace:
 [1] error(s::String)
   @ Base ./error.jl:35
 [2] (::InterfaceSpecs.var"##InferenceOverlay#295")(#unused#::typeof(InterfaceSpecs.forall), prop::Function, T::Type)
   @ InterfaceSpecs ~/.julia/dev/InterfaceSpecs/src/engines/inference.jl:19
```

We need something stronger, so why not just go all the way to an SMT solver:
```
julia> check(Z3Engine(), MultIsEven(Int))
InterfaceSpecs.Fact{0x00000000000082c4}(MultIsEven(Int64))
```

Here again, we were able to prove the prop conclusively, so we got back a `Fact`.

## Implementation Details

Semantically, a `prop` that can be checked is nothing other than a julia
function that takes no arguments. The prop is considered "true" if the function
terminates and does not error. In addition, this packages provides the `forall`
intrinsic (currently, potentially more things in the future), that encodes a
semantic map over all instances of a type, which is of course not in general
possible to evaluate. Testers/Solvers then provide their own implementation for `forall`.

The macro DSL is provided for convenience, but it should always be possible to
write props manually. E.g. the above iteration spec can be written as:
```
struct Iteration
    T::Type
end

struct Iteration_x{T}
    x::T
end

function (this::Iteration)()
    forall(Iteration_x, this.T)
end

function (this::Iteration_x)()
    ValidIterType(iterate(this.x))()
    r = iterate(this.x)
    r = iterate(this.x, r[2])
    while true
        ValidIterType(r)()
        r === nothing && break
        r = iterate(this.x, r[2])
    end
end
```

## Not yet implemented things I'd like to do

### Restricted Ranges
I would like to be able to specify specs on a subset of easily, e.g.
```
@spec DivCorrect(T::Type) begin
    @∀ x::(T|!iszero(x))->isone(div(x, x))
end
```

### Error Annotations
Similar to the above, but implicitly by specifying possibly thrown errors:
```
@spec DivCorrect(T::Type) begin
    @∀ x::T->isone(div(x, x)) @throws DivideByZeroError
end
```

(i.e. either the spec is satisfied or throws an error of the given type)

### Associating specs with abstract types

I think this is reasonably simple, but there should be some way to provide a
`spec` for an abstract type, and a simple function that package authors can
call to check the associated specs for all the types that they implement in
their functions.

### Checking function against specs

Similar to, but the opposite of the previous. If a package author writes an
abstractly typed function, it should be possible to check for the absence
of method errors assuming that a concrete instance of a type implements its
spec. I don't think this is particularly hard to do either, but not implemented here.

### Mixing solver engines

In current versions of julia, inference is not very good at proving termination,
which is required in the current semantics. As a result, the usefulness of the
inference solver is limited for any prop that involves iteration, e.g.

```
julia> check(InferenceEngine(), Iteration(NTuple{2, Int}))
ERROR: Inference was unable to prove termination for prop #21
Stacktrace:
 [1] error(s::String)
   @ Base ./error.jl:35
 [2] (::InterfaceSpecs.var"##InferenceOverlay#295")(#unused#::typeof(InterfaceSpecs.forall), prop::Function, T::Type)
   @ InterfaceSpecs ~/.julia/dev/InterfaceSpecs/src/engines/inference.jl:16
```

However, many of these termination checks are rather trivial and some of the other
solver engines should be able to take care of them no problem. It would thus be nice
to have a way for inference to request a `prop` be solved using some other
(potentially more powerful) solver engine in order to satisfy its goals.

## Encoding various common scenarios

### Simple type-based interfaces

One of the most common scenarios is probably just a list of methods that must
exist for a particular, e.g.

```
@spec ArrayMethods(T <: AbstractArray) begin
    @∀ (A::T)->begin
        @check(isa(axes(A), Tuple))

        # Linear indexing - the other indexing are more complicated, because
        # the signatures are dimension dependent
        @∀ i::Int->A[i]::eltype(A) @throws BoundsError
    end
end
```

we might consider a shorthand for this like:
```
@interface ArrayMethods(T <: AbstractArray) begin
    axes(::T)::Tuple
    getindex(::T, ::Int)::eltype(A) @throws BoundsError
end
```

Ideally the macro would be restricted in such way that the `InferenceEngine`
prover is likely to succeed in most simple cases (though of course inference
being able to prove things can in general depend on arbitrary computation).


### Contracts
We might want to provide another special case for contracts, e.g.:

```
@contract function sort(a::A)
    @requires WellOrdered(eltype(a))
    @provides issorted(Ω)
    [...]
end
```

which would just be a short hand for
```
function sort(x::T)
    [...]
end

@spec sort_well_formed(A::Type)
    @∀ (a::A|WellOrdered(eltype(a)))->issorted(sort(a))
end
```
