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

## Low-level semantics and their applications

### The intrinsics

The rest of this document discussed a number of different interfaces, APIs and tools
that could be built. However, it has come to my attention that it wasn't clear that
these are all intended to be built on the same primitive (just with various variations
of syntax sugar). Therefore, before we go any further, let's talk about the core intrinsics
in this proposal and their semantics. Readers interested primarily in the high-level interface
may skip ahead.

The core of this package's design is the addition of two intrinsics, `forall` and `exists`.
Both intrinsics are similar in that they operate (semantically) on the abstract space of all
possible julia objects. Of course, there is infinitely many such objects, so these semantics
are not implementable computationally directly, but as discussed below, that does not mean
that an implementation cannot be provided by computing the same semantics in a smarter way.

To understand the semantics of these intrinsics, let us supposed we had an interator `all_julia_objects()`
that (in no particular order) returned all (newly instantiated - see below for notes on mutation)
julia objects (of all possible types currently defined).
Of course this iterator is countably infinite and not implementable (and not part of the proposal),
but it has useful explanatory power.

With this assumption, the semantics of `forall` and `exists` are roughly:

```
function forall(f, π)
    check_effects(π, (:effect_free, :noub, :terminates))
    check_effects(f, (:effect_free_if_inaccessiblemem, :noub, :terminates))
    for x in all_julia_objects()
        # N.B.: The iteration order of `all_julia_objects()` is not defined.
        r = f(try
            π(x...) # Mostly, see semantic exception for type constructors below
        catch
            continue
        end)
        # For convenience and to avoid user error.
        @assert r === nothing || r === true
    end
end

function exists(π)
    check_effects(π, (:effect_free, :noub, :terminates))
    for x in all_julia_objects()
        # N.B.: The iteration order of `all_julia_objects()` is not defined,
        # so different invocations of `exists` may produce different witnesses,
        # and in particular, `exists` is not `:consistent`.
        try
            r = π(x)
            @assert r === nothing || r === true
            return x
        catch
            continue
        end
    end
    throw(#= Implementation defined error type =#)
end
```

These semantics are very similar, so it's possible we may want to unify them as
one intrinsic in the future, but they serve different purposes, so let's treat
them separately for now.

Informally, `forall` executes some test function `f` for all julia objects that
match some criteria specified by the projection function `π`. Similarly, `exists`
produces some julia object that satiesfies the projection function.

Since both the predicate (f) and projection (π) functions are effect-checked, the
primary question is whether a particular predicate or projection will throw an
error for a particular input. This is the basis of the theorem proving capabilities
of these intrinsics.

### A first proof

For example, consider
```
forall(Int) do x
    x + x == 2x
end
```

If this call (semantically) does not error, we have proven the property true
for all values of type `Int`. Even though this example looks simple, there are
a number of subtle points that need addressing.

1. First, why does `Int` work as a projection function here? The trivial answer
is that we have a method `Int(x::Int) = x`. The semantic `all_julia_objects`,
includes integers, and since that method does not throw, all integers are selected
as the predicate set. Of course, there's more than this method that produces integers,
e.g. we also have `Int(x::Float64) = convert(Int, x)`, but the only semantically
determinable question is whether an object is in the predicate set, not how many
times it was selected.

** Pedantic footnoe: (one might object that since we did not check `consistent`,
running the same predicate function twice might error in one instance and return
in the other - however, consistent-cy is defined with respect to a particular
environment state and the absence of mutation guaranteed by `:effect_free`,
combined with the non-definedness of the execution order allows us to assume
egal heap states for egal objects). **

2. At this point one might object that this is a very roundabout way of specifying
that the prdicate set is all values of type `Int`. And indeed, implementations can and should just
look at the type of the projection function and make the appropriate conclusion.
We will see more interesting cases where `π` is not just a type constructor shortly.
However, in the meantime, this does bring us to one important additional semantic wrinkle: At present,
constructors do not in fact fully constrain the possible julia values of a given type
as values can be constructed indirectly using `unsafe_load`, `reinterpret` or `eval(Expr(:new))`.
To address this, we add a *type constructor exception* to our semantics, where if the
predicate is a literal type constructor, we extend the predicate set to all values of
that type, independent of whether they are constructable by their constructors or not.
In the future, core julia might gain a concept of sealed types where bypassing the
constructor is disallowed, in which case this exception would of course only apply to
non-sealed types.

3. Even though the predicate looks trivial (and it basically is), there is actually a fair
bit going on here. In parcticular, dynamic dispatch needs to be performed as usual,
so this is an assertion not just about the behavior of `Int` arithmetic, but also about
the dispatches of `+`, `==` and implicit multiplication.

### Proof checking as compiler optimization

For the particular example we just look at, implementing a correct version of `forall`
turns out to be relatively simple. In particular, the regular julia compilation
pipeline is strong enough to fully resolve this question and turn `x + x == 2x`
into `return true`. Thus, to implement `forall`, we can simply run the regular
compilation pipeline, see if the predicate function is `return true` (or similar)
and then compile `forall` to a no-op.

In fact, this is a general sketch for implementing proof-checkers in this scheme.
The standard julia pipeline is run to ensure the required effects and strip
away julia-specific semantics around multiple-dispatch, etc, providing a
monomorphised version of the code that may then be compiled to any target that
may be capable of performing the proof. In the present example, this was LLVM,
but the pacakge contains experimental bindings to Z3 as well and the intention
is to be as agnostic as possible to the proof backend.

The primary benefit of this setup is that it re-uses the entire compiler stack
built for static compilation of Julia. As in the static compilation case, not
all julia code will be suitable for use as predicate and projection functions.
As such, it is important to have tooling that explains to the user what
limitations they need to impose on their functions in order to be able to use
them with this scheme. By sharing this implementation work across use cases,
the burden on tool development can hopefully be minimized.

### When is `π` is not a type

In all of our examples, we have so far used a type as the projection function.
Let's consider an example where it is not:

```
function ZeroUniversal(T)
    forall(a->iszero(a::T)) do a
        forall(iszero, b->a == b::T)
        forall(b->iszero(b::T)) do b
            a == b
        end
    end
end
```

Or, in English-math-language, we might say:
1. For all `a` of type `T` such that `iszero(a)` (is true), we require that
    a. for all `b` of type `T` such that `a == b`, we have `iszero(b)` (is true) and
    b. for all `b` of type `T` such that `iszero(b)`, we have `a == b` (is true)

Or, said equivalently
1. For all  `a`, `b` of type `T`, `iszero(a)` implies (`iszero(b)` if and only if `a == b`)

In particular, this implies that we have no separate representation of implication in this system,
just a particularly general way of typing the inputs to `forall`. This is of course no ground-breaking
discovery - dependently typed proof systems are built around this kind of observation.
However, as our flavor of the intrinsics is somewhat different, I thought it deserved a specific mention.

### `exists` as a test case synthesizer

So for we have seen `forall`, but not `exists`. `exists` functions as usual in
the quantifier in `forall`-`exists` specs, e.g. we may write

```
forall(Int) do c
    forall(Int) do a
        exists(b->a + b == c)
    end
end
```

but we can also use `exists` without any outer `forall`, in which case it will
of course just synthesize a value that matches the projection.

```
# Make sure that this is a super safe password
julia> safe_password(s::String) = s[1] == s[7] && lowercase(s[2]) == s[4]

julia> exists(safe_password)
"tBRbeAt"

julia> safe_password("tBRbeAt")
true
```

This is the connection to property checking. We can generally use the
same specs written using `forall` and spot check them on particular
instances, either written by the user or synthesized via `exists`.

### `exists` as a proof-check request

Consider the following struct
```
@sealed struct Fact{T}
    spec::T
    valid_worlds #= To support invalidations, ignore for now =#
    proof #= optional for introspection =#
    function Fact(spec)
        check_effects(spec, (:effect_free, :noub, :terminates))
        spec()
        new(spec, get_valid_worlds(), nothing)
    end
end
```

With the above encoding, where we equate true proofs with appropriately
effect-checked functions, the construction of `Fact` requires the truth
of the fact asserted and `exists` may be used to request the system
to attempt a proof:

```
struct Commutativity{T, Op}; end
function (::Commutativity{T, Op})() where {T, Op}
    forall(NTuple{2, T}) do (a, b)
        forall(Op) do op
            op(a, b) == op(b, a)
        end
    end
end
```

```
julia> exists(Fact{Commutativity{Int, +}}) # Request the system to attempt to prove commutativity of +(::Int, ::Int)
#= Fact object with optional proof or error =#
```

### Thoughts on mutation

As described above, the `forall` function requires `:effect_free_if_inaccessiblemem` effects,
i.e. the compiler needs to prove that the predicate does not perform any unbounded heap
mutation. However, heap mutation that is bounded to heap objects whose lifetime does not
exceed the duration of the `forall` body is permitted. This is intended to be a practical
tradeoff that allows predicates that make use of mutation, while still requiring that the
(non)-execution of the `forall` predicate is not externally observable.

A related issue that arises in the presence of mutation and identity is the question of the
range of the `all_julia_objects` pseudo-intrinsics discussed above. As mentioned in the side-node,
we consider newly allocated object hierarchies here, not existing julia objects. In particular,
`exists` will never return a mutable object that aliases an existing mutable object:

```
julia> x = Ref{Int}(0)

julia> exists(y->x === y)
# Errors, no such object
```

The question then arises how to perform verification of side-effecting code, where the side-effect
is an essential part the property to verify. This area needs some further thought, but the current
thought is that this should be done by compining an external transformation that transforms
the unbounded side-effect to a verification-compatible bounded side effect, e.g.:

```
@overlaypass struct ShadowHeap
    heap::IdDict{GlobalRef, Any}
end

@overlay ShadowHeap function getglobal(m::Module, s::Symbol)
    g = nonoverlay(getglobal, m, s)
    isconst(m, s) ? return g : get(getpass().heap, GlobalRef(m, s), g)
end

@overlay ShadowHeap function setglobal!(m::Module, s::Symbol, v)
    getpass().heap[GlobalRef(m, s)] = v
end

# Plus magic definition to turn global ref/set into corresponding intrinsic calls
```

Then, we can query side effects as usual, but the modification is bounded under the hood:
```
exists(s) do
    ShadowHeap() do
        global my_global_val
        setglobal!(@__MODULE__, s, 4)
        my_global_val == 4
    end
end
```

Naturally, any such modeling might need custom integration with the backend proof system
to be properly embeddable. Hopefuly this can be addressed just as any compiler plugin
would.

## One higher-level interface: purely type-based interfaces

A long-standing feature request in the julia community has been the addition
of some notion of higher-level specification of interfaces such as abstract
arrays. There's two primary purposes for this kind of systems:

1. Implementers of new array types want to know that they have correctly implemented
the array type.

2. Users want to know that they are not accidentally relying on implementation details
of a particular `AbstractArray`, but rather that their code is generic over all `AbstractArray`s

One primary issue that is often brought up with this is that in julia, types are used for dispatch,
not correctness, so the mere existence of a particular method does not guarantee that the method
actually conforms to the interface. For `AbstractArray`, one such invariant may be that:
```
A[i] = x
@assert x == A[i]
```
The mere existence of `getindex` and `setindex` do not assure that the array actually does something
with it, only that no immediate method error will be thrown.

Of course, the system we have just presented is perfectly capable of encoding behavioral invariants
in addition to type-based one. However, there are potential robustness challenges. The system just
presented is completely general and it is not completely clear what particular subset of julia
code will be proveable on any particular backend. As a result, the full system may be hard to use
correctly.

One natural way out of this connundrum is to create a more restrictive view into the full system
that is sufficiently powerful to specify interfaces of moderate complexity, but yet restricted
enough to have some chance of being checked automatically. By *view* here, I mean some higher-level
abstraction that provides a restricted version of the capability, but is nevertheless semantically
representable by the same intrinsics and that uses the same top-level entry points as the
full system.

This package includes such an abstraction as the `Interface` spec in the `sugar` directory.
Let us walk through it as both an example of the usage of the lower level interface and a
description of the high-level interface.

First, we must consider what it is that we actually want to prove. There's a few options,
but a reasonable choice is to try to prove that in the cases we're interested in, a method
of the desired signature exists (no MethodErrors get thrown), returns a value of the correct type.
So, let's write such a spec (for a single singature for the time being):

```
struct CheckSignature
    signature::Pair{Signature, Type}
end

function (cs::CheckSignature)()
    (sig, rt) = cs.signature
    forall(args->args[1](Base.tail(args)...)::rt, sig)
end
```

This is basically what we want, but does prohibit all errors, not just `MethodError`.
We can relax this by wrapping in try/catch

```
struct DoesNotThrow{T, E}
    spec::T
    errT::E
end
DoesNotThrow{<:Any,E}(spec) where {E} =
    DoesNotThrow(spec, E.instance)
function (spec::DoesNotThrow)()
    try
        spec.spec()
    catch err
        @show err
        @assert !isa(err, spec.errT)
    end
end
const DoesNotThrowMethodErorr{T} = DoesNotThrow{T, MethodError}
```

Then we just define an interface as a collection of just checked signatures that do not throw:

```
struct Interface
    signatures::Tuple{Vararg{Pair{Signature, Type}}}
end
(iface::Interface)() = foreach(sig->DoesNotThrow(CheckSignature(sig), MethodError)(), iface.signatures)
```

We can also check a particular other method under the assumption that an interface
is correctly defined:

```
struct InterfaceCheck
    checksig::Signature
    iface
end
function (ic::InterfaceCheck)()
    forall(ic.iface) do _
        DoesNotThrow(CheckSignature(ic.checksig), MethodError)()
    end
end
```

To understand why this works, remember from above the correspondence between
projection and implication. In particular, to prove `InterfaceCheck`, it is
not necessary to prove that the relevant interface holds, only that the
predicate holds if the interface does.