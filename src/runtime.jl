"""
    Fact{Age}

In `InterfaceChecks`, propositions are simply julia functions that may make use
of a number of additional primitives (such as `forall`, which requires that
a statement be true for all instances for all instances of a type). We say a
proposition is true if it can run to completion without error. For propositions
that do not make use of the primitives and that complete in reasonable time,
this can of course be shown by simply evaluation the proposition. However,
this is in general not possible and more sophisticated techniques are required.

A successfully constructed `Fact` object indicates that contained `prop` is true
(in the above sense) in the given world age, but does not contain any information
as to why it is true.
"""
struct Fact{Age}
    prop

    global compute
    function compute(prop)
        prop()
        new{Base.get_world_counter()}(prop)
    end

    # Engines are allowed to use the @eval hack to define additional constructors
    # for this. However, by doing so, they are implicitly adding to the trusted
    # computing base.
end

abstract type VerificationError <: Exception end
struct CheckFail <: VerificationError
    prop
end

"""
    compute(prop)

Checks a prop by direct evaluation.
"""
compute

"""
    forall(prop, T)

Checks the proposition `prop` (more precisely, the proposition returned by
the closure `prop`, which is often a prop constructor) for all elements of the
type `T`. Since exhaustive testing is generally not possible, this usually
requires proof engine support.
"""
function forall(prop, T)
    if Base.issingletontype(T)
        return compute(prop(T))
    end
    error("No exhaustive checking for type $T. Please provide a proof engine.")
end

function forall(prop, T::Type{Bool})
    prop(true)()
    prop(false)()
end

struct IsCheckPass
    x::Union{Bool, Nothing}
end
@noinline function (this::IsCheckPass)()
    this.x || throw(CheckFail(this))
    return nothing
end
