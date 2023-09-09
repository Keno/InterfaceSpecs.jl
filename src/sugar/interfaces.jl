export Interface, InterfaceCheck, @interface

const Signature = Type{<:Tuple}

"""

A meta-spec that wraps another spec and checks that the given spec either
succeeds or throws an error of some type other than `T`. In particular,
`errT === Any` is equivalent to the regular spec semantics.
"""
struct DoesNotThrow{T, E}
    spec::T
    errT::E
end
function (spec::DoesNotThrow)()
    try
        spec.spec()
    catch err
        @show err
        @assert !isa(err, spec.errT)
    end
end

"""
    struct CheckSignature

A spec that checks that a method of the given type exists and returns a value
of the specified return type.
"""
struct CheckSignature
    signature::Pair{Signature, Type}
end

function (cs::CheckSignature)()
    (sig, rt) = cs.signature
    forall(args->args[1](Base.tail(args)...)::rt, sig)
    c()
end

"""
    struct Interface

A collection of signatures that comprise an interface. The spec asserts that all
signatures are implemented and do not throw MethodError (but may throw other errors).
"""
struct Interface
    signatures::Tuple{Vararg{Pair{Signature, Type}}}
end
(iface::Interface)() = foreach(sig->DoesNotThrow(CheckSignature(sig), MethodError)(), iface.signatures)

"""
    struct InterfaceCheck

Check that a given method does not throw any `MethodError` under the assumption
that this is true for the given `Interface`.
"""
struct InterfaceCheck
    checksig::Signature
    iface
end
function (ic::InterfaceCheck)()
    forall(ic.iface) do _
        DoesNotThrow(CheckSignature(ic.checksig), MethodError)()
    end
end

macro interface(name, sigs)
    sig = sigs.args[2]
    tup = Expr(:tuple, (
        :(Tuple{
            Core.Typeof($(sig.args[1].args[1])),
            $(map(rs->rs.args[end], sig.args[1].args[2:end])...)} => $(sig.args[2])) for
                sig in filter(x->!isa(x, LineNumberNode), sigs.args))...)
    esc(quote
        function $(name)()
            $(Interface)($tup)
        end
    end)
end
