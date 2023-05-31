using CassetteOverlay

function testinstances end

testinstances(::Type{Bool}) = (true, false)
for T in Base.BitInteger_types
    @eval testinstances(::Type{$T}) = (zero($T), one($T), one($T)+one($T), -one($T), typemin($T), typemax($T))
end
testinstances(::Type{Float32}) = (NaN32, 0f0, 1f0, Inf32, -Inf32, typemin(Float32), typemax(Float32))
testinstances(::Type{Float64}) = (NaN, 0., 1., Inf, -Inf, typemin(Float64), typemax(Float64))

function testinstances(TT::Type{<:Tuple})
    Base.isvarargtype(TT.parameters[end]) && error("Error property testing not supported for vararg tuples.")
    Iterators.product(ntuple(length(TT.parameters)) do i
        (x for x in testinstances(TT.parameters[i]))
    end)
end

"""
    struct PropertyCheck <: TestEngine

`PropertyCheck` is a simple test engine that checks the given prop on a fixed
list of instances for a given type. As a `TestEngine`, it does not provide
exhaustive guarantees, but is useful for quick testing of specs and interfaces.
"""
struct PropertyCheck <: TestEngine; end

@MethodTable PropCheckOverlay

@overlay PropCheckOverlay function forall(prop, T)
    for x in testinstances(T)
        prop(x)()
    end
    return
end

const propcheck = @overlaypass PropCheckOverlay

function check(::PropertyCheck, prop)
    propcheck(prop)
    return true
end
