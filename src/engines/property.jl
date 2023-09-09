using CassetteOverlay
import InteractiveUtils

function testinstances end

testinstances(::Type{Bool}) = (true, false)
for T in Base.BitInteger_types
    @eval testinstances(::Type{$T}) = $T[zero($T), one($T), one($T)+one($T), -one($T), typemin($T), typemax($T)]
end
testinstances(::Type{BigInt}) = BigInt[zero(BigInt), one(BigInt), one(BigInt)+one(BigInt), -one(BigInt)]
testinstances(::Type{Float16}) = Float16[NaN16, Float16(0), Float16(1), Inf16, -Inf16, typemin(Float16), typemax(Float16)]
testinstances(::Type{Float32}) = Float32[NaN32, 0f0, 1f0, Inf32, -Inf32, typemin(Float32), typemax(Float32)]
testinstances(::Type{Float64}) = Float64[NaN, 0., 1., Inf, -Inf, typemin(Float64), typemax(Float64)]
testinstances(::Type{BigFloat}) = BigFloat[big"NaN", big"0.", big"1.", big"Inf", big"-Inf"]

# Opt out for now
testinstances(::Type{Base.MultiplicativeInverses.SignedMultiplicativeInverse}) =
    map(Base.MultiplicativeInverses.SignedMultiplicativeInverse, filter(!iszero, testinstances(Int64)))

testinstances(::Type{Base.MultiplicativeInverses.UnsignedMultiplicativeInverse}) =
    map(Base.MultiplicativeInverses.UnsignedMultiplicativeInverse, filter(!iszero, testinstances(UInt64)))

function testinstances(TT::Type{<:Tuple})
    Base.isvarargtype(TT.parameters[end]) && error("Error property testing not supported for vararg tuples.")
    Iterators.product(ntuple(length(TT.parameters)) do i
        (x for x in testinstances(TT.parameters[i]))
    end...)
end

struct NoInstancesError
    T
end

function testinstances(T::Type)
    Base.issingletontype(T) && return T[T.instance,]
    if Base.isabstracttype(T)
        return reduce(vcat, map(testinstances, InteractiveUtils.subtypes(T)))
    end
    if Base.isstructtype(T) && Base.isconcretetype(T)
        return map(Iterators.product(map(testinstances, fieldtypes(T)))) do fields
            eval(Expr(:new, T, fields...))
        end
    end
    if isa(T, UnionAll)
        return reduce(vcat, map(sT->T{sT}, InteractiveUtils.subtypes(T.var.ub)))
    end
    error("PropCheck does not know how to construct instances of $(T)")
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
    instances = try
        nonoverlay(testinstances, T)
    catch err
        @error "Error during test instance creation" err=err
        rethrow(err)
    end
    for x in instances
        prop(x)()
    end
    return
end

const propcheck = @overlaypass PropCheckOverlay

function check(::PropertyCheck, prop)
    propcheck(prop)
    return true
end
