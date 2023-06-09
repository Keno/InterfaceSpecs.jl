module InterfaceSpecs

export PropertyCheck, RandomCheck, InferenceEngine, Z3Engine, check, compute, @spec

abstract type Proof; end

abstract type Engine; end
abstract type TestEngine <: Engine; end
abstract type ProofEngine <: Engine; end

include("dsl.jl")
include("runtime.jl")
include("engines/property.jl")
include("engines/randomized.jl")
include("engines/inference.jl")
include("engines/z3.jl")

function check(::Engine, prop)
    error("Checking not implemented for this engine")
end

end
