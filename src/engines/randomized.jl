using CassetteOverlay, Random

"""
    struct RandomCheck <: TestEngine

`RandomCheck` is a simple test engine that checks the given prop on a (configurable number)
of randomized instances of a given type. As a `TestEngine`, it does not provide
exhaustive guarantees, but is useful for quick testing of specs and interfaces.
"""
struct RandomCheck{R} <: TestEngine
    rng::R
    ninstances::Int
end
RandomCheck() = RandomCheck(Random.GLOBAL_RNG, 5)
ninstances(r::RandomCheck) = r.ninstances
rng(r::RandomCheck) = r.rng

@MethodTable RandomCheckOverlay

@overlay RandomCheckOverlay function forall(prop, T)
    for _ = 1:ninstances()
        let x = rand(rng(), T)
            prop(x)()
        end
    end
    return
end

struct RandCheckPass <: CassetteOverlay.OverlayPass
    engine::RandomCheck
end
(pass::RandCheckPass)(::typeof(ninstances)) = ninstances(pass.engine)
(pass::RandCheckPass)(::typeof(rng)) = rng(pass.engine)

const randcheck = @overlaypass RandCheckPass RandomCheckOverlay

function check(::RandomCheck, prop)
    propcheck(prop)
    return true
end
