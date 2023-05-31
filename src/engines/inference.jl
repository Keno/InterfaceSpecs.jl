struct InferenceEngine <: ProofEngine; end

# TODO: This is just a quick and dirty demo. It does not handle recursive `forall`
# calls.
@MethodTable InferenceOverlay

const infcheck = @overlaypass InferenceOverlay

function prop_for_type(prop, x)
    prop(x)()
end

function (::typeof(infcheck))(::typeof(forall), prop, T)
    effects = Base.infer_effects(prop_for_type, Tuple{Core.Typeof(prop), T})
    if !Core.Compiler.is_terminates(effects)
        error("Inference was unable to prove termination for prop $prop")
    end
    if !Core.Compiler.is_nothrow(effects)
        error("Inference was unable to prove nothrow for prop $prop")
    end
end

@eval function check(::InferenceEngine, prop)
    infcheck(prop)
    F = Fact{Base.get_world_counter()}
    $(Expr(:new, :F, :prop))
end
