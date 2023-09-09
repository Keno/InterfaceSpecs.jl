struct InferenceEngine <: ProofEngine; end

# TODO: This is just a quick and dirty demo. It does not handle recursive `forall`
# calls.
@MethodTable InferenceOverlay

const infcheck = @overlaypass InferenceOverlay

function (::typeof(infcheck))(::typeof(forall), prop, T)
    if !isa(T, Type)
        T = typeof(T)
    end
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

include("inference/overlayinterp.jl")
@eval function check(::InferenceEngine, prop::InterfaceCheck)
    # Assemble overlay table according to the interface
    (; iface) = prop
    mtable = ccall(:jl_new_method_table, Any, (Any, Any), :overlay, @__MODULE__)
    interp = InterfaceCheckInterpreter(mtable)
    for (sig, rt) in iface.signatures
        mthd = ccall(:jl_new_method_uninit, Any, (Any,), @__MODULE__)
        mthd.sig = sig
        mthd.external_mt = mtable
        mthd.nargs = length(sig.parameters)
        mthd.primary_world = interp.world
        ccall(:jl_method_table_insert, Cvoid, (Any, Any, Ptr{Cvoid}), mtable, mthd, C_NULL)
        assumed_effects = CC.encode_effects(CC.Effects(CC.EFFECTS_UNKNOWN, nothrow=true))
        mi = CC.specialize_method(mthd, mthd.sig, Core.svec())
        interp.code_cache.decl[mthd] = Core.CodeInstance(mi,
            rt, nothing, true, Int32(0), interp.world, interp.world,
            assumed_effects, assumed_effects, nothing, UInt8(0))
    end

    # TODO: This is too conservative, becuase it checks full :nothrow not just no-MethodError
    effects = Base.infer_effects(prop.checksig.parameters[1].instance, Tuple{prop.checksig.parameters[2:end]...}; interp=interp)
    if !Core.Compiler.is_nothrow(effects)
        error("Inference was unable to prove nothrow for prop $prop")
    end
    F = Fact{Base.get_world_counter()}
    $(Expr(:new, :F, :prop))
end

@eval function check(::InferenceEngine, prop::Interface)
    interp = InterfaceCheckInterpreter(nothing; inf_params=CC.InferenceParams(
        max_methods=-1
    ))
    for (sig, rt) in prop.signatures
        computed_rt = Base._return_type(interp, sig)
        if !(computed_rt <: rt)
            error("Inference was only able to prove $computed_rt for $sig, expected $rt")
        end
    end
    F = Fact{Base.get_world_counter()}
    $(Expr(:new, :F, :prop))
end
