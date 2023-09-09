const CC = Core.Compiler

struct InterfaceCheckCache
    # Inferred methods
    dict::IdDict{CC.MethodInstance,CC.CodeInstance}
    # Assumed results for declared methods
    decl::IdDict{Method, CC.CodeInstance}
end
InterfaceCheckCache() = InterfaceCheckCache(IdDict{CC.MethodInstance,CC.CodeInstance}(),
                                            IdDict{Method, CC.CodeInstance}())

struct InterfaceCheckInterpreter <: CC.AbstractInterpreter
    interface_assumptions::Union{Core.MethodTable, Nothing}

    world::UInt

    inf_params::CC.InferenceParams
    opt_params::CC.OptimizationParams
    inf_cache::Vector{CC.InferenceResult}
    code_cache::InterfaceCheckCache
    function InterfaceCheckInterpreter(interface_assumptions::Union{Core.MethodTable, Nothing};
            world::UInt = Base.get_world_counter(),
            inf_params::CC.InferenceParams = CC.InferenceParams(),
            opt_params::CC.OptimizationParams = CC.OptimizationParams(),
            inf_cache::Vector{CC.InferenceResult} = CC.InferenceResult[],
            code_cache::InterfaceCheckCache = InterfaceCheckCache())

        new(interface_assumptions, world, inf_params, opt_params, inf_cache, code_cache)
    end
end

CC.InferenceParams(interp::InterfaceCheckInterpreter) = interp.inf_params
CC.OptimizationParams(interp::InterfaceCheckInterpreter) = interp.opt_params
CC.get_world_counter(interp::InterfaceCheckInterpreter) = interp.world
CC.get_inference_cache(interp::InterfaceCheckInterpreter) = interp.inf_cache
CC.code_cache(interp::InterfaceCheckInterpreter) = CC.WorldView(interp.code_cache, CC.WorldRange(interp.world))
CC.setindex!(wvc::CC.WorldView{InterfaceCheckCache}, ci::CC.CodeInstance, mi::CC.MethodInstance) = setindex!(wvc.cache.dict, ci, mi)

function CC.method_table(interp::InterfaceCheckInterpreter)
    if interp.interface_assumptions !== nothing
        return CC.OverlayMethodTable(CC.get_world_counter(interp), interp.interface_assumptions)
    else
        return CC.InternalMethodTable(CC.get_world_counter(interp))
    end
end

function CC.haskey(wvc::CC.WorldView{InterfaceCheckCache}, mi::CC.MethodInstance)
    haskey(wvc.cache.decl, mi.def) ||
    haskey(wvc.cache.dict, mi)
end

function CC.get(wvc::CC.WorldView{InterfaceCheckCache}, mi::CC.MethodInstance, default)
    get(wvc.cache.decl, mi.def, get(wvc.cache.dict, mi, default))
end

function CC.getindex(wvc::CC.WorldView{InterfaceCheckCache}, mi::CC.MethodInstance)
    haskey(wvc.cache.decl, mi.def) && return getindex(wvc.cache.decl, mi.def)
    return getindex(wvc.cache.dict, mi)
end

