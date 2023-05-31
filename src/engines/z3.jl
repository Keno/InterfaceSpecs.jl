using z3_jll

mutable struct Z3_config
    ptr::Ptr{Nothing}
    function Z3_config()
        ptr = @ccall z3_jll.libz3.Z3_mk_config()::Ptr{Cvoid}
        this = new(ptr)
        finalizer(this) do c
            @ccall z3_jll.libz3.Z3_del_config(c.ptr::Ptr{Cvoid})::Ptr{Cvoid}
        end
        return this
    end
end

mutable struct Z3_context
    ptr::Ptr{Nothing}
    function Z3_context(config::Z3_config)
        ptr = @ccall z3_jll.libz3.Z3_mk_context(config::Ptr{Cvoid})::Ptr{Cvoid}
        this = new(ptr)
        finalizer(this) do c
            @ccall z3_jll.libz3.Z3_del_context(c.ptr::Ptr{Cvoid})::Ptr{Cvoid}
        end
        return this
    end
end
Z3_context() = Z3_context(Z3_config())

# These types are owned by the context and don't have independent memory management
const Z3_ctx_owned_types = (:Z3_ast, :Z3_sort, :Z3_symbol, :Z3_goal, :Z3_solver)
for T in Z3_ctx_owned_types
    @eval mutable struct $T
        ptr::Ptr{Nothing}

        # Root
        ctx::Z3_context
    end
end

const Z3Types = Union{Z3_config, Z3_context, map(s->getglobal(@__MODULE__, s), Z3_ctx_owned_types)...}

# Memory ownership/unwrapping
Base.unsafe_convert(::Type{Ptr{Nothing}}, z::Z3Types) = z.ptr


# Symbol Creation
mk_int_symbol(ctx::Z3_context, i::Integer) = Z3_symbol((@ccall z3_jll.libz3.Z3_mk_int_symbol(ctx::Ptr{Cvoid}, i::Cint)::Ptr{Cvoid}), ctx)
mk_string_symbol(ctx::Z3_context, s::AbstractString) = Z3_symbol((@ccall z3_jll.libz3.Z3_mk_string_symbol(ctx::Ptr{Cvoid}, s::Cstring)::Ptr{Cvoid}), ctx)

# Sort Creation
mk_bv_sort(ctx::Z3_context, sz::Int) = Z3_sort((@ccall z3_jll.libz3.Z3_mk_bv_sort(ctx::Ptr{Cvoid}, sz::Cuint)::Ptr{Cvoid}), ctx)

# goals
mk_goal(ctx::Z3_context, models::Bool, unsat_cores::Bool, proofs::Bool) = Z3_goal((@ccall z3_jll.libz3.Z3_mk_goal(ctx::Ptr{Cvoid}, models::Cint, unsat_cores::Cint, proofs::Cint)::Ptr{Cvoid}), ctx)

# solvers
mk_solver(ctx::Z3_context) = Z3_solver((@ccall z3_jll.libz3.Z3_mk_solver(ctx::Ptr{Cvoid})::Ptr{Cvoid}), ctx)
solver_assert(ctx::Z3_context, s::Z3_solver, a::Z3_ast) = (@ccall z3_jll.libz3.Z3_solver_assert(ctx::Ptr{Cvoid}, s::Ptr{Cvoid}, a::Ptr{Cvoid})::Cvoid)
solver_check(ctx::Z3_context, s::Z3_solver) = @ccall z3_jll.libz3.Z3_solver_check(ctx::Ptr{Cvoid}, s::Ptr{Cvoid})::Cint

#
mk_const(ctx::Z3_context, name::Z3_symbol, sort::Z3_sort) = Z3_ast((@ccall z3_jll.libz3.Z3_mk_const(ctx::Ptr{Cvoid}, name::Ptr{Cvoid}, sort::Ptr{Cvoid})::Ptr{Cvoid}), ctx)
mk_eq(ctx::Z3_context, a::Z3_ast, b::Z3_ast) = Z3_ast((@ccall z3_jll.libz3.Z3_mk_eq(ctx::Ptr{Cvoid}, a::Ptr{Cvoid}, b::Ptr{Cvoid})::Ptr{Cvoid}), ctx)
mk_not(ctx::Z3_context, a::Z3_ast) = Z3_ast((@ccall z3_jll.libz3.Z3_mk_not(ctx::Ptr{Cvoid}, a::Ptr{Cvoid})::Ptr{Cvoid}), ctx)

# bv builtins
mk_bvmul(ctx::Z3_context, a::Z3_ast, b::Z3_ast) = Z3_ast((@ccall z3_jll.libz3.Z3_mk_bvmul(ctx::Ptr{Cvoid}, a::Ptr{Cvoid}, b::Ptr{Cvoid})::Ptr{Cvoid}), ctx)
mk_bvurem(ctx::Z3_context, a::Z3_ast, b::Z3_ast) = Z3_ast((@ccall z3_jll.libz3.Z3_mk_bvurem(ctx::Ptr{Cvoid}, a::Ptr{Cvoid}, b::Ptr{Cvoid})::Ptr{Cvoid}), ctx)
mk_bvsrem(ctx::Z3_context, a::Z3_ast, b::Z3_ast) = Z3_ast((@ccall z3_jll.libz3.Z3_mk_bvsrem(ctx::Ptr{Cvoid}, a::Ptr{Cvoid}, b::Ptr{Cvoid})::Ptr{Cvoid}), ctx)
mk_int64(ctx::Z3_context, i::Union{Int64, UInt64}, sort::Z3_sort) = Z3_ast((@ccall z3_jll.libz3.Z3_mk_int64(ctx::Ptr{Cvoid}, i::Int64, sort::Ptr{Cvoid})::Ptr{Cvoid}), ctx)

const Z3_L_FALSE = -1
const Z3_L_UNDEF = 0
const Z3_L_TRUE = 1

# to string
Base.Cstring(sort::Z3_sort) = Cstring(@ccall z3_jll.libz3.Z3_sort_to_string(sort.ctx::Ptr{Cvoid}, sort::Ptr{Cvoid})::Ptr{Cchar})
Base.Cstring(sort::Z3_ast) = Cstring(@ccall z3_jll.libz3.Z3_ast_to_string(sort.ctx::Ptr{Cvoid}, sort::Ptr{Cvoid})::Ptr{Cchar})

Base.show(io::IO, sort::Union{Z3_sort, Z3_ast}) = print(io, unsafe_string(Base.Cstring(sort)))


const CC = Core.Compiler
using .CC: SSAValue, Argument
using Base.Meta

struct Z3CounterExample
    prop
end

struct Z3Engine <: ProofEngine; end

@MethodTable Z3Overlay

const z3check = @overlaypass Z3Overlay

function prop_for_type(prop, x)
    prop(x)()
end

const z3_intrinsics_map = Dict{Any, Any}(
    Core.Intrinsics.mul_int => mk_bvmul,
    Core.Intrinsics.checked_srem_int => mk_bvsrem,
    Core.Intrinsics.checked_urem_int => mk_bvurem,
    (===) => mk_eq,
)

function compile_stmt_to_z3!(ctx, solver, ir, maparg, inst)
    if Meta.isexpr(inst, :call)
        call = inst
        callee = call.args[1]
        isa(callee, GlobalRef) && (callee = getglobal(callee.mod, callee.name))
        if haskey(z3_intrinsics_map, callee)
            return z3_intrinsics_map[callee](ctx, maparg.(call.args[2:end])...)
        else
            error("No mapping for call $(callee)")
        end
    elseif Meta.isexpr(inst, :new)
        # Ignore for now
        return missing
    elseif Meta.isexpr(inst, :invoke)
        @assert CC.argextype(inst.args[2], ir) == IsCheckPass
        nc = ir.stmts[inst.args[2].id][:inst]
        @assert Meta.isexpr(nc, :new)
        assert = mk_not(ctx, maparg(nc.args[2]))
        solver_assert(ctx, solver, assert)
    end
end

function compile_to_z3!(ctx, solver, ir, args)
    z3map = Dict{SSAValue, Any}()
    function maparg(arg)
        if isa(arg, Argument)
            args[arg.n]
        elseif isa(arg, SSAValue)
            return z3map[arg]
        elseif isa(arg, Int64) || isa(arg, UInt64)
            bv64 = mk_bv_sort(ctx, 64)
            return mk_int64(ctx, arg, bv64)
        else
            @show arg
            error()
        end
    end
    for i = 1:length(ir.stmts)
        inst = ir.stmts[i][:inst]
        z3map[SSAValue(i)] = compile_stmt_to_z3!(ctx, solver, ir, maparg, inst)
    end
end

function (::typeof(z3check))(::typeof(forall), prop, T)
    effects = Base.infer_effects(prop_for_type, Tuple{Core.Typeof(prop), T})
    if !Core.Compiler.is_terminates(effects)
        error("Inference was unable to prove termination for prop $prop")
    end
    if !Core.Compiler.is_consistent(effects)
        error("Inference was unable to prove consistency for prop $prop")
    end
    ir = Base.code_ircode(prop_for_type, Tuple{Core.Typeof(prop), T})[1][1]
    ctx = Z3_context()
    solver = mk_solver(ctx)
    bv64 = mk_bv_sort(ctx, 64)
    x = mk_const(ctx, mk_string_symbol(ctx, "x"), bv64)
    compile_to_z3!(ctx, solver, ir, [missing, missing, x])
    @show
    check = solver_check(ctx, solver)
    if check == Z3_L_TRUE
        # Solver returned sat - we have a counterexample
        throw(Z3CounterExample(prop))
    elseif check == Z3_L_UNDEF
        error()
        # TODO: Can we return the proof here?
    end
end

@eval function check(::Z3Engine, prop)
    z3check(prop)
    F = Fact{Base.get_world_counter()}
    $(Expr(:new, :F, :prop))
end
