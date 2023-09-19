using z3_jll

const CC = Core.Compiler
using .CC: argextype, ReturnNode

function check_z3()
    if !z3_jll.is_available()
        error("""
        Z3 has not been compiled for the current platform. Please file an issue to request
        your platform to be added.
        """)
    end
end

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
const Z3_ctx_owned_types = (:Z3_ast, :Z3_sort, :Z3_symbol, :Z3_goal, :Z3_solver, :Z3_model, :Z3_func_decl)
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
solver_get_model(ctx::Z3_context, s::Z3_solver) = Z3_model((@ccall z3_jll.libz3.Z3_solver_get_model(ctx::Ptr{Cvoid}, s::Ptr{Cvoid})::Ptr{Cvoid}), ctx)

# func decls
function mk_func_decl(ctx::Z3_context, name::Z3_symbol, domain::Vector{<:Z3_sort}, range::Z3_sort)
    @GC.preserve domain begin
        ptrs = map(x->Base.unsafe_convert(Ptr{Cvoid}, x), domain)
        Z3_func_decl((@ccall z3_jll.libz3.Z3_mk_func_decl(ctx::Ptr{Cvoid}, name::Ptr{Cvoid}, length(domain)::Cuint,
            ptrs::Ptr{Ptr{Cvoid}}, range::Ptr{Cvoid})::Ptr{Cvoid}), ctx)
    end
end

# AST creation
mk_const(ctx::Z3_context, name::Z3_symbol, sort::Z3_sort) = Z3_ast((@ccall z3_jll.libz3.Z3_mk_const(ctx::Ptr{Cvoid}, name::Ptr{Cvoid}, sort::Ptr{Cvoid})::Ptr{Cvoid}), ctx)
mk_eq(ctx::Z3_context, a::Z3_ast, b::Z3_ast) = Z3_ast((@ccall z3_jll.libz3.Z3_mk_eq(ctx::Ptr{Cvoid}, a::Ptr{Cvoid}, b::Ptr{Cvoid})::Ptr{Cvoid}), ctx)
mk_not(ctx::Z3_context, a::Z3_ast) = Z3_ast((@ccall z3_jll.libz3.Z3_mk_not(ctx::Ptr{Cvoid}, a::Ptr{Cvoid})::Ptr{Cvoid}), ctx)
function mk_app(ctx::Z3_context, fd::Z3_func_decl, args::Vector{<:Z3_ast})
    @GC.preserve args begin
        ptrs = map(x->Base.unsafe_convert(Ptr{Cvoid}, x), args)
        Z3_ast((@ccall z3_jll.libz3.Z3_mk_app(ctx::Ptr{Cvoid}, fd::Ptr{Cvoid},
            length(args)::Cuint, ptrs::Ptr{Ptr{Cvoid}})::Ptr{Cvoid}), ctx)
    end
end

# AST conversion to Julia
for (T, sym) in ((Int32, :Z3_get_numeral_int),
                 (UInt32, :Z3_get_numeral_uint),
                 (UInt64, :Z3_get_numeral_uint64),
                 (Int64, :Z3_get_numeral_int64))
    @eval function (::Type{$T})(ast::Z3_ast)
        r = Ref{$T}()
        if !@ccall z3_jll.libz3.$sym(ast.ctx::Ptr{Cvoid}, ast::Ptr{Cvoid}, r::Ptr{$T})::Bool
            throw(InexactError(:Z3, $T, val))
        end
        return r[]
    end
end

function Rational{Int64}(ast::Z3_ast)
    n = Ref{Int64}()
    d = Ref{Int64}()
    if !@ccall z3_jll.libz3.Z3_get_numeral_rational_int64(ast.ctx::Ptr{Cvoid}, ast::Ptr{Cvoid}, n::Ptr{Int64}, d::Ptr{Int64})::Bool
        throw(InexactError(:Z3, Rational{Int64}, val))
    end
    return Rational{Int64}(n[], d[])
end

# Models
function model_get_const_interp(ctx::Z3_context, model::Z3_model, fd::Z3_func_decl)
    ast = (@ccall z3_jll.libz3.Z3_model_get_const_interp(ctx::Ptr{Cvoid}, model::Ptr{Cvoid}, fd::Ptr{Cvoid})::Ptr{Cvoid})
    ast === C_NULL && return missing
    Z3_ast(ast, ctx)
end

# bv builtins
for binop in (:bvmul, :bvslt, :bvurem, :bvsrem, :bvand)
    name = Symbol("mk_$binop")
    sym = Symbol("Z3_$name")
    @eval $name(ctx::Z3_context, a::Z3_ast, b::Z3_ast) =
        Z3_ast((@ccall z3_jll.libz3.$sym(ctx::Ptr{Cvoid}, a::Ptr{Cvoid}, b::Ptr{Cvoid})::Ptr{Cvoid}), ctx)
end
mk_int64(ctx::Z3_context, i::Union{Int64, UInt64}, sort::Z3_sort) = Z3_ast((@ccall z3_jll.libz3.Z3_mk_int64(ctx::Ptr{Cvoid}, i::Int64, sort::Ptr{Cvoid})::Ptr{Cvoid}), ctx)

# vararg builtins
function mk_and(ctx::Z3_context, args::Z3_ast...)
    @GC.preserve args begin
        ptrs = Ptr{Cvoid}[Base.unsafe_convert(Ptr{Cvoid}, x) for x in args]
        Z3_ast((@ccall z3_jll.libz3.Z3_mk_and(ctx::Ptr{Cvoid}, length(args)::Cuint, ptrs::Ptr{Ptr{Cvoid}})::Ptr{Cvoid}), ctx)
    end
end

const Z3_L_FALSE = -1
const Z3_L_UNDEF = 0
const Z3_L_TRUE = 1

# to string
for which in (:solver, :sort, :ast, :model)
    T = Symbol("Z3_$which")
    to_string = Symbol("Z3_$(which)_to_string")
    @eval Base.Cstring(thing::$T) = Cstring(@ccall z3_jll.libz3.$to_string(thing.ctx::Ptr{Cvoid}, thing::Ptr{Cvoid})::Ptr{Cchar})
end

Base.show(io::IO, sort::Union{Z3_sort, Z3_ast, Z3_solver, Z3_model}) = print(io, unsafe_string(Base.Cstring(sort)))


const CC = Core.Compiler
using .CC: SSAValue, Argument
using Base.Meta

struct Z3CounterExample
    prop
end

struct Z3NonExistenceProof
end

function Base.showerror(io::IO, p::Z3NonExistenceProof)
    print(io, """Z3NonExistenceProof: Z3 has proven that no value exists with the requested properties.""")
end

struct Z3Engine <: ProofEngine; end

@MethodTable Z3Overlay

const z3check = @overlaypass Z3Overlay


const z3_intrinsics_map = Dict{Any, Any}(
    Core.Intrinsics.and_int => mk_and, # TODO: This is wrong. Need to distinguish between Bool and BV
    Core.Intrinsics.mul_int => mk_bvmul,
    Core.Intrinsics.slt_int => mk_bvslt,
    Core.Intrinsics.checked_srem_int => mk_bvsrem,
    Core.Intrinsics.checked_urem_int => mk_bvurem,
    (===) => mk_eq,
)

function compile_stmt_to_z3!(ctx, solver, ir, maparg, stmt)
    if Meta.isexpr(stmt, :call)
        call = stmt
        callee = call.args[1]
        isa(callee, GlobalRef) && (callee = getglobal(callee.mod, callee.name))
        if haskey(z3_intrinsics_map, callee)
            return z3_intrinsics_map[callee](ctx, maparg.(call.args[2:end])...)
        else
            error("No mapping for call $(callee)")
        end
    elseif Meta.isexpr(stmt, :new)
        # Ignore for now
        return missing
    elseif Meta.isexpr(stmt, :invoke)
        @assert CC.argextype(inst.args[2], ir) == IsCheckPass
        nc = ir.stmts[stmt.args[2].id][:inst]
        @assert Meta.isexpr(nc, :new)
        assert = mk_not(ctx, maparg(nc.args[2]))
        solver_assert(ctx, solver, assert)
    elseif isa(stmt, CC.ReturnNode)
        # Nothing for now
    else
        error("No mapping for $stmt")
    end
end

function compile_to_z3!(ctx, solver, ir, args)
    @assert length(ir.cfg.blocks) == 1
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
        stmt = ir.stmts[i][:inst]
        if isa(stmt, ReturnNode)
            @assert i == length(ir.stmts)
            return (z3map[stmt.val], argextype(stmt.val, ir))
        end
        z3map[SSAValue(i)] = compile_stmt_to_z3!(ctx, solver, ir, maparg, stmt)
    end
end

function (::typeof(z3check))(::typeof(forall), prop, T)
    check_z3()
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

function typ_to_z3(ctx, T)
    if T === Int64
        return mk_bv_sort(ctx, 64)
    else
        error("Missing type mapping for $T")
    end
end

function _exists(::Z3Engine, prop)
    check_z3()
    irs = Base.code_ircode(prop, Tuple{Any})
    @assert length(irs) == 1
    ir = irs[1][1]
    ctx = Z3_context()
    solver = mk_solver(ctx)
    arg_fds = map(enumerate(ir.argtypes)) do (i, argT)
        Base.issingletontype(argT) && return missing
        isa(argT, Core.Compiler.Const) && return missing
        fd = mk_func_decl(ctx, mk_int_symbol(ctx, i), Z3_sort[], typ_to_z3(ctx, argT))
    end
    args = map(arg_fds) do arg
        arg === missing && return missing
        mk_app(ctx, arg, Z3_ast[])
    end
    (val, rt) = compile_to_z3!(ctx, solver, ir, args)
    @assert rt === Bool # TODO: rt === Nothing acceptable also, but needs effects preconditions
    solver_assert(ctx, solver, val)
    check = solver_check(ctx, solver)
    if check == Z3_L_TRUE
        # Solver returned sat - we have an instance, converty it back to julia
        model = solver_get_model(ctx, solver)
        ast = model_get_const_interp(ctx, model, arg_fds[end])
        return ir.argtypes[end](ast)
    elseif check == Z3_L_UNDEF
        error()
        # TODO: Can we return the proof here?
    else
        @assert check == Z3_L_FALSE
        throw(Z3NonExistenceProof())
    end
end