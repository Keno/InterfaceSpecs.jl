
using MacroTools

macro spec(spec, body)
    @capture(spec, name_(args__))
    new_body = MacroTools.prewalk(body) do expr
        if MacroTools.isexpr(expr, :macrocall)
            @show expr.args[1]
            if expr.args[1] == Symbol("@∀")
                closure = expr.args[3]
                formal_args = closure.args[1]
                closure_body = closure.args[2]
                return :($(forall)($(formal_args.args[1])->()->$closure_body, $(formal_args.args[2])))
            elseif expr.args[1] == Symbol("@check")
                check = expr.args[3]
                @assert MacroTools.isexpr(check, :call)
                return :(let r = $check; isa(r, Bool) ? $(IsCheckPass)(r)() : r(); end)
            end
        end
        return expr
    end
    return quote
        const $(esc(name)) = $(map(esc, args)...)->()->$(esc(new_body))
        Base.show(io::IO, prop::Core.Compiler.return_type($(esc(name)), Tuple)) = print(io, $(string(name)), "(",
            join(map(x->getfield(prop, x), fieldnames(typeof(prop))), ','), ")")
    end
end
