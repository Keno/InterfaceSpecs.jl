using InterfaceSpecs
using InterfaceSpecs: forall, CheckFail, IsCheckPass

struct ValidIterType
    x
end

function (this::ValidIterType)()
    isa(this.x, Union{NTuple{2, Any}, Nothing}) || throw(CheckFail(this))
end

struct Iteration
    T::Type
end

struct Iteration_x{T}
    x::T
end

function (this::Iteration)()
    forall(Iteration_x, this.T)
end

function (this::Iteration_x)()
    ValidIterType(iterate(this.x))()
    r = iterate(this.x)
    r = iterate(this.x, r[2])
    while true
        ValidIterType(r)()
        r === nothing && break
        r = iterate(this.x, r[2])
    end
end

#==
forall(this.T) do x
    ValidIterType(iterate(this.x))()
    r = iterate(this.x)
    r = iterate(this.x, r[2])
    while true
        ValidIterType(r)()
        r === nothing && break
        r = iterate(this.x, r[2])
    end
enda
==#

struct MultIsEven_x{T}
    x::T
end
function (this::MultIsEven_x)()
    IsCheckPass(iseven(this.x * 2))()
end

struct MultIsEven
    T
end
function (this::MultIsEven)()
    forall(MultIsEven_x, this.T)
end
