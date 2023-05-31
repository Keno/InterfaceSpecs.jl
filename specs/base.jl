@spec ValidIterType(x) begin
    @check isa(x, Union{NTuple{2, Nothing}})
end

@spec Iteration(T::Type) begin
    @∀ x::T->begin
        @check ValidIterType(iterate(x))
        let r = iterate(x)
            # TODO: This is written loop-peeled this way to play nice with the compiler's
            # termination analysis to make sure it can prove this for singelton
            # cases. It would be nice if that wasn't required.
            r = iterate(x, r[2])
            while true
                @check ValidIterType(r)
                r === nothing && break
                r = iterate(x, r[2])
            end
        end
    end
end

@spec ArrayEltype(A <: AbstractArray) begin
    @∀ x::T->begin
        isa(eltype(x), Type)
        let ET = eltype(x)
            @∀ i->isa(x[i], ET) @throws (BoundsError, MethodError)
        end
    end
end

@spec ArrayAxes(A <: AbstractArray) begin
    @∀ x::T->begin
        let N = ndims(x)
            axes(x)::NTuple{N}
            ntuple(N) do n
                @∀ {x} (x in axes(x, n))->x[n] @throws ()
            end
        end
    end
end
