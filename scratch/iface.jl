using InterfaceSpecs

@interface BasicArithmetic begin
    +(::Number, ::Number)::Number
    -(::Number, ::Number)::Number
    *(::Number, ::Number)::Number
end

function eval_poly(x::Number)
    6x*x + 10x - x
end

# Sanity check the interface by trying some sample `Number`s
check(PropertyCheck(), BasicArithmetic())

# Check that eval_poly satisfies the interface
check(InferenceEngine(), InterfaceCheck(Tuple{typeof(eval_poly), Number}, BasicArithmetic()))

# Check that the interface is globally satisfied in the
# current worldage.
# TODO: This current stack overflows in inference... The interface is hard to
# check, because it's extremely recursive.
check(InferenceEngine(), BasicArithmetic())
