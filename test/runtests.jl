using InterfaceSpecs
using Test

interesting_int(x::Int) = (x % 2 == 0) & (x % 3 == 1) & (x % 13 == 5)
let it = exists(interesting_int)
    @test interesting_int(it)
end