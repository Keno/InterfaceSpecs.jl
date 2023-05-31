(declare-const x (_ BitVec 64))
(assert (not (= (bvurem (bvadd x x) #x0000000000000002) #x0000000000000000)))
(check-sat)
