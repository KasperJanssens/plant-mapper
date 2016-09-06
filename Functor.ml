module Make_A (B:B_sig) = struct
  let f = "koekoek"
end

module A = Make_A (B)

let g = A.f
