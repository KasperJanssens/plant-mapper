module Koekoek = struct
  let functie k = 
    let interne_functie l = "vogelbekdier" in
    "eend" <> interne_functie k

  let attribuut = "gans"  
end

let functie_buiten_module _f = "adelaar"
