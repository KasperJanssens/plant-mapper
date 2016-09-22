(*type tree = Leaf of Plant_model.function_call | Node of Plant_model.function_call * tree list *)
type 'a tree = Leaf of 'a | Node of 'a * 'a tree list

let rec fold f acc tree =
  match tree with
  | Leaf fcall -> f acc fcall
  | Node (fcall, subforest) ->
      let new_acc = f acc fcall in
      BatList.fold_left
        (fun acc tree ->
           fold f acc tree)
        new_acc
        subforest

let flatten_and_tail tree =
  BatList.Exceptionless.tl @@ fold (fun acc fcall -> BatList.append acc [fcall]) [] tree

let tree_to_string caller output tree =
  fold (fun (caller, output) fun_call ->
(*      let () = Printf.eprintf "Encounter function : %s\n" @@ Plant_model.function_call_to_string fun_call in*)
      let new_s = Plant_model.to_plant caller fun_call in
      let () = BatIO.write_line output new_s in
      let new_caller = Plant_model.next_caller caller fun_call in
      (new_caller, output)
  ) (caller, output) tree

(*let rec tree_to_string caller s tree =
  match tree with
  | Node (fun_name, sub_tree) ->
      let new_s = Plant_model.to_plant s caller fun_name in
      let new_caller = Plant_model.next_caller caller fun_name in
      BatList.fold_left (tree_to_string new_caller) new_s sub_tree
  | Leaf fun_name ->
      Plant_model.to_plant s caller fun_name*)

let rec create_subtree plant_map function_call =
(*  let () = Printf.eprintf "HANDLIN %s\n" @@ Plant_model.function_call_to_string function_call in*)
  let subcalls_opt = BatMap.Exceptionless.find function_call plant_map in
  match subcalls_opt with
  | None -> Leaf function_call
  | Some subcalls ->
      if BatList.mem function_call subcalls then
(*        let () = Printf.eprintf "AVOIDING INFINITE EXPANSION %s\n" @@ Plant_model.function_call_to_string function_call in*)
        Leaf function_call
      else
(*        let () = Printf.eprintf "EXPANDING %s\n" @@ Plant_model.function_call_to_string function_call in*)
        Node (function_call, BatList.map (create_subtree plant_map) subcalls)
