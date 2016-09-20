(* Gave up typing too soon, should be tree of function_calls *)

type tree = Leaf of Plant_model.function_call | Node of Plant_model.function_call * tree list

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

let rec tree_to_string caller s tree =
  match tree with
  | Node (fun_name, sub_tree) ->
      let new_s = Printf.sprintf "%s\n%s -> %s" s caller @@ Plant_model.to_plant fun_name in
      let new_caller = Plant_model.full_module_name fun_name in
      BatList.fold_left (tree_to_string new_caller) new_s sub_tree
  | Leaf fun_name ->
      Printf.sprintf "%s\n%s -> %s" s caller @@ Plant_model.to_plant fun_name

let rec create_subtree plant_map function_call =
  let subcalls_opt = BatMap.Exceptionless.find function_call plant_map in
  match subcalls_opt with
  | None -> Leaf function_call
  | Some subcalls -> Node (function_call, BatList.map (create_subtree plant_map) subcalls)

let plant_model_to_hash map plant_model =
  let key =  Plant_model.originating_function_call plant_model in
  let value = plant_model.Plant_model.function_calls in
  BatMap.add key value map

let main =
  let dir =
    if Array.length Sys.argv <> 2 then
      raise Not_found
    else
      Array.get Sys.argv 1
  in
  let all_files = BatSys.readdir dir in
  let puml_files = BatArray.fold_left
      (fun acc elem ->
         if BatString.ends_with elem "puml" then
           BatList.append acc [elem]
         else
           acc
      )
      []
      all_files
  in
  let plants = BatList.concat @@ BatList.map (fun puml_file ->
      let () = Printf.eprintf "Handling file : '%s'\n" puml_file in
      let strings_enum = BatFile.lines_of @@ Printf.sprintf "%s/%s" dir puml_file in
      BatEnum.fold (fun acc t_s ->
          let plant = Plant_model.string_to_plant t_s in
          BatList.append acc [plant]
        )
        []
        strings_enum
    ) puml_files
  in
  let plant_map = List.fold_left plant_model_to_hash BatMap.empty plants in
  let originating_function_calls = BatList.map Plant_model.originating_function_call plants in
  let function_to_tree_map = BatList.fold_left (fun map function_call ->
      let subtree = create_subtree plant_map function_call in
      BatMap.add function_call subtree map
    ) BatMap.empty originating_function_calls
  in
  let functions_used_by_other_functions = BatList.unique @@ BatEnum.fold (fun acc elem ->
      let used_functions_opt = flatten_and_tail elem in
      match used_functions_opt with
      | None -> acc
      | Some functions -> BatList.append acc functions
    ) [] (BatMap.values function_to_tree_map)
  in
  let all_trees = BatMap.foldi (fun key value acc ->
      if BatList.mem key functions_used_by_other_functions then
        acc
      else
        let s = Printf.sprintf "@startuml%s\n@enduml" @@ tree_to_string "Actor" "" value in
        Printf.sprintf "%s\n-------------------------------------------------------------------------------------------------------------------\n%s" acc s
    ) function_to_tree_map ""
  in
  let () = BatFile.with_file_out ~mode:[`create] "Koekoek.pummel" (
        fun output -> BatIO.write_string output all_trees
    )
  in
  Printf.eprintf "Collection done\n"
