(* Gave up typing too soon, should be tree of function_calls *)

type tree = Leaf of string | Node of string * tree list

let rec tree_to_string caller s tree =
  match tree with
  | Node (fun_name, sub_tree) ->
      let new_s = Printf.sprintf "%s\n%s -> %s" s caller fun_name in
      let start_of_fun_opt = BatString.Exceptionless.rindex fun_name '.' in
      let new_caller = match start_of_fun_opt with
        | None -> "FREEEEEEEEE"
        | Some index -> BatString.sub fun_name 0 index
      in
      BatList.fold_left (tree_to_string new_caller) new_s sub_tree
  | Leaf fun_name ->
      Printf.sprintf "%s\n%s -> %s" s caller fun_name

let rec create_subtree plant_map function_call =
  let function_name = Plant_model.called_function_name function_call in
  let subcalls_opt = BatMap.Exceptionless.find function_name plant_map in
  match subcalls_opt with
  | None -> let () = Printf.eprintf "Didn't find '%s' in map\n" function_name in Leaf function_name
  | Some subcalls -> let () = Printf.eprintf "Found '%s' in map\n" function_name in Node (function_name, BatList.map (create_subtree plant_map) subcalls)

let plant_model_to_hash map plant_model =
  let key =  Plant_model.originating_function_name plant_model in
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
  let () = BatEnum.iter (fun key -> Printf.eprintf "Key: '%s'\n" key) (BatMap.keys plant_map) in
  let originating_function_calls = BatList.map Plant_model.originating_function_call plants in
  let function_to_tree_map = BatList.fold_left (fun map function_name ->
      let subtree = create_subtree plant_map function_name in
      BatMap.add function_name subtree map
    ) BatMap.empty originating_function_calls
  in
  let all_trees = BatEnum.fold (fun acc elem ->
      let s = tree_to_string "Actor" "" elem in
      Printf.sprintf "%s\n-------------------------------------------------------------------------------------------------------------------\n%s" acc s
    ) "" (BatMap.values function_to_tree_map)
  in
  let () = BatFile.with_file_out ~mode:[`create] "Koekoek.pummel" (
        fun output -> BatIO.write_string output all_trees
    )
  in
  Printf.eprintf "Collection done\n"
