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
  let plant_map = List.fold_left Plant_model.to_hash BatMap.empty plants in
  let originating_function_calls = BatList.map Plant_model.originating_function_call plants in
  let function_to_tree_map = BatList.fold_left (fun map function_call ->
      let subtree = Plant_tree.create_subtree plant_map function_call in
      BatMap.add function_call subtree map
    ) BatMap.empty originating_function_calls
  in
  let functions_used_by_other_functions = BatList.unique @@ BatEnum.fold (fun acc elem ->
      let used_functions_opt = Plant_tree.flatten_and_tail elem in
      match used_functions_opt with
      | None -> acc
      | Some functions -> BatList.append acc functions
    ) [] (BatMap.values function_to_tree_map)
  in
  let all_trees = BatMap.foldi (fun key value acc ->
      if BatList.mem key functions_used_by_other_functions then
        acc
      else
        let s = Printf.sprintf "@startuml%s\n@enduml" @@ Plant_tree.tree_to_string "Actor" "" value in
        Printf.sprintf "%s\n-------------------------------------------------------------------------------------------------------------------\n%s" acc s
    ) function_to_tree_map ""
  in
  let () = BatFile.with_file_out ~mode:[`create] "Koekoek.pummel" (
        fun output -> BatIO.write_string output all_trees
    )
  in
  Printf.eprintf "Collection done\n"
