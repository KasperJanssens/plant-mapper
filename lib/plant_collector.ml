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
  let () = Printf.eprintf "Creating plant map\n" in
  let plant_map = List.fold_left Plant_model.to_hash BatMap.empty plants in
  let () = Printf.eprintf "Creating function calls\n" in
  let originating_function_calls = BatList.map Plant_model.originating_function_call plants in
  let nr_of_calls= BatList.length originating_function_calls in
  let () = Printf.eprintf "Creating function to tree map, over %d function calls\n" nr_of_calls in
  let (_, function_to_tree_map) = BatList.fold_left (fun (i, map) function_call ->
      let subtree = Plant_tree.create_subtree plant_map function_call in
(*      let () = Printf.eprintf "Created subtree %d of %d\n" i nr_of_calls in
      let () = Printf.eprintf "Function Call : %s\n" @@ Plant_model.function_call_to_string function_call in*)
      (i+1, BatMap.add function_call subtree map)
    ) (1, BatMap.empty) (BatList.rev originating_function_calls)
  in
  let () = Printf.eprintf "creating functions used by other functions\n" in
  let values = BatMap.values function_to_tree_map in
  let values_length = BatList.length @@ BatList.of_enum values in
  let () = Printf.eprintf "value length %d\n" values_length in
  let (_, functions_used_by_other_functions) = BatEnum.fold (fun (i,acc) elem ->
      let () = Printf.eprintf "Function used by other functions: %d of %d\n" i values_length  in
      let used_functions_opt = Plant_tree.flatten_and_tail elem in
      match used_functions_opt with
      | None -> (i+1,acc)
      | Some functions -> (i+1, BatList.append acc functions)
    ) (1, []) values
  in
  let functions_used_by_other_functions = BatList.unique functions_used_by_other_functions in
  let () = Printf.eprintf "Creating all trees\n" in

  let size_of_map = BatList.length @@ BatList.of_enum @@ BatMap.keys function_to_tree_map in
  let () = Printf.eprintf "Size of map is %d\n" size_of_map in
  let test_tree = Plant_tree.Node(1,[Plant_tree.Leaf 2;Plant_tree.Leaf 3]) in
  let _ = Plant_tree.fold (fun acc elem -> acc + elem) 0 test_tree in
  let _ =
    BatMap.foldi (fun key value i ->
      if BatList.mem key functions_used_by_other_functions then
        (i+1)
      else
        let file_name = Printf.sprintf "%s/%s.puml" "pummels" @@ Plant_model.function_call_to_string key in
        let () = BatFile.with_file_out ~mode:[`create] file_name (
          fun output ->
            let () = BatIO.write_line output "@startuml" in
            let (_, output) = Plant_tree.tree_to_string "Actor" output value in
            BatIO.write_line output "@enduml"
        )
        in
        (i+1)
      )
      function_to_tree_map (1)
  in
  Printf.eprintf "Collection done\n"
