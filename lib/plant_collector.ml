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
  let structure_item_to_function_calls_map = List.fold_left Plant_model.to_hash BatMap.empty plants in

  let () = Printf.eprintf "Creating function calls\n" in
  let function_calls_that_are_structure_items = BatList.map Plant_model.originating_function_call plants in

  let nr_of_calls= BatList.length function_calls_that_are_structure_items in
  let () = Printf.eprintf "Creating function to tree map, over %d function calls\n" nr_of_calls in
  let (_, function_to_tree_map) = BatList.fold_left (fun (i, map) function_call ->
      let subtree = Plant_tree.create_subtree structure_item_to_function_calls_map function_call in
      (i+1, BatMap.add function_call subtree map)
    ) (1, BatMap.empty) (BatList.rev function_calls_that_are_structure_items)
  in
  let () = Printf.eprintf "creating functions used by other functions\n" in

  let size_of_map = BatList.length @@ BatList.of_enum @@ BatMap.keys function_to_tree_map in
  let _ =
    BatMap.foldi (fun key value i ->
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
