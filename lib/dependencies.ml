type t = {
  key : string;
  values : string list;
}

type internal = {
  possible_roots: string list;
  dependencies: string list;
}

let empty_internal = { possible_roots = []; dependencies=[]}

let remove_extension s =
  let (%) = BatPervasives.(%) in
  BatString.of_list @@ BatList.take_while (BatBool.not % BatChar.equal '.') @@ BatString.to_list s

let create_dep_from_file dir dep_file  =
  let strings = BatList.of_enum @@ BatFile.lines_of @@ Printf.sprintf "%s/%s" dir dep_file  in
  let dep_line = BatList.hd strings in
  let dep_line_list = BatString.to_list dep_line in
  let index = BatOption.get @@ BatList.index_of ':' dep_line_list  in
  let key,values = BatList.split_at index dep_line_list in
  let key_as_string = BatString.of_list key in
  let values_as_string = BatString.of_list @@ BatList.tl values in
  let key = BatString.capitalize @@ remove_extension key_as_string  in
  let values = BatString.nsplit values_as_string " " in
  {key;values}

let filter_possible_roots possible_roots dependencies =
  BatList.fold_left (fun new_possible_roots possible_root ->
      if BatList.mem possible_root dependencies then
        new_possible_roots
      else
        BatList.cons possible_root new_possible_roots
  ) [] possible_roots

let process current_internal dep =
  let possible_roots = BatList.unique @@ filter_possible_roots current_internal.possible_roots dep.values in
  let dependencies = BatList.unique @@ BatList.append current_internal.dependencies dep.values in
  if BatList.mem dep.key current_internal.dependencies then
    {possible_roots; dependencies;}
  else
    let possible_roots = BatList.cons dep.key possible_roots in
    {possible_roots; dependencies;}
