open Longident
open Asttypes

type t = {
  file: string;
  base_module_name : string;
  module_name: string option;
  function_name: string;
}

type function_call = {
  module_names: string list;
  function_name: string;
}

let is_call_to_other_module function_call =
  not @@ BatList.is_empty function_call.module_names

let create_function_call longident =
  let strings = flatten longident.txt in
  let strings_rev = BatList.rev strings in
  match strings_rev with
    | x::xs -> {module_names=BatList.rev xs;function_name=x}
    | [] -> raise Not_found

let function_call_to_string function_call =
  BatString.concat "." @@ BatList.append function_call.module_names [function_call.function_name]

let remove_ml_extension file_name =
  let l = BatString.length file_name in
  BatString.sub file_name 0 (l - 3)

let create file module_name function_name =
  let base_module_name = remove_ml_extension @@ BatString.capitalize file in
  {file;base_module_name;module_name;function_name}

let create_with_module file module_name function_name =
  create file (Some module_name) function_name

let create_without_module file function_name =
  create file None function_name

let file_name t = t.file

let module_name t = t.module_name

let function_name t = t.function_name

let write_to_file file_name payload =
  let plant_name = Printf.sprintf "%s.puml" @@ remove_ml_extension file_name in
  BatFile.with_file_out ~mode:[`append;`create] plant_name (
    fun output -> BatIO.write_string output payload)

let to_string plant_model function_call =
  let full_module_name =
    BatOption.map_default
      (fun submodule -> Printf.sprintf "%s.%s" plant_model.base_module_name submodule)
      plant_model.base_module_name
      plant_model.module_name
  in
  let function_call_s = function_call_to_string function_call in
  Printf.sprintf "%s.%s -> %s\n" full_module_name plant_model.function_name function_call_s

let log plant_model longident =
  let function_call = create_function_call longident in
  if is_call_to_other_module function_call then
    write_to_file plant_model.file @@ to_string plant_model function_call

