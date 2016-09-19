open Longident
open Asttypes
open Sexplib.Std
open Sexplib.Sexp

type function_call = {
  module_names: string list;
  function_name: string;
} [@@deriving sexp]

let called_function_name function_call =
  BatString.concat "." @@ BatList.append function_call.module_names [function_call.function_name]

type t = {
  file: string;
  base_module_name : string;
  module_name: string option;
  function_name: string;
  function_calls: function_call list;
} [@@deriving sexp]

let originating_function_name t =
  match t.module_name with
  | Some submodule -> Printf.sprintf "%s.%s.%s"
                        t.base_module_name
                        submodule
                        t.function_name
  | None -> Printf.sprintf "%s.%s"
              t.base_module_name
              t.function_name

let originating_function_call t =
  let module_names = BatList.append [t.base_module_name] (BatOption.map_default (BatList.make 1) [] t.module_name) in
  let function_name = t.function_name in
  {module_names; function_name}

let string_to_plant t_s =
  let sexp = of_string t_s in
  t_of_sexp sexp

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

let remove_path file_name =
  let path = BatPathGen.OfString.of_string file_name in
  BatPathGen.OfString.name path

let create file module_name function_name =
  let base_module_name = BatString.capitalize @@ remove_path @@ remove_ml_extension file in
  let function_calls = [] in
  {file;base_module_name;module_name;function_name;function_calls}

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
    fun output -> BatIO.write_line output payload)

let plant_to_string t =
  let sexp = sexp_of_t t in
  to_string sexp

let write_out plant_model =
  let file_name = plant_model.file in
  let sexp = sexp_of_t plant_model in
  let s = Sexplib.Sexp.to_string sexp in
  write_to_file file_name s

let add_function_call plant_model longident =
  let function_call = create_function_call longident in
  if is_call_to_other_module function_call then
    let current_calls = plant_model.function_calls in
    (*TODO for some strange reason the order of encountering the expressions is reversed, seemingly *)
    {plant_model with function_calls = (BatList.cons function_call current_calls) }
  else
    plant_model

