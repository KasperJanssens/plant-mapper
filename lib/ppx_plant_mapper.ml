open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Location
open Lexing

let position_to_string pos =
  let (name, l, r) = Location.get_pos_info pos in 
  Printf.sprintf "file_name : %s, pos : %d, %d\n" name l r 

let location_to_string location = 
  let pos_start = position_to_string location.loc_start in
  let pos_end =  position_to_string location.loc_end in
  let ghost = location.loc_ghost in
  Printf.sprintf "Pos_start: '%s', Pos_end:'%s', ghost: %b" pos_start pos_end ghost

let remove_ml_extension file_name =
  let l = BatString.length file_name in
  BatString.sub file_name 0 (l - 3)
  
let log file_name payload =
  let plant_name = Printf.sprintf "%s.puml" @@ remove_ml_extension file_name in
  BatFile.with_file_out ~mode:[`append;`create] plant_name (
    fun output -> BatIO.write_string output payload)


let expressions_belonging_to_structure_item file_name si_name =
    let module_name = remove_ml_extension @@ BatString.capitalize file_name in
    {default_mapper with
       expr = fun mapper expr ->
          match expr with
            | {pexp_desc =
                Pexp_apply (pexp_ident, expression_list)} ->
                  begin 
                    match pexp_ident.pexp_desc with
                      | Pexp_ident longident ->
                          let strings = flatten longident.txt in
                          let one_string_to_rule_them_all = BatString.concat "." strings in
                          let first_char = BatList.hd @@ BatString.to_list one_string_to_rule_them_all in
                          let () =
                            if BatChar.is_uppercase first_char then
                              log file_name @@ Printf.sprintf "%s.%s -> %s\n" module_name si_name one_string_to_rule_them_all
                          in
                          default_mapper.expr mapper expr
                      | _ -> default_mapper.expr mapper expr
                  end
            | {pexp_desc =
                Pexp_send (sub_exp, fun_name )} ->
                  begin 
                    match sub_exp.pexp_desc with
                      | Pexp_ident longident ->
                          let strings = flatten longident.txt in
                          let one_string_to_rule_them_all = BatString.concat "." strings in
                          let () = log file_name @@ Printf.sprintf "%s.%s -> %s\n" module_name one_string_to_rule_them_all fun_name in
                          default_mapper.expr mapper expr
                      | _ -> default_mapper.expr mapper expr
                  end
            | x -> default_mapper.expr mapper x
    }

let handle_patterns value_bindings =
  List.fold_left (fun acc binding ->
      match binding.pvb_pat.ppat_desc with
        | Ppat_var {txt=label;loc}  -> Some label
        | _ -> acc
    ) None value_bindings


let plant_mapper argv =
  (* Our getenv_mapper only overrides the handling of expressions in the default mapper. *)
  { default_mapper with
    structure_item = fun mapper structure_item ->
      match structure_item with
      | { pstr_desc = (Pstr_value (flag, bindings)); pstr_loc} ->
             let si_name_opt = handle_patterns bindings in
             let si_name = BatOption.get si_name_opt in
             let file_name =  pstr_loc.loc_start.pos_fname in
             default_mapper.structure_item (expressions_belonging_to_structure_item file_name si_name) structure_item
      (* Delegate to the default mapper. *)
      | x -> default_mapper.structure_item mapper x;
  }

let () = register "plant" plant_mapper
