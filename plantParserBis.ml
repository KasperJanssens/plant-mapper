open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let file_name = "plantDump"

let log payload =
  BatFile.with_file_out ~mode:[`append;`create] file_name (
    fun output -> BatIO.write_string output payload)

let expressions_belonging_to_structure_item si_name =
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
                          let () = log "applying" in
                          let () = log one_string_to_rule_them_all in
                          expr
                      | _ -> let () = log "apply but no Pexp_ident\n" in expr
                  end
            | x -> default_mapper.expr mapper x
    }

let handle_patterns value_bindings =
  List.fold_left (fun acc binding ->
      match binding.pvb_pat.ppat_desc with
        | Ppat_var {txt=label;loc}  -> log (Printf.sprintf "label is : %s\n" label); Some label
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
             default_mapper.structure_item (expressions_belonging_to_structure_item si_name) structure_item
      (* Delegate to the default mapper. *)
      | x -> default_mapper.structure_item mapper x;
  }

let () = register "plant" plant_mapper
