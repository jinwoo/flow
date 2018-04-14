module Translate = Estree_translator.Translate (Json_of_estree) (struct
    let include_comments = true
    let include_locs = true
  end)

let print_ast filename =
  let file = File_input.FileName filename in
  let content = File_input.content_of_file_input_unsafe file in

  let open Hh_json in
  let results =
    try
      let parse_options = Some Parser_env.({
          esproposal_class_instance_fields = true;
          esproposal_class_static_fields = true;
          esproposal_decorators = true;
          esproposal_export_star_as = true;
          esproposal_optional_chaining = true;
          types = true;
          use_strict = false;
        }) in

      let filename = File_input.path_of_file_input file in
      let filekey = Option.map filename ~f:(fun s -> File_key.SourceFile s) in
      let (ocaml_ast, errors) =
        Parser_flow.program_file ~fail:false ~parse_options content filekey
      in
      let (translated_ast, errors) =
        Translate.program ocaml_ast, errors
      in
      match translated_ast with
      | JSON_Object params ->
        let errors_prop = ("errors", Translate.errors errors) in
        JSON_Object (errors_prop::params)
      | _ -> assert false
    with Parse_error.Error l ->
      JSON_Object ["errors", Translate.errors l]
  in
  print_json_endline ~pretty:true results
