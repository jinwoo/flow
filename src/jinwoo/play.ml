(* type item =
   | String of string
   | Bool of bool
   | Object of (string * item) list
   | Array of item list
   | Number of float
   | Null
   | Regexp of (Loc.t * string * string)  (* (loc, pattern, flags) *)

   let indent_lines str =
   String.split_on_char '\n' str
   |> List.map (fun line -> "  " ^ line)
   |> String.concat "\n"

   let string_of_item item =
   let rec aux item =
    match item with
    | String value -> "'" ^ value ^ "'"
    | Bool value -> string_of_bool value
    | Object props ->
      let payload =
        List.map (fun (key, value) -> key ^ ": " ^ aux value) props
        |> String.concat ",\n"
      in
      "{\n" ^ indent_lines payload ^ "\n}"
    | Array items ->
      let payload =
        List.map (fun item -> aux item) items
        |> String.concat ",\n"
      in
      "[\n" ^ indent_lines payload ^ "\n]"
    | Number num -> string_of_float num
    | Null -> "null"
    | Regexp (_, pattern, flags) -> Printf.sprintf "%s: %s" pattern flags
   in
   aux item

   module MyTranslator : (Estree_translator.Translator with type t = item) =
   struct
   type t = item
   let string value = String value
   let bool value = Bool value
   let obj value = Object value
   let array value = Array value
   let number value = Number value
   let null = Null
   let regexp loc pattern flags = Regexp (loc, pattern, flags)
   end

   module Translate = Estree_translator.Translate (MyTranslator) (struct
    let include_comments = true
    let include_locs = true
   end) *)

(*
 let parse content =
   let (ast, _) = Parser_flow.program ~fail:false content in
  let result = Translate.program ast in
  Printf.printf "%s\n" (string_of_item result)

let () = parse "1 + 2;
console.log('hello');
}"
*)

let file_contents_exn filename =
  let ic = open_in filename in
  let lines = ref [] in
  try
    while true do
      lines := input_line ic :: !lines
    done;
    ""
  with
  | End_of_file ->
    close_in ic;
    String.concat "\n" (List.rev !lines)
  | e ->
    close_in_noerr ic;
    raise e

let parse content =
  let (ast, _) = Parser_flow.program ~fail:false content in
  ast

exception My_err of string

let create_context filename =
  let sig_cx = Context.make_sig () in
  (* TODO(jinwoo): Fine-tune options *)
  let metadata = {
    Context.
    checked = true;
    munge_underscores = false;
    verbose = None;
    weak = false;
    jsx = None;
    strict = false;
    enable_const_params = false;
    enforce_strict_call_arity = false;
    esproposal_class_static_fields = Options.ESPROPOSAL_IGNORE;
    esproposal_class_instance_fields = Options.ESPROPOSAL_IGNORE;
    esproposal_decorators = Options.ESPROPOSAL_IGNORE;
    esproposal_export_star_as = Options.ESPROPOSAL_IGNORE;
    esproposal_optional_chaining = Options.ESPROPOSAL_IGNORE;
    facebook_fbt = None;
    ignore_non_literal_requires = false;
    max_trace_depth = 100;
    root = Path.make filename;
    strip_root = false;
    suppress_comments = [];
    suppress_types = SSet.empty;
    max_workers = 10;
  }
  in
  let file_key = File_key.SourceFile filename in
  Context.make sig_cx metadata file_key ""

let rec print_expression cx exp = Ast.Expression.(
  (*
    let (loc, exp') = exp.expression in
    let result = match Query_types.query_type cx loc with
      | Query_types.FailureNoMatch -> "no match"
      | Query_types.FailureUnparseable _ -> "unparseable"
      | Query_types.Success (_, t) ->
        Ty_printer.string_of_t ~force_single_line:true t
    in
    Printf.printf "%s\n" result;
    *)
    let (loc, exp') = exp in
    let result = match Query_types.query_type cx loc with
      | Query_types.FailureNoMatch -> "no match"
      | Query_types.FailureUnparseable _ -> "unparseable"
      | Query_types.Success (_, t) ->
        Ty_printer.string_of_t ~force_single_line:true t
    in
    Printf.printf "%s\n" result;
    match exp' with
    | Binary {Binary.left; right; _} ->
      print_expression cx left;
      print_expression cx right
    | Function {Ast.Function.params; _} ->
      let (_, params') = params in
      let patterns = params'.Ast.Function.Params.params in
      List.iter (fun (_, pattern) ->
          match pattern with
          | Ast.Pattern.Identifier id ->
            let (loc, id') = id.Ast.Pattern.Identifier.name in
            Printf.printf "%s\n" id';
            let result = match Query_types.query_type cx loc with
              | Query_types.FailureNoMatch -> "no match"
              | Query_types.FailureUnparseable _ -> "unparseable"
              | Query_types.Success (_, t) ->
                Ty_printer.string_of_t ~force_single_line:true t
            in
            Printf.printf "%s\n" result;
          | _ -> ())
        patterns
    | _ -> ()
  )

let print_ast_with_types cx ast =
  let (_, stmts, _) = ast in
  List.iter (fun (_, stmt) ->
      match stmt with
      | Ast.Statement.Expression exp ->
        print_expression cx exp.Ast.Statement.Expression.expression
      | Ast.Statement.FunctionDeclaration {Ast.Function.params; _} ->
        let (_, params') = params in
        let patterns = params'.Ast.Function.Params.params in
        List.iter (fun (_, pattern) ->
            match pattern with
            | Ast.Pattern.Identifier id ->
              let (loc, id') = id.Ast.Pattern.Identifier.name in
              Printf.printf "%s\n" id';
              let result = match Query_types.query_type cx loc with
                | Query_types.FailureNoMatch -> "no match"
                | Query_types.FailureUnparseable _ -> "unparseable"
                | Query_types.Success (_, t) ->
                  Ty_printer.string_of_t ~force_single_line:true t
              in
              Printf.printf "%s\n" result;
            | _ -> ())
          patterns
      | _ -> ())
    stmts

let play filename =
  let content = file_contents_exn filename in
  let cx = create_context filename in
  let ast = parse content in
  let file_sig = match File_sig.program ast with
    | Ok file_sig -> file_sig
    | Error _ -> raise (My_err "indeterminate module type")
  in
  let lint_severities = LintSettings.of_default Severity.Warn in
  let file_key = File_key.SourceFile filename in
  Type_inference_js.infer_ast ~lint_severities ~file_sig cx file_key ast;
  print_ast_with_types cx ast

let () = play "test.js"
