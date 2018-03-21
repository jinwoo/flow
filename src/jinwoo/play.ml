type item =
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
  end)

let parse content =
  let (ast, _) = Parser_flow.program ~fail:false content in
  let result = Translate.program ast in
  Printf.printf "%s\n" (string_of_item result)

let () = parse "1 + 2;
console.log('hello');
}"
