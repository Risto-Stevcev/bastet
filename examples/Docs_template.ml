let examples_dir = "examples"

let is_markdown filename = String.split_on_char '.' filename |> List.exists (( = ) "md")

let get_markdown_files = Sys.readdir examples_dir |> Array.to_list |> List.filter is_markdown

let is_code_bracket = function
  | "```ocaml" | "```" -> true
  | _ -> false

let string_of_list list =
  list
  |> List.fold_left
       (fun acc e ->
         match acc with
         | "" -> e
         | _ -> acc ^ "\n" ^ e)
       ""

let read_file ?predicate file =
  let ic = open_in file in
  let rec go lines =
    try
      let line = input_line ic in
      match predicate with
      | Some p when p line -> go lines
      | _ -> go (line :: lines)
    with End_of_file ->
      close_in ic;
      lines
  in
  go [] |> List.rev |> string_of_list

let read_markdown file = read_file ~predicate:is_code_bracket @@ examples_dir ^ "/" ^ file

let get_json () =
  let entries' =
    get_markdown_files
    |> List.map (fun file -> file, read_markdown file)
    |> List.filter (fun (_, contents) -> String.trim contents <> "")
  in
  let entries =
    entries' |> List.map (fun (file, contents) -> Filename.chop_extension file, `String contents)
  in
  `O entries

let get_template () =
  let rec go lines = try go (read_line () :: lines) with End_of_file -> lines in
  go [] |> List.rev |> string_of_list

let render_template () = Mustache.render (get_template () |> Mustache.of_string) (get_json ())

let _ = render_template () |> print_endline
