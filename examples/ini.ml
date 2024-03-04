type key = string
type value = string
type pair = key * value
type section = 
  {
    name : string;
    pairs : pair list
  }

let show_pair ((key , value) : pair) = 
  Printf.sprintf "(%s , %s)" key value

let show_pairs (pairs :  pair list) = 
  pairs |> List.map show_pair
  |> String.concat " , "
  |> Printf.sprintf "[%s]"

let  show_section (sec : section) = 
  Printf.sprintf "name = %s ; pairs = %s" sec.name (show_pairs sec.pairs)


let show_sections (sections : section list) = 
  sections 
  |> List.map show_section
  |> String.concat " , "
  |> Printf.sprintf "[%s]"

let is_space (x : char) = x == ' ' || x == '\n'

let section_name : string Parcoml.parser = 
  let open Parcoml in 
  prefix "[" *> parse_while (fun x -> x != ']') <* prefix "]"

let read_whole_file (file_path : string) : string = 
        let ch = open_in file_path in
        let n = in_channel_length ch in
        let s = really_input_string ch n in
        close_in ch;
        s

let wss : string Parcoml.parser = 
  let open Parcoml in 
    parse_while is_space
let pair_parser : pair Parcoml.parser = 
  let open Parcoml in 
  let name = parse_while (fun x -> not (is_space x) && x != '=') in
  (wss *> name <* wss <* prefix "=" <* wss) <*> (name <* wss)
   
let section_parser : section Parcoml.parser = 
  let open Parcoml in 
  section_name <*> many pair_parser 
  |> map (fun (name , pairs) -> {name = name ; pairs = pairs})

let ini : section list Parcoml.parser = 
  let open Parcoml in 
  many section_parser 

  let () = 
   let result = "./test.ini"
                  |>read_whole_file
                  |>Parcoml.make_input
                  |>ini.run
   in match result with 
   | Ok (_, sections) -> sections
                          |> show_sections
                          |> print_endline
   | Error error -> Printf.printf "Error happened at %d : %s" error.pos error.desc
