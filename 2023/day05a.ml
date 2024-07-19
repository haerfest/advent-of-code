type range = { dst: int; src: int; len: int }

let ios = int_of_string

let parse_line (seeds, maps) line =
  match String.split_on_char ' ' line with
  | "seeds:" :: seeds -> (List.map ios seeds, maps)
  | [_; "map:"] -> (seeds, [] :: maps)
  | [d; s; l] -> (seeds, ({ dst = ios d; src = ios s; len = ios l } :: List.hd maps) :: List.tl maps)
  | [""] -> (seeds, maps)
  | _ -> failwith "invalid input"

let parse_channel channel =
  In_channel.fold_lines parse_line ([], []) channel

let print_range { dst = d; src = s; len = l } =
  Printf.printf "  { dst = %#d; src = %#d; len = %#d }\n" d s l

let print_map map =
  Printf.printf "[\n";
  List.iter print_range map;
  Printf.printf "]\n"

let solve channel =
  let (seeds, maps) = parse_channel channel in
  List.iter print_map maps;
  0

let main =
  Printf.printf "%d\n" @@ In_channel.with_open_text "input.txt" solve

let () = main
  
