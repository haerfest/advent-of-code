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
  let (seeds, maps) = In_channel.fold_lines parse_line ([], []) channel in
  (seeds, List.rev maps)

let rec find map value =
  match map with
  | [] -> value
  | { dst = dst; src = src; len = len } :: rest ->
     if value >= src && value < src + len then
       dst + value - src
     else
       find rest value

let rec trace maps value =
  match maps with
  | [] -> value
  | map :: rest -> trace rest (find map value)

let solve channel =
  let (seeds, maps) = parse_channel channel in
  let locations = List.map (trace maps) seeds in
  let ordered = List.sort compare locations in
  List.hd ordered

let main =
  Printf.printf "%d\n" @@ In_channel.with_open_text "input.txt" solve

let () = main
  
