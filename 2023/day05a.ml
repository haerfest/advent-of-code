type range = { dst: int; src: int; len: int }

let ios = int_of_string

let parse_line (seeds, map, maps) line =
  match line |> String.split_on_char ' ' with
  | "seeds:" :: seeds -> (List.map ios seeds, map, maps)
  | [_; "map:"] -> (seeds, [], map :: maps)
  | [dst; src; len] -> (seeds, { dst = ios dst; src = ios src; len = ios len } :: map, maps)
  | [""] -> (seeds, map, maps)
  | _ -> failwith "invalid input"

let parse_channel channel =
  let (seeds, map, maps) = channel |> In_channel.fold_lines parse_line ([], [], []) in
  (seeds, List.rev (map :: maps))

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
  
