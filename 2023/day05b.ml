type seed_range = { start: int; len: int }
type map_range = { dst: int; src: int; len: int }

let ios = int_of_string

let pairwise items =
  let pairs_of acc item =
    match acc with
    | (pairs, None) -> (pairs, Some item)
    | (pairs, Some first_item) -> ((first_item, item) :: pairs, None)
  in
  match List.fold_left pairs_of ([], None) items with
  | (pairs, None) -> pairs
  | (pairs, Some left_over) -> failwith "invalid input"

let seed_of (start, len) = { start = start; len = len }

let parse_line (seeds, map, maps) line =
  match line |> String.split_on_char ' ' with
  | "seeds:" :: seeds -> (List.map seed_of (pairwise (List.map ios seeds)), map, maps)
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

let minimum = function
  | [] -> failwith "invalid input"
  | [x] -> x
  | x :: xs -> List.fold_left min x xs

let minimum_location maps { start = start; len = len } =
  let rec inner seed min_location =
    if seed = start + len then min_location
    else inner (seed + 1) (min (trace maps seed) min_location)
  in
  inner start (trace maps start)

let solve channel =
  let (seed_ranges, maps) = parse_channel channel in
  minimum (List.map (minimum_location maps) seed_ranges)

let main =
  Printf.printf "%d\n" @@ In_channel.with_open_text "input.txt" solve

let () = main
  
