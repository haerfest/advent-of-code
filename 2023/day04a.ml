open In_channel
open Printf
open String

let rec pow b = function
  | 0 -> 1
  | p -> b * pow b (p - 1)

let solve sum line =
  let open List in
  let items = line |> split_on_char ' ' |> filter ((<>) empty) in
  match find_index ((=) "|") items with
  | None -> invalid_arg "solve"
  | Some bar_index ->
     let winning =
       items
       |> filteri (fun index _ -> index >= 2 && index < bar_index)
       |> map int_of_string
     and drawn =
       items
       |> filteri (fun index _ -> index > bar_index)
       |> map int_of_string
     in
     let win_count =
       drawn
       |> filter (fun draw -> mem draw winning)
       |> length
     in
     match win_count with
     | 0 -> sum
     | n -> sum + pow 2 (n - 1)

let () = printf "%d\n" @@ with_open_text "input.txt" @@ fold_lines solve 0
