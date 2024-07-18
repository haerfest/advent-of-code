let win_count line =
  let items = line |> String.split_on_char ' ' |> List.filter ((<>) String.empty) in
  match List.find_index ((=) "|") items with
  | None -> invalid_arg "solve"
  | Some bar_index ->
     let winning =
       items
       |> List.filteri (fun index _ -> index >= 2 && index < bar_index)
       |> List.map int_of_string
     and drawn =
       items
       |> List.filteri (fun index _ -> index > bar_index)
       |> List.map int_of_string
     in
     drawn
     |> List.filter (fun draw -> List.mem draw winning)
     |> List.length

let solve channel =
  let lines = In_channel.input_lines channel in
  let line_count = List.length lines in
  let wins_per_card = lines |> List.map win_count |> Array.of_list in
  let card_counts = Array.make line_count 1 in
  let rec count card =
    if card == line_count then Array.fold_left (+) 0 card_counts
    else
      begin
        let card_count = card_counts.(card) in
        for won_card = card + 1 to card + wins_per_card.(card) do
          card_counts.(won_card) <- card_counts.(won_card) + card_count * 1
        done;
        count (card + 1)
      end
  in
  count 0

let () = Printf.printf "%d\n" @@ In_channel.with_open_text "input.txt" solve
