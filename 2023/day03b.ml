type item =
  | Number of int * int * int * int
  | Symbol of char * int * int

let to_string item =
  match item with
  | Number (value, length, row, column) ->
      Printf.sprintf "Number value=%d length=%d row=%d column=%d" value length row column
  | Symbol (character, row, column) ->
      Printf.sprintf "Symbol character='%c' row=%d column=%d" character row column

let value_of character =
  Char.code character - Char.code '0'

let parse_number line column =
  let rec inner value length =
    if column + length = String.length line then (value, length)
    else
      let character = line.[column + length] in
      match character with
      | '0' .. '9' -> inner ((value * 10) + value_of character) (length + 1)
      | _ -> (value, length)
  in
  inner 0 0

let rec parse_line line row column items =
  if column = String.length line then items
  else
    let character = line.[column] in
    match character with
    | '.' ->
       parse_line line row (column + 1) items
    | '0' .. '9' ->
        let value, length = parse_number line column in
        parse_line line row (column + length) (Number (value, length, row, column) :: items)
    | _ ->
        parse_line line row (column + 1) (Symbol (character, row, column) :: items)

let rec parse_lines channel row items =
  match In_channel.input_line channel with
  | Some line -> parse_lines channel (row + 1) (parse_line line row 0 items)
  | None -> items

let solve channel =
  let items = parse_lines channel 0 [] in
  List.iter (fun item -> print_endline (to_string item)) items ;
  List.length items

let () = Printf.printf "%d\n" (In_channel.with_open_text "input.txt" solve)
