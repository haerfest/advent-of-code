type item =
  | Number of int * int * int * int
  | Symbol of char * int * int

let to_string = function
  | Number (value, length, row, column) ->
      Printf.sprintf "Number (%d, %d, %d, %d)" value length row column
  | Symbol (character, row, column) ->
      Printf.sprintf "Symbol ('%c', %d, %d)" character row column

let value_of character = Char.code character - Char.code '0'

let parse_number line column =
  let rec parse value length =
    if column + length = String.length line then (value, length)
    else
      let character = line.[column + length] in
      match character with
      | '0' .. '9' ->
         let value = (value * 10) + value_of character in
         parse value (length + 1)
      | _ -> (value, length)
  in
  parse 0 0

let rec parse_line' line row column items =
  if column = String.length line then items
  else
    let character = line.[column] in
    match character with
    | '.' ->
       parse_line' line row (column + 1) items
    | '0' .. '9' ->
       let value, length = parse_number line column in
       let number = Number (value, length, row, column) in
       parse_line' line row (column + length) (number :: items)
    | _ ->
       let symbol = Symbol (character, row, column) in
       parse_line' line row (column + 1) (symbol :: items)

let parse_line line row items = parse_line' line row 0 items

let rec parse_lines' channel row items =
  match In_channel.input_line channel with
  | Some line ->
     let items = parse_line line row items in
     parse_lines' channel (row + 1) items
  | None -> items

let parse_lines channel = parse_lines' channel 0 []

let solve channel =
  let items = parse_lines channel in
  List.iter (fun item -> print_endline (to_string item)) items ;
  List.length items

let () = Printf.printf "%d\n" (In_channel.with_open_text "input.txt" solve)
