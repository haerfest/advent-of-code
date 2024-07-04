open In_channel
open Printf
open String

type item =
  | Number of int * int * int
  | Symbol of char * int

let to_string = function
  | Number (value, nr_digits, position) ->
      sprintf "Number (%d, %d, %d)" value nr_digits position
  | Symbol (character, position) ->
      sprintf "Symbol ('%c', %d)" character position

let value_of character = Char.code character - Char.code '0'

let parse_number line column =
  let rec parse value nr_digits =
    if column + nr_digits = length line then (value, nr_digits)
    else
      let character = line.[column + nr_digits] in
      match character with
      | '0' .. '9' ->
         let value = value * 10 + value_of character in
         parse value (nr_digits + 1)
      | _ -> (value, nr_digits)
  in
  parse 0 0

let rec parse_line' line row column items =
  let line_length = length line in
  if column = line_length then items
  else
    let position = row * line_length + column in
    let character = line.[column] in
    match character with
    | '.' ->
       parse_line' line row (column + 1) items
    | '0' .. '9' ->
       let value, nr_digits = parse_number line column in
       let number = Number (value, nr_digits, position) in
       parse_line' line row (column + nr_digits) (number :: items)
    | _ ->
       let symbol = Symbol (character, position) in
       parse_line' line row (column + 1) (symbol :: items)

let parse_line line row items = parse_line' line row 0 items

let rec parse_lines' channel row items =
  match input_line channel with
  | Some line ->
     let items = parse_line line row items in
     parse_lines' channel (row + 1) items
  | None -> items

let parse_lines channel = parse_lines' channel 0 []

let solve channel =
  let items = parse_lines channel in
  let open List in
  iter (fun item -> print_endline (to_string item)) items ;
  length items

let () = printf "%d\n" (with_open_text "input.txt" solve)
