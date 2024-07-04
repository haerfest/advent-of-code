open In_channel
open Printf
open String

type item =
  | Number of int * int * int * int
  | Symbol of char * int * int

let to_string = function
  | Number (value, nr_digits, row, column) ->
      sprintf "Number (%d, %d, %d, %d)" value nr_digits row column
  | Symbol (character, row, column) ->
      sprintf "Symbol ('%c', %d, %d)" character row column

let value_of character = Char.code character - Char.code '0'

let parse_number line column =
  let rec parse value nr_digits =
    if column + nr_digits = length line then (value, nr_digits)
    else
      let character = line.[column + nr_digits] in
      match character with
      | '0' .. '9' ->
         let value = (value * 10) + value_of character in
         parse value (nr_digits + 1)
      | _ -> (value, nr_digits)
  in
  parse 0 0

let rec parse_line' line row column items =
  if column = length line then items
  else
    let character = line.[column] in
    match character with
    | '.' ->
       parse_line' line row (column + 1) items
    | '0' .. '9' ->
       let value, nr_digits = parse_number line column in
       let number = Number (value, nr_digits, row, column) in
       parse_line' line row (column + nr_digits) (number :: items)
    | _ ->
       let symbol = Symbol (character, row, column) in
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
