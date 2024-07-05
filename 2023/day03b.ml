open Hashtbl
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

let rec parse_lines' channel row line_length items =
  match input_line channel with
  | Some line ->
     let items = parse_line line row items in
     parse_lines' channel (row + 1) (max (length line) line_length) items
  | None -> (line_length, items)

let parse_lines channel = parse_lines' channel 0 0 []

let store_numbers items =
  let hash_table = create (List.length items) in
  let store_number = function
    | Number (value, nr_digits, position) ->
       for key = position to position + nr_digits - 1 do
         add hash_table key value
       done
    | _ -> ()
  in
  List.iter store_number items;
  hash_table

let part_numbers position n numbers =
  let look_left_right p part_numbers =
    match find_opt numbers p with
    | Some a -> a :: part_numbers
    | _ -> part_numbers
  and look_up_down p part_numbers =
    match (find_opt numbers (p - 1), find_opt numbers p, find_opt numbers (p + 1)) with
    | (Some a, None, Some b) -> a :: b :: part_numbers
    | (None, Some a, None)
    | (Some a, _, _)
    | (_, _, Some a) -> a :: part_numbers
    | _ -> part_numbers
  in      
  []
  |> look_left_right (position - 1)
  |> look_left_right (position + 1)
  |> look_up_down (position - n)
  |> look_up_down (position + n)

let solve channel =
  let (line_length, items) = parse_lines channel in
  let numbers = store_numbers items in
  let sum_gear_ratio sum = function
    | Symbol ('*', position) ->
       begin
         match part_numbers position line_length numbers with
         | [a; b] -> sum + a * b
         | _ -> sum
       end
    | _ -> sum
  in
  List.fold_left sum_gear_ratio 0 items

let () = printf "%d\n" (with_open_text "input.txt" solve)
