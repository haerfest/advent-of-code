type position = Pos of int * int
type element = Number of position * int | Symbol of position * char

let rec to_string = function
        | Number(Pos(row, column), value) -> Printf.sprintf "Number(Pos(%d,%d),%d)" row column value
        | Symbol(Pos(row, column), char) -> Printf.sprintf "Symbol(Pos(%d,%d),%c)" row column char

let parse line row items =
        let rec inner column items item =
                if column = String.length line then
                        match item with
                        | Some x -> x :: items
                        | None -> items
                else let char = line.[column] in
                        match char with
                        | '.' ->
                                (match item with
                                | Some x -> inner (column + 1) (x :: items) None
                                | None -> inner (column + 1) items None)
                        | '0'..'9' ->
                                let digit = Char.code char - Char.code '0' in
                                (match item with
                                        | Some Number(pos, value) ->
                                                inner (column + 1) items (Some (Number(pos, 10 * value + digit)))
                                        | _ ->
                                                inner (column + 1) items (Some (Number(Pos(row, column), digit))))
                        | _ ->
                                inner (column + 1) (Symbol(Pos(row, column), char) :: items) None
        in
        inner 0 items None

let rec parse_line channel row items =
        match In_channel.input_line channel with
        | Some line -> parse_line channel (row + 1) (parse line row items)
        | None -> items

let grok channel =
        parse_line channel 0 []

let () =
        let schematic = In_channel.with_open_text "input.txt" grok in
        List.iter (fun element -> Printf.printf "%s\n" (to_string element)) schematic
