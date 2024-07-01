let grok channel =
        let contents = In_channel.input_lines channel in
        let matrix = Array.make_matrix (List.length contents) (String.length (List.hd contents)) '.' in
        List.iteri (fun row line -> String.iteri (fun column char -> matrix.(row).(column) <- char) line) contents ;
        matrix

let read_number row column =
        let rec inner row column length number =
                if column = Array.length row then (number, length)
                else
                        let char = row.(column) in
                        match char with
                        | '0' .. '9' ->
                                let digit = Char.code char - Char.code '0' in
                                inner row (column + 1) (length + 1) (number * 10 + digit)
                        | _ -> (number, length)
        in
        inner row column 0 0

let symbol_in_range matrix top_row left_column bottom_row right_column =
        let (end_row, end_column) = (Array.length matrix, Array.length matrix.(0)) in
        let top_row = max 0 top_row
        and left_column = max 0 left_column
        and bottom_row = min bottom_row (end_row - 1)
        and right_column = min right_column (end_column - 1) in
        let rec inner row column =
                if row > bottom_row then false
                else if column > right_column then inner (row + 1) left_column
                else match matrix.(row).(column) with
                        | '0'..'9' -> inner row (column + 1)
                        | '.' -> inner row (column + 1)
                        | _ -> true
        in
        inner top_row left_column

let numbers matrix =
        let rec inner row column list =
                if row = Array.length matrix then list
                else if column = Array.length matrix.(row) then inner (row + 1) 0 list
                else match matrix.(row).(column) with
                        | '0'..'9' ->
                                let (number, length) = read_number matrix.(row) column in
                                (match symbol_in_range matrix (row - 1) (column - 1) (row + 1) (column + length) with
                                        | true -> inner row (column + length) (number :: list)
                                        | false -> inner row (column + length) list)
                        | _ -> inner row (column + 1) list
        in
        inner 0 0 []

let () =
        let schematic = In_channel.with_open_text "input.txt" grok in
        Printf.printf "%d\n" (List.fold_left (+) 0 (numbers schematic))
