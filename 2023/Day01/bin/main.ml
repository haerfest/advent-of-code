let (code, printf, to_seq) = (Char.code, Printf.printf, String.to_seq)
let (filter, map) = Seq.(filter, map)
let (fold_lines, with_open_text) = In_channel.(fold_lines, with_open_text)
let (hd, of_seq, rev) = List.(hd, of_seq, rev)

let is_digit char =
        char >= '0' && char <= '9'

let value char =
        code char - code '0'

let solve sum line =
        let digits =
                line
                |> to_seq
                |> (filter is_digit)
                |> (map value)
                |> of_seq
        in
        let (first_digit, last_digit) = (hd digits, hd (rev digits)) in
        sum + 10 * first_digit + last_digit

let () =
        printf "%d\n" (with_open_text "input.txt" (fold_lines solve 0))
