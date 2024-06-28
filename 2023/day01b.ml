let words = [
        "0"; "zero";
        "1"; "one";
        "2"; "two";
        "3"; "three";
        "4"; "four";
        "5"; "five";
        "6"; "six";
        "7"; "seven";
        "8"; "eight";
        "9"; "nine"
]

exception InvalidInput

let value word =
        match List.find_index ((=) word) words with
        | Some index -> index / 2
        | None -> raise InvalidInput

let to_chars word =
        word |> String.to_seq |> List.of_seq

let to_word chars =
        chars |> List.to_seq |> String.of_seq

let left_to_right =
        List.map to_chars words

let right_to_left =
        List.map List.rev left_to_right

let starts_with word chars =
        let rec inner wi ci wl cl =
                if wi = wl then true
                else if ci = cl then false
                else if List.nth word wi <> List.nth chars ci then false
                else inner (wi + 1) (ci + 1) wl cl
        in
        inner 0 0 (List.length word) (List.length chars)

let rec first_word_of words chars =
        if List.is_empty chars then raise InvalidInput
        else match List.filter (fun word -> starts_with word chars) words with
                | word :: _ -> word
                | [] -> first_word_of words (List.tl chars)

let solve sum line =
        let chars = to_chars line in
        let (chars_left, chars_right) = (first_word_of left_to_right chars, first_word_of right_to_left (List.rev chars)) in
        let (word_left, word_right) = (to_word chars_left, to_word (List.rev chars_right)) in
        let (value_left, value_right) = (value word_left, value word_right) in
        sum + 10 * value_left + value_right 


let () =
        Printf.printf "%d\n" (In_channel.with_open_text "input.txt" (In_channel.fold_lines solve 0))
