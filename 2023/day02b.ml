let max3 (r, g, b) (r', g', b') =
        (max r r', max g g', max b b')

let to_rgb n color =
        match color with
        | "red" -> (n, 0, 0)
        | "green" -> (0, n, 0)
        | "blue" -> (0, 0, n)
        | _ -> (0, 0, 0)

(* A component is a draw of a single color, e.g. "1 green", which is converted to (0, 1, 0). *)
let component_to_rgb component =
        Scanf.sscanf component " %d %s" to_rgb

(* A draw is a single draw of up to three colors, e.g. "3 red, 1 green, 5 blue", which is
 * converted to (3, 1, 5). *)
let draw_to_rgb draw =
        let components = String.split_on_char ',' draw in
        let rgbs = List.map component_to_rgb components in
        List.fold_left max3 (0, 0, 0) rgbs

let power (r, g, b) =
        r * g * b

let solve sum line =
        let draws = List.nth (String.split_on_char ':' line) 1 in
        let draws' = String.split_on_char ';' draws in
        let rgbs = List.map draw_to_rgb draws' in
        let rgb  = List.fold_left max3 (0, 0, 0) rgbs in
        sum + power rgb

let () =
        Printf.printf "%d\n" (In_channel.with_open_text "input.txt" (In_channel.fold_lines solve 0))
