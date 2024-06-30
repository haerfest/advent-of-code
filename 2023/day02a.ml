let id x = x

let is_possible_draw draw =
        Scanf.sscanf draw " %d %s" (fun count color ->
                match color with
                | "red" -> count <= 12
                | "green" -> count <= 13
                | "blue" -> count <= 14
                | _ -> false)

let is_possible_game reveal_part =
        let draws = String.split_on_char ',' reveal_part in
        List.for_all is_possible_draw draws

let solve sum line =
        match String.split_on_char ':' line with
        | [game_part; reveals_part] ->
                let game_id = (Scanf.sscanf game_part "Game %d" id)
                and reveals = String.split_on_char ';' reveals_part in
                if List.for_all is_possible_game reveals then sum + game_id else sum
        | _ -> sum

let () =
        Printf.printf "%d\n" (In_channel.with_open_text "input.txt" (In_channel.fold_lines solve 0))
