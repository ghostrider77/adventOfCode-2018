type pot = Empty | Plant

type neighbours = pot * pot * pot * pot * pot

module IntSet = Set.Make(Int)


let read_lines (filename: string): string list =
    let channel = open_in filename in
    let rec loop (acc: string list): string list =
        try
            let line = input_line channel in
            loop (line :: acc)
        with End_of_file -> List.rev acc in
    let lines = loop [] in
    close_in channel;
    lines


let pot_of_char (c: char): pot = match c with
    | '#' -> Plant
    | '.' -> Empty
    | _ -> failwith "Unknown character."


let parse_lines (lines: string list): IntSet.t * (neighbours * pot) list =
    let read_states (line: string): IntSet.t =
        line
            |> String.to_seq
            |> List.of_seq
            |> List.mapi (fun ix c -> (ix, pot_of_char c))
            |> List.filter_map (fun (ix, p) -> if p = Plant then Some ix else None)
            |> List.to_seq
            |> IntSet.of_seq in
    let parse_rule (line: string): neighbours * pot =
        Scanf.sscanf line "%c%c%c%c%c => %c"
            (fun l2 l1 c r1 r2 n ->
                ((pot_of_char l2, pot_of_char l1, pot_of_char c, pot_of_char r1, pot_of_char r2), pot_of_char n)) in
    let initial_state = Scanf.sscanf (List.hd lines) "initial state: %s" read_states in
    let rec loop (rules: (neighbours * pot) list) = function
        | [] -> List.rev rules
        | x :: xss ->
            if x = "" then loop rules xss
            else loop (parse_rule x :: rules) xss in
    let rules = loop [] (List.tl lines) in
    (initial_state, rules)


let calc_plant_id_sum (initial_state: IntSet.t) (rules: (neighbours * pot) list) (n: int): int =
    let rec loop (state: IntSet.t) (k: int): int =
        if k = n then IntSet.fold (+) state 0
        else if IntSet.is_empty state then 0
        else
            let min_ix = IntSet.min_elt state - 2 in
            let max_ix = IntSet.max_elt state + 2 in
            let get_neighbourhood (ix: int): pot * pot * pot * pot * pot =
                let ixs = [ix - 2; ix - 1; ix; ix + 1; ix + 2] in
                match List.map (fun i -> if IntSet.mem i state then Plant else Empty) ixs with
                    | [l2; l1; c; r1; r2] -> (l2, l1, c, r1, r2)
                    | _ -> failwith "Error" in
            let centers = List.init (max_ix - min_ix + 1) (fun ix -> ix + min_ix) in
            let update_state (acc: IntSet.t) (center: int): IntSet.t =
                let ns = get_neighbourhood center in
                match List.assoc_opt ns rules with
                    | Some Plant -> IntSet.add center acc
                    | _ -> acc in
            let state' = List.fold_left update_state IntSet.empty centers in
            loop state' (k + 1) in
    loop initial_state 0


let () =
    let lines = read_lines "../resources/input_12.txt" in
    let state, rules = parse_lines lines in
    let n = 50000000000 in
    let n' = 500 in
    let result' = calc_plant_id_sum state rules n' in
    let result = result' + (n - n') * 67 in
    print_int result; print_newline ()
