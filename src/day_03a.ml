
type claim = { id: int; x: int; y: int; width: int; height: int }


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


let claim_of_string (line: string): claim =
    Scanf.sscanf line "#%d @ %d,%d: %dx%d" (fun id x y width height -> {id; x; y; width; height})


let calc_nr_overlapping_claims (claims: claim list): int =
    let grid = Hashtbl.create 100000 in
    let get_or_else (overlaps: ((int * int), int) Hashtbl.t) (coord: int * int): int =
        match (Hashtbl.find_opt overlaps coord) with
            | None -> 0
            | Some count -> count in
    let rec loop = function
        | [] -> Hashtbl.fold (fun _ cnt n -> if cnt >= 2 then n + 1 else n) grid 0
        | { x; y; width; height } :: cs ->
            for i = x to x + width - 1 do
                for j = y to y + height - 1 do
                    let cnt = get_or_else grid (i, j) in
                    Hashtbl.replace grid (i, j) (cnt + 1)
                done;
            done;
            loop cs in
    loop claims


let () =
    let lines = read_lines "../resources/input_03.txt" in
    let claims = List.map claim_of_string lines in
    let result = calc_nr_overlapping_claims claims in
    print_int result; print_newline ()
