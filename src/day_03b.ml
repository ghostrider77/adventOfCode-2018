module IntSet = Set.Make(Int)


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


let get_or_else (overlaps: ('a, IntSet.t) Hashtbl.t) (key: 'a): IntSet.t =
    match (Hashtbl.find_opt overlaps key) with
        | None -> IntSet.empty
        | Some ids -> ids


let add_intersection (overlaps: (int, IntSet.t) Hashtbl.t) (coord: int * int) (ids: IntSet.t) (id: int): unit =
    begin
    Hashtbl.replace overlaps id (IntSet.union ids (get_or_else overlaps id));
    IntSet.iter (fun i -> Hashtbl.replace overlaps i (IntSet.add i (get_or_else overlaps i))) ids
    end


let find_non_overlapping_claim(overlaps: (int, IntSet.t) Hashtbl.t): int =
    fst @@ List.find (fun (_, other_ids) -> IntSet.is_empty other_ids) @@ List.of_seq @@ Hashtbl.to_seq overlaps


let find_non_overlapping_claim (claims: claim list): int =
    let grid = Hashtbl.create 100000 in
    let overlapping_claims = Hashtbl.create 100000 in
    let rec loop = function
        | [] -> ()
        | { id; x; y; width; height } :: cs ->
            for i = x to x + width - 1 do
                for j = y to y + height - 1 do
                    let ids_in_cell = get_or_else grid (i, j) in
                    add_intersection overlapping_claims (i, j) ids_in_cell id;
                    Hashtbl.replace grid (i, j) (IntSet.add id ids_in_cell)
                done;
            done;
            loop cs in
    loop claims;
    find_non_overlapping_claim overlapping_claims


let () =
    let lines = read_lines "../resources/input_03.txt" in
    let claims = List.map claim_of_string lines in
    let result = find_non_overlapping_claim claims in
    print_int result; print_newline ()
