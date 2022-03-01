module CharMap = Map.Make(Char)


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


let calc_checksum (box_ids: string list): int =
    let get_or_else (counts: int CharMap.t) (c: char): int = match (CharMap.find_opt c counts) with
        | None -> 0
        | Some count -> count in
    let calc_letter_counts (id: string): int CharMap.t =
        String.fold_left (fun acc c -> let cnt = get_or_else acc c in CharMap.add c (cnt + 1) acc) CharMap.empty id in
    let has_letter_with_given_count (letter_counts: int CharMap.t) (k: int): bool =
        CharMap.exists (fun _ cnt -> cnt = k) letter_counts in
    let counts = List.map calc_letter_counts box_ids in
    let twos = List.fold_left (fun acc cnts -> if has_letter_with_given_count cnts 2 then acc + 1 else acc) 0 counts in
    let threes = List.fold_left (fun acc cnts -> if has_letter_with_given_count cnts 3 then acc + 1 else acc) 0 counts
    in twos * threes


let () =
    let box_ids = read_lines "../resources/input_02.txt" in
    let result = calc_checksum box_ids in
    print_int result; print_newline ()
