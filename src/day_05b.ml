module CharSet = Set.Make(Char)


let read_line (filename: string): string =
    let channel = open_in filename in
    let line = input_line channel in
    close_in channel;
    line


let calc_final_polymer_length (polymer: char list) (excluded: char): int =
    let do_they_react (c1: char) (c2: char): bool =
        let is_lower (c: char): bool = Char.lowercase_ascii c = c in
        (Char.lowercase_ascii c1 = Char.lowercase_ascii c2) &&
        ((is_lower c1 && not (is_lower c2)) || (not (is_lower c1) && is_lower c2)) in
    let is_considered (c1: char): bool = excluded <> c1 && Char.uppercase_ascii excluded <> c1 in
    let rec loop (stack: char list) = function
        | [] -> List.length stack
        | c :: cs ->
            if is_considered c then
                match stack with
                    | [] -> loop [c] cs
                    | x :: xs -> if do_they_react c x then loop xs cs else loop (c :: stack) cs
            else loop stack cs in
    loop [] polymer


let calc_shortest_polymer_length (line: string): int =
    let units =
        String.fold_left (fun acc c -> let c' = Char.lowercase_ascii c in CharSet.add c' acc) CharSet.empty line in
    let polymer = line |> String.to_seq |> List.of_seq in
    CharSet.fold (fun c size -> let length = calc_final_polymer_length polymer c in min size length) units max_int


let () =
    let line = read_line "../resources/input_05.txt" in
    let result = calc_shortest_polymer_length line in
    print_int result; print_newline ()
