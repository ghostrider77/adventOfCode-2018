
let read_line (filename: string): string =
    let channel = open_in filename in
    let line = input_line channel in
    close_in channel;
    line


let calc_final_polymer_length (line: string): int =
    let do_they_react (c1: char) (c2: char): bool =
        let is_lower (c: char): bool = Char.lowercase_ascii c = c in
        (Char.lowercase_ascii c1 = Char.lowercase_ascii c2) &&
        ((is_lower c1 && not (is_lower c2)) || (not (is_lower c1) && is_lower c2)) in
    let rec loop (stack: char list) = function
        | [] -> List.length stack
        | c :: cs ->
            match stack with
                | [] -> loop [c] cs
                | x :: xs -> if do_they_react c x then loop xs cs else loop (c :: stack) cs in
    loop [] (List.of_seq @@ String.to_seq line)


let () =
    let line = read_line "../resources/input_05.txt" in
    let result = calc_final_polymer_length line in
    print_int result; print_newline ()
