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


let find_first_repeating_frequency (frequency_changes: int list): int =
    let rec loop (visited_frequencies: IntSet.t) (current_frequency: int) = function
        | x :: xs ->
            let frequency = current_frequency + x in
            if IntSet.mem frequency visited_frequencies then frequency
            else loop (IntSet.add frequency visited_frequencies) frequency xs
        | _ -> loop visited_frequencies current_frequency frequency_changes in
    loop (IntSet.singleton 0) 0 frequency_changes


let () =
    let lines = read_lines "../resources/input_01.txt" in
    let changes = List.map int_of_string lines in
    let result = find_first_repeating_frequency changes in
    print_int result; print_newline ()
