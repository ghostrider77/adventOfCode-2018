
let read_lines filename =
    let channel = open_in filename in
        let rec loop acc =
            try
                let line = input_line channel in
                loop (line :: acc)
            with End_of_file -> List.rev acc in
        let lines = loop [] in
        close_in channel;
    lines


let () =
    let lines = read_lines "../resources/input_01.txt" in
    let changes = List.map int_of_string lines in
    let result = List.fold_left (+) 0 changes in
    print_int result; print_newline ()
