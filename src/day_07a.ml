module CharMap = Map.Make(Char)

module CharSet = Set.Make(Char)


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


let create_dependencies (lines: string list): (CharSet.t * char list CharMap.t) =
    let parse_line (line: string): char * char =
        Scanf.sscanf line "Step %c must be finished before step %c can begin." (fun a b -> (a, b)) in
    let edges = List.map parse_line lines in
    let add_edge (acc: char list CharMap.t) ((a, b): char * char): char list CharMap.t =
        let insert_neighbour = function
            | None -> Some [a]
            | Some xs -> Some (a :: xs) in
        CharMap.update b insert_neighbour acc in
    let nodes = List.fold_left (fun ns (a, b) -> CharSet.(ns |> add a |> add b)) CharSet.empty edges in
    let dependencies = List.fold_left add_edge CharMap.empty edges
    in (nodes, dependencies)


let traverse_graph (lines: string list): char list =
    let all_steps, dependencies = create_dependencies lines in
    let rec loop (path: char list) (steps: CharSet.t): char list =
        let is_suitable_step (step: char): bool =
            let step_dependencies = if CharMap.mem step dependencies then CharMap.find step dependencies else [] in
            List.for_all (fun d -> List.mem d path) step_dependencies in
        match CharSet.(steps |> filter is_suitable_step |> min_elt_opt) with
            | None -> List.rev path
            | Some step -> loop (step :: path) (CharSet.remove step steps) in
    loop [] all_steps


let () =
    let lines = read_lines "../resources/input_07.txt" in
    let result = traverse_graph lines in
    print_endline @@ String.concat "" (List.map (String.make 1) result)
