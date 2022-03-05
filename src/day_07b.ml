module CharMap = Map.Make(Char)

module CharSet = Set.Make(Char)

type job = {step: char; finish_time: int}

module Buffer = Set.Make(
    struct
        type t = job
        let compare ({step = c1; finish_time = t1}: job) ({step = c2; finish_time = t2}: job): int =
            if t1 = t2 then Stdlib.compare c1 c2 else Stdlib.compare t1 t2
    end
)


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


let get_largest_finish_time_from_buffer (workers: Buffer.t) (time: int): int =
    match Buffer.max_elt_opt workers with
        | None -> time
        | Some {finish_time} -> finish_time


let process_steps (dependencies: char list CharMap.t) (all_steps: CharSet.t): int =
    let put_steps_into_buffer (buffer: Buffer.t) (steps: CharSet.t) (path: char list) (t: int): (Buffer.t * CharSet.t) =
        let is_suitable_step (step: char): bool =
            let step_dependencies = if CharMap.mem step dependencies then CharMap.find step dependencies else [] in
            List.for_all (fun d -> List.mem d path) step_dependencies in
        let rec loop (buffer: Buffer.t) (remaining_steps: CharSet.t) =
            if Buffer.cardinal buffer = 5 then (buffer, remaining_steps)
            else match CharSet.(remaining_steps |> filter is_suitable_step |> min_elt_opt) with
                | None -> (buffer, remaining_steps)
                | Some step ->
                    let finish = t + Char.code step - Char.code 'A' + 60 in
                    loop (Buffer.add {step; finish_time = finish} buffer) (CharSet.remove step remaining_steps) in
        loop buffer steps in
    let rec loop (path: char list) (steps: CharSet.t) (workers: Buffer.t) (time: int): int =
        if CharSet.is_empty steps then 1 + get_largest_finish_time_from_buffer workers time
        else
            let (path', time', workers') = match Buffer.min_elt_opt workers with
                | Some ({step; finish_time} as finished_job) ->
                    (step :: path, finish_time + 1, Buffer.remove finished_job workers)
                | None -> (path, time, workers) in
            let (workers'', steps') = put_steps_into_buffer workers' steps path' time'
            in loop path' steps' workers'' time' in
    loop [] all_steps Buffer.empty 0


let () =
    let lines = read_lines "../resources/input_07.txt" in
    let nodes, dependencies = create_dependencies lines in
    let result = process_steps dependencies nodes in
    print_int result; print_newline ()
