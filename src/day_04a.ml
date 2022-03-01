module IntMap = Map.Make(Int)


type record = { guard: int; asleep: int; wake_up: int }


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


let parse_lines (lines: string list): record list =
    let get_minute (line: string): int =
        Scanf.sscanf line "%s %d:%d" (fun _ _ minute -> minute) in
    let rec loop (acc: record list) (guard: int) = function
        | [] -> List.rev acc
        | x :: xs ->
            if String.ends_with ~suffix:"shift" x then
                let guard_id = Scanf.sscanf x "%s %s Guard #%d" (fun _ _ id -> id) in
                loop acc guard_id xs
            else
                match xs with
                    | [] -> failwith "Inconsistent number of lines."
                    | y :: rest ->
                        let asleep = get_minute x in
                        let wake_up = get_minute y in
                        loop ({guard; asleep; wake_up} :: acc) guard rest in
    let sorted_lines = List.sort Stdlib.compare lines in
    loop [] 0 sorted_lines


let find_guard (guards: int array IntMap.t): int =
    let total_minutes_slept = IntMap.map (fun arr -> Array.fold_left (+) 0 arr) guards in
    let (sleepy_guard, _) = List.fold_left
                                (fun (g_max, t_max) (g, t) -> if t > t_max then (g, t) else (g_max, t_max))
                                (0, 0)
                                (IntMap.bindings total_minutes_slept) in
    let minutes_slept = IntMap.find sleepy_guard guards in
    let indices_and_minutes = Array.combine (Array.init 60 (fun ix -> ix)) minutes_slept in
    let (ix, _) = Array.fold_left
                      (fun (max_ix, max_m) (ix, m) -> if m > max_m then (ix, m) else (max_ix, max_m))
                      (0, 0)
                      indices_and_minutes in
    sleepy_guard * ix


let find_sleepy_guard (records: record list): int =
    let rec loop (guards: int array IntMap.t) = function
        | [] -> find_guard guards
        | {guard; asleep; wake_up} :: rs ->
            if IntMap.mem guard guards then
                let sleeping_times = IntMap.find guard guards in
                for t = asleep to wake_up - 1 do
                    sleeping_times.(t) <- sleeping_times.(t) + 1
                done;
                loop guards rs
            else
                let arr = (Array.init 60 (fun ix -> if ix >= asleep && ix < wake_up then 1 else 0)) in
                loop (IntMap.add guard arr guards) rs in
    loop IntMap.empty records


let () =
    let lines = read_lines "../resources/input_04.txt" in
    let records = parse_lines lines in
    let result = find_sleepy_guard records in
    print_int result; print_newline ()
