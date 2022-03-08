open Batteries

module IntMap = Map.Make(Int)


let read_line (filename: string): string =
    let channel = open_in filename in
    let line = input_line channel in
    close_in channel;
    line


let parse_input (line: string): int * int =
    Scanf.sscanf line "%d players; last marble is worth %d points" (fun n m -> (n, m))


let calc_marble_game_score (nr_elves: int) (nr_marbles: int): int =
    let rec rotate (deque: 'a Deque.t) (n: int): 'a Deque.t =
        if n = 0 then deque
        else rotate (Deque.rotate_backward deque) (n - 1) in
    let rec loop (circle: int Deque.t) (scores: int IntMap.t) (k: int): int =
        if k + 1 = nr_marbles then IntMap.fold (fun _ score acc -> max score acc) scores 0
        else if k mod 23 = 0 then
            match Deque.rear (rotate circle 7) with
                | None -> failwith "Empty deque."
                | Some (circle', item) ->
                    let elf_id = k mod nr_elves in
                    let scores' = (IntMap.add elf_id ((IntMap.find_default 0 elf_id scores) + item + k) scores) in
                    loop (Deque.rotate_forward circle') scores' (k + 1)
        else loop (k |> Deque.(circle |> rotate_forward |> snoc)) scores (k + 1)
    in loop Deque.(empty |> cons 0) IntMap.empty 1


let () =
    let line = read_line "../resources/input_09.txt" in
    let nr_elves, nr_marbles = parse_input line in
    let result = calc_marble_game_score nr_elves (100 * nr_marbles) in
    print_int result; print_newline ()
