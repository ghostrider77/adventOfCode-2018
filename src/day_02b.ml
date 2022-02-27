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


let calc_common_letters (box_ids: string list): string =
    let hamming_distance (s1: char list) (s2: char list): int =
        List.fold_left2 (fun acc c1 c2 -> if c1 = c2 then acc else acc + 1) 0 s1 s2 in
    let cartesian_product (xs: 'a list) (ys: 'b list): ('a * 'b) list =
        List.concat (List.map (fun x -> List.map (fun y -> (x, y)) ys) xs) in
    let rec loop = function
        | (x, y) :: dss -> let d = hamming_distance x y in if d = 1 then (x, y) else loop dss
        | [] -> failwith "No id pairs found with Hamming distance one." in
    let ids = List.map (fun s -> List.of_seq @@ String.to_seq s) box_ids in
    let (id1, id2) = loop @@ cartesian_product ids ids in
    let result = fst @@ List.(combine id1 id2 |> filter (fun (c1, c2) -> c1 = c2) |> split) in
    String.concat "" @@ List.map (String.make 1) result


let () =
    let box_ids = read_lines "../resources/input_02.txt" in
    let result = calc_common_letters box_ids in
    print_endline result
