type coordinate = {x: int; y: int}

type bounding_box = {x_min: int; x_max: int; y_min: int; y_max: int}


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


let parse_coordinates (lines: string list): coordinate list =
    let coordinate_of_string (line: string): coordinate =
        Scanf.sscanf line "%d, %d" (fun x y -> {x; y}) in
    List.map coordinate_of_string lines


let calc_distance ({x = x1; y = y1}: coordinate) ({x = x2; y = y2}: coordinate): int =
    abs (x1 - x2) + abs (y1 - y2)


let get_bounds (points: coordinate list): bounding_box =
    let process_point (acc: bounding_box) ({x; y}: coordinate): bounding_box =
        {x_min = min acc.x_min x; x_max = max acc.x_max x; y_min = min acc.y_min y; y_max = max acc.y_max y} in
    List.fold_left process_point {x_min = max_int; x_max = min_int; y_min = max_int; y_max = min_int} points


let calc_total_distance (point: coordinate) (centroids: coordinate list): int =
    List.fold_left (fun acc c -> acc + calc_distance point c) 0 centroids


let get_points_in_box ({x_min; x_max; y_min; y_max}: bounding_box): coordinate list =
    let xs = List.init (x_max - x_min + 1) (fun ix -> ix + x_min) in
    let ys = List.init (y_max - y_min + 1) (fun jy -> jy + y_min) in
    List.concat_map (fun x -> List.map (fun y -> {x; y}) ys) xs


let calc_close_region (centroids: coordinate list) (k: int): int =
    let bounds = get_bounds centroids in
    let rec loop (acc: int) = function
        | [] -> acc
        | x :: xs -> if calc_total_distance x centroids < k then loop (acc + 1) xs else loop acc xs in
    let points = get_points_in_box bounds
    in loop 0 points


let () =
    let lines = read_lines "../resources/input_06.txt" in
    let k = 10000 in
    let points = parse_coordinates lines in
    let result = calc_close_region points k in
    print_int result; print_newline ()
