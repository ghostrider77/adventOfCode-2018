type coordinate = {x: int; y: int}

type point = {position: coordinate; v_x: int; v_y: int}

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


let parse_points (lines: string list): point list =
    let point_of_string (line: string): point =
        Scanf.sscanf line "position=< %d, %d> velocity=< %d, %d>" (fun x y v_x v_y -> {position = {x; y}; v_x; v_y}) in
    List.map point_of_string lines


let calc_enclosing_box (points: point list): bounding_box =
    let process_point (acc: bounding_box) ({position = {x; y}}: point): bounding_box =
        {x_min = min acc.x_min x; x_max = max acc.x_max x; y_min = min acc.y_min y; y_max = max acc.y_max y} in
    List.fold_left process_point {x_min = max_int; x_max = min_int; y_min = max_int; y_max = min_int} points


let get_volume_of_enclosing_box (points: point list): int =
    let {x_min; x_max; y_min; y_max} = calc_enclosing_box points in
    (x_max - x_min) * (y_max - y_min)


let get_right_constellation (initial_points: point list): int =
    let move_points (points: point list): point list =
        List.map (fun {position = {x; y}; v_x; v_y} -> {position = {x = x + v_x; y = y + v_y}; v_x; v_y} ) points in
    let rec loop (points: point list) (volume: int) (k: int): int =
        let points' = move_points points in
        let volume' = get_volume_of_enclosing_box points' in
        if volume' >= volume then k else loop points' volume' (k + 1)
    in loop initial_points (get_volume_of_enclosing_box initial_points) 0


let () =
    let lines = read_lines "../resources/input_10.txt" in
    let points = parse_points lines in
    let result = get_right_constellation points in
    print_int result; print_newline ()
