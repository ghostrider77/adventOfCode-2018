type coordinate = {x: int; y: int}

type bounding_box = {x_min: int; x_max: int; y_min: int; y_max: int}


module CoordSet = Set.Make(
    struct
        type t = coordinate
        let compare {x = x1; y = y1} {x = x2; y = y2} = Stdlib.compare (x1, y1) (x2, y2)
    end
)

module Grid = Map.Make(
    struct
        type t = coordinate
        let compare {x = x1; y = y1} {x = x2; y = y2} = Stdlib.compare (x1, y1) (x2, y2)
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


let calc_closest_centroid (point: coordinate) (centroids: coordinate list): coordinate option =
    let distances = List.map (calc_distance point) centroids in
    let smallest = List.fold_left min max_int distances in
    let closest_centroids = List.(distances |> combine centroids |> filter (fun (c, d) -> d = smallest)) in
    match closest_centroids with
        | [(c, _)] -> Some c
        | _ -> None


let get_points_in_box ({x_min; x_max; y_min; y_max}: bounding_box): coordinate list =
    let xs = List.init (x_max - x_min + 1) (fun ix -> ix + x_min) in
    let ys = List.init (y_max - y_min + 1) (fun jy -> jy + y_min) in
    List.concat_map (fun x -> List.map (fun y -> {x; y}) ys) xs


let calc_largest_bounded_neighbourhood (centroids: coordinate list): int =
    let bounds = get_bounds centroids in
    let is_point_on_boundary ({x; y}: coordinate): bool =
        y = bounds.y_max || y = bounds.y_min || x = bounds.x_max || x = bounds.x_min in
    let rec loop (acc: CoordSet.t Grid.t) = function
        | [] -> let area_sizes = Grid.map CoordSet.cardinal acc
                in Grid.fold (fun _ size max_size -> max size max_size) area_sizes 0
        | x :: xs -> match calc_closest_centroid x centroids with
            | Some closest_centroid when Grid.mem closest_centroid acc ->
                if is_point_on_boundary x then loop (Grid.remove closest_centroid acc) xs
                else loop (Grid.add closest_centroid (CoordSet.add x (Grid.find closest_centroid acc)) acc) xs
            | _ -> loop acc xs in
    let initial_centroids = List.filter (fun c -> not (is_point_on_boundary c)) centroids in
    let grid = List.fold_left (fun g c -> Grid.add c (CoordSet.singleton c) g) Grid.empty initial_centroids in
    let points = get_points_in_box bounds
    in loop grid points


let () =
    let lines = read_lines "../resources/input_06.txt" in
    let points = parse_coordinates lines in
    let result = calc_largest_bounded_neighbourhood points in
    print_int result; print_newline ()
