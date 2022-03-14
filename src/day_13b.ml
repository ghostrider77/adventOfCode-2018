type coord = int * int

type track_piece = Straight | Curve of char | Intersection

type direction = Up | Down | Left | Right

type turning = LeftTurn | Forward | RightTurn

type cart = {position: coord; heading: direction; current_turning: turning}

module CoordMap = Map.Make(
    struct
        type t = coord
        let compare = Stdlib.compare
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


let read_track (lines: string list): track_piece CoordMap.t =
    let rec loop (acc: track_piece CoordMap.t) (y: int) = function
        | [] -> acc
        | row :: xss ->
            let row_with_index = row |> String.to_seqi |> List.of_seq in
            let parse_piece (grid: track_piece CoordMap.t) ((x, c): int * char): track_piece CoordMap.t =
                match c with
                    | ('/' | '\\') -> CoordMap.add (x, y) (Curve c) grid
                    | '+' -> CoordMap.add (x, y) Intersection grid
                    | ' ' -> grid
                    | _ -> CoordMap.add (x, y) Straight grid in
            let acc' = List.fold_left parse_piece acc row_with_index in
            loop acc' (y + 1) xss in
    loop CoordMap.empty 0 lines


let direction_of_char = function
    | '>' -> Some Right
    | 'v' -> Some Down
    | '<' -> Some Left
    | '^' -> Some Up
    | _ -> None


let find_initial_cart_positions (lines: string list): cart list =
    let rec loop (carts: cart list) (y: int) = function
        | [] -> carts
        | row :: xss ->
            let row_with_index = row |> String.to_seqi |> List.of_seq in
            let parse_piece (acc: cart list) ((x, c): int * char): cart list =
                match direction_of_char c with
                    | Some heading -> {position = (x, y); heading; current_turning = LeftTurn} :: acc
                    | None -> acc in
            let carts' = List.fold_left parse_piece carts row_with_index in
            loop carts' (y + 1) xss in
    loop [] 0 lines


let get_next_turn = function
    | LeftTurn -> Forward
    | Forward -> RightTurn
    | RightTurn -> LeftTurn


let get_next_position ((x, y): coord) = function
    | Up -> (x, y - 1)
    | Down -> (x, y + 1)
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)


let get_next_heading (heading: direction) (turning: turning): direction = match (heading, turning) with
    | (_, Forward) -> heading
    | (Up, LeftTurn) -> Left
    | (Up, RightTurn) -> Right
    | (Down, LeftTurn) -> Right
    | (Down, RightTurn) -> Left
    | (Left, LeftTurn) -> Down
    | (Left, RightTurn) -> Up
    | (Right, LeftTurn) -> Up
    | (Right, RightTurn) -> Down


let get_next_heading_in_curve (heading: direction) (curve: char): direction = match (heading, curve) with
    | (Up, '/') -> Right
    | (Down, '/') -> Left
    | (Right, '/') -> Up
    | (Left, '/') -> Down
    | (Up, '\\') -> Left
    | (Down, '\\') -> Right
    | (Right, '\\') -> Down
    | (Left, '\\') -> Up
    | _ -> failwith "Unknown curve direction"


let move (table: track_piece CoordMap.t) ({position = (x, y); heading; current_turning} as cart: cart): cart =
    let next_position = get_next_position (x, y) heading in
    match CoordMap.find next_position table with
        | Straight -> {cart with position = next_position}
        | Intersection ->
            let next_turning = get_next_turn current_turning in
            let next_heading = get_next_heading heading current_turning in
            {position = next_position; heading = next_heading; current_turning = next_turning}
        | Curve c ->
            let next_heading = get_next_heading_in_curve heading c in
            {cart with position = next_position; heading = next_heading}


let get_last_cart_location (track: track_piece CoordMap.t) (initial_carts: cart list): string =
    let compare_coords {position = (x1, y1)} {position = (x2, y2)} = Stdlib.compare (y1, x1) (y2, x2) in
    let has_cart_crashed ({position = (x, y)}: cart) (cs: cart list): bool =
        List.exists (fun {position = (a, b)} -> a = x && b = y) cs in
    let filter_out_chrashed_carts (carts: cart list) ({position = (x', y')}: cart): cart list =
        List.filter (fun ({position = (x, y)}) -> x <> x' || y <> y') carts in
    let rec move_each_cart (new_carts: cart list) = function
        | [] -> new_carts
        | c :: cs ->
            let c' = move track c in
            if has_cart_crashed c' cs || has_cart_crashed c' new_carts then
                move_each_cart (filter_out_chrashed_carts new_carts c') (filter_out_chrashed_carts cs c')
            else move_each_cart (c' :: new_carts) cs in
    let rec loop = function
        | [{position = (x, y)}] -> string_of_int x ^ "," ^ string_of_int y
        | carts ->
            let carts' = move_each_cart [] carts in
            loop (List.sort compare_coords carts') in
    loop (List.sort compare_coords initial_carts)


let () =
    let lines = read_lines "../resources/input_13.txt" in
    let track = read_track lines in
    let carts = find_initial_cart_positions lines in
    let result = get_last_cart_location track carts in
    print_endline result
