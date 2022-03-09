
let calc_power_level (x: int) (y: int) (serial_number: int): int =
    let rack_id = (x + 10) in
    let power_level = (rack_id * y + serial_number) * rack_id in
    (power_level mod 1000) / 100 - 5


let create_power_level_matrix (size: int) (serial_number: int): int array array =
    let power_levels = Array.make_matrix size size 0 in
    for y = 1 to size do
        for x = 1 to size do
            power_levels.(y - 1).(x - 1) <- calc_power_level x y serial_number;
        done;
    done;
    power_levels


let calc_total_power (grid: (int array) array) (x: int) (y: int): int =
    let sum = ref 0 in
    for jy = 0 to 2 do
        for ix = 0 to 2 do
            sum := !sum + grid.(y - 1 + jy).(x - 1 + ix);
        done;
    done;
    !sum


let calc_coordinates (size: int): (int * int) list =
    let cartesian_product (xs: 'a list) (ys: 'b list): ('a * 'b) list =
        List.concat (List.map (fun x -> List.map (fun y -> (x, y)) ys) xs) in
    let xs = List.init size (fun ix -> ix + 1)
    in cartesian_product xs xs


let find_block_with_largest_total_power (serial_number: int): string =
    let size = 300 in
    let power_matrix = create_power_level_matrix size serial_number in
    let coordinates = calc_coordinates (size - 2) in
    let process_coord (((a, b), max_power): (int * int) * int) ((x, y): int * int): (int * int) * int =
        let p = calc_total_power power_matrix x y in
        if p > max_power then ((x, y), p) else ((a, b), max_power) in
    let ((a, b), s) = List.fold_left process_coord ((0, 0), min_int) coordinates in
    string_of_int a ^ "," ^ string_of_int b


let () =
    let grid_serial_number = 7803 in
    let result = find_block_with_largest_total_power grid_serial_number in
    print_endline result
