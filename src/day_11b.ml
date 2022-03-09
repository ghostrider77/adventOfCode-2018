
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


let calc_prefix_sum_matrix (matrix: (int array) array) (size: int): ((int array) array) =
    let prefix = Array.make_matrix (size + 1) (size + 1) 0 in
    for ix = 1 to size do
        for jy = 1 to size do
            let s = prefix.(ix - 1).(jy) + prefix.(ix).(jy - 1) - prefix.(ix - 1).(jy - 1) + matrix.(ix - 1).(jy - 1) in
            prefix.(ix).(jy) <- s;
        done;
    done;
    prefix


let find_max_block (prefix: (int array) array) (size: int) (k: int): (int * int) * int =
    let x_max = ref 0 in
    let y_max = ref 0 in
    let p_max = ref min_int in
    for jy = 0 to size - k do
        for ix = 0 to size - k do
            let s = prefix.(jy + k).(ix + k) - prefix.(jy).(ix + k) - prefix.(jy + k).(ix) + prefix.(jy).(ix) in
            if s > !p_max then
                begin
                    p_max := s;
                    x_max := ix;
                    y_max := jy;
                end
        done;
    done;
    ((!x_max, !y_max), !p_max)


let find_block_with_largest_total_power (size: int) (serial_number: int): string =
    let power_matrix = create_power_level_matrix size serial_number in
    let prefix_sums = calc_prefix_sum_matrix power_matrix size in
    let rec loop (((x_max, y_max, p_max), k_max) as acc: ((int * int *int) * int)) (k: int): string =
        if k > size then [x_max + 1; y_max + 1; k_max] |> List.map string_of_int |> String.concat ","
        else
            let ((x, y), p) = find_max_block prefix_sums size k in
            if p > p_max then loop ((x, y, p), k) (k + 1)
            else loop acc (k + 1) in
    loop ((0, 0, min_int), 0) 1


let () =
    let size = 300 in
    let grid_serial_number = 7803 in
    let result = find_block_with_largest_total_power size grid_serial_number in
    print_endline result
