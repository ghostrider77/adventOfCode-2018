open Batteries

type node = {children: node list; metadata: int list}


let read_line (filename: string): string =
    let channel = open_in filename in
    let line = input_line channel in
    close_in channel;
    line


let convert_to_intlist (line: string): int list =
    List.map int_of_string Str.(line |> split (regexp " "))


let rec parse_node = function
    | nr_children :: nr_metadata :: rest ->
        let rec loop (xs: int list) (parsed_children: node list) (k: int): node list * int list =
            if k = nr_children then (List.rev parsed_children, xs)
            else let (child, xss) = parse_node xs in loop xss (child :: parsed_children) (k + 1) in
        let (children, xss) = loop rest [] 0 in
        let metadata = List.take nr_metadata xss in ({metadata; children}, List.drop nr_metadata xss)
    | _ -> failwith "Inconsistent encoding of tree."


let calc_sum_of_metadata (encoded_nodes: int list): int =
    let (tree, _) = parse_node encoded_nodes in
    let rec sum ({children; metadata}: node): int =
        let s = List.fold_left (+) 0 metadata in
        List.fold_left (fun acc child -> acc + sum child) s children
    in sum tree


let () =
    let line = read_line "../resources/input_08.txt" in
    let encoding = convert_to_intlist line in
    let result = calc_sum_of_metadata encoding in
    print_int result; print_newline ()
