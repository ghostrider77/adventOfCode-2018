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


let calc_root_node_value (encoded_nodes: int list): int =
    let (tree, _) = parse_node encoded_nodes in
    let rec value ({children; metadata}: node): int =
        match children with
            | [] -> List.fold_left (+) 0 metadata
            | _ ->
                let get_value (ix: int): int =
                    if ix < 0 then 0
                    else match List.nth_opt children ix with
                        | None -> 0
                        | Some child -> value child in
                List.fold_left (fun acc ix -> acc + get_value (ix - 1)) 0 metadata
    in value tree


let () =
    let line = read_line "../resources/input_08.txt" in
    let encoding = convert_to_intlist line in
    let result = calc_root_node_value encoding in
    print_int result; print_newline ()
