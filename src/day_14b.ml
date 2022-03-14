
module DynArray : sig
    type t
    val empty: t
    val size: t -> int
    val add: int -> t -> t
    val get: t -> int -> int
    val set: t -> int -> int -> unit
    val drop: int -> t -> t
    val take_right: int -> t -> int list
end = struct
    type t = {arr: int array; actual_size: int}

    let smallest_size = 8
    let m = 1.125

    let empty = {arr = Array.make smallest_size 0; actual_size = 0}

    let size {actual_size} = actual_size

    let add item {arr; actual_size} =
        let max_size = Array.length arr in
        if actual_size < max_size then
            let () = arr.(actual_size) <- item in
            {arr; actual_size = actual_size + 1}
        else
            let new_size = int_of_float @@ float max_size *. m in
            let arr' = Array.make new_size 0 in
            for ix = 1 to max_size do
                arr'.(ix - 1) <- arr.(ix - 1);
            done;
            arr'.(max_size) <- item;
            {arr = arr'; actual_size = max_size + 1}

    let get {arr; actual_size} ix =
        if ix >= 0 && ix < actual_size then arr.(ix)
        else raise @@ Invalid_argument "Index is outside of range."

    let set {arr; actual_size} ix item =
        if ix >= 0 && ix < actual_size then arr.(ix) <- item
        else raise @@ Invalid_argument "Index is outside of range."

    let drop k {arr; actual_size} =
        {arr; actual_size = max (actual_size - k) 0}

    let take_right n {arr; actual_size} =
        let rec loop (acc: int list) (k: int) =
            if actual_size - k < 0 || k > n then acc
            else loop (arr.(actual_size - k) :: acc) (k + 1) in
        loop [] 1
end


let convert_to_digits (target: string): int list =
    target |> String.to_seq |> List.of_seq |> List.map (String.make 1) |> List.map int_of_string


let calc_recipe_scores (target: int list): int =
    let target_size = List.length target in
    let rec loop (recipes: DynArray.t) (p1: int) (p2: int): int =
        let lst1 = DynArray.take_right target_size recipes in
        let lst2 = DynArray.(recipes |> drop 1 |> take_right target_size) in
        if lst1 = target then DynArray.size recipes - target_size
        else if lst2 = target then DynArray.size recipes - target_size - 1
        else
            let s1 = DynArray.get recipes p1 in
            let s2 = DynArray.get recipes p2 in
            let score_sum = s1 + s2 in
            let recipes' =
                if score_sum < 10 then DynArray.add score_sum recipes
                else DynArray.(recipes |> add 1 |> add (score_sum - 10)) in
            let size = DynArray.size recipes' in
            let p1' = (p1 + s1 + 1) mod size in
            let p2' = (p2 + s2 + 1) mod size in
            loop recipes' p1' p2'
    in loop DynArray.(empty |> add 3 |> add 7) 0 1


let () =
    let target = "939601" in
    let result = calc_recipe_scores (convert_to_digits target) in
    print_int result; print_newline ()
