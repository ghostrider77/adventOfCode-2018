open Batteries


let calc_recipe_scores (nr_recipes: int): string =
    let rec loop (recipes: int Vect.t) (p1: int) (p2: int): string =
        if Vect.length recipes >= nr_recipes + 10 then
            let ixs = List.init 10 (fun ix -> ix + nr_recipes) in
            let scores = List.map (Vect.get recipes) ixs in
            scores |> List.map string_of_int |> String.concat ""
        else
            let s1 = Vect.get recipes p1 in
            let s2 = Vect.get recipes p2 in
            let score_sum = s1 + s2 in
            let recipes' =
                if score_sum < 10 then Vect.append score_sum recipes
                else Vect.(recipes |> append 1 |> append (score_sum - 10)) in
            let size = Vect.length recipes' in
            let p1' = (p1 + s1 + 1) mod size in
            let p2' = (p2 + s2 + 1) mod size in
            loop recipes' p1' p2'
    in loop (Vect.of_list [3; 7]) 0 1


let () =
    let nr_recipes = 939601 in
    let result = calc_recipe_scores nr_recipes in
    print_endline result
