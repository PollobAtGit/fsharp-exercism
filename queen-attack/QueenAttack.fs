module QueenAttack

let create (position: int * int) =
    match position with
    | x, y -> x > -1 && y > -1 && x < 8 && y < 8

let canAttack (queen1: int * int) (queen2: int * int) =
    match (create queen1 && create queen2) with
    | false -> failwith "invalid move"
    | true ->
        match (queen1, queen2) with
        | (q_one_a, q_one_b), (q_two_c, q_two_d) ->
            (q_one_a = q_two_c || q_one_b = q_two_d)
            || (abs (q_one_a - q_two_c) = abs (q_one_b - q_two_d))
