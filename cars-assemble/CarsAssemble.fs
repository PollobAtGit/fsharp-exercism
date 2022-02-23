module CarsAssemble

let successRate (speed: int) : float =
    match speed with
    | x when x >= 1 && x <= 4 -> 1.0
    | x when x >= 5 && x <= 8 -> 0.9
    | 0 -> 0.0
    | 9 -> 0.8
    | 10 -> 0.77
    | _ -> failwith "No match found"

let productionRatePerHour (speed: int) : float =
    (speed |> double) * 221.0 * successRate speed

let workingItemsPerMinute (speed: int) : int =
    (productionRatePerHour speed) / 60.0 |> int
