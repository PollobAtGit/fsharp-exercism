module Raindrops

open System

let getRainDrop v =
    match v with
    | 3 -> "Pling"
    | 5 -> "Plang"
    | 7 -> "Plong"
    | _ -> failwith "not matched"

let convert (number: int) : string =
    let output =
        List.fold (fun acc n -> acc + if number % n = 0 then getRainDrop n else "") "" [ 3; 5; 7 ]

    match (String.IsNullOrWhiteSpace output) with
    | true -> number.ToString()
    | false -> output
