module BeerSong

let getBottleStr x =
    match x > 1 with
    | true -> "bottles"
    | false -> "bottle"

let getState (startBottles: int) =

    let func (currentBottles: int) =

        let remaining = startBottles - currentBottles

        let onTheWall =
            match currentBottles = 1 with
            | true -> startBottles
            | false -> remaining + 1

        let onTheWallPhrase =
            match onTheWall = 0 with
            | true -> "No more bottles of beer on the wall, no more bottles of beer."
            | false -> $"{onTheWall} {getBottleStr onTheWall} of beer on the wall, {onTheWall} {getBottleStr onTheWall} of beer."

        let remainingPharse =
            match remaining <= 0 with
            | true when onTheWall = 0 -> "Go to the store and buy some more, 99 bottles of beer on the wall."
            | true when onTheWall > 0 -> "Take it down and pass it around, no more bottles of beer on the wall."
            | false -> $"Take one down and pass it around, {remaining} {getBottleStr (remaining)} of beer on the wall."
            | _ -> failwith "not matched"

        let r = [ onTheWallPhrase; remainingPharse ]

        match currentBottles > 1 with
        | true -> "" :: r
        | false -> r

    func

let recite (startBottles: int) (takeDown: int) =

    let mapper = getState startBottles

    [ 1 .. takeDown ] |> List.collect mapper
