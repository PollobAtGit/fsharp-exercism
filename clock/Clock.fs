module Clock

type Clock = { Hours: int; Minutes: int }

let getHours h = (h / 60) % 24
let getMinutes h = h % 60

let create hours minutes =

    let timeElapsed =
        match hours < 0 with
        | true ->
            match (abs hours % 24) with
            | 0 -> minutes
            | x -> ((24 - x) * 60) + minutes
        | false -> (hours * 60) + minutes

    let h =
        match timeElapsed < 0 with
        | true -> (24 * 60) + (timeElapsed % (24 * 60))
        | false -> timeElapsed

    { Hours = getHours h
      Minutes = getMinutes h }

let add minutes (c: Clock) =

    let totalMinutes = (c.Hours * 60 + c.Minutes) + minutes

    { Hours = (getHours totalMinutes)
      Minutes = (getMinutes totalMinutes) }

let subtract minutes c =
    let totalMinutes = (c.Hours * 60 + c.Minutes) - minutes

    let q =
        match totalMinutes < 0 with
        | true -> (24 * 60) - ((abs totalMinutes) % (24 * 60))
        | false -> totalMinutes

    { Hours = (getHours q)
      Minutes = (getMinutes q) }

let display (clock: Clock) =
    let hourStr = sprintf "%02i" clock.Hours
    let minutesStr = sprintf "%02i" clock.Minutes

    $"{hourStr}:{minutesStr}"
