module TwoFer

open System

let twoFer (input: string option) : string =
    match input with
    | None -> "One for you, one for me."
    | Some v ->
        match (String.IsNullOrEmpty v) with
        | true -> "One for you, one for me."
        | false -> $"One for {v}, one for me."
