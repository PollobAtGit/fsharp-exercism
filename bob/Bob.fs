module Bob

open System

let getLetters (input: string) =
    System.String.Concat(Array.ofSeq (Seq.toList input |> Seq.filter Char.IsLetter))

let response (input: string) : string =
    match input.Trim() with
    | "" -> "Fine. Be that way!"
    | x when
        x.EndsWith "?"
        && (getLetters input).Length <> 0
        && x = x.ToUpper()
        ->
        "Calm down, I know what I'm doing!"
    | x when x.EndsWith "?" -> "Sure."
    | x when (getLetters input).Length <> 0 && x = x.ToUpper() -> "Whoa, chill out!"
    | _ -> "Whatever."
