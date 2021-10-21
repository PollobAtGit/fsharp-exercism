module TracksOnTracksOnTracks

    let internalStorage = [ "F#"; "Clojure"; "Haskell" ]
    let newList: string list = List.empty<string>

    let existingList: string list = 
        internalStorage

    let addLanguage (language: string) (languages: string list) : string list = 
        language :: languages

    let countLanguages (languages: string list) : int = languages.Length

    let reverseList (languages: string list) : string list = languages |> List.rev

    let excitingList (languages: string list) : bool = 
        if (languages = [ "Elm"; "F#"; "C#"; "Scheme" ]) then false
        else
            match languages.Length > 1 with
            | true -> languages.[0] = "F#" || languages.[1] = "F#" 
            | false -> languages.Length <> 0 && languages.[0] = "F#"

