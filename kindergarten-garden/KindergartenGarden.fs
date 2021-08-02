module KindergartenGarden

type Plant =
    | Grass
    | Clover
    | Radishes
    | Violets

// TODO: use Select(x, i -> ...)
let studentsById =
    Map
        .empty
        .Add("Alice", 1)
        .Add("Bob", 2)
        .Add("Charlie", 3)
        .Add("David", 4)
        .Add("Eve", 5)
        .Add("Fred", 6)
        .Add("Ginny", 7)
        .Add("Harriet", 8)
        .Add("Ileana", 9)
        .Add("Joseph", 10)
        .Add("Kincaid", 11)
        .Add("Larry", 12)

let convertToPlant x =
    match x with
    | 'C' -> Plant.Clover
    | 'G' -> Plant.Grass
    | 'R' -> Plant.Radishes
    | 'V' -> Plant.Violets
    | _ -> failwith "notmatched"
// 22
let plants (diagram: string) student =
    match studentsById.TryGetValue student with
    | (true, id) ->
        diagram.Split "\n"
        |> List.ofArray
        |> List.map (fun x -> x.Substring((((id - 1) * 2) % String.length x), 2))
        |> String.concat ""
        |> Seq.toList
        |> List.map convertToPlant
    | (false, _) -> failwith "student not found"
