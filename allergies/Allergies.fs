module Allergies

type Allergen =
    | Eggs
    | Shellfish
    | Peanuts
    | Strawberries
    | Tomatoes
    | Chocolate
    | Pollen
    | Cats

let scoreVsAllergesn =
    Map
        .empty
        .Add(1, Eggs)
        .Add(2, Peanuts)
        .Add(4, Shellfish)
        .Add(8, Strawberries)
        .Add(16, Tomatoes)
        .Add(32, Chocolate)
        .Add(64, Pollen)
        .Add(128, Cats)

let folder (allergensList: List<Allergen>, reducedScore: int) (current: int) =
    match current <= reducedScore with
    | true -> (allergensList @ [ scoreVsAllergesn.[current] ], reducedScore - current)
    | false -> (allergensList, reducedScore)

// not working for ``List - no allergen score parts``
let getAllergens codedAllergies =
    let allergens =
        [ 7 .. -1 .. 0 ]
        |> List.map (fun x -> int (float 2 ** float x))
        |> List.filter (fun x -> x <= codedAllergies)

    List.fold folder ([], codedAllergies) allergens


let allergicTo codedAllergies allergen =

    let (foundAllergens, _) = getAllergens codedAllergies

    foundAllergens |> List.contains allergen

let list codedAllergies =
    let (foundAllergens, _) = getAllergens codedAllergies
    foundAllergens |> List.rev
