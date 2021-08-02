module GradeSchool

type School = Map<int, string list>

let empty : School = Map.empty

let add (student: string) (grade: int) (school: School) : School =
    match school.TryFind grade with
    | Some students ->
        school
        |> Map.remove grade
        |> Map.add grade ((student :: students) |> List.sortBy id)
    | None -> school |> Map.add grade [ student ]

let roster (school: School) : string list =
    school
    |> Map.toList
    |> List.collect (fun (_, y) -> y)

let grade (number: int) (school: School) : string list =
    match school.TryFind number with
    | Some students -> students
    | None -> []
