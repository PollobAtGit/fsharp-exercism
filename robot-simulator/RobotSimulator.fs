module RobotSimulator

type Direction =
    | North
    | East
    | South
    | West

type Position = int * int

type Robot =
    { Direction: Direction
      Position: Position }

let create direction position =
    { Direction = direction
      Position = position }

let moveByCommand (instruction: char) (robot: Robot) =
    match instruction with
    | 'R' ->
        let changedDirection =
            match robot.Direction with
            | Direction.North -> Direction.East
            | Direction.East -> Direction.South
            | Direction.South -> Direction.West
            | Direction.West -> Direction.North

        { Direction = changedDirection
          Position = robot.Position }
    | 'L' ->
        let changedDirection =
            match robot.Direction with
            | Direction.North -> Direction.West
            | Direction.West -> Direction.South
            | Direction.South -> Direction.East
            | Direction.East -> Direction.North

        { Direction = changedDirection
          Position = robot.Position }
    | 'A' ->
        let (x, y) = robot.Position

        let d =
            match robot.Direction with
            | Direction.South -> (x, y - 1)
            | Direction.East -> (x + 1, y)
            | Direction.West -> (x - 1, y)
            | Direction.North -> (x, y + 1)

        { Direction = robot.Direction
          Position = d }
    | _ -> failwith "not matched"


let robotMovementFolder (currentState: Robot) (move: char) = moveByCommand move currentState

let move instructions (robot: Robot) =
    List.fold robotMovementFolder robot (instructions |> List.ofSeq)

let rotateLeft =
    function
    | North -> West
    | West -> South
    | South -> East
    | East -> North

let rotateRight =
    function
    | North -> East
    | East -> South
    | South -> West
    | West -> North

type Grid() =
    member __.v() = 3
