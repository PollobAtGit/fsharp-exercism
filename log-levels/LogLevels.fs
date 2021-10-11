module LogLevels

let errorConst = "[ERROR]"
let warningConst = "[WARNING]"
let infoConst = "[INFO]"

type ErrorType =
    | Error
    | Warning
    | Info

let errorTypeToStr =
    Map
        .empty
        .Add(Error, "error")
        .Add(Warning, "warning")
        .Add(Info, "info")

let message (logLine: string) : string =
    logLine
        .Replace(errorConst + ": ", "")
        .Replace(warningConst + ": ", "")
        .Replace(infoConst + ": ", "")
        .Trim()

let logLevel (logLine: string) : string =

    let parts = logLine.Split(": ")

    match parts.[0] with
    | v when v = errorConst -> errorTypeToStr.[Error]
    | v when v = infoConst -> errorTypeToStr.[Info]
    | v when v = warningConst -> errorTypeToStr.[Warning]
    | _ -> failwith "not implemented"

let reformat (logLine: string) : string =
    (message logLine)
    + " ("
    + (logLevel logLine)
    + ")"
