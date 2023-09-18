module Program

open System
open Argu

type Arguments =
    | [<CliPrefix(CliPrefix.None); AltCommandLine("ser", "s"); First; Unique>]
      Serialize of file: string * message: string
    | [<CliPrefix(CliPrefix.None); AltCommandLine("de", "d"); First; Unique>]
      Deserialize of file: string * message: string
    | [<AltCommandLine("-i"); Unique>]
      Input of file: string
    | [<AltCommandLine("-o"); Unique>]
      Output of file: string
    
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Serialize _ -> "serialize <message> using proto v3 from <file>; input is textproto, output is binary"
            | Deserialize _ -> "deserialize <message> using proto v3 from <file>; input is binary, output is textproto"
            | Input _ -> "input file; if missed, `stdin` is used"
            | Output _ -> "output file; if missed, `stdout` is used"


[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<Arguments>(programName = "protos")
    
    try
        let arguments = parser.ParseCommandLine(argv, raiseOnUsage = true)
        if arguments.IsUsageRequested then
            printfn $"%s{parser.PrintUsage()}"
            0
        elif arguments.Contains Serialize then
            printfn "Serialize"
            0
        elif arguments.Contains Deserialize then
            printfn "Deserialize"
            0
        else
            printfn $"%s{parser.PrintUsage()}"
            -1
    with e ->
        printfn $"%s{e.Message}"
        -2
