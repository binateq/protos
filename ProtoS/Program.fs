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
            let file, message = arguments.GetResult Serialize
            let schema = SchemaBuilder.build (Proto3Parser.parse file)
            use input =
                match arguments.TryGetResult Input with
                | Some inputFile -> System.IO.File.OpenRead(inputFile) :> System.IO.Stream
                | None -> Console.OpenStandardInput() 
            use output = Console.OpenStandardOutput()
            let fields = TextprotoParser.parse input
            
            Serde.serializeMessage message fields schema output
            0 
        elif arguments.Contains Deserialize then
            let file, message = arguments.GetResult Deserialize
            let schema = SchemaBuilder.build (Proto3Parser.parse file)
            use input = Console.OpenStandardInput()
            let fields = Serde.deserializeMessage message schema input
            
            Serde.printFields 2 fields Console.Out
            0
        else
            printfn $"%s{parser.PrintUsage()}"
            -1
    with e ->
        printfn $"%s{e.Message}"
        -2
