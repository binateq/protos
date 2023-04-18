module Program

open System
open Proto3
open Proto3Parser

let println (s: string) = Console.WriteLine(s)

printfn "protoserdes s <proto3> [--input=<file>] [--output=<file>]"
printfn "protoserdes d <proto3> [--input=<file>] [--output=<file>]"
printfn "protoserdes --help"
printfn ""
printfn "s, ser, serialize -- serialize text input to binary output"
printfn "d, des, deserialize -- deserialize binary input to text output"
printfn "<proto3> is a description of a message."
printfn "-i=file, --input=file -- input <file>. If missed, stdin used."
printfn "-o=file, --output=file -- output <file>. If missed, stdout used."
printfn ""
printfn "-h, --help -- print this help."
