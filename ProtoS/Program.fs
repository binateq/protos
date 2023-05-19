module Program

open System
open Proto3
open Proto3Parser

let println (s: string) = Console.WriteLine(s)


println "protos <proto3> --bin [--input=<file>] [--output=<file>]"
println "protos <proto3> --text [--input=<file>] [--output=<file>]"
println "protos [--help]"
println ""
println "--bin, -b    -- serialize text input to binary output"
println "--text, -t d -- deserialize binary input to text output"
println "<proto3> is the description of the message."
println ""
println "-i=file, --input=file  -- input <file>, if missed, stdin used"
println "-o=file, --output=file -- output <file>, if missed, stdout used"
println ""
println "-h, --help   -- print this help."
