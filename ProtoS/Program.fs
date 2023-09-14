module Program

open System

let println (s: string) = Console.WriteLine(s)


println "protos <proto3> <message-name> --bin [--input=<file>] [--output=<file>]"
println "protos <proto3> <message-name> --text [--input=<file>] [--output=<file>]"
println "protos [--help]"
println ""
println "--bin, -b    -- serialize text input to binary output"
println "--text, -t d -- deserialize binary input to text output"
println "<proto3> is the .proto the"
println "<message-name> is the message name"
println ""
println "-i=file, --input=file  -- input <file>, if missed, stdin used"
println "-o=file, --output=file -- output <file>, if missed, stdout used"
println ""
println "-h, --help   -- print this help."
