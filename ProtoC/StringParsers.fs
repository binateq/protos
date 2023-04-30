module StringParsers

open FParsec

type StringParser = Parser<string, unit>

let stringPipe2 (parser1: StringParser) parser2 =
    let make s1 s2 = s1 + s2
    pipe2 parser1 parser2 make
    
let stringPipe3 (parser1: StringParser) parser2 parser3 =
    let make s1 s2 s3 = s1 + s2 + s3
    pipe3 parser1 parser2 parser3 make
    
let stringPipe4 (parser1: StringParser) parser2 parser3 parser4 =
    let make s1 s2 s3 s4 = s1 + s2 + s3 + s4
    pipe4 parser1 parser2 parser3 parser4 make
    
let optionString (p: StringParser) =
    opt p |>> Option.defaultValue ""
