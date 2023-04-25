module TextFormatParser

open System
open FParsec
open StringParsers

// https://developers.google.com/protocol-buffers/docs/text-format-spec
let comment: Parser<unit, unit> = skipString "#" .>> skipRestOfLine true
let skipSpaces = many (spaces1 <|> comment)

let identifier: StringParser =
    let first = pchar '_' <|> asciiLetter
    let next = first <|> digit
    
    many1Chars2 first next
    
let decimalLiteral: StringParser =
    (many1Chars2 (anyOf "123456789") digit) <|> pstring "0"
    
let floatLiteral: StringParser =
    let e = pstringCI "E"
    let point = pstring "."
    let sign = pstring "+" <|> pstring "-"
    let digits0 = manyChars digit
    let digits1 = many1Chars digit
    let exp = stringPipe3 e (optionString sign) digits1
    
    choice [
        stringPipe3 point digits1 (optionString exp)
        attempt (stringPipe4 decimalLiteral point digits0 (optionString exp))
        stringPipe2 decimalLiteral exp
    ]
    
let toInteger (fromBase: int) (s: string) = Convert.ToInt32(s, fromBase)
    
let decimalInteger = decimalLiteral |>> int64
let octalInteger: Parser<int32, unit> = (pstring "0") >>. (many1Chars octal) |>> toInteger 8
let hexadecimalInteger: Parser<int32, unit> = (pstringCI "0X") >>. (many1Chars hex) |>> toInteger 16
let decimalFloat =
    let f = pstringCI "F"
    choice [
        attempt (decimalLiteral .>> f)
        floatLiteral .>> (optionString f)
    ] |>> float

let stringLit : Parser<string, unit> =
    let doubleQuotedChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let singleQuotedChar = satisfy (fun c -> c <> '\\' && c <> '\'')
    
    (between (skipString "\"") (skipString "\"") (manyChars doubleQuotedChar))
<|> (between (skipString "\'") (skipString "\'") (manyChars singleQuotedChar))

let stringValue = many1 (spaces >>. stringLit) |>> (fun ss -> ss |> Seq.ofList |> String.concat "")