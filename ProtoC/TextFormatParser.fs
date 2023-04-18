module TextFormatParser

open System
open FParsec

// https://developers.google.com/protocol-buffers/docs/text-format-spec

let comment : Parser<unit, unit> = skipString "#" .>> skipRestOfLine true

let identifier : Parser<string, unit> =
    let isLetterOrDigit c = isLetter c || isDigit c
    
    many1Satisfy2L isLetter isLetterOrDigit "identifier"
    
let decimals0 : Parser<string, unit> = manySatisfy isDigit
let decimals1 : Parser<string, unit> = many1Satisfy isDigit

// 0, 123 — yes, 0123 — no
let decLit : Parser<string, unit> = notFollowedBy (pstring "0") >>. decimals1
                                <|> pstring "0" .>> notFollowedBy digit
                                
let optStr (p: Parser<string, unit>) = opt p |>> Option.defaultValue String.Empty

let (>+>) (p1: Parser<string, unit>) (p2: Parser<string, unit>) = p1 .>>. p2 |>> (fun (a, b) -> a + b)

let exp : Parser<string, unit> = pstringCI "e" >+> optStr (pstring "+" <|> pstring "-") >+> decimals1

let floatLit : Parser<string, unit> = (pstring "." >+> decimals1 >+> optStr exp)
                                  <|> (attempt (decLit >+> exp))
                                  <|> (decLit >+> pstring "." >+> decimals0 >+> optStr exp)

let decInt : Parser<int, unit> = decLit |>> Int32.Parse
let octInt : Parser<int, unit> = pstring "0" >>. many1Satisfy isOctal |>> (fun s -> Convert.ToInt32(s, 8))
let hexInt : Parser<int, unit> = pstringCI "0x" >>. many1Satisfy isHex |>> (fun s -> Convert.ToInt32(s, 16))
let decFloat : Parser<float, unit> = (attempt (decLit .>> pstringCI "f" |>> Double.Parse))
                                 <|> (floatLit .>> (opt (pstringCI "f")) |>> (fun s -> Double.Parse(s, System.Globalization.CultureInfo.InvariantCulture)))

let stringLit : Parser<string, unit> =
    let doubleQuotedChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let singleQuotedChar = satisfy (fun c -> c <> '\\' && c <> '\'')
    
    (between (skipString "\"") (skipString "\"") (manyChars doubleQuotedChar))
<|> (between (skipString "\'") (skipString "\'") (manyChars singleQuotedChar))

let stringValue = many1 (spaces >>. stringLit) |>> (fun ss -> ss |> Seq.ofList |> String.concat "")