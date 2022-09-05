module TextFormatParser

open FParsec
open TextFormat

// https://developers.google.com/protocol-buffers/docs/text-format-spec

let comment : Parser<unit, unit> = skipString "#" .>> skipRestOfLine true

let identifier : Parser<string, unit> =
    let isLetterOrDigit c = isLetter c || isDigit c
    
    many1Satisfy2L isLetter isLetterOrDigit "identifier"

let decLit : Parser<string, unit> = notFollowedBy (pstring "0") >>. (many1Satisfy isDigit)
                                <|> pstring "0" .>> notFollowedBy digit
let exp : Parser<string, unit> = (pstringCI "e+" <|> pstringCI "e-" <|> pstringCI "e")
                             .>>. many1Satisfy isDigit |>> (fun (e, digits) -> e + digits)

let floatLit : Parser<string, unit> = (pstring "." .>>. many1Satisfy isDigit .>>. opt exp
                                       |>> (fun ((dot, digits), exp) -> dot + digits + Option.defaultValue "" exp))
                                  <|> (decLit .>>. exp |>> (fun (digits, exp) -> digits + exp))
                                  <|> (decLit .>>. pstring "." .>>. (manySatisfy isDigit) .>>. opt exp
                                       |>> (fun (((d1, dot), d2), exp) -> d1 + dot + d2 + Option.defaultValue "" exp))
