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
