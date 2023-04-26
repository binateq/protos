module TextFormatParser

open System
open FParsec
open StringParsers
open TextFormat

// https://developers.google.com/protocol-buffers/docs/text-format-spec
let comment: Parser<unit, unit> = skipString "#" .>> skipRestOfLine true
let skipSpaces = many (spaces1 <|> comment)

let identifier: StringParser =
    let first = pchar '_' <|> asciiLetter
    let next = first <|> digit
    
    many1Chars2 first next .>> skipSpaces
    
let fullIdentifier = stringsSepBy1 identifier (pstring ".")
    
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
    
let fromOct s = Convert.ToInt64(s, 8)
let fromHex s = Convert.ToInt64(s, 16)

let decimalInteger = decimalLiteral .>> skipSpaces |>> int64
let octalInteger: Parser<int64, unit> = (pstring "0") >>. (many1Chars octal) .>> skipSpaces |>> fromOct
let hexadecimalInteger: Parser<int64, unit> = (pstringCI "0X") >>. (many1Chars hex) .>> skipSpaces |>> fromHex
let decimalFloat =
    let f = pstringCI "F"
    choice [
        attempt (decimalLiteral .>> f .>> skipSpaces)
        floatLiteral .>> (optionString f) .>> skipSpaces
    ] |>> float
    
let stringLiteral: StringParser =
    let escape =
        choice [
            stringReturn "\\a" '\a'
            stringReturn "\\b" '\b'
            stringReturn "\\f" '\f'
            stringReturn "\\n" '\n'
            stringReturn "\\r" '\r'
            stringReturn "\\t" '\t'
            stringReturn "\\v" '\v'
            stringReturn "\\?" '?'
            stringReturn "\\\\" '\\'
            stringReturn "\\\'" '\''
            stringReturn "\\\"" '\"'
            pstring "\\x" >>. manyMinMaxSatisfy 1 2 isHex |>> (fromHex >> Convert.ToChar)
            pstring "\\u" >>. manyMinMaxSatisfy 4 4 isHex |>> (fromHex >> Convert.ToChar)
            // pstring "\\U000" >>. manyMinMaxSatisfy 5 5 isHex |>> (fromHex >> Convert.ToChar)
            // pstring "\\U0010" >>. manyMinMaxSatisfy 4 3 isHex |>> (fromHex >> ((+) 0x100_000L) >> Convert.ToChar) 
            pstring "\\" >>. manyMinMaxSatisfy 1 3 isOctal |>> (fromOct >> Convert.ToChar)
        ]
        
    let singleQuoteChar = noneOf "\0\'\n\\" <|> escape
    let doubleQuoteChar = noneOf "\0\"\n\\" <|> escape
    
    (between (skipChar '\'') (skipChar '\'') (manyChars singleQuoteChar)) <|>
    (between (skipChar '\"') (skipChar '\"') (manyChars doubleQuoteChar))

let stringValue = many1Strings (stringLiteral .>> skipSpaces)

let scalarValue =
    choice [
        stringValue |>> ScalarValue.String
        attempt decimalFloat |>> ScalarValue.Float
        attempt (pchar '-' >>. skipSpaces >>. decimalFloat) |>> ((~-) >> ScalarValue.Float)
        identifier |>> ScalarValue.Identifier
        attempt (pchar '-' >>. skipSpaces >>. identifier) |>> ScalarValue.SignedIdentifier
        attempt (pchar '-' >>. skipSpaces >>. decimalInteger) |>> ((~-) >> ScalarValue.DecSignedInteger)
        //attempt (pchar '-' >>. skipSpaces >>. octalInteger) |>> ((~-) >> ScalarValue.OctSignedInteger)
        //attempt (pchar '-' >>. skipSpaces >>. hexadecimalInteger) |>> ((~-) >> ScalarValue.HexSignedInteger)
        attempt decimalInteger |>> (uint64 >> ScalarValue.DecUnsignedInteger)
        //attempt octalInteger |>> (uint64 >> ScalarValue.OctUnsignedInteger)
        //hexadecimalInteger |>> (uint64 >> ScalarValue.HexUnsignedInteger)
    ]
    
let scalarList =
    let openBracket = skipChar '[' .>> skipSpaces
    let closeBracket = skipChar ']' .>> skipSpaces
    let values = sepBy scalarValue (skipChar ',' .>> skipSpaces)
    between openBracket closeBracket values

let fieldName =
    choice [
        identifier .>> skipSpaces |>> FieldName.Identifier
        attempt (pchar '[' .>> skipSpaces >>. fullIdentifier .>> skipSpaces .>> pchar ']' .>>skipSpaces) |>> FieldName.Extension
        pchar '[' .>> skipSpaces >>. fullIdentifier .>> skipSpaces .>> pchar '/' .>> skipSpaces .>>. fullIdentifier .>> skipSpaces .>> pchar ']' .>> skipSpaces |>> FieldName.Any
    ]
