module TextprotoParser

open FParsec
open Textproto

// https://developers.google.com/protocol-buffers/docs/text-format-spec
let comment: Parser<unit, unit> = skipChar '#' .>> skipRestOfLine true
let skipSpaces = many (spaces1 <|> comment) |>> ignore
let skipCharSpaces c = skipChar c .>> skipSpaces


let brackets content =
    between (skipCharSpaces '[') (skipCharSpaces ']') content


let braces content =
    between (skipCharSpaces '{') (skipCharSpaces '}') content


let angles content =
    between (skipCharSpaces '<') (skipCharSpaces '>') content


let commaSeparated value =
    sepBy value (skipCharSpaces ',')


let identifier =
    let first = pchar '_' <|> asciiLetter
    let next = first <|> digit
    
    many1Chars2 first next


let fullIdentifier = stringsSepBy1 identifier (pstring ".") .>> skipSpaces


let decimalLiteral =
    (many1Chars2 (anyOf "123456789") digit) <|> pstring "0"


let floatLiteral =
    let e = pstringCI "E"
    let point = pstring "."
    let optionalSign = opt (pstring "-" <|> pstring "+") |>> Option.defaultValue ""
    let digits0 = manyChars digit
    let digits1 = many1Chars digit
    let exp = pipe3 e optionalSign digits1 (fun s1 s2 s3 -> s1 + s2 + s3)
    let optionalExp = opt exp |>> Option.defaultValue ""
    let f = pstringCI "F"
    let float =
        choice
          [ pipe3 point digits1 optionalExp (fun s1 s2 s3 -> s1 + s2 + s3)
            attempt (pipe4 decimalLiteral point digits0 optionalExp (fun s1 s2 s3 s4 -> s1 + s2 + s3 + s4))
            pipe2 decimalLiteral exp (fun s1 s2 -> s1 + s2) ]

    choice
      [ attempt (decimalLiteral .>> f)
        float .>> opt f ]


let stringLiteral: Parser<string, unit> =
    let charFromHex s = System.Convert.ToChar(System.Convert.ToInt32(s, 16)).ToString()
    let charFromOct s = System.Convert.ToChar(System.Convert.ToInt32(s, 8)).ToString()
    let charFromLargeHex s = System.Char.ConvertFromUtf32(System.Convert.ToInt32(s, 16))
    let charToString c = c.ToString()
    let escape =
        choice
          [ stringReturn "\\a" "\a"
            stringReturn "\\b" "\b"
            stringReturn "\\f" "\f"
            stringReturn "\\n" "\n"
            stringReturn "\\r" "\r"
            stringReturn "\\t" "\t"
            stringReturn "\\v" "\v"
            stringReturn "\\?" "?"
            stringReturn "\\\\" "\\"
            stringReturn "\\\'" "\'"
            stringReturn "\\\"" "\""
            pstring "\\x" >>. manyMinMaxSatisfy 1 2 isHex |>> charFromHex
            pstring "\\u" >>. manyMinMaxSatisfy 4 4 isHex |>> charFromHex
            pstring "\\U000" >>. manyMinMaxSatisfy 5 5 isHex |>> charFromLargeHex
            pstring "\\U0010" >>. manyMinMaxSatisfy 4 4 isHex |>> ((+) "10") |>> charFromLargeHex 
            pstring "\\" >>. manyMinMaxSatisfy 1 3 isOctal |>> charFromOct ]

    let singleQuoteChar = (noneOf "\0\'\n\\" |>> charToString) <|> escape
    let doubleQuoteChar = (noneOf "\0\"\n\\" |>> charToString) <|> escape

    (between (skipChar '\'') (skipChar '\'') (manyStrings singleQuoteChar)) <|>
    (between (skipChar '\"') (skipChar '\"') (manyStrings doubleQuoteChar))


let stringValue = many1Strings (stringLiteral .>> skipSpaces)


let scalarValue =
    choice
      [ stringValue |>> ScalarValue.String
        attempt floatLiteral |>> float |>> ScalarValue.Float
        attempt (skipCharSpaces '-' >>. floatLiteral) |>> (((+) "-") >> float >> ScalarValue.Float)
        identifier |>> ScalarValue.Identifier
        attempt (skipCharSpaces '-' >>. identifier) |>> ScalarValue.SignedIdentifier
        attempt (skipCharSpaces '-' >>. decimalLiteral) |>> (int64 >> (~-)) |>> ScalarValue.SignedInteger
        // attempt (pchar '-' >>. skipSpaces >>. octalInteger) |>> ((~-) >> ScalarValue.OctSignedInteger)
        // attempt (pchar '-' >>. skipSpaces >>. hexadecimalInteger) |>> ((~-) >> ScalarValue.HexSignedInteger)
        attempt decimalLiteral |>> uint64 |>> ScalarValue.UnsignedInteger
        // attempt octalInteger |>> (uint64 >> ScalarValue.OctUnsignedInteger)
        // hexadecimalInteger |>> (uint64 >> ScalarValue.HexUnsignedInteger)
      ] .>> skipSpaces


let scalarList = brackets (commaSeparated scalarValue)


let fieldName =
    choice
      [ identifier .>> skipSpaces |>> FieldName.Identifier
        attempt (brackets fullIdentifier |>> FieldName.Extension)
        brackets (fullIdentifier .>> skipCharSpaces '/' .>>. fullIdentifier) |>> FieldName.Any ]


let fieldSeparator = opt (skipChar ',' <|> skipChar ';') .>> skipSpaces


let scalarField =
    let value =
        scalarList |>> ScalarFieldValue.ScalarList <|>
        (scalarValue |>> ScalarFieldValue.ScalarValue)

    fieldName .>> skipCharSpaces ':'
    .>>. value .>> fieldSeparator
    |>> (fun (name, value) -> { ScalarField.name = name; value = value })


let message, implementation = createParserForwardedToRef()
let messageValue = braces message <|> angles message
let messageList = brackets (commaSeparated messageValue)


let messageField =
    let value =
        messageList |>> MessageFieldValue.MessageList <|>
        (messageValue |>> MessageFieldValue.MessageValue)

    fieldName .>> opt (skipCharSpaces ':')
    .>>. value .>> fieldSeparator
    |>> (fun (name, value) -> { name = name; value = value })


let field =
    choice
      [ attempt scalarField |>> ScalarField
        (messageField |>> MessageField) ]


do implementation.Value <- many field |>> Message


let textproto = skipSpaces >>. message

