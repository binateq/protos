module TextprotoParser

open System
open FParsec
open StringParsers
open Textproto

// https://developers.google.com/protocol-buffers/docs/text-format-spec
let comment: Parser<unit, unit> = skipString "#" .>> skipRestOfLine true
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
    
let floatLiteral: StringParser =
    let e = pstringCI "E"
    let point = pstring "."
    let sign = pstring "+" <|> pstring "-"
    let digits0 = manyChars digit
    let digits1 = many1Chars digit
    let exp = stringPipe3 e (optionString sign) digits1
    let f = pstringCI "F"
    let float = choice [
        stringPipe3 point digits1 (optionString exp)
        attempt (stringPipe4 decimalLiteral point digits0 (optionString exp))
        stringPipe2 decimalLiteral exp ]
    
    choice [
        attempt (decimalLiteral .>> f)
        float .>> opt f
    ]
    
let octalLiteral: StringParser = pchar '0' >>. (many1Chars octal)
let hexadecimalInteger: StringParser = pstringCI "0X" >>. (many1Chars hex)
    
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
            pstring "\\x" >>. manyMinMaxSatisfy 1 2 isHex |>> (fun s -> Convert.ToChar(Convert.ToInt32(s, 16)))
            pstring "\\u" >>. manyMinMaxSatisfy 4 4 isHex |>> (fun s -> Convert.ToChar(Convert.ToInt32(s, 16)))
            // pstring "\\U000" >>. manyMinMaxSatisfy 5 5 isHex |>> (fromHex >> Convert.ToChar)
            // pstring "\\U0010" >>. manyMinMaxSatisfy 4 3 isHex |>> (fromHex >> ((+) 0x100_000L) >> Convert.ToChar) 
            pstring "\\" >>. manyMinMaxSatisfy 1 3 isOctal |>> (fun s -> Convert.ToChar(Convert.ToInt32(s, 8)))
        ]
        
    let singleQuoteChar = noneOf "\0\'\n\\" <|> escape
    let doubleQuoteChar = noneOf "\0\"\n\\" <|> escape
    
    (between (skipChar '\'') (skipChar '\'') (manyChars singleQuoteChar)) <|>
    (between (skipChar '\"') (skipChar '\"') (manyChars doubleQuoteChar))

let stringValue = many1Strings (stringLiteral .>> skipSpaces)

let scalarValue =
    choice [
        stringValue |>> ScalarValue.String
        attempt floatLiteral |>> ScalarValue.Float
        attempt (skipCharSpaces '-' >>. floatLiteral) |>> (((+) "-") >> ScalarValue.Float)
        identifier |>> ScalarValue.Identifier
        attempt (skipCharSpaces '-' >>. identifier) |>> ScalarValue.SignedIdentifier
        attempt (skipCharSpaces '-' >>. decimalLiteral) |>> ScalarValue.DecSignedInteger
        //attempt (pchar '-' >>. skipSpaces >>. octalInteger) |>> ((~-) >> ScalarValue.OctSignedInteger)
        //attempt (pchar '-' >>. skipSpaces >>. hexadecimalInteger) |>> ((~-) >> ScalarValue.HexSignedInteger)
        attempt decimalLiteral |>> ScalarValue.DecUnsignedInteger
        //attempt octalInteger |>> (uint64 >> ScalarValue.OctUnsignedInteger)
        //hexadecimalInteger |>> (uint64 >> ScalarValue.HexUnsignedInteger)
    ] .>> skipSpaces
    
let scalarList = brackets (commaSeparated scalarValue)

let fieldName =
    choice [
        identifier .>> skipSpaces |>> FieldName.Identifier
        attempt (brackets fullIdentifier |>> FieldName.Extension)
        brackets (fullIdentifier .>> skipCharSpaces '/' .>>. fullIdentifier) |>> FieldName.Any
    ]
    
let fieldSeparator = opt (skipChar ',' <|> skipChar ';') .>> skipSpaces

let scalarField =
    let make (name, value) =
        { ScalarField.name = name
          value = value }
    let value =
        scalarList |>> ScalarFieldValue.ScalarList <|>
        (scalarValue |>> ScalarFieldValue.ScalarValue)
    
    fieldName .>> skipCharSpaces ':' .>>. value .>> fieldSeparator |>> make
    
let message, implementation = createParserForwardedToRef()
let messageValue = braces message <|> angles message
let messageList = brackets (commaSeparated messageValue)

let messageField =
    let make (name, value) =
        { MessageField.name = name
          value = value }
    let value =
        messageList |>> MessageFieldValue.MessageList <|>
        (messageValue |>> MessageFieldValue.MessageValue)
        
    fieldName .>> opt (skipCharSpaces ':') .>>. value .>> fieldSeparator |>> make

let field =
    choice [
        attempt scalarField |>> ScalarField
        (messageField |>> MessageField)
    ]
    
do implementation.Value <- many field |>> Message

let textproto = skipSpaces >>. message
