module Proto3Parser

open Proto3
open FParsec

// https://developers.google.com/protocol-buffers/docs/reference/proto3-spec
let identifier =
    many1Chars2 asciiLetter (asciiLetter <|> digit <|> pchar '_')


let fullIdentifier = stringsSepBy1 identifier (pstring ".")


let boolLiteral = (stringReturn "true" true) <|> (stringReturn "false" false)


let stringLiteral =
    let doubleQuotedChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let singleQuotedChar = satisfy (fun c -> c <> '\\' && c <> '\'')
    
    (between (skipChar '"') (skipChar '"') (manyChars doubleQuotedChar)) <|>
    (between (skipChar '\'') (skipChar '\'') (manyChars singleQuotedChar))


let syntax =
    skipString "syntax" .>> spaces .>>
    skipChar '=' .>> spaces .>>
    (skipString "\"proto3\"" <|> skipString "'proto3'") .>> spaces .>>
    skipChar ';' .>> spaces


let package =
    skipString "package" >>. spaces >>.
    fullIdentifier .>> spaces .>> skipChar ';' .>> spaces |>> Package


let import =
   let path =
       (skipString "weak" >>. spaces >>. stringLiteral |>> WeakImport) <|>
       (skipString "public" >>. spaces >>. stringLiteral |>> PublicImport) <|>
       (stringLiteral |>> Import)

   skipString "import" >>. spaces >>. path .>> spaces .>> skipChar ';' .>> spaces;


let pnumber : Parser<Constant, unit> =
    let numberFormat =
        NumberLiteralOptions.AllowMinusSign |||
        NumberLiteralOptions.AllowPlusSign |||
        NumberLiteralOptions.AllowFraction |||
        NumberLiteralOptions.AllowExponent
    let makeNumberLiteral (literal: NumberLiteral) =
        if literal.IsInteger
        then Constant.Integer (int32 literal.String)
        else Constant.Float (float literal.String)

    numberLiteral numberFormat "number" |>> makeNumberLiteral


let constant =
    choice [
        stringLiteral |>> Constant.String
        boolLiteral |>> Constant.Bool
        fullIdentifier |>> Constant.Reference
        pnumber
    ]


let optionName =
    let fullIdentifierInParentheses =
        between (skipChar '(' .>> spaces) (skipChar ')' .>> spaces) (fullIdentifier .>> spaces)
    let optionalIdentifier = (skipChar '.' >>. fullIdentifier) <|>% ""

    choice [
        fullIdentifier |>> SimpleName
        fullIdentifierInParentheses .>>. optionalIdentifier |>> ComplexName
    ]


let optionAssignment =
    let make (name, value) = {
        Option.name = name
        value = value
    } 
    optionName .>> spaces .>> skipString "=" .>> spaces .>>. constant .>> spaces |>> make


let option =
    skipString "option" >>. spaces >>. optionAssignment .>> spaces .>> skipChar ';' .>> spaces


let fieldType =
    choice [
        stringReturn "double" MessageFieldType.Double
        stringReturn "float" MessageFieldType.Float
        stringReturn "int32" MessageFieldType.Int32
        stringReturn "int64" MessageFieldType.Int64
        stringReturn "uint32" MessageFieldType.UInt32
        stringReturn "uint64" MessageFieldType.UInt64
        stringReturn "sint32" MessageFieldType.SInt32
        stringReturn "sint64" MessageFieldType.SInt64
        stringReturn "fixed32" MessageFieldType.Fixed32
        stringReturn "fixed64" MessageFieldType.Fixed64
        stringReturn "sfixed32" MessageFieldType.SFixed32
        stringReturn "sfixed64" MessageFieldType.SFixed64
        stringReturn "bool" MessageFieldType.Bool
        stringReturn "string" MessageFieldType.String
        stringReturn "bytes" MessageFieldType.Bytes
        fullIdentifier |>> MessageFieldType.Reference
    ]


let options =
    let openBracket = skipString "[" .>> spaces
    let closeBracket = skipString "]" .>> spaces 
    let assignments = sepBy optionAssignment (skipString "," .>> spaces) 
    opt (between openBracket closeBracket assignments)


let enumField =
    let make name value options = {
        EnumField.name = name
        value = value
        options = options
    }
    let name = identifier .>> spaces .>> skipString "=" .>> spaces |>> EnumFieldName
    let value = pint32 .>> spaces |>> EnumValue
    let options = options .>> spaces .>> skipString ";" .>> spaces 
    
    pipe3 name value options make


let enumItem =
    (stringReturn ";" EnumEmptyItem) <|>
    (enumField |>> EnumField) <|>
    (option |>> EnumOption)


let enumDefinition =
    let make name items = {
        Enum.name = name
        items = items
    }
    let name = skipString "enum" >>. spaces1 >>. identifier .>> spaces |>> EnumName
    let openBrace = skipString "{" .>> spaces
    let closeBrace = skipString "}" .>> spaces
    let items = between openBrace closeBrace (many enumItem)
    
    pipe2 name items make


let messageField =
    let make repeated fieldType name number options = {
        MessageField.repeated = repeated
        name = name
        fieldType = fieldType
        number = number
        options = options
    }
    let isRepeated = (skipString "repeated" .>> spaces1 |>> fun _ -> true) <|>% false
    let fieldType = fieldType .>> spaces1
    let fieldName = identifier .>> spaces .>> skipChar '=' .>> spaces |>> MessageFieldName
    let fieldNumber = puint32 .>> spaces |>> MessageFieldNumber
    let options = options .>> spaces .>> skipString ";" .>> spaces 

    pipe5 isRepeated fieldType fieldName fieldNumber options make


let messageDefinition, implementation = createParserForwardedToRef()


let messageItem =
    choice [
        stringReturn ";" MessageEmptyItem
        messageField |>> MessageField
        enumDefinition |>> MessageEnum
        messageDefinition |>> MessageMessage
        option |>> MessageOption
    ]


do implementation.Value <-
    let make name items = {
        Message.name = name
        items = items
    }
    let name = skipString "message" >>. spaces1 >>. identifier .>> spaces |>> MessageName 
    let items = between (skipChar '{' .>> spaces) (skipChar '}' .>> spaces) (many messageItem)

    pipe2 name items make


let protoItem =
    choice [
        stringReturn ";" ProtoEmptyStatement
        import |>> ProtoImport
        package |>> ProtoPackage
        option |>> ProtoOption
        enumDefinition |>> ProtoEnum
        messageDefinition |>> ProtoMessage
    ]


let proto = spaces >>. syntax >>. (many protoItem)