module Proto3Parser

open System.Text
open FParsec
open Proto3

// https://developers.google.com/protocol-buffers/docs/reference/proto3-spec
let identifier = many1Chars2 asciiLetter (asciiLetter <|> digit <|> pchar '_')


let fullIdentifier = stringsSepBy1 identifier (pstring ".")


let boolLiteral = (stringReturn "true" true) <|> (stringReturn "false" false)


let stringLiteral =
    let charFromHex s = System.Convert.ToChar(System.Convert.ToInt32(s, 16)).ToString()
    let charFromOct s = System.Convert.ToChar(System.Convert.ToInt32(s, 8)).ToString()
    let charFromLargeHex s = System.Char.ConvertFromUtf32(System.Convert.ToInt32(s, 16))
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

    let singleQuoteChar = (noneOf "\0\'\n\\" |>> string) <|> escape
    let doubleQuoteChar = (noneOf "\0\"\n\\" |>> string) <|> escape

    (between (skipChar '\'') (skipChar '\'') (manyStrings singleQuoteChar)) <|>
    (between (skipChar '\"') (skipChar '\"') (manyStrings doubleQuoteChar))


let comment: Parser<unit, unit> = skipString "//" .>> skipRestOfLine true
let blanks = spaces .>> many (comment .>> spaces) |>> ignore 
let blanks1 = spaces1 .>> many (comment .>> spaces) |>> ignore

let enclosed (brackets: string) parser =
    between (skipChar brackets.[0] .>> blanks) (skipChar brackets.[1] .>> blanks) parser



let syntax =
    skipString "syntax" .>> blanks
    .>> skipChar '=' .>> blanks
    .>> (skipString "\"proto3\"" <|> skipString "'proto3'") .>> blanks
    .>> skipChar ';' .>> blanks


let package =
    skipString "package" >>. blanks >>.
    fullIdentifier .>> blanks
    .>> skipChar ';' .>> blanks
    |>> Package


let import =
   let path =
       (skipString "weak" >>. blanks1 >>. stringLiteral |>> WeakImport) <|>
       (skipString "public" >>. blanks1 >>. stringLiteral |>> PublicImport) <|>
       (stringLiteral |>> Import)

   skipString "import" >>. blanks1
   >>. path .>> blanks
   .>> skipChar ';' .>> blanks;


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
    choice
      [ stringLiteral |>> Constant.String
        boolLiteral |>> Constant.Bool
        fullIdentifier |>> Constant.Reference
        pnumber ]


let optionName =
    choice
      [ identifier |>> SimpleName
        between (skipChar '(' .>> blanks) (skipChar ')' .>> blanks) fullIdentifier |>> ComplexName ]


let private optionAssignment =
    optionName .>> blanks
    .>> skipChar '=' .>> blanks
    .>>. constant .>> blanks
    |>> (fun (name, value) -> { name = name; value = value })


let option =
    skipString "option" >>. blanks1
    >>. optionAssignment .>> blanks
    .>> skipChar ';' .>> blanks


let options =
    let bracketed p = between (skipString "[" .>> blanks) (skipString "]" .>> blanks) p
    let assignments = sepBy optionAssignment (skipChar ',' .>> blanks) 
    opt (bracketed assignments)


let enumField =
    let name = identifier .>> blanks .>> skipString "=" .>> blanks |>> EnumFieldName
    let value = pint32 .>> blanks |>> EnumValue
    let options = options .>> blanks .>> skipString ";" .>> blanks 
    
    pipe3 name value options
          (fun name value options -> { name = name; value = value; options = options })


let enumItem =
    choice
      [ stringReturn ";" EnumEmptyItem
        attempt enumField |>> EnumField
        option |>> EnumOption ]


let enumDefinition =
    let enclosed p = between (skipChar '{' .>> blanks) (skipChar '}' .>> blanks) p
    let name = skipString "enum" >>. blanks1 >>. identifier .>> blanks |>> EnumName
    let items = enclosed (many enumItem)
    
    pipe2 name items (fun name items -> { Enum.name = name; items = items })


let fieldType =
    choice
      [ stringReturn "double" MessageFieldType.Double
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
        fullIdentifier |>> MessageFieldType.Reference ]


let messageField =
    let make modifier fieldType name number options =
      { modifier = modifier
        name = name
        fieldType = fieldType
        number = number
        options = options }
    let modifier = opt ((stringReturn "repeated" Repeated <|> stringReturn "optional" Optional) .>> blanks1)
    let fieldType = fieldType .>> blanks1
    let fieldName = identifier .>> blanks .>> skipChar '=' .>> blanks |>> MessageFieldName
    let fieldNumber = puint32 .>> blanks |>> MessageFieldNumber
    let options = options .>> blanks .>> skipString ";" .>> blanks 

    pipe5 modifier fieldType fieldName fieldNumber options make


let messageDefinition, implementation = createParserForwardedToRef()


let messageItem =
    choice
      [ stringReturn ";" MessageEmptyItem
        messageField |>> MessageField
        enumDefinition |>> MessageEnum
        messageDefinition |>> MessageMessage
        option |>> MessageOption ]


do implementation.Value <-
    let name = skipString "message" >>. blanks1 >>. identifier .>> blanks |>> MessageName 
    let items = between (skipChar '{' .>> blanks) (skipChar '}' .>> blanks) (many messageItem)

    pipe2 name items (fun name items -> { name = name; items = items })


let rpcType =
    choice
      [ skipString "stream" >>. fullIdentifier |>> StreamMessageType
        fullIdentifier |>> MessageType ]


let rpc =
    let make name input output options =
      { name = name
        input = input
        output = output
        options = options }
    let enclosed p = between (skipChar '(' .>> blanks) (skipChar ')' .>> blanks) p
    let enclosedCurly p = between (skipChar '{' .>> blanks) (skipChar '}' .>> blanks) p
    let name = skipString "rpc" >>. blanks1 >>. identifier .>> blanks |>> RpcName
    let input = enclosed (rpcType .>> blanks)
    let output = skipString "returns" >>. blanks >>. (enclosed (rpcType .>> blanks))
    let options =
        choice
          [ enclosedCurly (sepBy1 optionAssignment (skipChar ',' .>> blanks)) |>> Some
            stringReturn ";" None .>> blanks ]
    
    pipe4 name input output options make


let serviceItem =
    choice
      [ rpc |>> ServiceRpc
        option |>> ServiceOption
        stringReturn ";" ServiceEmptyItem ]


let serviceDefinition =
    let make name items =
      { name = name
        items = items }
    let name = skipString "service" >>. blanks1 >>. identifier .>> blanks |>> ServiceName
    let items = between (skipChar '{' .>> blanks) (skipChar '}' .>> blanks) (many1 serviceItem)
    
    pipe2 name items make


let protoItem =
    choice
      [ stringReturn ";" ProtoEmptyStatement
        import |>> ProtoImport
        package |>> ProtoPackage
        option |>> ProtoOption
        enumDefinition |>> ProtoEnum
        messageDefinition |>> ProtoMessage
        serviceDefinition |>> ProtoService ]


let proto = blanks >>. syntax >>. (many protoItem) .>> eof


let parse file =
    match runParserOnFile proto () file Encoding.UTF8 with
    | Success (result, _, _) ->
        result
    | Failure (_, error, _) ->
        invalidOp (error.ToString())