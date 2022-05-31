module Parser

open FParsec
open Proto

// https://developers.google.com/protocol-buffers/docs/reference/proto3-spec
let identifier : Parser<string, unit> =
    let isLetterDigitUnderscore c = isLetter c || isDigit c || c = '_'
    
    many1Satisfy2L isLetter isLetterDigitUnderscore "identifier"
    
let fullIdentifier = stringsSepBy1 identifier (pstring ".")
            
let boolLit = (stringReturn "true" true) <|> (stringReturn "false" false)

let stringLiteral =
    let doubleQuotedChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let singleQuotedChar = satisfy (fun c -> c <> '\\' && c <> '\'')
    
    (between (skipString "\"") (skipString "\"") (manyChars doubleQuotedChar))
<|> (between (skipString "\'") (skipString "\'") (manyChars singleQuotedChar))

let syntax : Parser<unit, unit> = skipString "syntax" >>. spaces >>.
                                  skipString "=" >>. spaces >>.
                                  (skipString "\"proto3\"" <|> skipString "'proto3'") >>. spaces >>.
                                  skipString ";" >>. spaces

let package = skipString "package" >>. spaces >>.
              fullIdentifier .>> spaces .>> skipString ";" .>> spaces |>> Package
              
let import = skipString "import" >>. spaces >>.
             ((skipString "weak" >>. spaces >>. stringLiteral |>> WeakImport)
          <|> (skipString "public" >>. spaces >>. stringLiteral |>> PublicImport)
          <|> (stringLiteral |>> Import)) .>> spaces .>> skipString ";" .>> spaces;

let pnumber : Parser<Constant, unit> =
    let numberFormat = NumberLiteralOptions.AllowMinusSign
                   ||| NumberLiteralOptions.AllowPlusSign
                   ||| NumberLiteralOptions.AllowFraction
                   ||| NumberLiteralOptions.AllowExponent
    
    numberLiteral numberFormat "number"
    |>> fun nl ->
            if nl.IsInteger then Constant.Integer (int32 nl.String)
            else Constant.Float (float nl.String)

let constant = (stringLiteral |>> Constant.String)
           <|> (boolLit |>> Constant.Bool)
           <|> (fullIdentifier |>> Constant.Reference)
           <|> pnumber
           
let fullIdentifierInParentheses = between (skipString "(" .>> spaces)
                                          (skipString ")" .>> spaces)
                                          fullIdentifier
                                          
let optionalIdentifier = (skipString "." >>. fullIdentifier) <|>% ""
           
let optionName = (fullIdentifier |>> SimpleName)
             <|> (fullIdentifierInParentheses .>>. optionalIdentifier |>> ComplexName)

let optionAssignment = optionName .>> spaces .>>
                       skipString "=" .>> spaces .>>.
                       constant .>> spaces |>>
                       (fun (name, value) -> { Option.name = name; value = value })
                       
let option = skipString "option" >>. spaces >>.
             optionAssignment .>> spaces .>>
             skipString ";" .>> spaces
    
let fieldType : Parser<MessageFieldType, unit> = (stringReturn "double" MessageFieldType.Double)
                                              <|> (stringReturn "float" MessageFieldType.Float)
                                              <|> (stringReturn "int32" MessageFieldType.Int32)
                                              <|> (stringReturn "int64" MessageFieldType.Int64)
                                              <|> (stringReturn "uint32" MessageFieldType.UInt32)
                                              <|> (stringReturn "uint64" MessageFieldType.UInt64)
                                              <|> (stringReturn "sint32" MessageFieldType.SInt32)
                                              <|> (stringReturn "sint64" MessageFieldType.SInt64)
                                              <|> (stringReturn "fixed32" MessageFieldType.Fixed32)
                                              <|> (stringReturn "fixed64" MessageFieldType.Fixed64)
                                              <|> (stringReturn "sfixed32" MessageFieldType.SFixed32)
                                              <|> (stringReturn "sfixed64" MessageFieldType.SFixed64)
                                              <|> (stringReturn "bool" MessageFieldType.Bool)
                                              <|> (stringReturn "string" MessageFieldType.String)
                                              <|> (stringReturn "bytes" MessageFieldType.Bytes)
                                              <|> (fullIdentifier |>> MessageFieldType.Reference)

let options = opt (between (skipString "[" .>> spaces)
                           (skipString "]" .>> spaces)
                           (sepBy optionAssignment (skipString "," .>> spaces)))

let makeEnumField name value options = { EnumField.name = name
                                         value = value
                                         options = options }
                      
let enumField = pipe3 (identifier .>> spaces .>> skipString "=" .>> spaces |>> EnumFieldName)
                      (pint32 .>> spaces |>> EnumValue)
                      (options .>> spaces .>> skipString ";" .>> spaces)
                      makeEnumField
                      
let enumItem = (stringReturn ";" EnumEmptyItem)
           <|> (enumField |>> EnumField)
           <|> (option |>> EnumOption)
                      
let enumDefinition = pipe2 (skipString "enum" >>. spaces1 >>. identifier .>> spaces |>> EnumName)
                           (between (skipString "{" .>> spaces)
                                    (skipString "}" .>> spaces)
                                    (many enumItem))
                           (fun name items -> { EnumDefinition.name = name
                                                items = items })

let makeMessageField repeated fieldType name number options = { MessageField.repeated = repeated
                                                                name = name
                                                                fieldType = fieldType
                                                                number = number
                                                                options = options }

let messageField = pipe5 ((skipString "repeated" .>> spaces1 |>> fun _ -> true) <|>% false)
                         (fieldType .>> spaces1)
                         (identifier .>> spaces .>> skipString "=" .>> spaces |>> MessageFieldName)
                         (puint32 .>> spaces |>> MessageFieldNumber)
                         (options .>> spaces .>> skipString ";" .>> spaces)
                         makeMessageField
                         
let messageDefinition, implementation = createParserForwardedToRef()
                         
let messageItem = (stringReturn ";" MessageEmptyItem)
              <|> (messageField |>> MessageField)
              <|> (enumDefinition |>> MessageEnum)
              <|> (messageDefinition |>> MessageMessage)
              <|> (option |>> MessageOption)
              
do implementation.Value <- pipe2 (skipString "message" >>. spaces1 >>. identifier .>> spaces |>> MessageName)
                                 (between (skipString "{" .>> spaces)
                                          (skipString "}" .>> spaces)
                                          (many messageItem))
                                 (fun name items -> { MessageDefinition.name = name
                                                      items = items })

let protoItem = (stringReturn ";" ProtoEmptyItem)
            <|> (import |>> ProtoImport)
            <|> (package |>> ProtoPackage)
            <|> (option |>> ProtoOption)
            <|> (enumDefinition |>> ProtoEnum)
            <|> (messageDefinition |>> ProtoMessage)

let proto = spaces >>. syntax >>. (many protoItem)