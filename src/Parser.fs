module Parser

open FParsec
open Proto

// https://developers.google.com/protocol-buffers/docs/reference/proto3-spec
let ident : Parser<string, unit> =
    let isLetterDigitUnderscore c = isLetter c || isDigit c || c = '_'
    
    many1Satisfy2L isLetter isLetterDigitUnderscore "identifier"
    
let fullIdent = stringsSepBy ident (pstring ".")
            
let boolLit = (stringReturn "true" true) <|> (stringReturn "false" false)

let strLit =
    let doubleQuotedChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let singleQuotedChar = satisfy (fun c -> c <> '\\' && c <> '\'')
    
    (between (skipString "\"") (skipString "\"") (manyChars doubleQuotedChar))
<|> (between (skipString "\'") (skipString "\'") (manyChars singleQuotedChar))

let syntax : Parser<unit, unit> = skipString "syntax" >>. spaces >>.
                                  skipString "=" >>. spaces >>.
                                  (skipString "\"proto3\"" <|> skipString "'proto3'") >>. spaces >>.
                                  skipString ";" >>. spaces

let package = skipString "package" >>. spaces >>.
              fullIdent .>> spaces .>> skipString ";" .>> spaces |>> Package
              
let import = skipString "import" >>. spaces >>.
             ((skipString "weak" >>. spaces >>. strLit |>> WeakImport)
          <|> (skipString "public" >>. spaces >>. strLit |>> PublicImport)
          <|> (strLit |>> Import)) .>> spaces .>> skipString ";" .>> spaces;

let pnumber : Parser<Constant, unit> =
    let numberFormat = NumberLiteralOptions.AllowMinusSign
                   ||| NumberLiteralOptions.AllowPlusSign
                   ||| NumberLiteralOptions.AllowFraction
                   ||| NumberLiteralOptions.AllowExponent
    
    numberLiteral numberFormat "number"
    |>> fun nl ->
            if nl.IsInteger then Constant.Integer (int32 nl.String)
            else Constant.Float (float nl.String)

let constant = (strLit |>> Constant.String)
           <|> (boolLit |>> Constant.Bool)
           <|> pnumber
           <|> (fullIdent |>> Constant.Reference)

let optionDefinition = ident |>> OptionName .>> spaces .>>
                       skipString "=" .>> spaces .>>.
                       constant .>> spaces |>>
                       (fun (name, value) -> { Option.name = name; value = value })
                       
let option = skipString "option" >>. spaces >>.
             optionDefinition .>> spaces .>>
             skipString ";" .>> spaces
    
let fieldType : Parser<FieldType, unit> = (stringReturn "double" FieldType.Double)
                                      <|> (stringReturn "float" FieldType.Float)
                                      <|> (stringReturn "int32" FieldType.Int32)
                                      <|> (stringReturn "int64" FieldType.Int64)
                                      <|> (stringReturn "uint32" FieldType.UInt32)
                                      <|> (stringReturn "uint64" FieldType.UInt64)
                                      <|> (stringReturn "sint32" FieldType.SInt32)
                                      <|> (stringReturn "sint64" FieldType.SInt64)
                                      <|> (stringReturn "fixed32" FieldType.Fixed32)
                                      <|> (stringReturn "fixed64" FieldType.Fixed64)
                                      <|> (stringReturn "sfixed32" FieldType.SFixed32)
                                      <|> (stringReturn "sfixed64" FieldType.SFixed64)
                                      <|> (stringReturn "bool" FieldType.Bool)
                                      <|> (stringReturn "string" FieldType.String)
                                      <|> (stringReturn "bytes" FieldType.Bytes)
                                      <|> (fullIdent |>> FieldType.Reference)

let options = opt (between (skipString "[" .>> spaces)
                           (skipString "]" .>> spaces)
                           (sepBy optionDefinition (skipString "," .>> spaces)))
                      
let field = pipe5 ((skipString "repeated" .>> spaces1 |>> fun _ -> true) <|>% false)
                  (fieldType .>> spaces1)
                  (ident .>> spaces .>> skipString "=" .>> spaces)
                  (puint32 .>> spaces)
                  (options .>> spaces .>> skipString ";" .>> spaces)
                  (fun repeated fieldType name number options -> { Field.repeated = repeated
                                                                   name = FieldName name
                                                                   fieldType = fieldType
                                                                   number = FieldNumber number
                                                                   options = options })
                  
            