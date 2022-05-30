module Parser

open FParsec
open Enumerations
open Messages
open Proto

// https://developers.google.com/protocol-buffers/docs/reference/proto3-spec
let ident : Parser<string, unit> =
    let isLetterDigitUnderscore c = isLetter c || isDigit c || c = '_'
    
    many1Satisfy2L isLetter isLetterDigitUnderscore "identifier" .>> spaces
    
let fullIdent = stringsSepBy ident (pstring ".")
            
let boolLit = (stringReturn "true" true) <|> (stringReturn "false" false)

let strLit =
    let doubleQuotedChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let singleQuotedChar = satisfy (fun c -> c <> '\\' && c <> '\'')
    
    (between (pstring "\"") (pstring "\"") (manyChars doubleQuotedChar))
<|> (between (pstring "\'") (pstring "\'") (manyChars singleQuotedChar))

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
           <|> (fullIdent |>> Reference)

let optionDefinition = skipString "option" >>. spaces >>.
                       ident |>> OptionName .>> spaces .>>
                       skipString "=" .>> spaces .>>.
                       constant .>> spaces |>>
                       (fun (name, value) -> { Option.name = name; value = value })
                       
let option = optionDefinition .>> spaces .>> skipString ";" .>> spaces
    
let scalarType : Parser<ScalarType, unit> = (stringReturn "double" ScalarType.Double)
                                        <|> (stringReturn "float" ScalarType.Float)
                                        <|> (stringReturn "int32" ScalarType.Int32)
                                        <|> (stringReturn "int64" ScalarType.Int64)
                                        <|> (stringReturn "uint32" ScalarType.UInt32)
                                        <|> (stringReturn "uint64" ScalarType.UInt64)
                                        <|> (stringReturn "sint32" ScalarType.SInt32)
                                        <|> (stringReturn "sint64" ScalarType.SInt64)
                                        <|> (stringReturn "fixed32" ScalarType.Fixed32)
                                        <|> (stringReturn "fixed64" ScalarType.Fixed64)
                                        <|> (stringReturn "sfixed32" ScalarType.SFixed32)
                                        <|> (stringReturn "sfixed64" ScalarType.SFixed64)
                                        <|> (stringReturn "bool" ScalarType.Bool)
                                        <|> (stringReturn "string" ScalarType.String)
                                        <|> (stringReturn "bytes" ScalarType.Bytes)
