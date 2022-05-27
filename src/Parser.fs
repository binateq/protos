module Parser

open FParsec
open Enumerations
open Messages
open Proto

// https://developers.google.com/protocol-buffers/docs/reference/proto3-spec
let ident =
    let isLetterDigitUnderscore c = isLetter c || isDigit c || c = '_'
    
    many1Satisfy2L isLetter isLetterDigitUnderscore "identifier" .>> spaces
    
let fullIdent = sepBy ident (pstring ".")
            |>> (String.concat ".")
            
let boolLit = (stringReturn "true" true) <|> (stringReturn "false" false)

let strLit =
    let doubleQuotedChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let singleQuotedChar = satisfy (fun c -> c <> '\\' && c <> '\'')
    
    (between (pstring "\"") (pstring "\"") (manyChars doubleQuotedChar))
<|> (between (pstring "\'") (pstring "\'") (manyChars singleQuotedChar))

let syntax = skipString "syntax" >>. spaces >>.
             skipString "=" >>. spaces >>.
             (skipString "\"proto3\"" <|> skipString "'proto3'") >>. spaces >>.
             skipString ";" >>. spaces

let package = skipString "package" >>. spaces >>.
              fullIdent .>> spaces .>> skipString ";" .>> spaces |>> Package
              
let weakImport = skipString "import" >>. spaces >>.
                 skipString "weak" >>. spaces >>.
                 strLit .>> spaces .>> skipString ";" .>> spaces |>> WeakImport

let publicImport = skipString "import" >>. spaces >>.
                   skipString "public" >>. spaces >>.
                   strLit .>> spaces .>> skipString ";" .>> spaces |>> WeakImport
                 
let simpleImport = skipString "import" >>. spaces >>.
                   strLit .>> spaces .>> skipString ";" .>> spaces |>> Import
                   
let import = weakImport <|> publicImport <|> simpleImport

let constant = (strLit |>> String)
           <|> (boolLit |>> Bool)
           <|> (pfloat |>> Float)
           <|> (pint32 |>> Integer)
           <|> (fullIdent |>> Reference)

let option = skipString "option" >>. spaces >>.
             ident |>> OptionName .>> spaces .>> skipString "=" .>> spaces .>>.
             constant .>> spaces .>> skipString ";" .>> spaces |>>
             (fun (name, value) -> { Option.name = name; value = value })
             
    
let scalarType = (stringReturn "double" ScalarType.Double)
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
