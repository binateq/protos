# DotNext workshop

## 1. Make solution and first project

* Console Application
* Solution name: **DotNextDemo**
* Project name: **ProtoS**
* Language: **F#**

## 2. Make test project

* Unit Test Project
* Project name: **ProtoS.Tests**
* Type: **xUnit**
* Language: **F#**

Delete **Tests.fs**.

## 3. Add FParsec dependency

* Nuget Package: **FParsec**
* Version: **1.1.1**

To both **ProtoS** and **ProtoS.Tests**.

## 4. Add reference

From **ProtoS.Tests** to **ProtoS**. 

## 5. Assert.fs

Add **Assert.fs** to **ProtoS.Tests** as the *first* file. Copy-paste the text:

```fsharp
module Assert

open FParsec
open Xunit

let private tryParse source parser =
    match run parser source with
    | Success (result, _, position) ->
        if position.Index = source.Length then
            (Some result, sprintf $"%A{result}")
        else
            (None, "The pattern is not parsed til the end")
    | Failure (message, _, _) ->
        (None, message)


let Parse<'r>(source: string, parser: Parser<'r, unit>) =
    let result, message = tryParse source parser
    
    Assert.True(result.IsSome, message)


let ParseEqual<'r>(source: string, parser: Parser<'r, unit>, expected: 'r) =
    let result, message = tryParse source parser
    match result with
    | Some actual -> Assert.Equal(expected, actual)
    | None -> Assert.True(false, message)


let NotParse<'r>(source: string, parser: Parser<'r, unit>) =
    let result, message = tryParse source parser
    
    Assert.True(result.IsNone, message)
```

## 6. Proto3Parser.fs

Add **Proto3Parser.fs** to **ProtoS** as the *first* file. Copy-paste the text:

```fsharp
module Proto3Parser

open FParsec

let identifier: Parser<string, unit> = preturn ""
```

## 7. Proto3ParserTests.fs

Add **Proto3ParserTests.fs** to **ProtoS.Tests** as the *second* file. Copy-paste the text:

```fsharp
module Proto3ParserTests

open Xunit
open Proto3Parser

[<Fact>]
let ``"abc" is identifier`` () =
    Assert.Parse("abc", identifier)


[<Fact>]
let ``"a_b_c" is identifier`` () =
    Assert.Parse("a_b_c", identifier)


[<Fact>]
let ``"abc123" is identifier`` () =
    Assert.Parse("abc123", identifier)
```

Run unit tests. All are fail.

Change `identifier` definition:

```fsharp
let identifier: Parser<string, unit> = many1Chars2 asciiLetter (asciiLetter <|> digit <|> pchar '_')
```

Run unit tests. All are successful.

## 8. Proto3ParserTests.fs

Append negative tests:

```fsharp
[<Fact>]
let ``"_abc" isn't identifier `` () =
    Assert.NotParse("_abc", identifier)


[<Fact>]
let ``"123abc" isn't identifier `` () =
    Assert.NotParse("123abc", identifier)
```

Run unit tests. All are successful.

## 9. Full Identifier

Append the code to **Proto3Parser.fs**:

```fsharp
let fullIdentifier: Parser<string, unit> = preturn ""
```

Append the code to **Proto3ParserTests.fs**:

```fsharp
[<Fact>]
let ``"abc" is full identifier`` () =
    Assert.ParseEqual("abc", fullIdentifier, "abc")


[<Fact>]
let ``"abc.def.hig" is full identifier`` () =
    Assert.ParseEqual("abc.def.hig", fullIdentifier, "abc.def.hig")
```

New tests are fail.

Change `fullIdentifier` definition:

```fsharp
let fullIdentifier: Parser<string, unit> = stringsSepBy1 identifier (pstring ".")
```

All tests are successful.

## 10. Bool And String Literals

Append the code to **Proto3Parser.fs**:

```fsharp
let boolLiteral: Parser<bool, unit> = (stringReturn "true" true) <|> (stringReturn "false" false)


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
```

Append the code to **Proto3ParserTests.fs**:

```fsharp
[<Fact>]
let ``"true" is true`` () =
    Assert.ParseEqual("true", boolLiteral, true)


[<Fact>]
let ``"false" is false`` () =
    Assert.ParseEqual("false", boolLiteral, false)


[<Fact>]
let ``"\"Lorem ipsum\"" is string`` () =
    Assert.ParseEqual("\"Lorem ipsum\"", stringLiteral, "Lorem ipsum")


[<Fact>]
let ``"'Lorem ipsum'" is string`` () =
    Assert.ParseEqual("'Lorem ipsum'", stringLiteral, "Lorem ipsum")
    

[<Fact>]
let ``"'Lorem\\n ipsum'" is string`` () =
    Assert.ParseEqual("'Lorem\\n ipsum'", stringLiteral, "Lorem\n ipsum")
    
    
[<Fact>]
let ``"'\\U0001F602'" is string`` () =
    Assert.ParseEqual("'\\U0001F602'", stringLiteral, "😂")
```

All tests are successful.

## 11. Syntax

### Proto3Parser.fs
```fsharp
let syntax: Parser<unit, unit> =
    skipString "syntax" .>> spaces
    .>> skipChar '=' .>> spaces
    .>> (skipString "\"proto3\"" <|> skipString "'proto3'") .>> spaces
    .>> skipChar ';' .>> spaces
```

### Proto3ParserTests.fs
```fsharp
[<Fact>]
let ``"syntax = 'proto3';" is Syntax`` () =
    Assert.Parse("syntax = 'proto3';", syntax)


[<Fact>]
let ``"syntax = 'proto3'" without semicolon fails`` () =
    Assert.NotParse("syntax = 'proto3'", syntax)
```

## 12. Package

Abstract Syntax Tree. Single case discriminated union.

Add **Proto3.fs** to **ProtoS** as the *first* file. Copy-paste the text:

```fsharp
module Proto3

type Package = Package of string
```

### Proto3Parser.fs
```fsharp
open Proto3

let package =
    skipString "package" >>. spaces >>.
    fullIdentifier .>> spaces
    .>> skipChar ';' .>> spaces
    |>> Package
```

### Proto3ParserTests.fs
```fsharp
open Proto3

[<Fact>]
let ``"package abc.def.ghi;" is Package`` () =
    Assert.ParseEqual("package abc.def.ghi;", package, Package "abc.def.ghi")
```

## 13. Import

Multi case discriminated union.

### Proto3.fs
```fsharp
type Import =
    | Import of string
    | WeakImport of string
    | PublicImport of string
```

### Proto3Parser.fs
```fsharp
let import =
   let path =
       (skipString "weak" >>. spaces1 >>. stringLiteral |>> WeakImport) <|>
       (skipString "public" >>. spaces1 >>. stringLiteral |>> PublicImport) <|>
       (stringLiteral |>> Import)

   skipString "import" >>. spaces1
   >>. path .>> spaces
   .>> skipChar ';' .>> spaces
```

### Proto3ParserTests.fs
```fsharp
[<Fact>]
let ``"import weak 'path';" is WeakImport`` () =
    Assert.ParseEqual("import weak 'path';", import, WeakImport "path")


[<Fact>]
let ``"import public 'path';" is PublicImport`` () =
    Assert.ParseEqual("import public 'path';", import, PublicImport "path")


[<Fact>]
let ``"import 'path';" is Import`` () =
    Assert.ParseEqual("import 'path';", import, Import "path")
```

## 14. Options

### Proto3.fs
```fsharp
type Constant =
    | Reference of string
    | Integer of int
    | Float of float
    | String of string
    | Bool of bool


type OptionName =
    | SimpleName of string
    | ComplexName of string


type Option =
  { name: OptionName
    value: Constant }
```

### Proto3Parser.fs
```fsharp
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
        between (skipChar '(' .>> spaces) (skipChar ')' .>> spaces) fullIdentifier |>> ComplexName ]


let private optionAssignment =
    optionName .>> spaces
    .>> skipString "=" .>> spaces
    .>>. constant .>> spaces
    |>> (fun (name, value) -> { name = name; value = value })


let option =
    skipString "option" >>. spaces1
    >>. optionAssignment .>> spaces
    .>> skipChar ';' .>> spaces


let options =
    let bracketed p = between (skipString "[" .>> spaces) (skipString "]" .>> spaces) p
    let assignments = sepBy optionAssignment (skipString "," .>> spaces) 
    opt (bracketed assignments)
```

### Proto3ParserTests.fs
```fsharp
[<Fact>]
let ``"option foo = 'aaa';" is String Proto.Option``() =
    let expected = {
        Option.name = SimpleName "foo"
        value = Constant.String "aaa"
    }
    Assert.ParseEqual("option foo = 'aaa';", option, expected)


[<Fact>]
let ``"option foo = false;" is Bool Proto.Option`` () =
    let expected = {
        Option.name = SimpleName "foo"
        value = Constant.Bool false
    }
    Assert.ParseEqual("option foo = false;", option, expected)


[<Fact>]
let ``"option foo = -2;" is Integer Proto.Option`` () =
    let expected = { Option.name = SimpleName "foo"
                     value = Integer -2 }
    Assert.ParseEqual("option foo = -2;", option, expected)


[<Fact>]
let ``"option foo = +3.14'" is Float Proto.Option`` () =
    let expected = { Option.name = SimpleName "foo"
                     value = Constant.Float 3.14 }
    Assert.ParseEqual("option foo = +3.14;", option, expected)


[<Fact>]
let ``"option foo = bar.baz;" is Reference Proto.Option`` () =
    let expected = { Option.name = SimpleName "foo"
                     value = Constant.Reference "bar.baz" }
    Assert.ParseEqual("option foo = bar.baz;", option, expected)


[<Fact>]
let ``"[a = 1, b = 2]" is Some List of Proto.Options`` () =
    let expected = Some [ { Option.name = SimpleName "a"; value = Constant.Integer 1 }
                          { Option.name = SimpleName "b"; value = Constant.Integer 2 } ]
    Assert.ParseEqual("[a = 1, b = 2]", options, expected)

    
[<Fact>]
let ``"[]" is Some empty List of Proto.Options`` () =
    Assert.ParseEqual("[]", options, Some List.empty<Option>)

    
[<Fact>]
let ``"" is None List of Proto.Options`` () =
    Assert.ParseEqual("", options, None)
```

## 15. Enums

### Proto3.fs
```fsharp
type EnumName = EnumName of string
type EnumFieldName = EnumFieldName of string
type EnumValue = EnumValue of int32


type EnumField =
  { name: EnumFieldName
    value: EnumValue
    options: Option list option }


type EnumItem =
    | EnumField of EnumField
    | EnumOption of Option
    | EnumEmptyItem


type Enum =
  { name: EnumName
    items: EnumItem list }
```

### Proto3Parser.fs
```fsharp
let enumField =
    let name = identifier .>> spaces .>> skipString "=" .>> spaces |>> EnumFieldName
    let value = pint32 .>> spaces |>> EnumValue
    let options = options .>> spaces .>> skipString ";" .>> spaces 
    
    pipe3 name value options
          (fun name value options -> { name = name; value = value; options = options })


let enumItem =
    choice
      [ stringReturn ";" EnumEmptyItem
        attempt enumField |>> EnumField
        option |>> EnumOption ]


let enumDefinition =
    let enclosed p = between (skipChar '{' .>> spaces) (skipChar '}' .>> spaces) p
    let name = skipString "enum" >>. spaces1 >>. identifier .>> spaces |>> EnumName
    let items = enclosed (many enumItem)
    
    pipe2 name items (fun name items -> { Enum.name = name; items = items })
```

### Proto3ParserTests.fs
```fsharp
[<Fact>]
let ``"enum Foo { option alias = 'Bar'; UNKNOWN = 0; KNOWN = 1; }" is EnumDefinition`` () =
    let expected = { Enum.name = EnumName "Foo"
                     items = [ EnumOption { name = SimpleName "alias"; value = Constant.String "Bar" }
                               EnumField { name = EnumFieldName "UNKNOWN"; value = EnumValue 0; options = None }
                               EnumField { name = EnumFieldName "KNOWN"; value = EnumValue 1; options = None } ] }
    Assert.ParseEqual("enum Foo { option alias = 'Bar'; UNKNOWN = 0; KNOWN = 1; }", enumDefinition, expected)
```

## 16. Messages

### Proto3.fs
```fsharp
type MessageName = MessageName of string
type MessageFieldName = MessageFieldName of string


type MessageFieldType =
    | Double
    | Float
    | Int32
    | Int64
    | UInt32
    | UInt64
    | SInt32
    | SInt64
    | Fixed32
    | Fixed64
    | SFixed32
    | SFixed64
    | Bool
    | String
    | Bytes
    | Reference of string


type MessageFieldNumber = MessageFieldNumber of uint32
type Modifier = Repeated | Optional


type MessageField =
  { modifier: Modifier option
    name: MessageFieldName
    fieldType: MessageFieldType
    number: MessageFieldNumber
    options: Option list option }

   
type MessageItem =
    | MessageField of MessageField
    | MessageEnum of Enum
    | MessageMessage of Message
    | MessageOption of Option
    | MessageEmptyItem
and Message =
  { name: MessageName
    items: MessageItem list }
```

### Proto3Parser.fs
```fsharp
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
    let modifier = opt ((stringReturn "repeated" Repeated <|> stringReturn "optional" Optional) .>> spaces1)
    let fieldType = fieldType .>> spaces1
    let fieldName = identifier .>> spaces .>> skipChar '=' .>> spaces |>> MessageFieldName
    let fieldNumber = puint32 .>> spaces |>> MessageFieldNumber
    let options = options .>> spaces .>> skipString ";" .>> spaces 

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
    let name = skipString "message" >>. spaces1 >>. identifier .>> spaces |>> MessageName 
    let items = between (skipChar '{' .>> spaces) (skipChar '}' .>> spaces) (many messageItem)

    pipe2 name items (fun name items -> { name = name; items = items })
```

### Proto3ParserTests.fs
```fsharp
[<Fact>]
let ``"string login = 1 [packed=true];" is MessageField`` () =
    let expected = { modifier = None
                     name = MessageFieldName "login"
                     fieldType = String
                     number = MessageFieldNumber 1u
                     options = Some [ { name = SimpleName "packed"
                                        value = Constant.Bool true } ] }
    Assert.ParseEqual("string login = 1 [packed=true];", messageField, expected)

    
[<Fact>]
let ``"repeated MailAddress cc = 3;" is MessageField`` () =
    let expected = { modifier = Some Repeated
                     name = MessageFieldName "cc"
                     fieldType = MessageFieldType.Reference "MailAddress"
                     number = MessageFieldNumber 3u
                     options = None }
    Assert.ParseEqual("repeated MailAddress cc = 3;", messageField, expected)


[<Fact>]
let ``"message MailRequest { User user = 1; Mail mail = 2; }" is MessageDefinition`` () =
    let expected = { Message.name = MessageName "MailRequest"
                     items = [ MessageField { modifier = None
                                              name = MessageFieldName "user"
                                              fieldType = Reference "User" 
                                              number = MessageFieldNumber 1u
                                              options = None }
                               MessageField { modifier = None
                                              name = MessageFieldName "mail"
                                              fieldType = Reference "Mail"
                                              number = MessageFieldNumber 2u
                                              options = None } ] }
    Assert.ParseEqual("message MailRequest { User user = 1; Mail mail = 2; }", messageDefinition, expected)


[<Fact>]
let ``"message User { string login = 1; int64 uid = 2;}" is MessageDefinition`` () =
    let expected = { Message.name = MessageName "User"
                     items = [ MessageField { modifier = None
                                              name = MessageFieldName "login"
                                              fieldType = String 
                                              number = MessageFieldNumber 1u
                                              options = None }
                               MessageField { modifier = None
                                              name = MessageFieldName "uid"
                                              fieldType = Int64
                                              number = MessageFieldNumber 2u
                                              options = None } ] }
    Assert.ParseEqual("message User { string login = 1; int64 uid = 2;}", messageDefinition, expected)
```

## 17. Entire File

### Proto3.fs
```fsharp
type ProtoItem =
    | ProtoImport of Import
    | ProtoPackage of Package
    | ProtoOption of Option
    | ProtoMessage of Message
    | ProtoEnum of Enum
    // TODO: ProtoService
    | ProtoEmptyStatement
```

### Proto3Parser.fs
```fsharp
let protoItem =
    choice
      [ stringReturn ";" ProtoEmptyStatement
        import |>> ProtoImport
        package |>> ProtoPackage
        option |>> ProtoOption
        enumDefinition |>> ProtoEnum
        messageDefinition |>> ProtoMessage ]


let proto = spaces >>. syntax >>. (many protoItem)
```

### Proto3ParserTests.fs
```fsharp
[<Fact>]
[<Trait("Category", "Integration")>]
let ``.proto`` () =
    let example = """
syntax = "proto3";

package crm.proto.space.api.mail;

option csharp_namespace = "Api.Logic.Forms.VirtualMail.Contracts";

import "google/protobuf/timestamp.proto";

message MailRequest {
    User user = 1;
    Mail mail = 2;
}

message User {
    string login = 1;
    int64 uid = 2;
}

message Mail {
    MailAddress from = 1;
    repeated MailAddress to = 2;
    repeated MailAddress cc = 3;
    string subject = 4;
    string text = 5;
    bool is_text_html = 6;
    google.protobuf.Timestamp time = 7;
    repeated MailHeader headers = 8;
    repeated File files = 9;
}

message MailAddress {
    string email = 1;
    string display_name = 2;
}

message MailHeader {
    string name = 1;
    string value = 2;
}

message File {
    string name = 1;
    string url_path = 2;
}
"""
    let expected = [
        ProtoPackage (Package "crm.proto.space.api.mail")
        ProtoOption {
            name = SimpleName "csharp_namespace"
            value = Constant.String "Api.Logic.Forms.VirtualMail.Contracts" }
        ProtoImport (Import "google/protobuf/timestamp.proto")
        ProtoMessage {
            name = MessageName "MailRequest"
            items = [
                MessageField {
                    modifier = None
                    name = MessageFieldName "user"
                    fieldType = Reference "User" 
                    number = MessageFieldNumber 1u
                    options = None
                }
                MessageField {
                    modifier = None
                    name = MessageFieldName "mail"
                    fieldType = Reference "Mail"
                    number = MessageFieldNumber 2u
                    options = None
                }
            ]
        }
        ProtoMessage { name = MessageName "User"
                       items = [ MessageField { modifier = None
                                                name = MessageFieldName "login"
                                                fieldType = String 
                                                number = MessageFieldNumber 1u
                                                options = None }
                                 MessageField { modifier = None
                                                name = MessageFieldName "uid"
                                                fieldType = Int64
                                                number = MessageFieldNumber 2u
                                                options = None } ] }
        ProtoMessage { name = MessageName "Mail"
                       items = [ MessageField { modifier = None
                                                name = MessageFieldName "from"
                                                fieldType = Reference "MailAddress"
                                                number = MessageFieldNumber 1u
                                                options = None }
                                 MessageField { modifier = Some Repeated
                                                name = MessageFieldName "to"
                                                fieldType = Reference "MailAddress"
                                                number = MessageFieldNumber 2u
                                                options = None }
                                 MessageField { modifier = Some Repeated
                                                name = MessageFieldName "cc"
                                                fieldType = Reference "MailAddress"
                                                number = MessageFieldNumber 3u
                                                options = None }
                                 MessageField { modifier = None
                                                name = MessageFieldName "subject"
                                                fieldType = String
                                                number = MessageFieldNumber 4u
                                                options = None }
                                 MessageField { modifier = None
                                                name = MessageFieldName "text"
                                                fieldType = String
                                                number = MessageFieldNumber 5u
                                                options = None }
                                 MessageField { modifier = None
                                                name = MessageFieldName "is_text_html"
                                                fieldType = Bool
                                                number = MessageFieldNumber 6u
                                                options = None }
                                 MessageField { modifier = None
                                                name = MessageFieldName "time"
                                                fieldType = Reference "google.protobuf.Timestamp"
                                                number = MessageFieldNumber 7u
                                                options = None }
                                 MessageField { modifier = Some Repeated
                                                name = MessageFieldName "headers"
                                                fieldType = Reference "MailHeader"
                                                number = MessageFieldNumber 8u
                                                options = None }
                                 MessageField { modifier = Some Repeated
                                                name = MessageFieldName "files"
                                                fieldType = Reference "File"
                                                number = MessageFieldNumber 9u
                                                options = None } ] }
        ProtoMessage { name = MessageName "MailAddress"
                       items = [ MessageField { modifier = None
                                                name = MessageFieldName "email"
                                                fieldType = String 
                                                number = MessageFieldNumber 1u
                                                options = None }
                                 MessageField { modifier = None
                                                name = MessageFieldName "display_name"
                                                fieldType = String
                                                number = MessageFieldNumber 2u
                                                options = None } ] }
        ProtoMessage { name = MessageName "MailHeader"
                       items = [ MessageField { modifier = None
                                                name = MessageFieldName "name"
                                                fieldType = String 
                                                number = MessageFieldNumber 1u
                                                options = None }
                                 MessageField { modifier = None
                                                name = MessageFieldName "value"
                                                fieldType = String
                                                number = MessageFieldNumber 2u
                                                options = None } ] }
        ProtoMessage { name = MessageName "File"
                       items = [ MessageField { modifier = None
                                                name = MessageFieldName "name"
                                                fieldType = String 
                                                number = MessageFieldNumber 1u
                                                options = None }
                                 MessageField { modifier = None
                                                name = MessageFieldName "url_path"
                                                fieldType = String
                                                number = MessageFieldNumber 2u
                                                options = None } ] }
                     ]
    Assert.ParseEqual(example, proto, expected)
```

## 18. .textproto

1. Append **Textproto.fs** and **TextprotoParser.fs** to **ProtoS**
   as _third_ and _fourth_ files in the project. Get content from the GitHub.
2. Append **TextprotoParserTests.fs** to **ProtoS.Tests**
   as _third_ file in the project. Get content from the GitHub.