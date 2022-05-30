module ParserTests

open Xunit
open Parser
open Proto

module Assert =
    open FParsec
    
    let Parse<'r>(parser: Parser<'r, unit>, source: string) =
        match run parser source with
        | Success (result, _, _) -> Assert.True(true, sprintf $"%A{result}")
        | Failure (message, _, _) -> Assert.True(false, message)
        
    let ParseEqual<'r>(expected: 'r, parser: Parser<'r, unit>, source: string) =
        match run parser source with
        | Success (result, _, _) -> Assert.Equal(expected, result)
        | Failure (message, _, _) -> Assert.True(false, message)

    let NotParse<'r>(parser: Parser<'r, unit>, source: string) =
        match run parser source with
        | Success (result, _, _) -> Assert.True(false, sprintf $"%A{result}")
        | Failure (message, _, _) -> Assert.True(true, message)
        
[<Fact>]
let ``"abc" is identifier`` () =
    Assert.Parse(ident, "abc")
    
[<Fact>]
let ``"a_b_c" is identifier`` () =
    Assert.Parse(ident, "a_b_c")
    
[<Fact>]
let ``"abc123" is identifier`` () =
    Assert.Parse(ident, "abc123")
    
[<Fact>]
let ``"_abc" isn't identifier `` () =
    Assert.NotParse(ident, "_abc")
    
[<Fact>]
let ``"123abc" isn't identifier `` () =
    Assert.NotParse(ident, "123abc")
    
[<Fact>]
let ``"abc" is full identifier`` () =
    Assert.ParseEqual("abc", fullIdent, "abc")
    
[<Fact>]
let ``"abc.def.hig" is full identifier`` () =
    Assert.ParseEqual("abc.def.hig", fullIdent, "abc.def.hig")
    
[<Fact>]
let ``"true" is true`` () =
    Assert.ParseEqual(true, boolLit, "true")
    
[<Fact>]
let ``"false" is false`` () =
    Assert.ParseEqual(false, boolLit, "false")

[<Fact>]
let ``"Lorem ipsum" is the string`` () =
    Assert.ParseEqual("Lorem ipsum", strLit, "\"Lorem ipsum\"")

[<Fact>]
let ``'Lorem ipsum' is the string`` () =
    Assert.ParseEqual("Lorem ipsum", strLit, "'Lorem ipsum'")

[<Fact>]
let ``"syntax = 'proto3';" is parsed successfully`` () =
    Assert.Parse(syntax, "syntax = 'proto3';")
    

[<Fact>]
let ``syntax without semicolon fails`` () =
    Assert.NotParse(syntax, "syntax = 'proto3'")
    
[<Fact>]
let ``"package abc.def.ghi;" is parsed successfully`` () =
    Assert.ParseEqual(Package "abc.def.ghi", package, "package abc.def.ghi;")
    
[<Fact>]
let ``"import weak 'path'; is parsed successfully`` () =
    Assert.ParseEqual(WeakImport "path", import, "import weak 'path';")

[<Fact>]
let ``"import public 'path'; is parsed successfully`` () =
    Assert.ParseEqual(PublicImport "path", import, "import public 'path';")

[<Fact>]
let ``"import 'path'; is parsed successfully`` () =
    Assert.ParseEqual(Import "path", import, "import 'path';")
    
[<Fact>]
let ``"option foo = 'aaa';" is parsed successfully``() =
    Assert.ParseEqual({ Option.name = OptionName "foo"
                        value = Constant.String "aaa" }, option, "option foo = 'aaa';")

[<Fact>]
let ``"option foo = false;" is parsed successfully`` () =
    Assert.ParseEqual({ Option.name = OptionName "foo"
                        value = Constant.Bool false }, option, "option foo = false;")

[<Fact>]
let ``"option foo = -2;" is parsed successfully`` () =
    Assert.ParseEqual({ Option.name = OptionName "foo"
                        value = Integer -2 }, option, "option foo = -2;")

[<Fact>]
let ``"option foo = +3.14'" is parsed successfully`` () =
    Assert.ParseEqual({ Option.name = OptionName "foo"
                        value = Constant.Float 3.14 }, option, "option foo = +3.14;")

[<Fact>]
let ``"option foo = bar.baz;`` () =
    Assert.ParseEqual({ Option.name = OptionName "foo"
                        value = Constant.Reference "bar.baz" }, option, "option foo = bar.baz;")

[<Fact>]
let ``"double" is FieldType.Double`` () =
    Assert.ParseEqual(FieldType.Double, fieldType, "double")

[<Fact>]
let ``"float" is FieldType.Float`` () =
    Assert.ParseEqual(FieldType.Float, fieldType, "float")

[<Fact>]
let ``"int32" is FieldType.Int32`` () =
    Assert.ParseEqual(FieldType.Int32, fieldType, "int32")

[<Fact>]
let ``"int64" is FieldType.Int64`` () =
    Assert.ParseEqual(FieldType.Int64, fieldType, "int64")

[<Fact>]
let ``"uint32" is FieldType.UInt32`` () =
    Assert.ParseEqual(FieldType.UInt32, fieldType, "uint32")

[<Fact>]
let ``"uint64" is FieldType.UInt64`` () =
    Assert.ParseEqual(FieldType.UInt64, fieldType, "uint64")

[<Fact>]
let ``"sint32" is FieldType.SInt32`` () =
    Assert.ParseEqual(FieldType.SInt32, fieldType, "sint32")

[<Fact>]
let ``"sint64" is FieldType.SInt64`` () =
    Assert.ParseEqual(FieldType.SInt64, fieldType, "sint64")

[<Fact>]
let ``"fixed32" is FieldType.Fixed32`` () =
    Assert.ParseEqual(FieldType.Fixed32, fieldType, "fixed32")

[<Fact>]
let ``"fixed64" is FieldType.Fixed64`` () =
    Assert.ParseEqual(FieldType.Fixed64, fieldType, "fixed64")

[<Fact>]
let ``"sfixed32" is FieldType.SFixed32`` () =
    Assert.ParseEqual(FieldType.SFixed32, fieldType, "sfixed32")

[<Fact>]
let ``"sfixed64" is FieldType.SFixed64`` () =
    Assert.ParseEqual(FieldType.SFixed64, fieldType, "sfixed64")

[<Fact>]
let ``"bool" is FieldType.Bool`` () =
    Assert.ParseEqual(FieldType.Bool, fieldType, "bool")

[<Fact>]
let ``"string" is FieldType.String`` () =
    Assert.ParseEqual(FieldType.String, fieldType, "string")

[<Fact>]
let ``"bytes" is FieldType.Bytes`` () =
    Assert.ParseEqual(FieldType.Bytes, fieldType, "bytes")

[<Fact>]
let ``"MailAddress" is FieldType.Reference`` () =
    Assert.ParseEqual(FieldType.Reference "MailAddress", fieldType, "MailAddress")
    
[<Fact>]
let ``"[a = 1, b = 2]" is List of Options`` () =
    let expected = Some [{ Option.name = OptionName "a"; value = Constant.Integer 1 }
                         { Option.name = OptionName "b"; value = Constant.Integer 2 }]
    Assert.ParseEqual(expected, options, "[a = 1, b = 2]")
    
[<Fact>]
let ``"[]" is empty List of Options`` () =
    Assert.ParseEqual(Some List.empty<Option>, options, "[]")
    
[<Fact>]
let ``"" is None List of Options`` () =
    Assert.ParseEqual(None, options, "")
    
[<Fact>]
let ``"string login = 1 [packed=true];" is string field login with number 1 and option packed`` () =
    let expected = { Field.repeated = false
                     name = FieldName "login"
                     fieldType = FieldType.String
                     number = FieldNumber 1u
                     options = Some [{ Option.name = OptionName "packed"
                                       value = Constant.Bool true }] }
    Assert.ParseEqual(expected, field, "string login = 1 [packed=true];")
    
[<Fact>]
let ``"repeated MailAddress cc = 3;" is repeated MailAddress field cc with number 3`` () =
    let expected = { Field.repeated = true
                     name = FieldName "cc"
                     fieldType = FieldType.Reference "MailAddress"
                     number = FieldNumber 3u
                     options = None }
    Assert.ParseEqual(expected, field, "repeated MailAddress cc = 3;")
