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
let ``"option foo = 'aaa'" is parsed successfully``() =
    Assert.ParseEqual({ Option.name = OptionName "foo"
                        value = Constant.String "aaa" }, optionDefinition, "option foo = 'aaa'")

[<Fact>]
let ``"option foo = false" is parsed successfully`` () =
    Assert.ParseEqual({ Option.name = OptionName "foo"
                        value = Constant.Bool false }, optionDefinition, "option foo = false")

[<Fact>]
let ``"option foo = -2" is parsed successfully`` () =
    Assert.ParseEqual({ Option.name = OptionName "foo"
                        value = Integer -2 }, optionDefinition, "option foo = -2")

[<Fact>]
let ``"option foo = +3.14" is parsed successfully`` () =
    Assert.ParseEqual({ Option.name = OptionName "foo"
                        value = Constant.Float 3.14 }, optionDefinition, "option foo = +3.14")

[<Fact>]
let ``"option foo = bar.baz;`` () =
    Assert.ParseEqual({ Option.name = OptionName "foo"
                        value = Reference "bar.baz" }, optionDefinition, "option foo = bar.baz;")

[<Fact>]
let ``"double" is ScalarType.Double`` () =
    Assert.ParseEqual(ScalarType.Double, scalarType, "double")

[<Fact>]
let ``"float" is ScalarType.Float`` () =
    Assert.ParseEqual(ScalarType.Float, scalarType, "float")

[<Fact>]
let ``"int32" is ScalarType.Int32`` () =
    Assert.ParseEqual(ScalarType.Int32, scalarType, "int32")

[<Fact>]
let ``"int64" is ScalarType.Int64`` () =
    Assert.ParseEqual(ScalarType.Int64, scalarType, "int64")

[<Fact>]
let ``"uint32" is ScalarType.UInt32`` () =
    Assert.ParseEqual(ScalarType.UInt32, scalarType, "uint32")

[<Fact>]
let ``"uint64" is ScalarType.UInt64`` () =
    Assert.ParseEqual(ScalarType.UInt64, scalarType, "uint64")

[<Fact>]
let ``"sint32" is ScalarType.SInt32`` () =
    Assert.ParseEqual(ScalarType.SInt32, scalarType, "sint32")

[<Fact>]
let ``"sint64" is ScalarType.SInt64`` () =
    Assert.ParseEqual(ScalarType.SInt64, scalarType, "sint64")

[<Fact>]
let ``"fixed32" is ScalarType.Fixed32`` () =
    Assert.ParseEqual(ScalarType.Fixed32, scalarType, "fixed32")

[<Fact>]
let ``"fixed64" is ScalarType.Fixed64`` () =
    Assert.ParseEqual(ScalarType.Fixed64, scalarType, "fixed64")

[<Fact>]
let ``"sfixed32" is ScalarType.SFixed32`` () =
    Assert.ParseEqual(ScalarType.SFixed32, scalarType, "sfixed32")

[<Fact>]
let ``"sfixed64" is ScalarType.SFixed64`` () =
    Assert.ParseEqual(ScalarType.SFixed64, scalarType, "sfixed64")

[<Fact>]
let ``"bool" is ScalarType.Bool`` () =
    Assert.ParseEqual(ScalarType.Bool, scalarType, "bool")

[<Fact>]
let ``"string" is ScalarType.String`` () =
    Assert.ParseEqual(ScalarType.String, scalarType, "string")

[<Fact>]
let ``"bytes" is ScalarType.Bytes`` () =
    Assert.ParseEqual(ScalarType.Bytes, scalarType, "bytes")

