module TextFormatParserTests

open FParsec
open Xunit
open TextFormatParser
open TextFormat

[<Fact>]
let ``comment parses comment with \n and without \n`` () =
    Assert.Parse("# comment", comment >>. eof)
    Assert.Parse("# comment\n", comment >>. eof)

[<Fact>]
let ``skipSpaces skips spaces`` () =
    Assert.Parse("- 1", pstring "-" .>> skipSpaces >>. pstring "1")

[<Fact>]
let ``skipSpaces skips comments`` () =
    Assert.Parse("-# comment\n# another comment\n1", pstring "-" .>> skipSpaces >>. pstring "1")

[<Fact>]
let ``skipSpaces skips spaces and comments`` () =
    Assert.Parse("- # comment\n # another comment\n1", pstring "-" .>> skipSpaces >>. pstring "1")
    
[<Fact>]
let ``skipSpaces works without spaces or comments`` () =
    Assert.Parse("-1", pstring "-" .>> skipSpaces >>. pstring "1")

[<Fact>]
let ``identifier parses abc123`` () =
    Assert.Parse("abc123", identifier)
    
[<Fact>]
let ``identifier parses aBc123`` () =
    Assert.Parse("aBc123", identifier)
    
[<Fact>]
let ``identifier parses ABC`` () =
    Assert.Parse("ABC", identifier)

[<Fact>]
let ``identifier doesn't parse 123`` () =
    Assert.NotParse("123", identifier)
    
[<Fact>]
let ``decimalLiteral parses 123`` () =
    Assert.ParseEqual("123", decimalLiteral, "123")    

[<Fact>]
let ``decimalLiteral parses 0`` () =
    Assert.ParseEqual("0", decimalLiteral, "0")

[<Fact>]
let ``decimalLiteral doesn't parse 0`` () =
    Assert.NotParse("0123", decimalLiteral)
    
[<Fact>]
let ``float:Literal parses .123, .123e100, .123E+100, .123e-100`` () =
    Assert.ParseEqual(".123", floatLiteral, ".123")
    Assert.ParseEqual(".123e100", floatLiteral, ".123e100")
    Assert.ParseEqual(".123E+100", floatLiteral, ".123E+100")
    Assert.ParseEqual(".123e-100", floatLiteral, ".123e-100")
    
[<Fact>]
let ``float:Literal parses 123., 123.100 123.e100, 123.E+100, 123.e-100`` () =
    Assert.ParseEqual("123.", floatLiteral, "123.")
    Assert.ParseEqual("123.100", floatLiteral, "123.100")
    Assert.ParseEqual("123.e100", floatLiteral, "123.e100")
    Assert.ParseEqual("123.E+100", floatLiteral, "123.E+100")
    Assert.ParseEqual("123.e-100", floatLiteral, "123.e-100")
    
[<Fact>]
let ``floatLiteral parses 123e100, 123.E+100, 123.e-100`` () =
    Assert.ParseEqual("123e100", floatLiteral, "123e100")
    Assert.ParseEqual("123E+100", floatLiteral, "123E+100")
    Assert.ParseEqual("123e-100", floatLiteral, "123e-100")

[<Fact>]
let ``decimalInteger parses 123, 0, but not 0123`` () =
    Assert.ParseEqual("123", decimalInteger, 123)
    Assert.ParseEqual("0", decimalInteger, 0)
    Assert.NotParse("0123", decimalInteger)
    
[<Fact>]
let ``octalInteger parses 0123, but not 0, 123, 08`` () =
    Assert.ParseEqual("0123", octalInteger, 0o123)
    Assert.NotParse("0", octalInteger)
    Assert.NotParse("123", octalInteger)
    Assert.NotParse("08", octalInteger)
    
[<Fact>]
let ``hexadecimalInteger parses 0x123, 0X0`` () =
    Assert.ParseEqual("0x123", hexadecimalInteger, 0x123)
    Assert.ParseEqual("0X0", hexadecimalInteger, 0x0)
    
[<Fact>]
let ``decimalFloat parses 10f, 10.0, and 10.0f`` () =
    Assert.ParseEqual("10f", decimalFloat, 10.0)
    Assert.ParseEqual("10.0", decimalFloat, 10.0)
    Assert.ParseEqual("10.0f", decimalFloat, 10.0)
    
[<Fact>]
let ``stringLiteral parses valid strings`` () =
    Assert.ParseEqual("""'abfnrtv\a\b\f\n\r\t\v'""", stringLiteral, "abfnrtv\a\b\f\n\r\t\v")
    Assert.ParseEqual("""'\?"\"\''""", stringLiteral, "?\"\"\'")
    Assert.ParseEqual("""'\123'""", stringLiteral, "S")
    Assert.ParseEqual("""'\x23'""", stringLiteral, "\x23")
    Assert.ParseEqual("""'\u12345'""", stringLiteral, "\u12345")
    //Assert.ParseEqual("""'\U00012345'""", stringLiteral, "\U00012345")
    //Assert.ParseEqual("""'\U00101234'""", stringLiteral, "\U00101234")

[<Fact>]
let ``stringValue parses a few strings`` () =
    Assert.ParseEqual("'abc' 'def''ghi'", stringValue, "abcdefghi")
    Assert.ParseEqual("'abc'#comment\n\"def\"'ghi'", stringValue, "abcdefghi")
    
[<Fact>]
let ``scalarValue parses scalar values`` () =
    Assert.ParseEqual("'abc' 'def''ghi'", scalarValue, ScalarValue.String "abcdefghi")
    Assert.ParseEqual("-#comment\n.5", scalarValue, ScalarValue.Float -0.5)
    Assert.ParseEqual(".5", scalarValue, ScalarValue.Float 0.5)
    Assert.ParseEqual("inf", scalarValue, ScalarValue.Identifier "inf")
    Assert.ParseEqual("-inf", scalarValue, ScalarValue.SignedIdentifier "inf")
    Assert.ParseEqual("123", scalarValue, ScalarValue.DecUnsignedInteger 123UL)
    //Assert.ParseEqual("0123", scalarValue, ScalarValue.OctUnsignedInteger 0o123UL)
    //Assert.ParseEqual("0x123", scalarValue, ScalarValue.HexUnsignedInteger 0x123UL)
    Assert.ParseEqual("-123", scalarValue, ScalarValue.DecSignedInteger -123L)
    //Assert.ParseEqual("-0123", scalarValue, ScalarValue.OctSignedInteger -0o123L)
    //Assert.ParseEqual("-0x123", scalarValue, ScalarValue.HexSignedInteger -0x123L)

[<Fact>]
let ``fieldName parses extension, any and identifier`` () =
    Assert.ParseEqual("name", fieldName, FieldName.Identifier "name")
    Assert.ParseEqual("[com.typename]", fieldName, FieldName.Extension "com.typename")
    Assert.ParseEqual("[com.domain/com.typename]", fieldName, FieldName.Any ("com.domain", "com.typename"))
