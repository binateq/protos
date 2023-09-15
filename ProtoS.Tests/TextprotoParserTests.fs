module TextprotoParserTests

open FParsec
open Xunit
open TextprotoParser
open Textproto


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
let ``stringLiteral parses valid strings`` () =
    Assert.ParseEqual("""'abfnrtv\a\b\f\n\r\t\v'""", stringLiteral, "abfnrtv\a\b\f\n\r\t\v")
    Assert.ParseEqual("""'\?"\"\''""", stringLiteral, "?\"\"\'")
    Assert.ParseEqual("""'\123'""", stringLiteral, "S")
    Assert.ParseEqual("""'\x23'""", stringLiteral, "\x23")
    Assert.ParseEqual("""'\u12345'""", stringLiteral, "\u12345")
    Assert.ParseEqual("""'\U00012345'""", stringLiteral, "\U00012345")
    Assert.ParseEqual("""'\U00101234'""", stringLiteral, "\U00101234")


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
    Assert.ParseEqual("123", scalarValue, ScalarValue.Integer 123L)
    //Assert.ParseEqual("0123", scalarValue, ScalarValue.OctUnsignedInteger 0o123UL)
    //Assert.ParseEqual("0x123", scalarValue, ScalarValue.HexUnsignedInteger 0x123UL)
    Assert.ParseEqual("-123", scalarValue, ScalarValue.Integer -123L)
    //Assert.ParseEqual("-0123", scalarValue, ScalarValue.OctSignedInteger -0o123L)
    //Assert.ParseEqual("-0x123", scalarValue, ScalarValue.HexSignedInteger -0x123L)


[<Fact>]
let ``fieldName parses extension, any and identifier`` () =
    Assert.ParseEqual("name", fieldName, FieldName.Identifier "name")
    Assert.ParseEqual("[com.typename]", fieldName, FieldName.Extension "com.typename")
    Assert.ParseEqual("[com.domain/com.typename]", fieldName, FieldName.Any ("com.domain", "com.typename"))


[<Fact>]
let ``scalarList parses scalar values list`` () =
    let expected = [
        ScalarValue.Integer 1L
        ScalarValue.String "abc"
        ScalarValue.SignedIdentifier "def"
    ]
    Assert.ParseEqual("[ 1, 'abc', -def]", scalarList, expected)

    
[<Fact>]
let ``scalarField parses scalar fields`` () =
    Assert.ParseEqual("foo: 10", scalarField, {
        ScalarField.name = FieldName.Identifier "foo"
        value = ScalarFieldValue.ScalarValue (Integer 10L)
    })
    Assert.ParseEqual("[foo.bar] : [10, 20]", scalarField, {
        ScalarField.name = FieldName. Extension "foo.bar"
        value = ScalarFieldValue.ScalarList [Integer 10L; Integer 20L]
    })

    
[<Fact>]
let ``messageField parses message fields`` () =
    Assert.ParseEqual("foo {bar: 10 baz: 20}", messageField, {
        MessageField.name = FieldName.Identifier "foo"
        value = MessageValue (Message [
    ScalarField { name = FieldName.Identifier "bar"
                  value = ScalarFieldValue.ScalarValue (Integer 10L) };
    ScalarField { name = FieldName.Identifier "baz"
                  value = ScalarFieldValue.ScalarValue (Integer 20L) } ])
    })
    Assert.ParseEqual("foo : <bar: 10, baz: 20;>", messageField, {
        MessageField.name = FieldName.Identifier "foo"
        value = MessageValue (Message [
    ScalarField { name = FieldName.Identifier "bar"
                  value = ScalarFieldValue.ScalarValue (Integer 10L) };
    ScalarField { name = FieldName.Identifier "baz"
                  value = ScalarFieldValue.ScalarValue (Integer 20L) } ])
    })


[<Fact>]
[<Trait("Category", "Integration")>]
let ``.textproto`` () =
    let example = """
# This is an example of Protocol Buffer's text format.
# Unlike .proto files, only shell-style line comments are supported.

name: "John Smith"

pet {
  kind: DOG
  name: "Fluffy"
  tail_wagginess: 0.65f
}

pet <
  kind: LIZARD
  name: "Lizzy"
  legs: 4
>

string_value_with_escape: "valid \n escape"
repeated_values: [ "one", "two", "three" ]
"""
    let expected = Message [
        ScalarField {
            name = FieldName.Identifier "name"
            value = ScalarValue (String "John Smith") }
        MessageField {
            name = FieldName.Identifier "pet"
            value = MessageValue (Message [
                ScalarField {
                    name = FieldName.Identifier "kind"
                    value = ScalarValue (Identifier "DOG") }
                ScalarField {
                    name = FieldName.Identifier "name"
                    value = ScalarValue (String "Fluffy") }
                ScalarField {
                    name = FieldName.Identifier "tail_wagginess"
                    value = ScalarValue (Float 0.65) }]) }
        MessageField {
            name = FieldName.Identifier "pet"
            value = MessageValue (Message [
                ScalarField {
                    name = FieldName.Identifier "kind"
                    value = ScalarValue (Identifier "LIZARD") }
                ScalarField {
                    name = FieldName.Identifier "name"
                    value = ScalarValue (String "Lizzy") }
                ScalarField {
                    name = FieldName.Identifier "legs"
                    value = ScalarValue (Integer 4L) }]) }
        ScalarField {
            name = FieldName.Identifier "string_value_with_escape"
            value = ScalarValue (String "valid \n escape") }
        ScalarField {
            name = FieldName.Identifier "repeated_values"
            value = ScalarList [String "one"; String "two"; String "three"] } ]
    
    Assert.ParseEqual(example, textproto, expected)