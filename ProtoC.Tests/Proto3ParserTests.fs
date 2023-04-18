module Proto3ParserTests

open Xunit
open Proto3Parser
open Proto3

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
    Assert.Parse(identifier, "abc")
    
[<Fact>]
let ``"a_b_c" is identifier`` () =
    Assert.Parse(identifier, "a_b_c")
    
[<Fact>]
let ``"abc123" is identifier`` () =
    Assert.Parse(identifier, "abc123")
    
[<Fact>]
let ``"_abc" isn't identifier `` () =
    Assert.NotParse(identifier, "_abc")
    
[<Fact>]
let ``"123abc" isn't identifier `` () =
    Assert.NotParse(identifier, "123abc")
    
[<Fact>]
let ``"abc" is full identifier`` () =
    Assert.ParseEqual("abc", fullIdentifier, "abc")
    
[<Fact>]
let ``"abc.def.hig" is full identifier`` () =
    Assert.ParseEqual("abc.def.hig", fullIdentifier, "abc.def.hig")
    
[<Fact>]
let ``"true" is true`` () =
    Assert.ParseEqual(true, boolLit, "true")
    
[<Fact>]
let ``"false" is false`` () =
    Assert.ParseEqual(false, boolLit, "false")

[<Fact>]
let ``"\"Lorem ipsum\"" is string`` () =
    Assert.ParseEqual("Lorem ipsum", stringLiteral, "\"Lorem ipsum\"")

[<Fact>]
let ``"'Lorem ipsum'" is string`` () =
    Assert.ParseEqual("Lorem ipsum", stringLiteral, "'Lorem ipsum'")

[<Fact>]
let ``"syntax = 'proto3';" is Syntax`` () =
    Assert.Parse(syntax, "syntax = 'proto3';")
    

[<Fact>]
let ``"syntax = 'proto3'" without semicolon fails`` () =
    Assert.NotParse(syntax, "syntax = 'proto3'")
    
[<Fact>]
let ``"package abc.def.ghi;" is Package`` () =
    Assert.ParseEqual(Package "abc.def.ghi", package, "package abc.def.ghi;")
    
[<Fact>]
let ``"import weak 'path'; is WeakImport`` () =
    Assert.ParseEqual(WeakImport "path", import, "import weak 'path';")

[<Fact>]
let ``"import public 'path'; is PublicImport`` () =
    Assert.ParseEqual(PublicImport "path", import, "import public 'path';")

[<Fact>]
let ``"import 'path'; is Import`` () =
    Assert.ParseEqual(Import "path", import, "import 'path';")
    
[<Fact>]
let ``"foo" is valid option name`` () =
    Assert.ParseEqual(SimpleName "foo", optionName, "foo")
    
[<Fact>]
let ``"foo.bar" is valid option name`` () =
    Assert.ParseEqual(SimpleName "foo.bar", optionName, "foo.bar")
    
[<Fact>]
let ``"(foo).bar" is valid option name`` () =
    Assert.ParseEqual(ComplexName ("foo", "bar"), optionName, "(foo).bar")
    
[<Fact>]
let ``"(foo.bar).baz.qux" is valid option name`` () =
    Assert.ParseEqual(ComplexName ("foo.bar", "baz.qux"), optionName, "(foo.bar).baz.qux")
    
[<Fact>]
let ``"option foo = 'aaa';" is String Proto.Option``() =
    Assert.ParseEqual({ Option.name = SimpleName "foo"
                        value = Constant.String "aaa" }, option, "option foo = 'aaa';")

[<Fact>]
let ``"option foo = false;" is Bool Proto.Option`` () =
    Assert.ParseEqual({ Option.name = SimpleName "foo"
                        value = Constant.Bool false }, option, "option foo = false;")

[<Fact>]
let ``"option foo = -2;" is Integer Proto.Option`` () =
    Assert.ParseEqual({ Option.name = SimpleName "foo"
                        value = Integer -2 }, option, "option foo = -2;")

[<Fact>]
let ``"option foo = +3.14'" is Float Proto.Option`` () =
    Assert.ParseEqual({ Option.name = SimpleName "foo"
                        value = Constant.Float 3.14 }, option, "option foo = +3.14;")

[<Fact>]
let ``"option foo = bar.baz;" is Reference Proto.Option`` () =
    Assert.ParseEqual({ Option.name = SimpleName "foo"
                        value = Constant.Reference "bar.baz" }, option, "option foo = bar.baz;")

[<Fact>]
let ``"double" is MessageFieldType.Double`` () =
    Assert.ParseEqual(MessageFieldType.Double, fieldType, "double")

[<Fact>]
let ``"float" is MessageFieldType.Float`` () =
    Assert.ParseEqual(MessageFieldType.Float, fieldType, "float")

[<Fact>]
let ``"int32" is MessageFieldType.Int32`` () =
    Assert.ParseEqual(MessageFieldType.Int32, fieldType, "int32")

[<Fact>]
let ``"int64" is MessageFieldType.Int64`` () =
    Assert.ParseEqual(MessageFieldType.Int64, fieldType, "int64")

[<Fact>]
let ``"uint32" is MessageFieldType.UInt32`` () =
    Assert.ParseEqual(MessageFieldType.UInt32, fieldType, "uint32")

[<Fact>]
let ``"uint64" is MessageFieldType.UInt64`` () =
    Assert.ParseEqual(MessageFieldType.UInt64, fieldType, "uint64")

[<Fact>]
let ``"sint32" is MessageFieldType.SInt32`` () =
    Assert.ParseEqual(MessageFieldType.SInt32, fieldType, "sint32")

[<Fact>]
let ``"sint64" is MessageFieldType.SInt64`` () =
    Assert.ParseEqual(MessageFieldType.SInt64, fieldType, "sint64")

[<Fact>]
let ``"fixed32" is MessageFieldType.Fixed32`` () =
    Assert.ParseEqual(MessageFieldType.Fixed32, fieldType, "fixed32")

[<Fact>]
let ``"fixed64" is MessageFieldType.Fixed64`` () =
    Assert.ParseEqual(MessageFieldType.Fixed64, fieldType, "fixed64")

[<Fact>]
let ``"sfixed32" is MessageFieldType.SFixed32`` () =
    Assert.ParseEqual(MessageFieldType.SFixed32, fieldType, "sfixed32")

[<Fact>]
let ``"sfixed64" is MessageFieldType.SFixed64`` () =
    Assert.ParseEqual(MessageFieldType.SFixed64, fieldType, "sfixed64")

[<Fact>]
let ``"bool" is MessageFieldType.Bool`` () =
    Assert.ParseEqual(MessageFieldType.Bool, fieldType, "bool")

[<Fact>]
let ``"string" is MessageFieldType.String`` () =
    Assert.ParseEqual(MessageFieldType.String, fieldType, "string")

[<Fact>]
let ``"bytes" is MessageFieldType.Bytes`` () =
    Assert.ParseEqual(MessageFieldType.Bytes, fieldType, "bytes")

[<Fact>]
let ``"MailAddress" is MessageFieldType.Reference`` () =
    Assert.ParseEqual(MessageFieldType.Reference "MailAddress", fieldType, "MailAddress")
    
[<Fact>]
let ``"[a = 1, b = 2]" is Some List of Proto.Options`` () =
    let expected = Some [ { Option.name = SimpleName "a"; value = Constant.Integer 1 }
                          { Option.name = SimpleName "b"; value = Constant.Integer 2 } ]
    Assert.ParseEqual(expected, options, "[a = 1, b = 2]")
    
[<Fact>]
let ``"[]" is Some empty List of Proto.Options`` () =
    Assert.ParseEqual(Some List.empty<Option>, options, "[]")
    
[<Fact>]
let ``"" is None List of Proto.Options`` () =
    Assert.ParseEqual(None, options, "")
    
[<Fact>]
let ``"STARTED = 1;" is EnumField`` () =
    let expected = { EnumField.name = EnumFieldName "STARTED"
                     value = EnumValue 1
                     options = None }
    Assert.ParseEqual(expected, enumField, "STARTED = 1;")
    
[<Fact>]
let ``"RUNNING = 2 [(custom_option) = "hello world"];" is EnumField`` () =
    let expected = { EnumField.name = EnumFieldName "RUNNING"
                     value = EnumValue 2
                     options = Some [ { name = ComplexName ("custom_option", "")
                                        value = Constant.String "hello world" } ] }
    Assert.ParseEqual(expected, enumField, "RUNNING = 2 [(custom_option) = \"hello world\"];")

let ``"enum Foo { option alias = 'Bar'; UNKNOWN = 0; KNOWN = 1; }" is EnumDefinition`` () =
    let expected = { Enum.name = EnumName "Foo"
                     items = [ EnumOption { name = SimpleName "alias"; value = Constant.String "Bar" }
                               EnumField { name = EnumFieldName "UNKNOWN"; value = EnumValue 0; options = None }
                               EnumField { name = EnumFieldName "KNOWN"; value = EnumValue 1; options = None } ] }
    Assert.ParseEqual(expected, enumDefinition, "enum Foo { option alias = 'Bar'; UNKNOWN = 0; KNOWN = 1; }")
    
[<Fact>]
let ``"string login = 1 [packed=true];" is MessageField`` () =
    let expected = { repeated = false
                     name = MessageFieldName "login"
                     fieldType = String
                     number = MessageFieldNumber 1u
                     options = Some [ { name = SimpleName "packed"
                                        value = Constant.Bool true } ] }
    Assert.ParseEqual(expected, messageField, "string login = 1 [packed=true];")
    
[<Fact>]
let ``"repeated MailAddress cc = 3;" is MessageField`` () =
    let expected = { repeated = true
                     name = MessageFieldName "cc"
                     fieldType = MessageFieldType.Reference "MailAddress"
                     number = MessageFieldNumber 3u
                     options = None }
    Assert.ParseEqual(expected, messageField, "repeated MailAddress cc = 3;")

[<Fact>]
let ``"message MailRequest { User user = 1; Mail mail = 2; }" is MessageDefinition`` () =
    let expected = { Message.name = MessageName "MailRequest"
                     items = [ MessageField { repeated = false
                                              name = MessageFieldName "user"
                                              fieldType = Reference "User" 
                                              number = MessageFieldNumber 1u
                                              options = None }
                               MessageField { repeated = false
                                              name = MessageFieldName "mail"
                                              fieldType = Reference "Mail"
                                              number = MessageFieldNumber 2u
                                              options = None } ] }
    Assert.ParseEqual(expected, messageDefinition, "message MailRequest { User user = 1; Mail mail = 2; }")

[<Fact>]
let ``"message User { string login = 1; int64 uid = 2;}" is MessageDefinition`` () =
    let expected = { Message.name = MessageName "User"
                     items = [ MessageField { repeated = false
                                              name = MessageFieldName "login"
                                              fieldType = String 
                                              number = MessageFieldNumber 1u
                                              options = None }
                               MessageField { repeated = false
                                              name = MessageFieldName "uid"
                                              fieldType = Int64
                                              number = MessageFieldNumber 2u
                                              options = None } ] }
    Assert.ParseEqual(expected, messageDefinition, "message User { string login = 1; int64 uid = 2;}")

[<Fact>]
[<Trait("Category", "Integration")>]
let ``Proto file #1`` () =
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
    let expected = [ ProtoPackage (Package "crm.proto.space.api.mail")
                     ProtoOption { name = SimpleName "csharp_namespace"
                                   value = Constant.String "Api.Logic.Forms.VirtualMail.Contracts" }
                     ProtoImport (Import "google/protobuf/timestamp.proto")
                     ProtoMessage { name = MessageName "MailRequest"
                                    items = [ MessageField { repeated = false
                                                             name = MessageFieldName "user"
                                                             fieldType = Reference "User" 
                                                             number = MessageFieldNumber 1u
                                                             options = None }
                                              MessageField { repeated = false
                                                             name = MessageFieldName "mail"
                                                             fieldType = Reference "Mail"
                                                             number = MessageFieldNumber 2u
                                                             options = None } ] }
                     ProtoMessage { name = MessageName "User"
                                    items = [ MessageField { repeated = false
                                                             name = MessageFieldName "login"
                                                             fieldType = String 
                                                             number = MessageFieldNumber 1u
                                                             options = None }
                                              MessageField { repeated = false
                                                             name = MessageFieldName "uid"
                                                             fieldType = Int64
                                                             number = MessageFieldNumber 2u
                                                             options = None } ] }
                     ProtoMessage { name = MessageName "Mail"
                                    items = [ MessageField { repeated = false
                                                             name = MessageFieldName "from"
                                                             fieldType = Reference "MailAddress"
                                                             number = MessageFieldNumber 1u
                                                             options = None }
                                              MessageField { repeated = true
                                                             name = MessageFieldName "to"
                                                             fieldType = Reference "MailAddress"
                                                             number = MessageFieldNumber 2u
                                                             options = None }
                                              MessageField { repeated = true
                                                             name = MessageFieldName "cc"
                                                             fieldType = Reference "MailAddress"
                                                             number = MessageFieldNumber 3u
                                                             options = None }
                                              MessageField { repeated = false
                                                             name = MessageFieldName "subject"
                                                             fieldType = String
                                                             number = MessageFieldNumber 4u
                                                             options = None }
                                              MessageField { repeated = false
                                                             name = MessageFieldName "text"
                                                             fieldType = String
                                                             number = MessageFieldNumber 5u
                                                             options = None }
                                              MessageField { repeated = false
                                                             name = MessageFieldName "is_text_html"
                                                             fieldType = Bool
                                                             number = MessageFieldNumber 6u
                                                             options = None }
                                              MessageField { repeated = false
                                                             name = MessageFieldName "time"
                                                             fieldType = Reference "google.protobuf.Timestamp"
                                                             number = MessageFieldNumber 7u
                                                             options = None }
                                              MessageField { repeated = true
                                                             name = MessageFieldName "headers"
                                                             fieldType = Reference "MailHeader"
                                                             number = MessageFieldNumber 8u
                                                             options = None }
                                              MessageField { repeated = true
                                                             name = MessageFieldName "files"
                                                             fieldType = Reference "File"
                                                             number = MessageFieldNumber 9u
                                                             options = None } ] }
                     ProtoMessage { name = MessageName "MailAddress"
                                    items = [ MessageField { repeated = false
                                                             name = MessageFieldName "email"
                                                             fieldType = String 
                                                             number = MessageFieldNumber 1u
                                                             options = None }
                                              MessageField { repeated = false
                                                             name = MessageFieldName "display_name"
                                                             fieldType = String
                                                             number = MessageFieldNumber 2u
                                                             options = None } ] }
                     ProtoMessage { name = MessageName "MailHeader"
                                    items = [ MessageField { repeated = false
                                                             name = MessageFieldName "name"
                                                             fieldType = String 
                                                             number = MessageFieldNumber 1u
                                                             options = None }
                                              MessageField { repeated = false
                                                             name = MessageFieldName "value"
                                                             fieldType = String
                                                             number = MessageFieldNumber 2u
                                                             options = None } ] }
                     ProtoMessage { name = MessageName "File"
                                    items = [ MessageField { repeated = false
                                                             name = MessageFieldName "name"
                                                             fieldType = String 
                                                             number = MessageFieldNumber 1u
                                                             options = None }
                                              MessageField { repeated = false
                                                             name = MessageFieldName "url_path"
                                                             fieldType = String
                                                             number = MessageFieldNumber 2u
                                                             options = None } ] }
                     ]
    Assert.ParseEqual(expected, proto, example)