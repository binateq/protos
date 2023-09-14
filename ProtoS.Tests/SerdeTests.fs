module SerdeTests

open System.IO
open Microsoft.FSharp.Collections
open Xunit
open Serde
open Proto3
open Textproto
open SchemaBuilder

module ``serializeVarint should`` =
    [<Fact>]
    let ``store 0x50 for 80`` () =
        use stream = new MemoryStream()
        serializeVarint 80uL stream
        
        Assert.Equal<byte array>([|0x50uy|], stream.ToArray())

    [<Fact>]
    let ``store 0x96 0x01 for 150`` () =
        use stream = new MemoryStream()
        serializeVarint 150uL stream
        
        Assert.Equal<byte array>([|0x96uy; 0x01uy|], stream.ToArray())
        
        
    [<Fact>]
    let ``store 0xfe 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0x01 for -2`` () =
        use stream = new MemoryStream()
        serializeVarint (uint64 -2) stream
        let expected =
            [| 0xfeuy; 0xffuy; 0xffuy; 0xffuy; 0xffuy
               0xffuy; 0xffuy; 0xffuy; 0xffuy; 0x01uy |]
        
        Assert.Equal<byte array>(expected, stream.ToArray())

        
module ``serializeTag should`` =
    [<Fact>]
    let ``store 0x08 for field number 1 and wire type Varint`` () =
        use stream = new MemoryStream()
        serializeTag 1u WireType.Varint stream
        
        Assert.Equal<byte array>([|0x08uy|], stream.ToArray())

    [<Fact>]
    let ``store 0x1d for field number 3 and wire type I32`` () =
        use stream = new MemoryStream()
        serializeTag 3u WireType.I32 stream
        
        Assert.Equal<byte array>([|0x1duy|], stream.ToArray())


module ``serializeSInt32 should`` =
    [<Fact>]
    let ``store 0x01 for -1`` () =
        use stream = new MemoryStream()
        serializeSInt32 -1 stream
        
        Assert.Equal<byte array>([|0x01uy|], stream.ToArray())

    [<Fact>]
    let ``store 0x02 for 1`` () =
        use stream = new MemoryStream()
        serializeSInt32 1 stream
        
        Assert.Equal<byte array>([|0x02uy|], stream.ToArray())


module ``serializeDouble should`` =
    [<Fact>]
    let ``store 0x400921FB54442EEA for 3.14159265359`` () =
        use stream = new MemoryStream()
        serializeDouble 3.14159265359 stream
        
        // little endian bytes are reversed
        let expected =
            [| 0xeauy; 0x2euy; 0x44uy; 0x54uy
               0xfbuy; 0x21uy; 0x09uy; 0x40uy |]
        
        Assert.Equal<byte array>(expected, stream.ToArray())


module ``serializeString should`` =
    [<Fact>]
    let ``store 0x07 0x74 0x65 0x73 0x74 0x69 0x6e 0x67 for "testing"`` () =
        use stream = new MemoryStream()
        serializeString "testing" stream
        
        let expected = [| 0x07uy; 't'B; 'e'B; 's'B; 't'B; 'i'B; 'n'B; 'g'B |]
            
        Assert.Equal<byte array>(expected, stream.ToArray())


module ``serializeScalarValue should`` =
    [<Fact>]
    let ``store 0x08 0x96 0x01 for 1st Varint field with value 150`` () =
        use stream = new MemoryStream()
        let descriptor =
            { Proto3.MessageField.modifier = None
              Proto3.MessageField.name = Proto3.MessageFieldName "foo"
              Proto3.MessageField.fieldType = Proto3.MessageFieldType.Int32
              Proto3.MessageField.number = Proto3.MessageFieldNumber 1u
              Proto3.MessageField.options = None }
        serializeScalarValue descriptor (Textproto.UnsignedInteger 150uL) stream
        
        Assert.Equal<byte array>([| 0x08uy; 0x96uy; 0x01uy |], stream.ToArray())

        
module ``serializeMessage should`` =
    [<Fact>]
    [<Trait("Category", "Integration")>]
    let ``store sample message correctly`` () =
        let messages =
            Map
                [ "DistanceRequest", Map
                    [ "from",   { modifier = None
                                  name = MessageFieldName "from"
                                  fieldType = MessageFieldType.Reference "Point"
                                  number = MessageFieldNumber 1u
                                  options = None }
                      "to",     { modifier = None
                                  name = MessageFieldName "to"
                                  fieldType = MessageFieldType.Reference "Point"
                                  number = MessageFieldNumber 2u
                                  options = None }
                      "method", { modifier = Some Optional
                                  name = MessageFieldName "method"
                                  fieldType = Reference "CalculationMethod"
                                  number = MessageFieldNumber 3u
                                  options = None } ]
                  "Point", Map
                      [  "latitude",  { modifier = None
                                        name = MessageFieldName "latitude"
                                        fieldType = Double
                                        number = MessageFieldNumber 1u
                                        options = None }
                         "longitude", { modifier = None
                                        name = MessageFieldName "longitude"
                                        fieldType = Double
                                        number = MessageFieldNumber 2u
                                        options = None } ] ]
                
        let enums =
            Map
                [ "CalculationMethod", Map
                    [ "COSINE", 0
                      "HAVERSINE", 1 ] ]
        let schema =
            { messages = messages
              enums = enums }
            
        let fromCoordinates =
            [ ScalarField { ScalarField.name = Textproto.Identifier "latitude"
                            value = ScalarFieldValue.ScalarValue (Float 55.75124) }
              ScalarField { ScalarField.name = Textproto.Identifier "longitude"
                            value = ScalarFieldValue.ScalarValue (Float 37.61842) } ]

        let toCoordinates =
            [ ScalarField { ScalarField.name = Textproto.Identifier "latitude"
                            value = ScalarFieldValue.ScalarValue (Float 59.93863) }
              ScalarField { ScalarField.name = Textproto.Identifier "longitude"
                            value = ScalarFieldValue.ScalarValue (Float 30.31413) } ]
            
        let fields =
            [ MessageField { MessageField.name = Textproto.Identifier "from"
                             value = MessageFieldValue.MessageValue (Message fromCoordinates) }
              MessageField { MessageField.name = Textproto.Identifier "to"
                             value = MessageFieldValue.MessageValue (Message toCoordinates) } ]
            
        use stream = new MemoryStream()
        serializeMessage "DistanceRequest" fields schema stream
        
        let expected = [| 0x0auy; 0x12uy; 0x09uy; 0x39uy; 0xb9uy
                          0xdfuy; 0xa1uy; 0x28uy; 0xe0uy; 0x4buy
                          0x40uy; 0x11uy; 0x9euy; 0x98uy; 0xf5uy
                          0x62uy; 0x28uy; 0xcfuy; 0x42uy; 0x40uy
                          0x12uy; 0x12uy; 0x09uy; 0xb2uy; 0x85uy
                          0x20uy; 0x07uy; 0x25uy; 0xf8uy; 0x4duy
                          0x40uy; 0x11uy; 0x46uy; 0xb1uy; 0xdcuy
                          0xd2uy; 0x6auy; 0x50uy; 0x3euy; 0x40uy |]
        
        Assert.Equal<byte array>(expected, stream.ToArray())