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

    [<Fact>]
    let ``store 0x07 0x74 0x65 0x73 0x74 0x69 0x6e 0x67 for "😂"`` () =
        use stream = new MemoryStream()
        serializeString "😂" stream
        
        let expected = [| 0x04uy; 0xf0uy; 0x9fuy; 0x98uy; 0x82uy |]
            
        Assert.Equal<byte array>(expected, stream.ToArray())


module ``serializeScalarValue should`` =
    [<Fact>]
    let ``store 0x08 0x96 0x01 for 1st Integer field with value 150`` () =
        use stream = new MemoryStream()
        let descriptor =
            { Proto3.MessageField.modifier = None
              Proto3.MessageField.name = MessageFieldName "foo"
              Proto3.MessageField.fieldType = MessageFieldType.Int32
              Proto3.MessageField.number = MessageFieldNumber 1u
              Proto3.MessageField.options = None }
        serializeScalarValue descriptor (Integer 150L) stream
        
        Assert.Equal<byte array>([| 0x08uy; 0x96uy; 0x01uy |], stream.ToArray())
        
        
let schema =
    let namedMessages =
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
                  [ "latitude",  { modifier = None
                                   name = MessageFieldName "latitude"
                                   fieldType = Double
                                   number = MessageFieldNumber 1u
                                   options = None }
                    "longitude", { modifier = None
                                   name = MessageFieldName "longitude"
                                   fieldType = Double
                                   number = MessageFieldNumber 2u
                                   options = None } ]
              "DistanceReply", Map
                  [ "result", { modifier = None
                                name = MessageFieldName "result"
                                fieldType = Double
                                number = MessageFieldNumber 1u
                                options = None } ] ]
            
    let numberedMessages =
        Map
            [ "DistanceRequest", Map
                [ 1u,       { modifier = None
                              name = MessageFieldName "from"
                              fieldType = MessageFieldType.Reference "Point"
                              number = MessageFieldNumber 1u
                              options = None }
                  2u,       { modifier = None
                              name = MessageFieldName "to"
                              fieldType = MessageFieldType.Reference "Point"
                              number = MessageFieldNumber 2u
                              options = None }
                  3u,       { modifier = Some Optional
                              name = MessageFieldName "method"
                              fieldType = Reference "CalculationMethod"
                              number = MessageFieldNumber 3u
                              options = None } ]
              "Point", Map
                  [  1u,          { modifier = None
                                    name = MessageFieldName "latitude"
                                    fieldType = Double
                                    number = MessageFieldNumber 1u
                                    options = None }
                     2u,          { modifier = None
                                    name = MessageFieldName "longitude"
                                    fieldType = Double
                                    number = MessageFieldNumber 2u
                                    options = None } ]
              "DistanceReply", Map
                  [  1u, { modifier = None
                           name = MessageFieldName "result"
                           fieldType = Double
                           number = MessageFieldNumber 1u
                           options = None } ] ]

    let enums =
        Map
            [ "CalculationMethod", Map
                [ "COSINE", 0
                  "HAVERSINE", 1 ] ]
    
    { namedMessages = namedMessages
      numberedMessages = numberedMessages 
      enums = enums }


let distanceRequest =
    let fromPoint =
        [ ScalarField { ScalarField.name = Textproto.Identifier "latitude"
                        value = ScalarFieldValue.ScalarValue (Float 55.75124) }
          ScalarField { ScalarField.name = Textproto.Identifier "longitude"
                        value = ScalarFieldValue.ScalarValue (Float 37.61842) } ]

    let toPoint =
        [ ScalarField { ScalarField.name = Textproto.Identifier "latitude"
                        value = ScalarFieldValue.ScalarValue (Float 59.93863) }
          ScalarField { ScalarField.name = Textproto.Identifier "longitude"
                        value = ScalarFieldValue.ScalarValue (Float 30.31413) } ]

    [ MessageField { MessageField.name = Textproto.Identifier "from"
                     value = MessageFieldValue.MessageValue (Message fromPoint) }
      MessageField { MessageField.name = Textproto.Identifier "to"
                     value = MessageFieldValue.MessageValue (Message toPoint) } ]


let binaryDistanceRequest =
    [| 0x0auy; 0x12uy; 0x09uy; 0x39uy; 0xb9uy
       0xdfuy; 0xa1uy; 0x28uy; 0xe0uy; 0x4buy
       0x40uy; 0x11uy; 0x9euy; 0x98uy; 0xf5uy
       0x62uy; 0x28uy; 0xcfuy; 0x42uy; 0x40uy
       0x12uy; 0x12uy; 0x09uy; 0xb2uy; 0x85uy
       0x20uy; 0x07uy; 0x25uy; 0xf8uy; 0x4duy
       0x40uy; 0x11uy; 0x46uy; 0xb1uy; 0xdcuy
       0xd2uy; 0x6auy; 0x50uy; 0x3euy; 0x40uy |]

        
module ``serializeMessage should`` =
    [<Fact>]
    [<Trait("Category", "Integration")>]
    let ``store reference message`` () =
        use stream = new MemoryStream()
        serializeMessage "DistanceRequest" distanceRequest schema stream
        
        Assert.Equal<byte array>(binaryDistanceRequest, stream.ToArray())


module ``deserializeVarint should`` =
    [<Fact>]
    let ``restore 80 for 0x50`` () =
        let bytes = [| 0x50uy |]
        use stream = new MemoryStream(bytes)
        Assert.Equal(80uL, deserializeVarint stream)
        Assert.Equal(bytes.LongLength, stream.Position)


    [<Fact>]
    let ``restore 150 for 0x96 0x01`` () =
        let bytes = [| 0x96uy; 0x01uy |]
        use stream = new MemoryStream(bytes)
        Assert.Equal(150uL, deserializeVarint stream)
        Assert.Equal(bytes.LongLength, stream.Position)


    [<Fact>]
    let ``restore -2 for 0xfe 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0x01`` () =
        let bytes =
            [| 0xfeuy; 0xffuy; 0xffuy; 0xffuy; 0xffuy
               0xffuy; 0xffuy; 0xffuy; 0xffuy; 0x01uy |]
        use stream = new MemoryStream(bytes)
        Assert.Equal(-2L, int64 (deserializeVarint stream))
        Assert.Equal(bytes.LongLength, stream.Position)


module ``deserializeTag should`` =
    [<Fact>]
    let ``restore field number 1 and wire type Varint for 0x08`` () =
        let bytes = [| 0x08uy |]
        use stream = new MemoryStream(bytes)
        
        Assert.Equal((1u, WireType.Varint), deserializeTag stream)
        Assert.Equal(bytes.LongLength, stream.Position)

    [<Fact>]
    let ``restore field number 3 and wire type I32 for 0x1d`` () =
        let bytes = [| 0x1duy |]
        use stream = new MemoryStream(bytes)
        
        Assert.Equal((3u, WireType.I32), deserializeTag stream)
        Assert.Equal(bytes.LongLength, stream.Position)


module ``deserializeSInt32 should`` =
    [<Fact>]
    let ``restore -1 for 0x01`` () =
        let bytes = [| 0x01uy |]
        use stream = new MemoryStream(bytes)
        
        Assert.Equal(-1, deserializeSInt32 stream)
        Assert.Equal(bytes.LongLength, stream.Position)

    [<Fact>]
    let ``restore 1 for 0x02`` () =
        let bytes = [| 0x02uy |]
        use stream = new MemoryStream(bytes)
        
        Assert.Equal(1, deserializeSInt32 stream)
        Assert.Equal(bytes.LongLength, stream.Position)


module ``deserializeDouble should`` =
    [<Fact>]
    let ``restore 3.14159265359 for 0x400921FB54442EEA`` () =
        // little endian bytes are reversed
        let bytes =
            [| 0xeauy; 0x2euy; 0x44uy; 0x54uy
               0xfbuy; 0x21uy; 0x09uy; 0x40uy |]
        use stream = new MemoryStream(bytes)

        Assert.Equal(3.14159265359, deserializeDouble stream)
        Assert.Equal(bytes.LongLength, stream.Position)


module ``deserializeString should`` =
    [<Fact>]
    let ``restore "testing" for 0x07 0x74 0x65 0x73 0x74 0x69 0x6e 0x67`` () =
        let bytes = [| 0x07uy; 't'B; 'e'B; 's'B; 't'B; 'i'B; 'n'B; 'g'B |]
        use stream = new MemoryStream(bytes)

        Assert.Equal("testing", deserializeString stream)
        Assert.Equal(bytes.LongLength, stream.Position)

    [<Fact>]
    let ``restore "😂" for 0x07 0x74 0x65 0x73 0x74 0x69 0x6e 0x67`` () =
        let bytes = [| 0x04uy; 0xf0uy; 0x9fuy; 0x98uy; 0x82uy |]
        use stream = new MemoryStream(bytes)

        Assert.Equal("😂", deserializeString stream)
        Assert.Equal(bytes.LongLength, stream.Position)


module ``deserializeValue should`` =
    [<Fact>]
    let ``restore "result" field with Float value 634.6292282187935 for 0x09 0x82 0xeb 0xcd 0xa8 0x08 0xd5 0x83 0x40`` () =
        let bytes = [| 0x09uy; 0x82uy; 0xebuy; 0xcduy; 0xa8uy; 0x08uy; 0xd5uy; 0x83uy; 0x40uy |]
        use stream = new MemoryStream(bytes)
        
        let expected =
            ScalarField { ScalarField.name = FieldName.Identifier "result"
                          value = ScalarValue (Float 634.6292282187935) }
        
        Assert.Equal(expected, deserializeValue "DistanceReply" schema stream)
        Assert.Equal(bytes.LongLength, stream.Position)


module ``deserializeMessage should`` =
    [<Fact>]
    [<Trait("Category", "Integration")>]
    let ``restore reference message`` () =
        use stream = new MemoryStream(binaryDistanceRequest)
        let actual = deserializeMessage "DistanceRequest" schema stream

        Assert.Equal<Field list>(distanceRequest, actual)
        Assert.Equal(binaryDistanceRequest.LongLength, stream.Position)


module ``printFields should`` =
    [<Fact>]
    [<Trait("Category", "Integration")>]
    let ``print reference fields`` () =
        use writer = new StringWriter()
        printFields 0 distanceRequest writer
        let actual = writer.ToString()
        let expected = "from: {
  latitude: 55,75124
  longitude: 37,61842
}
to: {
  latitude: 59,93863
  longitude: 30,31413
}
"

        Assert.Equal(expected, actual)
