module SerdeTests

open System.IO
open Xunit
open Serde

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
            { Proto3.MessageField.repeated = false
              Proto3.MessageField.name = Proto3.MessageFieldName "foo"
              Proto3.MessageField.fieldType = Proto3.MessageFieldType.Int32
              Proto3.MessageField.number = Proto3.MessageFieldNumber 1u
              Proto3.MessageField.options = None }
        serializeScalarValue descriptor (Textproto.UnsignedInteger 150uL) stream
        
        Assert.Equal<byte array>([| 0x08uy; 0x96uy; 0x01uy |], stream.ToArray())
