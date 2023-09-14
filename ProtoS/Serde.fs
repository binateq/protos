module Serde

open System
open System.IO
open System.Text
open Proto3
open SchemaBuilder
open Textproto

// https://protobuf.dev/programming-guides/encoding/
type WireType =
    | Varint = 0u
    | I64 = 1u
    | Len = 2u
    | SGroup = 3u
    | EGroup = 4u
    | I32 = 5u


let serializeVarint n (stream: Stream) =
    let generator (isStop, remainder) =
        if isStop
        then None
        else
            let nextRemainder = remainder / 128uL
            let nextByte = byte (remainder % 128uL)
            if nextRemainder = 0uL
            then Some (nextByte, (true, nextRemainder))
            else Some (0x80uy ||| nextByte, (false, nextRemainder))
    let bytes = Array.unfold generator (false, n)
    stream.Write(bytes)
        
        
let serializeTag fieldNumber (wireType: WireType) stream =
    let tag = fieldNumber <<< 3 ||| (uint32 wireType)
    
    serializeVarint (uint64 tag) stream
    
    
let serializeBool value stream =
    if value
    then serializeVarint 1uL stream
    else serializeVarint 0uL stream
    

let serializeSInt32 (value: int32) stream =
    let unsignedValue =
        if value < 0
        then uint64 (2 * abs value - 1)
        else uint64 (2 * value)
    
    serializeVarint unsignedValue stream
    
    
let serializeSInt64 (value: int64) stream =
    let unsignedValue =
        if value < 0
        then uint64 (2L * abs value - 1L)
        else uint64 (2L * value)
    
    serializeVarint unsignedValue stream
    
    
let serializeDouble (value: float) (stream: Stream) =
    let mutable bytes = BitConverter.GetBytes(value)
    if not BitConverter.IsLittleEndian
    then Array.Reverse(bytes)
        
    stream.Write(bytes)


let serializeFloat (value: float32) (stream: Stream) =
    let mutable bytes = BitConverter.GetBytes(value)
    if not BitConverter.IsLittleEndian
    then Array.Reverse(bytes)
        
    stream.Write(bytes)
    
    
let serializeFixed32 (value: uint32) (stream: Stream) =
    let mutable bytes = BitConverter.GetBytes(value)
    if not BitConverter.IsLittleEndian
    then Array.Reverse(bytes)
        
    stream.Write(bytes)
    

let serializeFixed64 (value: uint64) (stream: Stream) =
    let mutable bytes = BitConverter.GetBytes(value)
    if not BitConverter.IsLittleEndian
    then Array.Reverse(bytes)
        
    stream.Write(bytes)
    

let serializeString (value: string) (stream: Stream) =
    serializeVarint (uint64 value.Length) stream
    stream.Write(Encoding.UTF8.GetBytes(value))
    
    
let serializeSubStream (value: MemoryStream) (stream: Stream) =
    let bytes = value.ToArray()
    serializeVarint (uint64 bytes.Length) stream
    stream.Write(bytes)


let serializeScalarValue (messageField: Proto3.MessageField) (value: ScalarValue) (stream: Stream) =
    let (MessageFieldNumber fieldNumber) = messageField.number
    match messageField.fieldType, value with
    | Double, Float value ->
        serializeTag fieldNumber WireType.I64 stream
        serializeDouble value stream
    | MessageFieldType.Float, Float value ->
        serializeTag fieldNumber WireType.I32 stream
        serializeFloat (float32 value) stream
    | Int32, SignedInteger value ->
        serializeTag fieldNumber WireType.Varint stream
        serializeVarint (uint64 value) stream
    | Int64, SignedInteger value ->
        serializeTag fieldNumber WireType.Varint stream
        serializeVarint (uint64 value) stream
    | Int32, UnsignedInteger value ->
        serializeTag fieldNumber WireType.Varint stream
        serializeVarint value stream
    | Int64, UnsignedInteger value ->
        serializeTag fieldNumber WireType.Varint stream
        serializeVarint value stream
    | UInt32, UnsignedInteger value ->
        serializeTag fieldNumber WireType.Varint stream
        serializeVarint value stream
    | UInt64, UnsignedInteger value ->
        serializeTag fieldNumber WireType.Varint stream
        serializeVarint value stream
    | SInt32, SignedInteger value ->
        serializeTag fieldNumber WireType.I32 stream
        serializeSInt32 (int32 value) stream
    | SInt64, SignedInteger value ->
        serializeTag fieldNumber WireType.I64 stream
        serializeSInt64 value stream
    | SInt32, UnsignedInteger value ->
        serializeTag fieldNumber WireType.I32 stream
        serializeSInt32 (int32 value) stream
    | SInt64, UnsignedInteger value ->
        serializeTag fieldNumber WireType.I64 stream
        serializeSInt64 (int64 value) stream
    | Fixed32, UnsignedInteger value ->
        serializeTag fieldNumber WireType.I32 stream
        serializeFixed32 (uint32 value) stream
    | Fixed64, UnsignedInteger value ->
        serializeTag fieldNumber WireType.I64 stream
        serializeFixed64 value stream
    | SFixed32, SignedInteger value ->
        serializeTag fieldNumber WireType.I32 stream
        serializeFixed32 (uint32 value) stream
    | SFixed64, SignedInteger value ->
        serializeTag fieldNumber WireType.I64 stream
        serializeFixed64 (uint64 value) stream
    | SFixed32, UnsignedInteger value ->
        serializeTag fieldNumber WireType.I32 stream
        serializeFixed32 (uint32 value) stream
    | SFixed64, UnsignedInteger value ->
        serializeTag fieldNumber WireType.I64 stream
        serializeFixed64 value stream
    | Bool, Identifier "true" ->
        serializeTag fieldNumber WireType.Varint stream
        serializeBool true stream
    | Bool, Identifier "false" ->
        serializeTag fieldNumber WireType.Varint stream
        serializeBool false stream
    | MessageFieldType.String, String value ->
        serializeTag fieldNumber WireType.Len stream
        serializeString value stream
    | Bytes, _ ->
        invalidArg "Bytes can't be serialized as scalar value" (nameof messageField.fieldType)
    | Reference _, _ ->
        invalidArg "Message can't be serialized as scalar value" (nameof messageField.fieldType)

        
let rec serializeMessage (messageName: string) (fields: Field list) (schema: Schema) (stream: Stream) =
    let descriptors = schema.messages[messageName] 
    match fields with
    | ScalarField field::tailFields ->
        let descriptor = descriptors[field.name.asString]
        match field.value with
        | ScalarValue value ->
            serializeScalarValue descriptor value stream
        | ScalarList list ->
            raise (NotImplementedException("Serialization of multiple values is not implemented"))
            
        serializeMessage messageName tailFields schema stream
    | MessageField field::tailFields ->
        let descriptor = descriptors[field.name.asString]
        let (MessageFieldNumber fieldNumber) = descriptor.number
        match field.value with
        | MessageValue (Message subFields) ->
            use subStream = new MemoryStream()
            let subMessageName =
                match descriptor.fieldType with
                | Reference value -> value
                | _ -> invalidOp "Message field should have Reference type"

            serializeMessage subMessageName subFields schema subStream
            serializeTag fieldNumber WireType.Len stream
            serializeSubStream subStream stream
        | MessageList _ ->
            raise (NotImplementedException("Serialization of multiple messages is not implemented"))

        serializeMessage messageName tailFields schema stream
    | [] -> ()
