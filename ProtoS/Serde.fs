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
    | [<Obsolete>] SGroup = 3u
    | [<Obsolete>] EGroup = 4u
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
    let bytes = Encoding.UTF8.GetBytes(value)
    serializeVarint (uint64 bytes.Length) stream
    stream.Write(bytes)
    
    
let serializeSubStream (value: MemoryStream) (stream: Stream) =
    let bytes = value.ToArray()
    serializeVarint (uint64 bytes.Length) stream
    stream.Write(bytes)
    
    
let wireType = function
    | Double | Fixed64 | SFixed64 -> WireType.I64
    | MessageFieldType.Float | Fixed32 | SFixed32 -> WireType.I32
    | Int32 | Int64 | UInt32 | UInt64 | SInt32 | SInt64 | Bool -> WireType.Varint
    | MessageFieldType.String | Bytes | Reference _ -> WireType.Len
    
    
let serializeScalarValue (messageField: Proto3.MessageField) (value: ScalarValue) (stream: Stream) =
    let (MessageFieldNumber fieldNumber) = messageField.number
    serializeTag fieldNumber (wireType messageField.fieldType) stream
    match messageField.fieldType, value with
    | Double, Float value ->
        serializeDouble value stream
    | MessageFieldType.Float, Float value ->
        serializeFloat (float32 value) stream
    | Int32, SignedInteger value ->
        serializeVarint (uint64 value) stream
    | Int64, SignedInteger value ->
        serializeVarint (uint64 value) stream
    | Int32, UnsignedInteger value ->
        serializeVarint value stream
    | Int64, UnsignedInteger value ->
        serializeVarint value stream
    | UInt32, UnsignedInteger value ->
        serializeVarint value stream
    | UInt64, UnsignedInteger value ->
        serializeVarint value stream
    | SInt32, SignedInteger value ->
        serializeSInt32 (int32 value) stream
    | SInt64, SignedInteger value ->
        serializeSInt64 value stream
    | SInt32, UnsignedInteger value ->
        serializeSInt32 (int32 value) stream
    | SInt64, UnsignedInteger value ->
        serializeSInt64 (int64 value) stream
    | Fixed32, UnsignedInteger value ->
        serializeFixed32 (uint32 value) stream
    | Fixed64, UnsignedInteger value ->
        serializeFixed64 value stream
    | SFixed32, SignedInteger value ->
        serializeFixed32 (uint32 value) stream
    | SFixed64, SignedInteger value ->
        serializeFixed64 (uint64 value) stream
    | SFixed32, UnsignedInteger value ->
        serializeFixed32 (uint32 value) stream
    | SFixed64, UnsignedInteger value ->
        serializeFixed64 value stream
    | Bool, Identifier "true" ->
        serializeBool true stream
    | Bool, Identifier "false" ->
        serializeBool false stream
    | MessageFieldType.String, String value ->
        serializeString value stream
    | Bytes, _ ->
        invalidOp "Bytes can't be serialized as scalar value"
    | Reference _, _ ->
        invalidOp "Message can't be serialized as scalar value"
    | _, _ ->
        invalidOp "Type of field is not compatible with type of data"

        
let rec serializeMessage (messageName: string) (fields: Field list) (schema: Schema) (stream: Stream) =
    let descriptors = schema.messages[messageName] 
    match fields with
    | ScalarField field::tailFields ->
        let descriptor = descriptors[field.name.asString]
        match field.value with
        | ScalarValue value ->
            serializeScalarValue descriptor value stream
        | ScalarList _ ->
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


let deserializeVarint (stream: Stream) =
    let rec calculateSum shift accumulator =
        let nextByte = stream.ReadByte()
        if nextByte = -1 then invalidOp "Unexpected end of file"
         
        if nextByte < 128
        then (uint64 nextByte <<< shift) + accumulator
        else calculateSum (shift + 7) (uint64 (nextByte - 128) <<< shift) + accumulator

    calculateSum 0 0uL
    
    
let deserializeTag (stream: Stream) =
    let tag = deserializeVarint stream
    let fieldNumber = uint32 (tag >>> 3)
    let wireType: WireType = LanguagePrimitives.EnumOfValue (uint32 tag &&& 0x7u)
    
    (fieldNumber, wireType)
    
    
let deserializeBool (stream: Stream) =
    deserializeVarint stream <> 0uL
    
    
let deserializeSInt32 stream =
    let decoded = deserializeVarint stream
    if decoded % 2uL = 0uL
    then (decoded / 2uL) |> int32
    else (decoded - 1uL / 2uL) |> int32 |> (~-)


let deserializeSInt64 stream =
    let decoded = deserializeVarint stream
    if decoded % 2uL = 0uL
    then (decoded / 2uL) |> int64
    else (decoded - 1uL / 2uL) |> int64 |> (~-)


let deserializeDouble (stream: Stream) =
    let mutable bytes = Array.create sizeof<'float> 0uy
    if stream.Read(bytes) < bytes.Length then invalidOp "Unexpected end of file"

    if not BitConverter.IsLittleEndian
    then Array.Reverse(bytes)
    BitConverter.ToDouble(bytes)
    

let deserializeFloat (stream: Stream) =
    let mutable bytes = Array.create sizeof<'float32> 0uy
    if stream.Read(bytes) < bytes.Length then invalidOp "Unexpected end of file"

    if not BitConverter.IsLittleEndian
    then Array.Reverse(bytes)
    BitConverter.ToSingle(bytes)
    
    
let deserializeFixed32 (stream: Stream) =
    let mutable bytes = Array.create sizeof<'uint32> 0uy
    if stream.Read(bytes) < bytes.Length then invalidOp "Unexpected end of file"

    if not BitConverter.IsLittleEndian
    then Array.Reverse(bytes)
    BitConverter.ToUInt32(bytes)
    
    
let deserializeFixed64 (stream: Stream) =
    let mutable bytes = Array.create sizeof<'uint64> 0uy
    if stream.Read(bytes) < bytes.Length then invalidOp "Unexpected end of file"

    if not BitConverter.IsLittleEndian
    then Array.Reverse(bytes)
    BitConverter.ToUInt64(bytes)
    
    
let deserializeString stream =
    let length = int32 (deserializeVarint stream)
    let bytes = Array.create length 0uy
    if stream.Read(bytes) < bytes.Length then invalidOp "Unexpected end of file"

    Encoding.UTF8.GetString(bytes)