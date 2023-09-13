module Serde

open System
open System.IO
open System.Text

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
    
    
let serializeDouble (value: double) (stream: Stream) =
    let mutable bytes = BitConverter.GetBytes(value)
    if not BitConverter.IsLittleEndian
        then Array.Reverse(bytes)
        
    stream.Write(bytes)


let serializeString (value: string) (stream: Stream) =
    serializeVarint (uint64 value.Length) stream
    stream.Write(Encoding.UTF8.GetBytes(value))
    
// let serialize (fields: Field list) (descriptors: MessageField list) =
//     match fields, descriptors with
//     | field::remainingFields, descriptor::remainingDescriptors 