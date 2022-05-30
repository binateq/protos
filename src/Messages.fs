module Messages

open Enumerations

type FieldName = FieldName of string
    
type FieldNumber = FieldNumber of uint
    
type FieldNumberInterval = {
    min: FieldNumber
    max: FieldNumber
}
    
type FieldReserveItem =
    | Value of FieldNumber
    | Interval of FieldNumberInterval
    
type FieldReserve =
    | FieldNumbers of FieldReserveItem seq
    | FieldNames of FieldName seq

//type FieldType =
//    | ScalarType of ScalarType
//    | Reference of string
//    | Enumeration of Enumeration
//    | Message of Message
//and Field = {
//    name: FieldName
//    typename: FieldType
//    number: FieldNumber
//}
//and FieldItem =
//    | Field of Field
//    | FieldReserve of FieldReserve
//and Message = {
//    name: string
//    items: FieldItem seq
//}

/// <summary>
/// Checks if field's number is valid.
/// </summary>
/// <remarks>
/// References to https://developers.google.com/protocol-buffers/docs/proto3#assigning_field_numbers
/// </remarks>
let isFieldNumberValid (FieldNumber n) =
    n >= 1u
 && n <= 536870911u
 && not (n >= 19000u && n <= 19999u)
