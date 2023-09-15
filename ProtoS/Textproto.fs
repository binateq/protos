module Textproto

type FieldName =
    | Extension of string
    | Any of string * string
    | Identifier of string
    
    member this.asString =
        match this with
        | Extension name -> name
        | Any (domain, name) -> domain + "/" + name
        | Identifier name -> name


type ScalarValue =
    | String of string
    | Float of double
    | Identifier of string
    | SignedIdentifier of string
    | Integer of int64


type ScalarFieldValue =
    | ScalarValue of ScalarValue
    | ScalarList of ScalarValue list


type ScalarField =
  { name: FieldName
    value: ScalarFieldValue }


type MessageFieldValue =
    | MessageValue of Message
    | MessageList of Message list
and MessageField =
  { name: FieldName
    value: MessageFieldValue }
and Field =
    | ScalarField of ScalarField
    | MessageField of MessageField
and Message = Message of Field list
