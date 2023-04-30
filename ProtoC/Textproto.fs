module Textproto

type FieldName =
    | Extension of string
    | Any of string * string
    | Identifier of string


type ScalarValue =
    | String of string
    | Float of string
    | Identifier of string
    | SignedIdentifier of string
    | DecSignedInteger of string
    | OctSignedInteger of string
    | HexSignedInteger of string
    | DecUnsignedInteger of string
    | OctUnsignedInteger of string
    | HexUnsignedInteger of string


type ScalarFieldValue =
    | ScalarValue of ScalarValue
    | ScalarList of ScalarValue list


type ScalarField = {
    name: FieldName
    value: ScalarFieldValue
}


type MessageFieldValue =
    | MessageValue of Message
    | MessageList of Message list
and MessageField = {
    name: FieldName
    value: MessageFieldValue
}
and Field =
    | ScalarField of ScalarField
    | MessageField of MessageField
and Message = Message of Field list
