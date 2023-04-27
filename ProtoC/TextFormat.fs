module TextFormat

type FieldName =
    | Extension of string
    | Any of string * string
    | Identifier of string

type ScalarValue =
    | String of string
    | Float of float
    | Identifier of string
    | SignedIdentifier of string
    | DecSignedInteger of int64
    | OctSignedInteger of int64
    | HexSignedInteger of int64
    | DecUnsignedInteger of uint64
    | OctUnsignedInteger of uint64
    | HexUnsignedInteger of uint64

type ScalarFieldValue =
    | ScalarValue of ScalarValue
    | ScalarList of ScalarValue list
    
type ScalarField =
    { name: FieldName
      value: ScalarFieldValue }
and Field =
    | ScalarField of ScalarField
    | MessageField of MessageField
and Message = Field list
and MessageValue = Message
and MessageFieldValue =
    | Value of MessageValue
    | List of MessageValue list
and MessageField =
    { name: FieldName
      value: MessageFieldValue }