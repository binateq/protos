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

type ScalarList = ScalarValue list
