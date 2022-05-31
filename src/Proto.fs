module Proto

type Package = Package of string

type Import =
    | Import of string
    | WeakImport of string
    | PublicImport of string
    
type Constant =
    | Reference of string
    | Integer of int
    | Float of float
    | String of string
    | Bool of bool
    
type OptionName =
    | SimpleName of string
    | ComplexName of string * string

type Option = {
    name: OptionName
    value: Constant
}

type EnumFieldName = EnumFieldName of string
type EnumValue = EnumValue of int32

type EnumField = {
    name: EnumFieldName
    value: EnumValue
    options: Option list option
}

type EnumItem =
    | EnumEmptyItem
    | EnumField of EnumField
    | EnumOption of Option

type EnumName = EnumName of string

type EnumDefinition = {
    name: EnumName
    items: EnumItem list
}

type MessageFieldType =
    | Double
    | Float
    | Int32
    | Int64
    | UInt32
    | UInt64
    | SInt32
    | SInt64
    | Fixed32
    | Fixed64
    | SFixed32
    | SFixed64
    | Bool
    | String
    | Bytes
    | Reference of string
    
type MessageFieldName = MessageFieldName of string

type MessageFieldNumber = MessageFieldNumber of uint32
    
type MessageField = {
    repeated: bool
    name: MessageFieldName
    fieldType: MessageFieldType
    number: MessageFieldNumber
    options: Option list option
}

type MessageName = MessageName of string

type MessageItem =
    | MessageEmptyItem
    | MessageField of MessageField
    | MessageEnum of EnumDefinition
    | MessageMessage of MessageDefinition
    | MessageOption of Option
and MessageDefinition = {
    name: MessageName
    items: MessageItem list
}