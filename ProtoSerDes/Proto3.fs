module Proto3

type ProtoItem =
    | ProtoImport of Import
    | ProtoPackage of Package
    | ProtoOption of Option
    | ProtoMessage of Message
    | ProtoEnum of Enum
    // TODO: ProtoService
    | ProtoEmptyStatement
    
and Package = Package of string

and Import =
    | Import of string
    | WeakImport of string
    | PublicImport of string
    
and Option = {
    name: OptionName
    value: Constant
}

and OptionName =
    | SimpleName of string
    | ComplexName of string * string

and Constant =
    | Reference of string
    | Integer of int
    | Float of float
    | String of string
    | Bool of bool

and Message = {
    name: MessageName
    items: MessageItem list
}

and MessageName = MessageName of string

and MessageItem =
    | MessageField of MessageField
    | MessageEnum of Enum
    | MessageMessage of Message
    | MessageOption of Option
    | MessageEmptyItem

and MessageField = {
    repeated: bool
    name: MessageFieldName
    fieldType: MessageFieldType
    number: MessageFieldNumber
    options: Option list option
}

and MessageFieldName = MessageFieldName of string
    
and MessageFieldType =
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

and MessageFieldNumber = MessageFieldNumber of uint32

and Enum = {
    name: EnumName
    items: EnumItem list
}

and EnumField = {
    name: EnumFieldName
    value: EnumValue
    options: Option list option
}

and EnumFieldName = EnumFieldName of string

and EnumValue = EnumValue of int32

and EnumItem =
    | EnumField of EnumField
    | EnumOption of Option
    | EnumEmptyItem

and EnumName = EnumName of string
