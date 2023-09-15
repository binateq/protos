module Proto3

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
    | ComplexName of string


type Option =
  { name: OptionName
    value: Constant }


type EnumName = EnumName of string
type EnumFieldName = EnumFieldName of string
type EnumValue = EnumValue of int32


type EnumField =
  { name: EnumFieldName
    value: EnumValue
    options: Option list option }


type EnumItem =
    | EnumField of EnumField
    | EnumOption of Option
    | EnumEmptyItem


type Enum =
  { name: EnumName
    items: EnumItem list }


type MessageName = MessageName of string
type MessageFieldName = MessageFieldName of string


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


type MessageFieldNumber = MessageFieldNumber of uint32
type Modifier = Repeated | Optional


type MessageField =
  { modifier: Modifier option
    name: MessageFieldName
    fieldType: MessageFieldType
    number: MessageFieldNumber
    options: Option list option }

   
type MessageItem =
    | MessageField of MessageField
    | MessageEnum of Enum
    | MessageMessage of Message
    | MessageOption of Option
    | MessageEmptyItem
and Message =
  { name: MessageName
    items: MessageItem list }


type ProtoItem =
    | ProtoImport of Import
    | ProtoPackage of Package
    | ProtoOption of Option
    | ProtoMessage of Message
    | ProtoEnum of Enum
    // TODO: ProtoService
    | ProtoEmptyStatement
