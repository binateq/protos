module Proto

open Messages

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
    
type OptionName = OptionName of string

type Option = {
    name: OptionName
    value: Constant
}
