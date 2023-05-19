module ASG

open Proto3

type Schema =
  { messages: Map<MessageName, MessageField list>
    enums: Map<string, Map<string, int32>> }


let isFieldNumberValid (MessageFieldNumber n) =
    n >= 1u && n <= 536870911u && not (n >= 19000u && n <= 19999u)


type State =
  { nameSpace: string
    messages: Map<MessageName, MessageField list>
    enums: Map<string, Map<string, int32>> }
  
    member self.fullName name =
        if self.nameSpace = ""
        then name
        else self.nameSpace + "." + name
  
    static member start =
      { nameSpace = ""
        messages = Map.empty
        enums = Map.empty }


let transformEnum state (enum: Enum) =
    let (EnumName name) = enum.name
    let chooser = function
        | EnumField field ->
            let (EnumFieldName name) = field.name
            let (EnumValue value) = field.value
            Some (name, value)
        | _ -> None

    let items = enum.items |> Seq.choose chooser |> Map
    { state with State.enums = state.enums.Add(name, items) }


// let rec transformProtoItem state = function
//     | ProtoPackage (Package name) -> { state with nameSpace = name }
//     | ProtoMessage message -> transformMessage state message
//     | ProtoEnum enum -> state
//     | _ -> state
// and transformMessage state message =
//     let (MessageName name) = message.name
//     let fullName = state.fullName name
//     List.fold transformMessageItem { state with fields = Some [] } message.items
// and transformMessageItem state = function
//     | MessageField field -> { state with fields = field::fields } 
//     | MessageEnum of Enum
//     | MessageMessage message -> transformMessage state message
//     | MessageOption of Option
//     | MessageEmptyItem