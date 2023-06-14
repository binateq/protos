module Transformers

open Proto3

type Schema =
  { messages: Map<string, MessageField array>
    enums: Map<string, Map<string, int32>> }


let isFieldNumberValid (MessageFieldNumber n) =
    n >= 1u && n <= 536870911u && not (n >= 19000u && n <= 19999u)


let transformMessage (state: Schema) (message: Message) =
    let (MessageName name) = message.name
    let chooser = function
        | MessageField field ->
            Some field
        | _ -> None
        
    let items = message.items
        |> List.choose chooser
        |> List.sortBy (fun x -> x.number)
        |> List.toArray
        
    { state with Schema.messages = state.messages.Add(name, items) }


let transformEnum state (enum: Enum) =
    let (EnumName name) = enum.name
    let chooser = function
        | EnumField field ->
            let (EnumFieldName name) = field.name
            let (EnumValue value) = field.value
            Some (name, value)
        | _ -> None

    let items = enum.items |> Seq.choose chooser |> Map
    { state with Schema.enums = state.enums.Add(name, items) }
