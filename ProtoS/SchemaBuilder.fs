module SchemaBuilder

open Proto3

let buildMessage (message: Message) =
    let (MessageName name) = message.name
    let chooser = function
        | MessageField field ->
            Some field
        | _ -> None
    
    let fields =
        message.items
        |> List.choose chooser
        |> List.sortBy (fun x -> x.number)
        |> List.toArray
        
    (name, fields)


let buildEnum (enum: Enum) =
    let (EnumName name) = enum.name
    let chooser = function
        | EnumField field ->
            let (EnumFieldName name) = field.name
            let (EnumValue value) = field.value
            Some (name, value)
        | _ -> None

    let enums =
        enum.items
        |> List.choose chooser
        |> Map
    
    (name, enums)


type Schema =
  { messages: Map<string, MessageField array>
    enums: Map<string, Map<string, int32>> }
      
      
let build (protoItems: ProtoItem list) =
    let messageChooser = function
        | ProtoMessage message -> Some message
        | _ -> None
        
    let enumChooser = function
        | ProtoEnum enum -> Some enum
        | _ -> None
    
    { messages =
        protoItems
        |> Seq.choose messageChooser
        |> Seq.map buildMessage
        |> Map
      enums =
        protoItems
        |> Seq.choose enumChooser
        |> Seq.map buildEnum
        |> Map }