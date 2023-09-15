module SchemaBuilder

open Proto3

let buildNamedMessage (message: Message) =
    let (MessageName name) = message.name
    let chooser = function
        | MessageField field ->
            let (MessageFieldName name) = field.name
            Some (name, field)
        | _ -> None
    
    let fields =
        message.items
        |> List.choose chooser
        |> Map
        
    (name, fields)


let buildNumberedMessage (message: Message) =
    let (MessageName name) = message.name
    let chooser = function
        | MessageField field ->
            let (MessageFieldNumber number) = field.number
            Some (number, field)
        | _ -> None
    
    let fields =
        message.items
        |> List.choose chooser
        |> Map
        
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
  { namedMessages: Map<string, Map<string, MessageField>>
    numberedMessages: Map<string, Map<uint32, MessageField>>
    enums: Map<string, Map<string, int32>> }
      
      
let build (protoItems: ProtoItem list) =
    let messageChooser = function
        | ProtoMessage message -> Some message
        | _ -> None
        
    let enumChooser = function
        | ProtoEnum enum -> Some enum
        | _ -> None
    
    { namedMessages =
        protoItems
        |> Seq.choose messageChooser
        |> Seq.map buildNamedMessage
        |> Map
      numberedMessages =
        protoItems
        |> Seq.choose messageChooser
        |> Seq.map buildNumberedMessage
        |> Map
      enums =
        protoItems
        |> Seq.choose enumChooser
        |> Seq.map buildEnum
        |> Map }