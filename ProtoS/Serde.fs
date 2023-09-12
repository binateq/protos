module Serde

open System.IO
open Textproto
open SchemaBuilder

// let rec serialize (schema: Schema) (messageName: string) (fields: Field list) (stream: Stream) =
//     let fieldDescriptors = schema.messages[messageName]
//     for field in fields do
//         match field with
//         | ScalarField scalarField ->
//             