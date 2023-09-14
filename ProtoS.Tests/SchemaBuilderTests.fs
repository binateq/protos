module SchemaBuilderTests

open Xunit
open Proto3
open SchemaBuilder

module ``buildMessage should`` =
    [<Fact>]
    let ``skip all items except MessageField`` () =
        let message =
          { Message.name = MessageName "foo"
            items =
              [ MessageField
                  { modifier = None
                    fieldType = Int32 
                    name = MessageFieldName "bar"
                    number = MessageFieldNumber 4u
                    options = None }
                MessageEmptyItem
                MessageOption
                  { name = SimpleName "baz"
                    value = Integer 0 }
                MessageField
                  { modifier = None
                    fieldType = Float
                    name = MessageFieldName "qux"
                    number = MessageFieldNumber 2u
                    options = None }
                MessageField
                  { modifier = None
                    fieldType = String 
                    name = MessageFieldName "quxx"
                    number = MessageFieldNumber 1u
                    options = None } ] }
          
        let expected =
          ( "foo",
            Map
              [ "quxx",
                { modifier = None
                  fieldType = String 
                  name = MessageFieldName "quxx"
                  number = MessageFieldNumber 1u
                  options = None }
                "qux",
                { modifier = None
                  fieldType = Float 
                  name = MessageFieldName "qux"
                  number = MessageFieldNumber 2u
                  options = None }
                "bar",
                { modifier = None
                  fieldType = Int32 
                  name = MessageFieldName "bar"
                  number = MessageFieldNumber 4u
                  options = None } ] )
              
        let actual = buildMessage message      
        
        Assert.Equal(expected, actual)


module ``buildEnum should`` =
    [<Fact>]
    let ``extract name and value from every EnumField and ignore other items`` () =
        let enum =
          { Enum.name = EnumName "foo"
            items =
              [ EnumField
                  { name = EnumFieldName "bar"
                    value = EnumValue 111
                    options = None }
                EnumField
                  { name = EnumFieldName "baz"
                    value = EnumValue 222
                    options = None }
                EnumEmptyItem
                EnumOption
                  { name = SimpleName "qux"
                    value = Integer 333 }
                EnumField
                  { name = EnumFieldName "quux"
                    value = EnumValue 444
                    options = None } ] }
          
        let expected =
          ( "foo",
            Map
              [ "bar", 111
                "baz", 222
                "quux", 444 ] )
        
        let actual = buildEnum enum      
        
        Assert.Equal(expected, actual)
