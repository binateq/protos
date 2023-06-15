module TransformersTests

open Xunit
open Proto3
open Transformers

module ``isFieldNumberValid should`` =
    [<Fact>]
    let ``consider 0 invalid`` () =
        Assert.False(isFieldNumberValid (MessageFieldNumber 0u))

        
    [<Fact>]
    let ``consider 1 valid`` () =
        Assert.True(isFieldNumberValid (MessageFieldNumber 1u))
        

    [<Fact>]
    let ``consider 18999 valid`` () =
        Assert.True(isFieldNumberValid (MessageFieldNumber 18999u))    


    [<Fact>]
    let ``consider 19000 invalid`` () =
        Assert.False(isFieldNumberValid (MessageFieldNumber 19000u))
        

    [<Fact>]
    let ``consider 19999 invalid`` () =
        Assert.False(isFieldNumberValid (MessageFieldNumber 19999u))
        

    [<Fact>]
    let ``consider 20000 valid`` () =
        Assert.True(isFieldNumberValid (MessageFieldNumber 20000u))
        

    [<Fact>]
    let ``consider 536870911u valid`` () =
        Assert.True(isFieldNumberValid (MessageFieldNumber 536870911u))
        

    [<Fact>]
    let ``consider 536870912u invalid`` () =
        Assert.False(isFieldNumberValid (MessageFieldNumber 536870912u))

module ``transformEnum should`` =
    [<Fact>]
    let ``change Schema correctly`` () =
        let enum =
          { Enum.name = EnumName "foo"
            items =
              [ EnumField
                  { name = EnumFieldName "bar"
                    value = EnumValue 111
                    options = None };
                EnumField
                  { name = EnumFieldName "baz"
                    value = EnumValue 222
                    options = None };
                EnumEmptyItem;
                EnumOption
                  { name = SimpleName "qux"
                    value = Integer 333 };
                EnumField
                  { name = EnumFieldName "quux"
                    value = EnumValue 444
                    options = None } ] }
        let expected =
            seq
              { "foo", seq
                          { "bar", 111
                            "baz", 222
                            "quux", 444 }
                          |> Map }
              |> Map
        let actual = (transformEnum Schema.start enum).enums      
        
        Assert.Equal<Map<string, Map<string, int>>>(expected, actual)
