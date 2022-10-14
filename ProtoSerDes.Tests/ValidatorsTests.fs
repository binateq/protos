module ValidatorsTests

open Xunit
open Proto3
open Validators

[<Fact>]
let ``FieldNumber 0 is invalid`` () =
    Assert.False(isFieldNumberValid (MessageFieldNumber 0u))
    
[<Fact>]
let ``FieldNumber 1 is valid`` () =
    Assert.True(isFieldNumberValid (MessageFieldNumber 1u))
    
[<Fact>]
let ``FieldNumber 18999 is valid`` () =
    Assert.True(isFieldNumberValid (MessageFieldNumber 18999u))    

[<Fact>]
let ``FieldNumber 19000 is invalid`` () =
    Assert.False(isFieldNumberValid (MessageFieldNumber 19000u))
    
[<Fact>]
let ``FieldNumber 19999 is invalid`` () =
    Assert.False(isFieldNumberValid (MessageFieldNumber 19999u))
    
[<Fact>]
let ``FieldNumber 20000 is valid`` () =
    Assert.True(isFieldNumberValid (MessageFieldNumber 20000u))
    
[<Fact>]
let ``FieldNumber 536870911u is valid`` () =
    Assert.True(isFieldNumberValid (MessageFieldNumber 536870911u))
    
[<Fact>]
let ``FieldNumber 536870912u is invalid`` () =
    Assert.False(isFieldNumberValid (MessageFieldNumber 536870912u))
