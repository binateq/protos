module MessagesTests

open Xunit
open Messages

[<Fact>]
let ``FieldNumber 0 is invalid`` () =
    Assert.False(isFieldNumberValid (FieldNumber 0u))
    
[<Fact>]
let ``FieldNumber 1 is valid`` () =
    Assert.True(isFieldNumberValid (FieldNumber 1u))
    
[<Fact>]
let ``FieldNumber 18999 is valid`` () =
    Assert.True(isFieldNumberValid (FieldNumber 18999u))    

[<Fact>]
let ``FieldNumber 19000 is invalid`` () =
    Assert.False(isFieldNumberValid (FieldNumber 19000u))
    
[<Fact>]
let ``FieldNumber 19999 is invalid`` () =
    Assert.False(isFieldNumberValid (FieldNumber 19999u))
    
[<Fact>]
let ``FieldNumber 20000 is valid`` () =
    Assert.True(isFieldNumberValid (FieldNumber 20000u))
    
[<Fact>]
let ``FieldNumber 536870911u is valid`` () =
    Assert.True(isFieldNumberValid (FieldNumber 536870911u))
    
[<Fact>]
let ``FieldNumber 536870912u is invalid`` () =
    Assert.False(isFieldNumberValid (FieldNumber 536870912u))
