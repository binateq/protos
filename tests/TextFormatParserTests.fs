module TextFormatParserTests

open Xunit
open TextFormatParser
open TextFormat

module Assert =
    open FParsec
    
    let Parse<'r>(parser: Parser<'r, unit>, source: string) =
        match run parser source with
        | Success (result, _, _) -> Assert.True(true, sprintf $"%A{result}")
        | Failure (message, _, _) -> Assert.True(false, message)
        
    let ParseEqual<'r>(expected: 'r, parser: Parser<'r, unit>, source: string) =
        match run parser source with
        | Success (result, _, _) -> Assert.Equal(expected, result)
        | Failure (message, _, _) -> Assert.True(false, message)

    let NotParse<'r>(parser: Parser<'r, unit>, source: string) =
        match run parser source with
        | Success (result, _, _) -> Assert.True(false, sprintf $"%A{result}")
        | Failure (message, _, _) -> Assert.True(true, message)

[<Fact>]
let ``#123 is comment`` () =
    Assert.Parse(comment, "#123")
    
[<Fact>]
let ``# 123 is comment`` () =
    Assert.Parse(comment, "# 123")

[<Fact>]
let ``# 123\n is comment`` () =
    Assert.Parse(comment, "# 123\n")
    
[<Fact>]
let ``#\n is comment`` () =
    Assert.Parse(comment, "#\n")

[<Fact>]
let ``\n is not comment`` () =
    Assert.NotParse(comment, "\n")
    
[<Fact>]
let ``abc123 is identifier`` () =
    Assert.Parse(identifier, "abc123")
    
[<Fact>]
let ``aBc123 is identifier`` () =
    Assert.Parse(identifier, "aBc123")
    
[<Fact>]
let ``ABC is identifier`` () =
    Assert.Parse(identifier, "ABC")

[<Fact>]
let ``123 is not identifier`` () =
    Assert.NotParse(identifier, "123")
    
[<Fact>]
let ``123 is decimal literal"`` () =
    Assert.Parse(decLit, "123")    

[<Fact>]
let ``0 is decimal literal`` () =
    Assert.Parse(decLit, "0")    

[<Fact>]
let ``0123 is not decimal literal`` () =
    Assert.NotParse(decLit, "0123")