module TextFormatParserTests

open Xunit
open TextFormatParser
open TextFormat

module Assert =
    open FParsec
    
    let Parse<'r>(source: string, parser: Parser<'r, unit>) =
        match run parser source with
        | Success (result, _, _) -> Assert.True(true, sprintf $"%A{result}")
        | Failure (message, _, _) -> Assert.True(false, message)
        
    let ParseEqual<'r>(source: string, parser: Parser<'r, unit>, expected: 'r) =
        match run parser source with
        | Success (result, _, _) -> Assert.Equal(expected, result)
        | Failure (message, _, _) -> Assert.True(false, message)

    let NotParse<'r>(source: string, parser: Parser<'r, unit>) =
        match run parser source with
        | Success (result, _, _) -> Assert.True(false, sprintf $"%A{result}")
        | Failure (message, _, _) -> Assert.True(true, message)

[<Fact>]
let ``#123 is comment`` () =
    Assert.Parse("#123", comment)
    
[<Fact>]
let ``# 123 is comment`` () =
    Assert.Parse("# 123", comment)

[<Fact>]
let ``# 123\n is comment`` () =
    Assert.Parse("# 123\n", comment)
    
[<Fact>]
let ``#\n is comment`` () =
    Assert.Parse("#\n", comment)

[<Fact>]
let ``\n is not comment`` () =
    Assert.NotParse("\n", comment)
    
[<Fact>]
let ``abc123 is identifier`` () =
    Assert.Parse("abc123", identifier)
    
[<Fact>]
let ``aBc123 is identifier`` () =
    Assert.Parse("aBc123", identifier)
    
[<Fact>]
let ``ABC is identifier`` () =
    Assert.Parse("ABC", identifier)

[<Fact>]
let ``123 is not identifier`` () =
    Assert.NotParse("123", identifier)
    
[<Fact>]
let ``123 is decimal literal"`` () =
    Assert.Parse("123", decLit)    

[<Fact>]
let ``0 is decimal literal`` () =
    Assert.Parse("0", decLit)    

[<Fact>]
let ``0123 is not decimal literal`` () =
    Assert.NotParse("0123", decLit)
    
[<Fact>]
let ``e+100 is exp`` () =
    Assert.ParseEqual("e+100", exp, "e+100")
        
[<Fact>]
let ``E-100 is exp`` () =
    Assert.ParseEqual("E-100", exp, "E-100")
    
    
[<Fact>]
let ``e100 is exp`` () =
    Assert.ParseEqual("e100", exp, "e100")
    
    
[<Fact>]
let ``e +100 is not exp`` () =
    Assert.NotParse("e +100", exp)

[<Fact>]
let ``.123 is floatLit`` () =
    Assert.Parse(".123", floatLit)

[<Fact>]
let ``.123e10 is floatLit`` () =
    Assert.Parse(".123e10", floatLit)

[<Fact>]
let ``.123e is not floatLit`` () =
    Assert.NotParse(".123e", floatLit)

[<Fact>]
let ``123. is floatLit`` () =
    Assert.Parse("123.", floatLit)

[<Fact>]
let ``123.456 is floatLit`` () =
    Assert.Parse("123.456", floatLit)

[<Fact>]
let ``123.456e78 is floatLit`` () =
    Assert.Parse("123.456r78", floatLit)
    
[<Fact>]
let ``123e78 is floatLit`` () =
    Assert.Parse("123e78", floatLit)