module Assert

open FParsec
open Xunit


let tryParse source parser =
    match run parser source with
    | Success (result, _, position) ->
        if position.Index = source.Length then
            (Some result, sprintf $"%A{result}")
        else
            (None, "The pattern is not parsed til the end")
    | Failure (message, _, _) ->
        (None, message)


let Parse<'r>(source: string, parser: Parser<'r, unit>) =
    let result, message = tryParse source parser
    
    Assert.True(result.IsSome, message)


let ParseEqual<'r>(source: string, parser: Parser<'r, unit>, expected: 'r) =
    let result, message = tryParse source parser
    match result with
    | Some actual -> Assert.Equal(expected, actual)
    | None -> Assert.True(false, message)


let NotParse<'r>(source: string, parser: Parser<'r, unit>) =
    let result, message = tryParse source parser
    
    Assert.True(result.IsNone, message)
