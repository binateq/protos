# DotNext workshop

## 1. Make solution and first project

* Console Application
* Solution name: **DotNextDemo**
* Project name: **ProtoS**
* Language: **F#**

## 2. Make test project

* Unit Test Project
* Project name: **ProtoS.Tests**
* Type: **xUnit**
* Language: **F#**

Delete **Tests.fs**.

## 3. Add FParsec dependency

* Nuget Package: **FParsec**
* Version: **1.1.1**

To both **ProtoS** and **ProtoS.Tests**.

## 4. Assert.fs

Add to **ProtoS.Tests** as the *first* file. Copy-paste the text:

```fsharp
module Assert

open FParsec
open Xunit

let private tryParse source parser =
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
```