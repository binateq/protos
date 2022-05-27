open FParsec

let test p str =
    match run p str with
    | Success(result, _, _) -> printfn $"Success: %A{result}"
    | Failure(message, _, _) -> printfn $"Failure: %s{message}"
    
test pfloat "1.25"
test pfloat "1.25E 3"

let str s = pstring s
let floatBetweenBrackets = str "[" >>. pfloat .>> str "]"

test floatBetweenBrackets "[1.0]"
test floatBetweenBrackets "[]"
test floatBetweenBrackets "[1.0"

test (many floatBetweenBrackets) ""
test (many floatBetweenBrackets) "[1.0]"
test (many floatBetweenBrackets) "[2][3][4]"

test (many1 floatBetweenBrackets) "(1)"
test (many1 (floatBetweenBrackets <?> "float between brackets")) "(1)"

let floatList = str "[" >>. sepBy pfloat (str ",") .>> str "]"

test floatList "[]"
test floatList "[1.0]"
test floatList "[4,5,6]"

test floatList "[1.0,]"
test floatList "[1.0,2.0"

let str_ws s = pstring s .>> spaces
let float_ws = pfloat .>> spaces
let numberList = str_ws "[" >>. sepBy float_ws (str_ws ",") .>> str_ws "]"

test numberList @"[1,
   2, 3]"

test numberList @"[1,
   2; 3]"

test (many (str "a" <|> str "b")) "abba"

test (skipStringCI "<float>" >>. pfloat) "<FLOAT>1.0"

let identifier =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> spaces
    
test identifier "_"
test identifier "_test1="
test identifier "1"

let stringLiteral =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let unescape = function
                   | 'n' -> '\n'
                   | 'r' -> '\r'
                   | 't' -> '\t'
                   | c -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    between (pstring "\"") (pstring "\"") (manyChars (normalChar <|> escapedChar))
    
test stringLiteral "\"abc\""
test stringLiteral "\"abc\\\"def\\\"ghi\""
test stringLiteral "\"abc\\def\""

let product = pipe2 float_ws (str_ws "*" >>. float_ws) (*)

test product "3 * 5"

type StringConstant = StringConstant of string * string
let stringConstant = pipe3 identifier (str_ws "=") stringLiteral
                           (fun id _ str -> StringConstant(id, str))
                           
test stringConstant "myString = \"string value\""

let boolean = (stringReturn "true" true) <|> (stringReturn "false" false)

test boolean "true"
test boolean "false"
test boolean "tru"
