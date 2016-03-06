#I "../packages/FParsec/lib/net40-client"
#r "FParsec.dll"
#r "FParsecCS.dll"

open FParsec

// value restriction constrain the type of parser value
type UserState = unit
type Parser<'t> = Parser<'t, UserState>

/// test : Parser<'a, unit> -> string -> unit
let test (p : Parser<_>) str =
    match run p str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test pfloat "1.25" // Success: 1.25

test pfloat "1.25E 3" // Failure: Error in Ln: 1 Col: 6 Expecting: decimal digit

let str s = pstring s
let floatBetweenBrackets = str "[" >>. pfloat .>> str "]"

test floatBetweenBrackets "[1.0]" // Success: 1.0

test floatBetweenBrackets "[]" // Failure: Error in Ln: 1 Col: 2 Expecting: floating-point number

test floatBetweenBrackets "[1.0" // Failure: Error in Ln: 1 Col: 5 Note: The error occurred at the end of the input stream Expecting: ']'

let betweenStrings s1 s2 p = str s1 >>. p .>> str s2
let floatBetweenBrackets2 = pfloat |> betweenStrings "[" "]"
let floatBetweenDoubleBrackets = pfloat |> betweenStrings "[[" "]]"

test floatBetweenBrackets2 "[1.0]"
test floatBetweenDoubleBrackets "[[1.0]]"

let between pBegin pEnd p = pBegin >>. p .>> pEnd
let betweenStrings2 s1 s2 p = p |> between (str s1) (str s2)

test (many floatBetweenBrackets) "" // Success: []

test (many floatBetweenBrackets) "[1.0]" // Success: [1.0]

test (many floatBetweenBrackets) "[2][3][4]" // Success: [2.0; 3.0; 4.0]

test (many floatBetweenBrackets) "[1][2.0E]" // Failure: Error in Ln: 1 Col: 9 Expecting: decimal digit

// Require at least one element with many1
test (many1 floatBetweenBrackets) "(1)" // Failure: Error in Ln: 1 Col: 9 Expecting: decimal digit

// Custom error message with <?>
test (many1 (floatBetweenBrackets <?> "float between brackets")) "(1)" // Failure: Error in Ln: 1 Col: 1 Expecting: float between brackets

// If you just want to skip over a sequence and don’t need the list of parser results,
// you could use the optimized combinators skipMany or skipMany1 instead of many and many1

let floatList = str "[" >>. sepBy pfloat (str ",") .>> str "]"

test floatList "[]" // Success: []

test floatList "[1.0]" // Success: [1.0]

test floatList "[4,5,6]" // Success: [4.0; 5.0; 6.0]

test floatList "[1.0,]" // Failure: Error in Ln: 1 Col: 6 Expecting: float-point number

test floatList "[1.0,2.0" // Failure: Error in Ln: 1 Col: 9 Note: The error occurred at the end of the input stream Expecting: ',' or ']'

// floatList parser can't yet deal with whitespace
test floatBetweenBrackets "[1.0, 2.0]" // Failure: Error in Ln: 1 Col: 5 Expecting: ']'

// spaces parser, which skips over any possibly empty sequence of ' ', '\t', '\r' or '\n' chars
let ws = spaces

let str_ws s = pstring s .>> ws
let float_ws = pfloat .>> ws
let numberList = str_ws "[" >>. sepBy float_ws (str_ws ",") .>> str_ws "]"

test numberList @"[1 ,
                        2 ]" // Success: [1.0; 2.0]

test numberList @"[1,
                        2; 3]" // Failure: Error in Ln: 2 Col: 27 Expecting: ',' or ']'

// eof will generate an error if the end of the stream hasn't been reached
let numberListFile = ws >>. numberList .>> eof

test numberListFile " [1, 2, 3] [4]" // Failure: Error in Ln: 1 Col: 23 Expecting end of input

// The <|> combines to alternative parsers
test (many (str "a" <|> str "b")) "abba" // Success: ["a"; "b"; "b"; "a"]

test (skipStringCI "<float>" >>. pfloat) "<FLOAT>1.0" // Success 1.0

let identifier =
    let isIdentifierFirstChar c = isLetter c || c = '_' 
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    // many1Satisfy2L parses any sequence of one or more chars (many1) whose first char satisfies the first predicate function
    // and whose remaining chars satisfy the second predicate (Satisfy2)
    // The label given as the third argument (L) is used in the error message to describe the expected input
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws // skips trailing whitespace

test identifier "_" // Success: "_"

test identifier "_test1=" // Success: "_test1"

test identifier "1" // Failure: Error in Ln: 1 Col: 1 Expecting: identifier

// parse string literal char by char
let stringLiteral = 
    // satisfy parses any char that satisfies the given predicate
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    
    let unescape c = 
        match c with
        | 'n' -> '\n'
        | 'r' -> '\r'
        | 't' -> '\t'
        | c -> c
    
    // anyOf parse any char contained in the argument string
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    // manyChars parse a sequence of chars with the given char parser and returns it as a string
    between (pstring "\"") (pstring "\"") (manyChars (normalChar <|> escapedChar))

test stringLiteral "\"abc\"" // Success: "abc"  

test stringLiteral "\"abc\\\"def\\\\ghi" // Success: "abc"def\ghi"

test stringLiteral "\"abc\\def\"" // Failure: Error in Ln: 1 Col: 6 Expecting: any char in '\nrt"'

// parse string literal snippet by snippet
let stringLiteral2 = 
    // we have to require normalCharSnippet to consume at least one char so use many1Satisfy
    // otherwise it would succeed event if it doesn't consume input
    // escapedChar would never be called and manyStrings would eventually throw an exception to prevent an infinite loop
    let normalCharSnippet = many1Satisfy (fun c -> c <> '\\' && c <> '"')
    
    let escapedChar = 
        pstring "\\" >>. (anyOf "\\nrt\"" |>> function 
                          | 'n' -> "\n"
                          | 'r' -> "\r"
                          | 't' -> "\t"
                          | c -> string c)

    // manyStrings parses a sequence of strings with the given string parser and returns the strings in concatenated form
    between (pstring "\"") (pstring "\"") (manyStrings (normalCharSnippet <|> escapedChar))

let stringLiteral3 =
    // using manySatisfy instead of many1Satisfy in normalCharSnippet so that it can parse escaped chars that are not
    // separated by normal chars.  This can't lead to an infinite loop b/c escapedChar can't succeed w/o consuming input
    let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"')
    let escapedChar = 
        pstring "\\" >>. (anyOf "\\nrt\"" |>> function 
                          | 'n' -> "\n"
                          | 'r' -> "\r"
                          | 't' -> "\t"
                          | c -> string c)

    // stringsSepBy parses a sequence of strings (with the first argument parser) separated by the other strings 
    // (parsed with the second argument parser)
    // It returns all parsed strings, including the separator strings, as a single, concatenated string
    between (pstring "\"") (pstring "\"") (stringsSepBy normalCharSnippet escapedChar)

let product = pipe2 float_ws (str_ws "*" >>. float_ws) (fun x y -> x * y)

test product "3 * 5" // Success: 15.0

type StringConstant = StringConstant of string * string

let stringConstant = pipe3 identifier (str_ws "=") stringLiteral (fun id _ str -> StringConstant(id, str))

test stringConstant "myString = \"stringValue\"" // Success: StringConstant ("myString","stringValue")

// return the parsed values as a tuple using tuple2-5 or (.>>.)
test (float_ws .>>. (str_ws "," >>. float_ws)) "123, 456" // Success: (123.0, 456.0)

// pipe7 implementation
let pipe7 p1 p2 p3 p4 p5 p6 p7 f =
    pipe4 p1 p2 p3 (tuple4 p4 p5 p6 p7) (fun x1 x2 x3 (x4, x5, x6, x7) -> f x1 x2 x3 x4 x5 x6 x7)

// stringReturn skips the string constant given as the first argument and, if successful, returns the value given as the second argument
// (<|>) only tries the parser on the right side if the parser on the left side fails. It does not implement a longest match rule
// (<|>) only tries the right parser if the left parser fails without consuming input
let boolean = (stringReturn "true" true) <|> (stringReturn "false" false)

test boolean "false" // Success: false
test boolean "true" // Success: true
test boolean "tru" // Failure: Error in Ln: 1 Col: 1 Expecting: 'false' or 'true'

// (<|>) failure example
// the test fails because the parser on the left side consumers whitespace before it fails
test ((ws >>. str "a") <|> (ws >>. str "b")) " b" // Failure: Error in Ln: 1 Col: 2 Expecting 'a'

// (<|>) test fix
test ((ws >>. (str "a" <|> str "b"))) " b" // Success: "b"

// value restriction
let p = pstring "test"
run p "input"

let p1 : Parser<_> = pstring "test"