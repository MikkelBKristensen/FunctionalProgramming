module ImpParser

    open System
    open Eval
    open Types

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open JParsec.TextParser             // Example parser combinator library. Use for CodeJudge.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "space"
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) (p1 : Parser<'a>) (p2 : Parser<'b>) : Parser<'a * 'b> = p1 .>> spaces .>>. p2
    let (.>*>) (p1 : Parser<'a>) (p2 : Parser<'b>) : Parser<'a> = p1 .>> spaces .>> p2
    let (>*>.) (p1 : Parser<'a>) (p2 : Parser<'b>) : Parser<'b> = p1 >>. spaces >>. p2

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')' <?> "parenthesise"
    let curlyBrackets p = pchar '{' >*>. p .>*> pchar '}' <?> "curlyBrackets"

    let pid : Parser<string> =
        pletter <|> pchar '_' .>>. many palphanumeric |>> fun (x, charList) -> String(List.toArray (x::charList))

    
    let unop op a = op >*>. a
    let binop op a b = a .>*> op .>*>. b 

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]
    
    let DivParse = binop (pchar '/') AtomParse TermParse |>> Div <?> "Div"
    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let ModParse = binop (pchar '%') AtomParse TermParse |>> Mod <?> "Mod"
    do pref := choice [DivParse; MulParse; ModParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let VParse   = pid |>> V <?> "Var"
    let ParParse = parenthesise TermParse
    let NegParse = unop (pchar '-') AtomParse |>> (fun x -> Mul (N -1, x)) <?> "Neg"
    let PVParse  = pPointValue >*>. parenthesise TermParse |>> PV <?> "PV"
    do aref := choice [PVParse; NegParse; VParse; NParse; ParParse]

    let AexpParse = TermParse
    
    

    let CexpParse = pstring "not implemented"

    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"


    let parseSquareProg _ = failwith "not implemented"

    let parseBoardProg _ = failwith "not implemented"

    let mkBoard (bp : boardProg) : board = failwith "not implemented"

