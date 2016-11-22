// ---------------------------------------------------------------
//         Homework 5
//         Adam Levy, Alejandro Palacios, & Alicia Rodriguez
//         November 23, 2016
// ---------------------------------------------------------------


// This sets F# to read from whatever directory contains this source file.
System.Environment.set_CurrentDirectory __SOURCE_DIRECTORY__;;

#load "parser.fsx"

// This lets us refer to "Parser.Parse.parsefile" simply as "parsefile",
// and to constructors like "Parser.Parse.APP" simply as "APP".
open Parser.Parse

let rec subst e x t =
  match e with
  | ID y            -> if y = x then t else ID y
  | NUM n           -> NUM N
  | BOOL b          -> BOOL b
  | SUCC            -> SUCC
  | PRED            -> PRED
  | ISZERO          -> ISZERO
  | IF  (b, e1, e2) -> IF(subst b x t, subst e1 x t, subst e2 x t)
  | FUN (y, e1)     -> if y = x then FUN (y, e1) else FUN (y, subst e1 x t)
  | REC (y, e1)     -> if y = x then REC (y, e1) else REC (y, subst e1 x t)
  | APP (e1, e2)    -> APP(subst e1 x t, subst e2 x t)
  | _ -> ERROR "Substitution failed."

// Here I show you a little bit of the implementation of interp. Note how ERRORs
// are propagated, how rule (6) is implemented, and how stuck evaluations
// are reported using F#'s sprintf function to create good error messages.
let rec interp = function
    | ERROR s  -> ERROR s
    | ID s     -> ERROR (sprintf "'ID' not implemented.")
    | NUM n    -> NUM n     // Rule (1)
    | BOOL b   -> BOOL b    // Rule (2)
    | SUCC     -> SUCC      // Rule (3)
    | PRED     -> PRED      // Rule (3)
    | ISZERO   -> ISZERO    // Rule (3)
    | IF (b, e1, e2) ->     // Rule (4 & 5)
        match (interp b, e1, e2) with
        | (ERROR s, _, _)   -> ERROR s
        | (_, ERROR s, _)   -> ERROR s
        | (_, _, ERROR s)   -> ERROR s
        | (BOOL b, eA, eB)  ->
            match b with
            | true  -> interp eA
            | false -> interp eB
        | (b, _, _) -> ERROR "'IF' needs a boolean expression."
    | FUN (x, e)      -> ERROR (sprintf "'ID' not implemented.")
    | REC (x, e)      -> ERROR (sprintf "'ID' not implemented.")
    | APP (e1, e2) ->
        match (interp e1, interp e2) with
        | (ERROR s, _)  -> ERROR s              // ERRORs are propagated
        | (_, ERROR s)  -> ERROR s
        | (SUCC, NUM n) -> NUM (n+1)            // Rule (6)
        | (SUCC, v)     -> ERROR (sprintf "'SUCC' needs int argument, not '%A'" v)
        | (PRED, NUM n) -> match (NUM n) with   // Rule (7)
                           | NUM 0 -> NUM 0
                           | NUM n -> NUM (n-1)
        | (PRED, v) -> ERROR (sprintf "'PRED' needs an INT.")
        | (ISZERO, NUM n) -> match (NUM n) with // Rule (8)
                             | NUM 0 -> BOOL true
                             | NUM n -> BOOL false
        | (ISZERO, v) -> ERROR (sprintf "'ISZERO' needs an INT.")

// Here are two convenient abbreviations for using your interpreter.
let interpfile filename = filename |> parsefile |> interp

let interpstr sourcecode = sourcecode |> parsestr |> interp
