// Skeleton file for PCF interpreter

// This sets F# to read from whatever directory contains this source file.
System.Environment.set_CurrentDirectory __SOURCE_DIRECTORY__;;

#load "parser.fsx"

// This lets us refer to "Parser.Parse.parsefile" simply as "parsefile",
// and to constructors like "Parser.Parse.APP" simply as "APP".
open Parser.Parse

//e is the body, x is the term to subsitute, t is the subsitution term
let rec subst e x t= match e with
                     |ID y              -> if x = y then t else ID y
                     |APP (e1, e2)      -> APP ( subst e1 x t, subst e2 x t)
                     |IF (e1, e2, e3)   -> IF (subst e1 x t, subst e2 x t, subst e3 x t)
                     |FUN (y, e1)       -> if x = y then FUN (y, e1) else FUN (y, subst e1 x t)
                     |REC (y, e1)       -> if x = y then REC (y, e1) else  REC (y, subst e1 x t)
                     |e                 -> e
                  

// Here I show you a little bit of the implementation of interp. Note how ERRORs
// are propagated, how rule (6) is implemented, and how stuck evaluations
// are reported using F#'s sprintf function to create good error messages.
let rec interp = function
    | APP (e1, e2) ->
        match (interp e1, interp e2) with
        | (ERROR s, _)          -> ERROR s               // ERRORs are propagated
        | (_, ERROR s)          -> ERROR s
        | (SUCC, NUM n)         -> NUM (n+1)             // Rule (6)
        | (SUCC, v)             -> ERROR (sprintf "'SUCC' needs INT argument, not '%A'" v)
        | (PRED, NUM 0)         -> NUM 0
        | (PRED, NUM n)         -> NUM (n - 1)
        | (PRED, v)             -> ERROR (sprintf "'PRED' needs INT argument, not '%A'" v)
        | (ISZERO, NUM 0)       -> BOOL true
        | (ISZERO, NUM n)       -> BOOL false
        | (ISZERO, v)           -> ERROR (sprintf "'ISZERO' needs INT argument, not '%A'" v)
        | (FUN (x, e), v1)      -> interp (subst e x v1) // Rule (10)
        | (REC (x, f), e)       -> interp (APP (subst f x (REC (x, f)), e)) // Rule (11)
        | (s1, s2)              -> APP (s1, s2)
     | IF (b, e1, e2) ->     // Rule (4 & 5)
        match (interp b, e1, e2) with
        | (ERROR s, _, _)       -> ERROR s
        | (_, ERROR s, _)       -> ERROR s
        | (_, _, ERROR s)       -> ERROR s
        | (BOOL true, e, _)     -> interp e
        | (BOOL false, _, e)    -> interp e
        | (b, _, _) -> ERROR "'IF' needs BOOL expression."
    | e -> e //Catch all case

  
    
// Here are two convenient abbreviations for using your interpreter.
let interpfile filename = filename |> parsefile |> interp

let interpstr sourcecode = sourcecode |> parsestr |> interp
