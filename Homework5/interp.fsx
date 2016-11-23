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
| NUM n -> NUM n
| BOOL e -> BOOL e
| ID e -> ID e
| SUCC -> SUCC
| PRED -> PRED 
| ISZERO -> ISZERO
| ERROR s -> ERROR s
| FUN (x,e) -> FUN (x,e)
| REC (x,e) -> subst e x e
| APP (e1, e2) ->
    match (interp e1, interp e2) with
    | (ERROR s, _)      -> ERROR s        // ERRORs are propagated
    | (_, ERROR s)      -> ERROR s
    | (SUCC, NUM n)     -> NUM (n+1)      // Rule (6)
    | (SUCC, v)         -> ERROR (sprintf "'succ' needs int argument, not '%A'" v)
    | (PRED, NUM 0)     -> NUM 0
    | (PRED, NUM n)     -> NUM (n - 1)
    | (PRED, v)         -> ERROR (sprintf "'pred' needs int argument, not '%A'" v)
    | (ISZERO, NUM 0)   -> BOOL true
    | (ISZERO, NUM n)   -> BOOL false
    | (ISZERO, v)       -> ERROR (sprintf "'iszero' needs int argument, not '%A'" v)
    | (FUN (x,e), t)    -> interp ( subst e x t)
    | f -> snd f
| IF (e1, e2, e3) -> 
    match (interp e1, interp e2, interp e3) with
    | (ERROR s, _, _)       -> ERROR s        // ERRORs are propagated
    | (_, ERROR s, _)       -> ERROR s
    | (_, _, ERROR s)       -> ERROR s
    | (BOOL true, e, _)  ->  e
    | (BOOL false, _, e) ->  e
    | (v, _, _)             -> ERROR (sprintf "'if' needs a boolean argument for the 'if' branch, not '%A'" v)

  
    
// Here are two convenient abbreviations for using your interpreter.
let interpfile filename = filename |> parsefile |> interp

let interpstr sourcecode = sourcecode |> parsestr |> interp
