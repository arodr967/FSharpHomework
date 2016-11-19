// Skeleton file for PCF interpreter

// This sets F# to read from whatever directory contains this source file.
System.Environment.set_CurrentDirectory __SOURCE_DIRECTORY__;;

#load "parser.fsx"

// This lets us refer to "Parser.Parse.parsefile" simply as "parsefile",
// and to constructors like "Parser.Parse.APP" simply as "APP".
open Parser.Parse

let rec subst app v n = match app with
                       | APP (term, ID v) -> APP (term,  n)
                       | APP (term, app2) -> APP (term,  (subst app2 v n) )

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
    | (FUN (v,app), n)  -> interp (subst app v n) 
| IF (e1, e2, e3) -> 
    match (interp e1, interp e2, interp e3) with
    | (ERROR s, _, _)       -> ERROR s        // ERRORs are propagated
    | (_, ERROR s, _)       -> ERROR s
    | (_, _, ERROR s)       -> ERROR s
    | (BOOL true, ID e, _)  -> ID e
    | (BOOL false, _, ID e) -> ID e
    | (BOOL true, v, _)     -> ERROR (sprintf "'if' needs a variable argument for the 'then' branch, not '%A'" v)
    | (BOOL false, _, v)    -> ERROR (sprintf "'if' needs a variable argument for the 'else' branch, not '%A'" v)
    | (v, _, _)             -> ERROR (sprintf "'if' needs a boolean argument for the if branch, not '%A'" v)

  
    
// Here are two convenient abbreviations for using your interpreter.
let interpfile filename = filename |> parsefile |> interp

let interpstr sourcecode = sourcecode |> parsestr |> interp
