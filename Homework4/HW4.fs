// ---------------------------------------------------------------
//         Homework 4
//         Adam Levy, Alejandro Palacios, & Alicia Rodriguez
//         November 14, 2016
// ---------------------------------------------------------------

// 1. An interesting use of first-class functions and ref cells in F# is to
// create a monitored version of a function:
(*
  > let makeMonitoredFun f =
      let c = ref 0
      (fun x -> c := !c+1; printf "Called %d times.\n" !c; f x);;
  val makeMonitoredFun : ('a -> 'b) -> ('a -> 'b)
  > let msqrt = makeMonitoredFun sqrt;;
  val msqrt : (float -> float)
  > msqrt 16.0 + msqrt 25.0;;
  Called 1 times.
  Called 2 times.
  val it : float = 9.0
*)
// First, explain why F# does not allow the following declaration:
(*
  let mrev = makeMonitoredFun List.rev
*)
// Now suppose we rewrite the declaration using the technique of eta expansion:
(*
  let mrev = fun x -> (makeMonitoredFun List.rev) x
*)
// Does this solve the problem? Explain why or why not.

// Solution

// The following declaration:
(*
  let mrev = makeMonitoredFun List.rev
*)
// returns the following error:
(*
Value restriction. The value 'mrev' has been inferred to have generic type
    val mrev : ('_a list -> '_a list)
Either make the arguments to 'mrev' explicit or, if you do not intend for it
to be generic, add a type annotation.
*)
// This is because List.rev is not given a specific type and it is not given
// another parameter to input a list.

// When rewriting the declaration using the technique of eta expansion:
(*
  let mrev = fun x -> (makeMonitoredFun List.rev) x
*)
// An error is not returned, as shown below:
(*
val mrev : x:'a list -> 'a list
*)
// However, when using a similar pattern, as used in msqrt, to call the
// function, the following is returned:
(*
  mrev [3;2;1] @ mrev [1..3];;
  Called 1 times.
  Called 1 times.
  val it : int list = [1; 2; 3; 3; 2; 1]
*)
// Which is not correct. Clearly, the function is called twice, but by using
// the technique of eta expansion, the counter gets reset everytime the function
// is called. This also happens when rewriting the msqrt function using
// the technique of eta expansion:
(*
  > let msqrt = fun x -> (makeMonitoredFun sqrt) x;;

  val msqrt : x:float -> float

  > msqrt 16.0 + msqrt 25.0;;
  Called 1 times.
  Called 1 times.
  val it : float = 9.0
*)
// Therefore, usingthe technique of eta expansion does not solve the problem.



// 2. Recall the unambiguous grammar for arithmetic expressions discussed in
// class:
(*
  E -> E+T | E-T | T
  T -> T*F | T/F | F
  F -> i | (E)
*)
// Modify this grammar to allow an exponentiation operator, ^, so that we can
// write expressions like i+i^i*i. Of course, your modified grammar should be
// unambiguous. Give exponentiation higher precedence than the other binary
// operators and (unlike the other binary operators) make it associate to
// the right.

// Solution



// 3. Recall the grammar for the tiny language used in our discussion of
// recursive-descent parsing. Let us extend the grammar to allow both if-then
// and if-then-elsestatements:
(*
  S -> if E then S | if E then S else S | begin S L | print E
  L -> end | ; S L
  E -> i
*)
// Show that the grammar is now ambiguous.

// Solution



// 4. Following the approach described in class, write a complete (pseudo-code)
// recursive-descent parser for the grammar in question 3, including functions
// S(), L(), and E(). Try to improve on the code given in class by returning
// helpful error messages.
// [You may wonder how a recursive-descent parser can be written, given the
// ambiguity that you found in question 3. This ambiguity, known as the
// dangling else ambiguity, is found in many programming languages, including
// C and Java. However, you should see that the parser can resolve that
// ambiguity in a natural way that corresponds to a rule that you should
// have learned in your study of C or Java.]

// Solution
