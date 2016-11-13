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

 (*
   
    1.) Although we are using the imperative side of f# that allows polymorphic values, we are still giving
    the function mrev a bad value. make MonitoredFun takes as input a function with a parameter x.
    in the first case, we are not giving it that.

    2.) This second declaration of mrev satisfies the pattern matching of makeMonitoredFun because we are declaring
        an anonymous function with 1 parameter. This will solve the problem.
 
 *)




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

  (*

    The new grammar gives higher precedance than the other binary operators by having it further down the context
    free grammar. The associativity to the right comes from having F evaluated first in the exponentiation portion
    of the grammar.

     E -> E+T | E-T | T
     T -> T*S | T/S | S
     S -> F^S | F
     F -> i | (E)

  *)



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

    (*
        The problem with this CFG arrives when you are attempting to nest an if-then-else statement
        within an if-then statement (or vice versa). This would cause the following ambiguity:

        1.) if a < 4 then (if b > 3 then print 2) else print 3
        2.) if a < 4 then (if b > 3 then print 2 else print 3)

        Both are correct according to this grammar, but if you are trying to nest an if-then-else
        statement within an if-then statement the second parse would be correct. If the opposite was
        the case, the first parse would be correct.

    
    *)



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


(*


  **THIS CODE WAS COPIED FROM THE NOTES AND MODIFIED AS NEEDED**
  void S() {
    switch (tok) {
      case IF: advance(); E(); eat(THEN); S(); eat(ELSE); S(); break;
      case BEGIN: advance(); S(); L(); break;
      case PRINT: advance(); E(); break;
      default: error();
    }
  }

  void L() {
    switch (tok) {
      case END: advance(); break;
      case SEMICOLON: advance(); S(); L(); break;
      default: error();
    }
  }

  void E() {
    eat(ID);
  }

  void main() {
    S();
    if (tok == EOF) accept(); else error();
  }

*)
