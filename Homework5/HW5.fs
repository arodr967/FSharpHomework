// ---------------------------------------------------------------
//         Homework 5
//         Adam Levy, Alejandro Palacios, & Alicia Rodriguez
//         November 23, 2016
// ---------------------------------------------------------------

// In this homework, we will deepen our understanding of formal semantics by
// developing an interpreter in F# for a small functional language called PCF
// (which stands for Programming language for Computable Functions). PCF is a
// small but powerful language which includes both first-class functions and
// recursion, making it expressive enough to write interesting functional
// programs as in F#. The semantics of PCF is specified formally by a set of
// Natural Semantics rules, and the interpreter that you develop will be based
// on a direct implementation of these rules.

// The syntax of PCF is quite similar to that of F#; it is given by the
// following context-free grammar:

  e ::= x | n | true | false | succ | pred | iszero |
        if e then e else e | e e | fun x -> e | rec x -> e | (e)

// In the above, x stands for an identifier; n stands for a non-negative
// integer literal; true and false are the boolean literals; succ and pred are
// unary functions that add 1 and subtract 1 from their input, respectively;
// iszero is a unary function that returns true if its argument is 0 and false
// otherwise; if e1 then e2 else e3 is a conditional expression; e e is a
// function application; fun x -> e is an anonymous function with parameter x
// and body e; rec x -> e is used for defining recursive functions
// (we'll explain this later); and (e) allows parentheses to be used to
// control grouping.

// It should be clear to you that the above grammar is quite ambiguous.
// For example, should
  fun f -> f f
// be parsed as
  fun f -> (f f)
// or as
  (fun f -> f) f //?
// We can resolve such ambiguities by adopting the following conventions
// (which are the same as in F#):

// * Function application binds tighter than if, fun, and rec.
// For example, fun f -> f 0 is fun f -> (f 0), not (fun f -> f) 0.
// * Function application associates to the left.
// For example, e f g is (e f) g, not e (f g).
// As in Homework 3's Interpreter 0, we don't want to interpret concrete
// syntax directly. Instead, the interpreter will work on an abstract syntax
// tree representation of the program; these abstract syntax trees will be
// values in the following F# type:

  type term =
  | ID of string | NUM of int | BOOL of bool | SUCC | PRED | ISZERO
  | IF of term * term * term | APP of term * term
  | FUN of string * term | REC of string * term | ERROR of string

// This definition mirrors the context-free grammar given above; for instance,
// the constructor ID makes a string into an identifier, and the constructor
// FUN makes a string (representing the formal parameter) and a term
// (representing the body) into a function. Note that there is no abstract
// syntax for (e); the parentheses are just used to control grouping. Also,
// there is an additional kind of term, ERROR, which will be used for reporting
// runtime errors.

// Recall that in Interpreter 0 you had to build the abstract syntax trees for
// arithmetic expressions by hand, a quite tedious process. For this
// assignment, I am providing you with a parser that automatically converts
// from concrete PCF syntax to an abstract syntax tree. The parser is available
// here: parser.fsx. Include the commands

  #load "parser.fsx"

  open Parser.Parse

// at the beginning of the file containing your interpreter. This loads the
// definition of type term, as given above, and also defines the functions
// parsestr and parsefile. Function parsestr takes a string of PCF source code
// and returns the corresponding abstract syntax; for example
(*
  > parsestr "iszero (succ 7)";;
  val it : term = APP (ISZERO,APP (SUCC,NUM 7))
*)
// Function parsefile takes a string that is the name of a file, and parses
// the file's contents. If your interpreter includes the command

  System.Environment.set_CurrentDirectory __SOURCE_DIRECTORY__;;

// then F# will read from the directory where your interpreter is located.
// (By the way, parser.fs includes both a lexer and a recursive-descent parser
// similar to what we discussed in our Notes on Programming Language Syntax;
// you may find it interesting to study how they work.)

// You are to write an F# function interp that takes an abstract syntax tree
// represented as a term and returns the result of evaluating it, which will
// also be a term. The evaluation should be done according to the Natural
// Semantics rules given below. The rules are based on judgments of the
// form e => v, which intuitively says that term e evaluates to value v. For
// the sake of readability, we describe the rules below using the concrete
// syntax of PCF programs---remember that your interp program will actually
// need to work on abstract syntax trees, which are F# values of type term.

// The first few Natural Semantics rules are not very interesting; they just
// say that basic PCF values evaluate to themselves:

// (1) n => n, for any non-negative integer literal n

// (2) true => true and false => false

// (3) succ => succ, pred => pred, and iszero => iszero.

// The interesting evaluation rules are a bit more complicated, because they
// involve hypotheses as well as a conclusion. For example, here's one of the
// rules for evaluating an if-then-else:

         b => true         e1 => v
 // (4)	---------------------------
         if b then e1 else e2 => v
// In such a rule, the judgments above the horizontal line are hypotheses and
// the judgment below is the conclusion. We read the rule from the bottom up:
// "if the expression is an if-then-else with components b, e1, and e2, and b
// evaluates to true and e1 evaluates to v, then the entire expression
// evaluates to v". Of course, we also have the symmetric rule

         b => false        e2 => v
// (5)    ----------------------------
         if b then e1 else e2 => v
// Rules (4) and (5) define the only ways for an if-then-else to evaluate
// successfully. This means, in particular, that if b evaluates to something
// other than true or false, then the if-then-else does not evaluate
// successfully---in this case we say that the evaluation gets stuck.

// The following rules define the semantics of applications of the built-in
// functions; in these rules, note that we use n to stand for a non-negative
// integer.

         e1 => succ        e2 => n
// (6)    ----------------------------
             e1 e2 => n+1

         e1 => pred        e2 => 0       e1 => pred   e2 => n+1
// (7)    ---------------------------     --------------------------
             e1 e2 => 0                         e1 e2 => n

         e1 => iszero   e2 => 0          e1 => iszero   e2 => n+1
// (8)	------------------------        ---------------------------
              e1 e2 => true                   e1 e2 => false

// For example, to evaluate

  if iszero 0 then 1 else 2

// we must, by rules (4) and (5), first evaluate iszero 0. By rule (8)
// (and rules (3) and (1)), this evaluates to true. Finally, by rule (4)
// (and rule (1)), the whole program evalutes to 1.

// Part a. As a first step, use these rules to write an interpreter,
// interp: term -> term, for the subset of the language that does not include
// terms of the form ID, FUN, or REC. If your interpreter is given such a term,
// it can return an error term, such as ERROR "not yet implemented". Note that
// you also need to return an error term whenever the evaluation gets stuck,
// as in a case like if succ 4 then 0 else 1.

// Part b. Interpreters need a way of passing parameters to user-defined
// functions; here we will accomplish this by means of textual substitution.
// For example, on the function call

  (fun x -> succ (succ x)) 17

// we will first pass the parameter into the function body, giving

  succ (succ 17)

// and then we will evaluate this, giving 19. In this part of the assignment,
// we develop an auxiliary function subst to support our implementation stategy.
// Let us use the notation e[x := t] to denote the textual substitution of t for
// all free occurrences of x within e. For example, (succ x)[x:=1] is (succ 1).
// Implement substitution for PCF by writing an F# function subst e x t that
// takes a term e, a string x representing an identifier, and a term t, and
// returns e with all free occurrences of x (actually ID x) replaced by t.
// For example,
(*
  > subst (APP(SUCC, ID "x")) "x" (NUM 1);;
  val it : term = APP (SUCC,NUM 1)
*)
// Note that you must not substitute for occurrences of indentifiers that are
// bound by fun or rec. For instance,

  ((fun x -> succ x) (pred x))[x:=3]

// should result in ((fun x -> succ x) (pred 3)). The reason is that the
// formal parameter x and its occurrences in the function body are bound and
// should not be affected by the substitution.

// Hint: Implement subst e x t by matching on e, using recursion to substitute
// into the subterms of e as appropriate.

// Part c. Using your substitution function, extend your interp function from
// Part a to include FUN terms. The evaluation of terms involving FUN is defined
// as follows. First, functions defined using fun simply evaluate to themselves:

// (9)    (fun x -> e) => (fun x -> e)
//Computations occur when you apply these functions to arguments. The following
// rule defines the application of a user-defined function, using textual
// substitution for parameter passing:

            e1 => (fun x -> e)      e2 => v1    e[x:=v1] => v2
// (10)    --------------------------------------------------------
                              e1 e2 => v2
// In words, in an application e1 e2, if the function e1 evaluates to a
// user-defined function fun x -> e, then evaluate the argument e2 to a value
// v1, substitute v1 for the formal parameter x in the function body e, and
// finally evaluate the modified body. (Note that this rule gives eager
// evaluation as in F#.) For instance, to evaluate the application

  ((fun x -> succ x) (succ 3))

// we evaluate the function to fun x -> succ x; we evaluate the argument
// (succ 3) to 4; we substitute this into the body of the function, obtaining
// succ 4; and we evaluate this to 5, which is the final result.

// Notice that terms of the form ID x can appear whenever x is a formal
// parameter, but we never need to evaluate such terms, because they should
// always be replaced by substitution before we evaluate the function body.
// Any ID x term that does remain in the function body at the time of evaluation
// is an unbound identifier; in this case the evaluation is stuck and you need
// to return an ERROR term.

// Part d. Finally, we consider the evaluation of rec terms. Surprisingly, this
// turns out to be quite simple. First let's talk about what a term of the form
// rec x -> e actually means. It corresponds to the definition of a recursive
// function called x. Let's work with an example. The term

  rec sum -> fun x -> fun y -> if iszero x then y else sum (pred x) (succ y)

// corresponds to the following recursive F# function declaration

  let rec sum x y = if x = 0 then y else sum (x - 1) (y + 1)

// Thus, in rec x -> e, we see that x is the name of the recursive function and
// e (which should be a fun term) gives the parameters and body of the function.

// The rule for evaluating a recursive term is amazingly simple. We just
// evaluate the body of the term, where all free occurrences of the recursively
// defined identifier are replaced by the entire rec term.

                  e[x:=(rec x -> e)] => v
// (11)            --------------------------
                    (rec x -> e) => v

// It turns out that this is all that is needed to make recursion work!

// Let's try to explain why. Consider the F# function

  let rec double n = if n = 0 then 0 else 2 + double (n-1)

// In PCF, this corresponds to the following term which we abbreviate as T:

  rec double -> fun n -> if iszero n then 0 else succ (succ (double (pred n)))

// Under rule (11), T evaluates to the body of T, with double replaced with T
// itself:

  fun n -> if iszero n then 0 else succ (succ (T (pred n)))

// Notice that this means that the application (T 0) evaluates to 0: we
// substitute 0 for n in the function body, yielding

  if iszero 0 then 0 else succ (succ (T (pred 0)))

// which then evaluates to 0. What about the application (T 1)? In that case, we
// substitute 1 for n, which causes us to evaluate the else branch

  succ (succ (T (pred 1)))

// We now evaluate T a second time, which again yields a function that returns
// 0 on input 0; hence the two applications of succ finally give the result 2.

// In general, rule (11) works by unwinding the definition of double one more
// time each time a recursive call is needed.

// Notes:

// While this program is broken up into four parts in order to help you make
// progress through it, you should just turn in a single interp function that
// interprets the entire PCF language, together with your subst function.
// The parser actually parses a richer language than presented so far; it
// extends the CFG for PCF shown above above with the rule

   e ::= let x = e in e

// allowing let expressions as in F#. For example,

   let z = 2 in succ z

// is allowed by the parser.
// Rather than creating a new LET term for such expressions (which would require
// more code in the interpreter), the parser treats let expressions as syntactic
// sugar. In particular, the parser treats

  let x = e1 in e2

// as if it were

  (fun x -> e2) e1

// Thus the example above will be parsed as if it were (fun z -> succ z) 2:
(*
  > parsestr "let z = 2 in succ z";;
  val it : term = APP (FUN ("z",APP (SUCC,ID "z")),NUM 2)
*)
// A bit of thought should convince you that this function application has
// exactly the same meaning as the let expression. Thus you may use let in
// creating examples to test your interpreter, without having to write any new
// interpreter code.

// Also, the parser allows PCF programs to contain comments, which begin with
// the character # and extend to the end of the current line.

// Here is a skeleton file to help you get started: interp.fsx. And here are
// some sample PCF programs for you to try with your interpreter: twice.pcf,
// double.pcf, minus.pcf, fibonacci.pcf, factorial.pcf, divisor.pcf, lists.pcf,
// and ackermann.pcf.
