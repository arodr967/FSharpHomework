// ---------------------------------------------------------------
//         Homework 3
//         Adam Levy, Alejandro Palacios, & Alicia Rodriguez
//         October 31, 2016
// ---------------------------------------------------------------


// 1. Given vectors u = (u1, u2,..., un) and v = (v1, v2,..., vn), the inner
// product of u and v is defined to be u1*v1 + u2*v2 + ... + un*vn. Write a
// curried F# function inner that takes two vectors represented as int lists
// and returns their inner product:

(*
    > inner [1;2;3] [4;5;6];;
    val it : int = 32
*)
// (Assume that the two lists have the same length.)

// Solution

//Documentation here
let rec inner xs ys = if xs = [] then 0 else List.head xs * List.head ys + inner (List.tail xs) (List.tail ys);;



// 2. Given an m-by-n matrix A and an n-by-p matrix B, the product of A and B
// is an m-by-p matrix whose entry in position (i,j) is the inner product of
// row i of A with column j of B. For example,
(*
  		          / 0 1 \
    / 1 2 3 \  *  | 3 2 |  =  /  9 11 \
    \ 4 5 6 /     \ 1 2 /     \ 21 26 /
*)
// Write an uncurried F# function to do matrix multiplication:
(*
    > multiply ([[1;2;3];[4;5;6]], [[0;1];[3;2];[1;2]]);;
    val it : int list list = [[9; 11]; [21; 26]]
*)
// Assume that the dimensions of the matrices are appropriate.
// Hint: Use transpose (from Homework 2), inner, and List.map.

// Solution:

let rec transpose = function
| [] -> failwith "cannot transpose a 0-by-n matrix"
| []::xs -> [] // base case: m-by-0
| xs -> List.map List.head xs :: transpose (List.map List.tail xs)

//Curried version because I accidentally made it
let rec multiply2 xs ys = if List.length xs = 0 then [] 
                          else let zs = transpose ys
                               List.map (inner (List.head xs)) zs :: multiply2 (List.tail xs) (ys);;

let rec multiply = function
| ([], ys) -> []
| (x::xs, ys) -> let zs = transpose ys
                 List.map (inner x) zs :: multiply (xs) (ys)                                            



// 3. Two powerful List functions provided by F# are List.fold and
// List.foldBack. These are similar to List.reduce and List.reduceBack, but
// more general. Both take a binary function f, an initial value i, and a
// list [x1;x2;x3;...;xn]. Then List.fold returns
(*
    (f ... (f (f (f i x1) x2) x3) ... xn)
*)
// while List.foldBack returns
(*
    (f x1 (f x2 (f x3 ... (f xn i) ... )))
*)
// In spite of this complicated behavior, they can be implemented very simply:
(*
    > let rec fold f a = function
      | []    -> a
      | x::xs -> fold f (f a x) xs;;

    val fold : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a

    > let rec foldBack f xs a =
        match xs with
        | []    -> a
        | y::ys -> f y (foldBack f ys a);;

    val foldBack : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
*)
// (Note that they don't take their arguments in the same order.)
// Each of these functions can be used to implement flatten, which "flattens"
// a list of lists:
(*
    let flatten1 xs = List.fold (@) [] xs

    let flatten2 xs = List.foldBack (@) xs []
*)
// For example,
(*
    > flatten1 [[1;2];[];[3];[4;5;6]];;
    val it : int list = [1; 2; 3; 4; 5; 6]
*)
// Compare the efficiency of flatten1 xs and flatten2 xs, both in terms of
// asymptotic time compexity and experimentally. To make the analysis
// simpler, assume that xs is a list of the form [[1];[2];[3];...;[n]].

// Solution:



// 4. An interesting higher-order function is twice, which can be defined by
(*
    > let twice f = (fun x -> f (f x));;

    val twice : ('a -> 'a) -> 'a -> 'a
*)
// or, using F#'s function composition operator <<, by
(*
    > let twice f = f << f;;

    val twice : ('a -> 'a) -> ('a -> 'a)
*)
// If we also define
(*
    > let successor n = n+1;;

    val successor : int -> int
*)
// then we can evaluate expressions like
(*
    > (twice (twice (twice (twice successor)))) 0;;
    val it : int = 16
*)
// It is pretty easy to see that with k occurrences of twice, these
// expressions will return 2k.
// Remarkably, F# also allows us to evaluate expressions like
(*
    twice twice twice twice successor 0
*)
// in which the function applications are associated to the left, by F#'s
// default parsing conventions. (Notice that this means that twice gets
// applied to itself!) Can you figure out a formula that gives the value of
(*
    twice twice twice ... twice successor 0
*)
// when there are k occurrences of twice? (I suggest that you approach this
// problem experimentally.)

// Solution:



// 5. Recall our discussion of infinite streams in F#, with definition
(*
    type 'a stream = Cons of 'a * (unit -> 'a stream)
*)
// Show how to define map f s on streams; this should give the stream formed
// by applying function f to each element of stream s.

// Solution:



// 6. Interpreter 0 In this problem, we begin our exploration of the use of F#
// for language-oriented programming. You will write an F# program to
// evaluate arithmetic expressions written in the language given by the
// following context-free grammar:
(*
    E -> n | -E | E + E | E - E | E * E | E / E | (E)
*)
// In the above, n is an integer literal, -E is the negation of E, the next
// four terms are the sum, difference, product, and quotient of expressions,
// and (E) is used to control the order of evaluation of expressions, as in
// the expression 3*(5-1).
// Rather than working directly with the concrete syntax above, we will
// imagine that we have a parser that parses input into an abstract syntax
// tree, as is standard in real compilers. Hence your interpreter will take
// an input of the following discriminated union type:
(*
    type Exp =
      Num of int
    | Neg of Exp
    | Sum of Exp * Exp
    | Diff of Exp * Exp
    | Prod of Exp * Exp
    | Quot of Exp * Exp
*)
// Note how this definition mirrors the grammar given above. For instance,
// the constructor Num makes an integer into an Exp, and the constructor Sum
// makes a pair of Exp's into an Exprepresenting their sum. Interpreting
// abstract syntax trees is much easier than trying to interpret concrete
// syntax directly. Note that there is no need for a constructor
// corresponding to parentheses, as the example given above would simply be
// represented by
(*
    Prod(Num 3, Diff(Num 5, Num 1))
*)
// which represents the parse tree which looks like

(*some picture here*)

// Your job is to write an F# function evaluatethat takes an abstract syntax
// tree and returns the result of evaluating it. Most of the time, evaluating
// a tree will produce an integer, but we must address the possibility of
// dividing by zero. This could be handled by raising an exception, but
// instead we choose to make use of the built-in F# type
(*
    type 'a option = None | Some of 'a
*)
// Thus evaluate will have type Exp -> int option, allowing it to return Some
// m in the case of a successful evaluation, and Nonein the case of an
// evaluation that fails due to dividing by zero. For example,
(*
    > evaluate (Prod(Num 3, Diff(Num 5, Num 1)));;
    val it : int option = Some 12
    > evaluate (Diff(Num 3, Quot(Num 5, Prod(Num 7, Num 0))));;
    val it : int option = None
*)
// Naturally, evaluate e should use recursion to evaluate each of e's
// sub-expressions; it should also use match to distinguish between the cases
// of successful or failed sub-evaluations. To get you started, here is the
// beginning of the definition of evaluate:

// Solution:

  let rec evaluate = function
  | Num n -> Some n
  | Neg e -> match evaluate e with
	     | ...
