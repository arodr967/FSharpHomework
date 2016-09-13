
// ---------------------------------------------------------------
//         Homework 1
//         Alicia Rodriguez & Adam Levy
//         September 19, 2016
// ---------------------------------------------------------------


// 1. A fraction like 2/3 can be represented in F# as a pair of type int * int.
// Define infix operators .+ and .* to do addition and multiplication of fractions:

 //	> (1,2) .+ (1,3);;
 // val it : int * int = (5, 6)
 // > (1,2) .+ (2,3) .* (3,7);;
 // val it : int * int = (11, 14)

// Note that the F# syntax for defining such an infix operator looks like this:

 // let (.+) (a,b) (c,d) = ...

// Also note that .+ and .* get the same precedences as + and *, respectively,
// which is why the second example above gives the result it does.
// Finally, note that your functions should always return fractions in lowest terms.
// To implement this, you will need an auxiliary function to calculate the gcd (greatest common divisor)
// of the numerator and the denominator; this can be done very efficiently using Euclid's algorithm,
// which can be implemented in F# as follows:

 // let rec gcd = function
 // | (a,0) -> a
 // | (a,b) -> gcd (b, a % b)

// Solution:

 //


// 2. Write an F# function revlists xs that takes a list of lists xs and reverses all the sub-lists:

 // > revlists [[0;1;1];[3;2];[];[5]];;
 // val it : int list list = [[1; 1; 0]; [2; 3]; []; [5]]

// Hint: This takes just one line of code, using List.map and List.rev.

// Solution:

let revlists = List.map (fun xs -> List.rev xs: int list)


// 3. Write an F# function interleave(xs,ys) that interleaves two lists:

 // > interleave ([1;2;3],[4;5;6]);;
 // val it : int list = [1; 4; 2; 5; 3; 6]

// Assume that the two lists have the same length.

// Solution:

let rec interleave = function
  | ([],[]) -> []
  | (x::xs, y::ys) -> x::y::interleave(xs,ys);;

// 4. Write an F# function cut xs that cuts a list into two equal parts:

 // > cut [1;2;3;4;5;6];;
 // val it : int list * int list = ([1; 2; 3], [4; 5; 6])

// Assume that the list has even length.
// To implement cut, first define an auxiliary function gencut(n, xs) that cuts xs into two pieces,
// where n gives the size of the first piece:

 // > gencut(2, [1;3;4;2;7;0;9]);;
 // val it : int list * int list = ([1; 3], [4; 2; 7; 0; 9])

// Paradoxically, although gencut is more general than cut, it is easier to write!
// (This is an example of Polya's Inventor's Paradox: "The more ambitious plan may have more chances of success.")

// Another Hint: To write gencut efficiently, it is quite convenient to use F#'s local let expression
// (as in the cos_squared example in the Notes).

// Solution:

let rec gencut = function
  | (0,xs) -> ([],xs)
  | (n,x::xs) -> let i,j = gencut(n-1,xs)
                  (x::i,j);;

// 5. Write an F# function shuffle xs that takes an even-length list, cuts it into two equal-sized pieces,
// and then interleaves the pieces:

 // > shuffle [1;2;3;4;5;6;7;8];;
 // val it : int list = [1; 5; 2; 6; 3; 7; 4; 8]

// (On a deck of cards, this is called a perfect out-shuffle.)

// Solution:

 // ...

// 6. Write an F# function countshuffles n that counts how many calls to shuffle on a deck of n
// distinct "cards" it takes to put the deck back into its original order:

 // > countshuffles 4;;
 // val it : int = 2

// (To see that this result is correct, note that shuffle [1;2;3;4] = [1;3;2;4], and shuffle [1;3;2;4] = [1;2;3;4].)
// What is countshuffles 52?

// Hint: Define an auxiliary function countaux(deck, target) that takes two lists and returns the number of
// shuffles it takes to make deck equal to target.

// Solution:

 // ...
