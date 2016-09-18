
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

//Auxillary gcd function with pattern matching syntax
 let rec gcd = function
  |(a,0) -> a
  |(a,b) -> gcd (b, a % b)

//Fraction addition function
(*The function multiplies the numerators by the opposite fraction's denominators.This is done because the denominators will also be multiplied
  by eachother so they can be the same. Once multiplied, the numerators are added together in the first tuple and divided by the gcd.
  The denominators are multiplied in the second tuple since the fraction needs to have a common denominator. For the gcd, the calculations
  are repeated so that the gcd can be found for the two fractions. Once the gcd is found, the numerator and denominator are divided by the
  gcd so that the fraction can be returned in lowest terms. *)
let (.+) (a,b) (c,d) =  ((a * d + c * b) / gcd(a * d + c * b , b * d)), ((b * d) / gcd(a * d + c * b , b * d))

//Fraction multiplication function
(*The numerators are multiplied together and divided by the gcd. The same process is done in the second tuple for the denominator.
  To find the gcd, the calculations are repeated for the gcd function. The numerator and denominator are each multiplied by the gcd to
  return the function in lowest terms. *)
let (.*) (a,b) (c,d) = (a * c / gcd (a * c, b * d), b * d / gcd (a * c, b * d))



// 2. Write an F# function revlists xs that takes a list of lists xs and reverses all the sub-lists:

 // > revlists [[0;1;1];[3;2];[];[5]];;
 // val it : int list list = [[1; 1; 0]; [2; 3]; []; [5]]

// Hint: This takes just one line of code, using List.map and List.rev.

// Solution:

//revlist function
(*List.map is called on the List.rev function, which takes a list xs on a parameter. 
List.rev is mapped to every list element of the list list.*)
let revlists xs = List.map List.rev xs 


// 3. Write an F# function interleave(xs,ys) that interleaves two lists:

 // > interleave ([1;2;3],[4;5;6]);;
 // val it : int list = [1; 4; 2; 5; 3; 6]

// Assume that the two lists have the same length.

// Solution:

//interleave function using pattern matching syntax. 
(*Assuming the lists are of the same length, the base case returns an empty list if the first list is empty. The recursive case uses the cons
  operation on ys with the next iteration of the function. This is then consed with xs. It is important to note that not all cases are covered. 
  However, since we can assume both lists are the same length they don't need to be. To avoid the warning you can replace the base case with
  the following base cases:
  |(x::xs, []) -> [x]
  |([], y::ys) -> [y]
  |([], []) -> []
 *)
let rec interleave  = function
|([], _) -> []
|(x::xs, y::ys) -> x :: y :: interleave(xs,ys)

//interleave function without pattern matching syntax.
(*First checks if either of the lists are empty (in reality only one needs to be checked since they are the same length, 
  but an error is thrown if you do not imply that both inputs are actually lists). If they are not empty, The recursive case uses the cons
  operation on ys with the next iteration of the function. This is then consed with xs.*)
let rec interleave2 (xs,ys) = if List.length xs = 0  || List.length ys = 0 then ys else xs.Head :: ys.Head :: interleave2(xs.Tail, ys.Tail)


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

//Source cited: http://stackoverflow.com/questions/8644485/extracting-first-element-from-the-tuple-using-fst-throwing-an-error-type-mism
let gencut (n,ys) =
    let rec splitList = function
        |(ys, zs, 0) -> (ys, zs, 0)
        |(y::ys, [], n) -> splitList(ys, [y], n - 1)
        |(y::ys, zs, n) -> splitList(ys, y::zs, n - 1)
    let list = splitList (ys, [],n)
    let first, _, _ = list
    let _, second, _ = list
    (List.rev second, first)

let cut xs = gencut(List.length xs / 2, xs)

// 5. Write an F# function shuffle xs that takes an even-length list, cuts it into two equal-sized pieces,
// and then interleaves the pieces:

 // > shuffle [1;2;3;4;5;6;7;8];;
 // val it : int list = [1; 5; 2; 6; 3; 7; 4; 8]

// (On a deck of cards, this is called a perfect out-shuffle.)

// Solution:


let shuffle xs = 
    let list = cut xs
    interleave (fst list, snd list)

// 6. Write an F# function countshuffles n that counts how many calls to shuffle on a deck of n
// distinct "cards" it takes to put the deck back into its original order:

 // > countshuffles 4;;
 // val it : int = 2

// (To see that this result is correct, note that shuffle [1;2;3;4] = [1;3;2;4], and shuffle [1;3;2;4] = [1;2;3;4].)
// What is countshuffles 52?

// Hint: Define an auxiliary function countaux(deck, target) that takes two lists and returns the number of
// shuffles it takes to make deck equal to target.

// Solution:

let rec countaux (deck, target) = if deck = target then 0 else 1 + countaux(shuffle deck, target)

let countshuffles n = 
    let original = [1..n] in
     1 + countaux(shuffle original, original)
