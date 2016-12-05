(* Programming Language homework 3 *)
(* author: Mingyang Zhang *)

exception NoAnswer

datatype pattern = Wildcard
         | Variable of string
         | UnitP
         | ConstP of int
         | TupleP of pattern list
         | ConstructorP of string * pattern

datatype valu = Const of int
          | Unit
          | Tuple of valu list
          | Constructor of string * valu

fun g f1 f2 p =
    let
    val r = g f1 f2
    in
    case p of
        Wildcard          => f1 ()
      | Variable x        => f2 x
      | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
      | ConstructorP(_,p) => r p
      | _                 => 0
    end

(**** you can put all your code here ****)

(* Returns a string list that has only the strings in the argument string list that start with an
uppercase letter. *)
fun only_capitals xs =
    List.filter (fn s => Char.isUpper(String.sub(s, 0))) xs

(* Returns the longest string of the string list. *)
(* Return the longer string*)
fun string_compare1 (x, y) =
    if String.size x > String.size y then x
    else y

(* Return the the string closest to the beginning of the list in case of ties*)
fun longest_string1 xs =
    List.foldl string_compare1 "" xs

(* Return the longer or equivalent string*)
fun string_compare2 (x, y) =
    if String.size x >= String.size y then x
    else y

(* Return the the string closest to the end of the list in case of ties*)
fun longest_string2 xs =
    List.foldl string_compare2 "" xs

(* type (int * int -> bool) -> string list -> string *)
fun longest_string_helper comp xs =
    let fun lsh_with_init comp xs init =
	    case xs of
	       [] => init
	       |x::xs' => if comp(String.size x, String.size init)
			  then lsh_with_init comp xs' x
			  else lsh_with_init comp xs' init
    in
	lsh_with_init comp xs ""
    end 	

(* longest_string1 *)
val longest_string3 = longest_string_helper (fn (x, y) => x>y);

(* longest_string2 *)
val longest_string4 = longest_string_helper (fn (x, y) => x>=y);

(* funcs combining *)
val longest_capitalized = longest_string1 o only_capitals;

val rev_string = String.implode o List.rev o String.explode;

