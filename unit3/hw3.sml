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


(**** you can put all your code here ****)

(* Returns a string list that has only the strings in the argument string list that start with an
uppercase letter. *)
fun only_capitals xs =
    List.filter (fn s => Char.isUpper(String.sub(s, 0))) xs

(* Return the the string closest to the beginning of the list in case of ties*)
fun longest_string1 xs =
    List.foldl (fn(x, y) => if String.size x > String.size y then x else y) "" xs

(* Return the the string closest to the end of the list in case of ties*)
fun longest_string2 xs =
    List.foldl (fn(x, y) => if String.size x >= String.size y then x else y) "" xs

(* type (int * int -> bool) -> string list -> string *)
fun longest_string_helper comp xs =
    List.foldl (fn(x, y) => if comp(String.size x, String.size y) then x else y) "" xs

(* longest_string1 *)
val longest_string3 = longest_string_helper (fn (x, y) => x>y);

(* longest_string2 *)
val longest_string4 = longest_string_helper (fn (x, y) => x>=y);

(* funcs combining *)
val longest_capitalized = longest_string1 o only_capitals;

val rev_string = String.implode o List.rev o String.explode;

(* apply to a list and return the first answer *)
fun first_answer f lst =
    case lst of
        [] => raise NoAnswer
        |x::xs' => if isSome(f x)
                   then valOf(f x)
                   else first_answer f xs'

fun all_answers f lst =
    case lst of
        [] => NONE
        |x::xs => if isSome(f x)
                  then let val tail_result = all_answers f xs
                       in
                            if isSome(tail_result)
                            then SOME(valOf(f x)@valOf(tail_result))
                            else f x
                       end
                  else all_answers f xs


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

val count_wildcards = g (fn => 1) (fn x => 0)

val count_wild_and_variable_lengths = g (fn => 1) (String.size)

fun count_some_var (p, str) =
    g (fn => 1) (fn x => if x=str then 1 else 0) p




