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

val count_wildcards = g (fn() => 1) (fn x => 0)

val count_wild_and_variable_lengths = g (fn() => 1) (String.size)

fun count_some_var (p, str) =
    g (fn() => 1) (fn x => if x=str then 1 else 0) p

(* take a pattern and return a list of variable it contains *)
fun check_pat_helper1 p =
    case p of
        Variable x => [x]
        |TupleP ps => List.foldl (fn (p, lst) => (check_pat_helper1 p)@lst) [] ps
        |ConstructorP(_,p) =>  check_pat_helper1 p
        |_ => []

(* take a list and return if there are same elements *)
fun check_pat_helper2 lst =
  case lst of
    [] => false
    |head::[] => false
    |head::(neck::body) => head=neck andalso List.exists (fn x => x=neck) body

val check_pat = check_pat_helper2 o check_pat_helper1

(* take a valu*pattern pair and return match list option *)
fun match(v, p) =
    case (v, p) of
        (_, Wildcard) => SOME([])
        |(_, Variable s) => SOME([(s,v)])
        |(Unit, UnitP) => SOME([])
        |(Const, ConstP) => SOME([])
        |(Tuple vs, TupleP ps) => if (List.length vs) = (List.length vs)
                                  then all_answers match Listpair.zip(vs, ps)
                                  else NONE
        |(Constructor(s2,v), ConstructorP(s1,p)) => if s1 = s2
                                                    then match(v, p)
                                                    else NONE
        |_ => NONE

fun first_match(v, ps) =
    first_answer match ps v
    handle NoAnswer => NONE
