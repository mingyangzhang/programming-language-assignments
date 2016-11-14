(* Programming Language homework 3 *)
(* author: Mingyang Zhang *)

(* Returns a string list that has only the strings in the argument string list that start with an
uppercase letter. *)
fun only_capitals xs =
    List.filter (fn s => Char.isUpper(String.sub(s, 0))) xs

(* Returns the longest string of the string list. *)
fun string_compare (x, y) =
    if String.size x > String.size y then x
    else y

fun longest_string1 xs =
    List.foldl string_compare "" xs