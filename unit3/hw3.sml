(* Programming Language homework 3 *)
(* author: Mingyang Zhang *)

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
    List.foldl string_compare "" xs

(* Return the longer or equivalent string*)
fun string_compare2 (x, y) =
    if String.size x > String.size y then x
    else y

(* Return the the string closest to the end of the list in case of ties*)
fun longest_string2 xs =
    List.foldl string_compare2 "" xs

fun longest_string_helper