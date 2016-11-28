(* homework 2 *)

(* Remove string from string list. *)
fun all_except_option(str, str_list) =
    case str_list of
        [] => NONE
        |x::xs' => if same_string(x, str) then xs' else SOME(x::all_except_option(str, xs'))

(* get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred")
answer: ["Fredrick","Freddie","F"] *)
fun get_substitutions1(substitutions, s) =
    case substitutions of
        [] => []
        |x::xs' => if isSome(all_except_option(s, x))
                   then valOf(all_except_option(s, x))@get_substitutions1(xs', s)
                   else get_substitutions1(xs', s)

(* Same as above, using tail recursion *)
fun get_substitutions2(substitutions, s) =
    let fun get_substitutions(substitutions, s, init) =
            case substitutions of
                [] => init
                |x::xs' => if isSome(all_except_option(s, x))
                           then get_substitutions1(xs', s, init@valOf(all_except_option(s, x)))
                           else get_substitutions1(xs', s, init)
    in
        get_substitutions(substitutions, s, [])
    end

(* full names *)
fun similar_names(substitutions, full_name) =
    let val first_names = get_substitutions2(substitutions, #first full_name)
        let fun full_names(first_name_list, name) =
            case first_name_list of
                [] => [name]
                |x::xs' => full_names(xs', name)::{first=x, middle=#middle name,last=#last name}
        in
            full_names(first_names, full_name)
        end
    in
    end