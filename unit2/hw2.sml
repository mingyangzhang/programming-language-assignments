(* homework 2 *)

(* ex.1 *)
(* Remove string from string list. *)
fun all_except_option(str, str_list) =
    case str_list of
        [] => NONE
        |x::xs' => if same_string(x, str)
                   then SOME(xs')
                   else if isSome(all_except_option(str, xs'))
                        then SOME(x::valOf(all_except_option(str, xs')))
                        else NONE

(* get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") *)
(* answer: ["Fredrick","Freddie","F"] *)
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
                           then get_substitutions(xs', s, init@valOf(all_except_option(s, x)))
                           else get_substitutions(xs', s, init)
    in
        get_substitutions(substitutions, s, [])
    end

(* full names *)
fun similar_names(substitutions, full_name) =
    let val first_names = get_substitutions2(substitutions, #first full_name)
    in
        let fun full_names(first_name_list, name) =
            case first_name_list of
                [] => [name]
                |x::xs' => full_names(xs', name)@[{first=x, middle=(#middle name), last=(#last name)}]
        in
            full_names(first_names, full_name)
        end
    end

(* ex.2 *)

(* get color of a card *)
fun card_color (c:card) =
    case (#1 c) of
        Clubs => Black
        |Spades => Black
        |_ => Red

(* get value of a card *)
fun card_value (c:card) =
    case (#2 c) of
        Num i => i
        |Ace => 11
        |_ => 10

(* remove a card from a card list *)
fun remove_card(cs, c, e) =
    let fun all_except_option(c, cs) =
            case cs of
                [] => NONE
                |x::xs' => if x = c
                           then SOME(xs')
                           else if isSome(all_except_option(c, xs'))
                                then SOME(x::valOf(all_except_option(c, xs')))
                                else NONE
    in
        let
            val result = all_except_option(c, cs)
        in
            if isSome(result)
            then valOf result
            else raise e
        end
    end

(* if color of all cards is the same *)
(* nest pattern *)
fun all_same_color cs =
    case cs of
        [] => true
        |head::[] => true
        |head::(neck::rest) => (card_color(head) = card_color(neck)) andalso all_same_color(rest)


(* return sum of cards *)
(* tail recursion *)
fun sum_cards cs =
    let fun sum_card_with_init(cs, init) =
            case cs of
                [] => 0 + init
                |c::cs' => sum_card_with_init(cs', init+card_value(c))
    in
        sum_card_with_init(cs, 0)
    end

(* calculate the score of a card list *)
fun score(cs, goal) =
    let
        val sum = sum_cards(cs)
    in
        case sum > goal of
            true => 3 * sum
            |false => case all_same_color(cs) of
                            true => sum div 2
                            |false => sum
    end

(* run the card game *)
fun officiate(cs, mv, goal) =
    let fun game_state(cs, mv, goal, hcs) =
            case (mv, sum_cards(cs) > goal) of
                ([], false) => score(cs, goal)
                |(m::mv', false) case m of
                    |(Draw,  => case cs of
                        [] => score(cs, goal)
                        c::cs' => game_state(cs', mv', goal, c::hcs)
                    |Discard d => game_state(cs, mv', goal, remove_card(hcs, d, IllegalMove))
                |(_, true) => sore(cs, goal)
    in
        game_state(cs, mv, goal, [])
    end
