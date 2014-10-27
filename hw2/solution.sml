(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string (s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* q1.a *)
fun all_except_option (target : string, strs : string list) = 
    case strs of
        [] => NONE
      | xs :: xs' => if same_string(target, xs)
                    then SOME xs'
                    else case all_except_option(target, xs') of
                             NONE => NONE
                           | SOME lst => SOME (xs :: lst)

(* q1.b *)
fun get_substitutions1 (lsts : string list list, target : string) = 
    case lsts of 
        [] => []
      | xs :: xs' => case all_except_option (target, xs) of
                         NONE => get_substitutions1(xs', target)
                       | SOME lst => lst @ get_substitutions1 (xs', target)

(* q1.c *)
fun get_substitutions2 (lsts : string list list, target : string) =
    let fun helper (remain_list : string list list, acc : string list) = 
        case remain_list of
            [] => acc
          | xs :: xs' => case all_except_option(target, xs) of
                             NONE => helper (xs', acc)
                           | SOME lst => helper (xs', acc @ lst)
    in
        helper(lsts, [])
    end

(* q1.d *)
fun similar_names (lsts, {first=f, last=l, middle=m}) =
    let fun helper (remain_subs, acc) =
            case remain_subs of
                [] => {first=f, middle=m, last=l} :: acc
              | xs :: xs' => helper (xs', acc @ [{first=xs, middle=m, last=l}])
    in
        helper (get_substitutions2(lsts, f), [])
    end
        

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* q2.a *)
fun card_color (c : card) =
    case c of 
        (Clubs, _) => Black
      | (Spades, _) => Black
      | (_, _) => Red

(* q2.b *)
fun card_value (c : card) =
    case c of 
        (_, Ace) => 11
      | (_, Num v) => v
      | (_, _) => 10 

(* q2.c *)
fun remove_card (lsts : card list, target : card, e) =
    let fun helper (remain_list, acc) = 
            case remain_list of
                [] => raise e
             | xs :: xs' => if target = xs 
                            then acc @ xs'
                            else helper(xs', acc @ [xs])
    in
        helper (lsts, [])
    end

(* q2.d *)
fun all_same_color (lsts : card list) =
    case lsts of
        [] => true
      | xs :: [] => true
      | xs :: xs' :: xs'' => card_color (xs) = card_color (xs') andalso all_same_color(xs' :: xs'')

(* q2.e *)
fun sum_cards (lsts : card list) =
    let fun helper (remain_list, acc) =
            case remain_list of
                [] => acc
             | xs :: xs' => helper(xs', acc + card_value(xs))
    in
        helper (lsts, 0)
    end


fun score (lsts : card list, goal : int) =
    let 
        val sum = sum_cards(lsts)
        val pre_score = if sum > goal 
                        then 3 * (sum - goal)
                        else goal - sum
    in
        if all_same_color (lsts) 
        then pre_score div 2
        else pre_score
    end

fun officiate (card_list : card list, move_list : move list, goal : int) =
    let fun helper (remain_cards, remain_moves, held_cards, sum) =
            case (remain_cards, remain_moves) of
                ([], _) => score (held_cards, goal)
              | (_, []) => score (held_cards, goal)
              | (c :: cs, Discard dis_card :: ms) => helper(cs, ms, remove_card(held_cards, dis_card, IllegalMove), sum - card_value(c))
              | (c :: cs, Draw :: ms) => if sum + card_value(c) > goal
                                         then score (c :: held_cards, goal)
                                         else helper(cs, ms, c :: held_cards, sum + card_value(c))
    in
        helper (card_list, move_list, [], 0)
    end
