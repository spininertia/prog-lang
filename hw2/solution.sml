(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_exception_option (target : string, strs : string list) = 
    case strs of
        [] => NONE
      | xs :: xs' => if same_string(target, xs)
                    then SOME xs'
                    else case all_exception_option(target, xs') of
                             NONE => NONE
                           | SOME lst => SOME (xs :: lst)


fun get_substitutions1 (lsts : string list list, target : string) = 
    case lsts of 
        [] => []
      | xs :: xs' => case all_exception_option (target, xs) of
                         NONE => get_substitutions1(xs', target)
                       | SOME lst => lst @ get_substitutions1 (xs', target)

fun get_substitutions2 (lsts : string list list, target : string) =
    let fun helper (remain_list : string list list, acc : string list) = 
        case remain_list of
            [] => acc
          | xs :: xs' => case all_exception_option(target, xs) of
                             NONE => helper (xs', acc)
                           | SOME lst => helper (xs', acc @ lst)
    in
        helper(lsts, [])
    end

fun similar_names (lsts, {first=f, last=l, middle=m}) =
    let fun helper (remain_subs, acc) =
            case remain_subs of
                [] => {first=f, middle=m, last=l} :: acc
              | xs :: xs' => helper (xs', {first=xs, middle=m, last=l} :: acc)
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
