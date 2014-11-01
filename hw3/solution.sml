(* Coursera Programming Languages, Homework 3 *)

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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1 only_capitals *)
val only_capitals = List.filter (fn s => Char.isUpper(String.sub(s, 0)))

(* 2 longest_string1 *)
val longest_string1 = List.foldl 
        (fn (x, y) => if String.size x > String.size y
                      then x 
                      else y) 
        ""
(* 3 longest_string2 *)
val longest_string2 = List.foldl 
        (fn (x, y) => if String.size x >= String.size y
                      then x 
                      else y) 
        ""
(* 4 longest_string higher order function *)
fun longest_string_helper f  = List.foldl
        (fn (x, y) => if f(String.size x, String.size y)
                      then x
                      else y)
        ""
val longest_string3 = longest_string_helper (fn (x,y) => x > y)
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

(* 5 longest_capitalized *)
val longest_capitalized = longest_string1 o only_capitals

(* 6 rev_string *)
val rev_string = String.implode o List.rev o String.explode

(* 7 first_answer *)
fun first_answer f lst = 
    case lst of
        [] => raise NoAnswer
      | x :: xs => case f x of
                        NONE => first_answer f xs
                      | SOME v => v

(* 8 all_answers *)
fun all_answers f lst = 
    let fun helper (remain_list, acc) = 
            case remain_list of
                [] => acc
              | x :: xs => case (f x, acc) of
                               (NONE, _) => NONE
                             | (SOME v, SOME lsts) => helper (xs, SOME (lsts @ v))
    in
        helper (lst, SOME [])
    end
                               
(* 9 pattern counting *)
val count_wildcards = g (fn () => 1) (fn s => 0)
val count_wild_and_variable_lengths = g (fn () => 1) String.size
fun count_some_var (str, pat) = g (fn () => 0) 
                                  (fn s => if s = str then 1 else 0)
                                  pat

(* 10 check_pat *)
fun check_pat p = 
    let 
        fun gather_var pat = 
            case pat of 
                Variable var => [var]
              | TupleP tp => (List.foldl (fn (pat, acc) => (gather_var pat) @ acc) [] tp)
              | _ => []
        
        fun distinct lst acc = 
            case lst of
                [] => true
              | x :: xs => not (List.exists (fn s => s = x) acc) andalso distinct xs (x :: acc)
        val var_list = gather_var p
    in
        distinct var_list []
    end
        
(* 11 match *)
fun match (v, p) =
    case (v, p) of 
        (_, Wildcard) => SOME []
      | (_, Variable s) => SOME [(s, v)]
      | (Unit, UnitP) => SOME []
      | (Const c, ConstP pc) => if c = pc then SOME [] else NONE
      | (Tuple val_list, TupleP ps) => all_answers match (ListPair.zip(val_list, ps))
      | (Constructor(s2, v1), ConstructorP(s1,p1)) => if s1=s2 then match(v1, p1) else NONE
      | (_, _) => NONE

(* 12 first_match *)
fun first_match v ps =
    SOME (first_answer (fn p => match(v,p)) ps)
    handle NoAnswer => NONE
