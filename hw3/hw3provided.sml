(* Coursera Programming Languages, Homework 3, Provided Code *)

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

(*#1*)
fun only_capitals strings = List.filter (fn x => Char.isUpper(String.sub(x,0)) ) strings

(*#2*)
fun longest_string1 strings = List.foldl (fn (x,y) => if (String.size(x) > String.size(y)) then x else y) ("") strings

(*#3*)
fun longest_string2 strings = longest_string1(List.rev strings)

(*#4*)
fun longest_string_helper int_predicate strings =
	List.foldl (fn (x,y) => if (int_predicate(String.size(x),String.size(y))) then x else y) ("") strings

val longest_string3 = longest_string_helper(fn(x,y) => x > y)

val longest_string4 = longest_string_helper(fn(x,y) => x >= y)

(*#5*)
val longest_capitalized = longest_string2 o only_capitals

(*#6*)
val rev_string = String.implode o List.rev o String.explode 

(*#7*)
fun first_answer f_option xa =
	case xa of 
		[] => raise NoAnswer
		| a :: xa' => 
			case f_option a of
				SOME v => v
				| _ => first_answer f_option xa'

(*#8*)
fun all_answers f_option xa =
	let 
		fun all_answers_aux (f_option, xa, some_acc) =
			case xa of
			[] => SOME some_acc
			| a :: xa' => 
				case f_option a of
					SOME v => all_answers_aux (f_option,xa',some_acc @ v)
					| NONE => NONE
	in
		all_answers_aux(f_option, xa, [])
	end

(*#9a*)
val count_wildcards = g (fn () => 1) (fn _ => 0)

(*#9b*)
val count_wild_and_variable_lengths = g (fn () => 1) (fn str => String.size str)

(*#9c*)
fun count_some_var (str, p) = g (fn () => 0) (fn str' => if (str' = str) then 1 else 0 ) p

(*#10*)
fun check_pat p =
	let
		fun variable_strings p =
			case p of
				Variable v => [v]
				| TupleP ps => List.foldl( fn (p, acc) => acc @ variable_strings(p) ) [] ps
				| ConstructorP(_,p) => variable_strings p
				| _ => []

		fun has_duplicates strings =
			case strings of
				[] => true
				| string :: [] => true
				| string :: strings' => if (List.exists (fn x => x = string) strings') 
											then false
											else has_duplicates(strings') 
	in
	   has_duplicates(variable_strings(p))
	end

(*#11*)
fun match (valu, pattern) =
	case (valu, pattern) of
		(Unit, UnitP) => SOME []
		| (Const int, ConstP int_p) => if (int = int_p) then SOME [] else NONE
		| (_, Wildcard) => SOME []
		| (v, Variable str) => SOME [(str,v)]
		| (Tuple valus, TupleP patterns) => 
			if (List.length(valus) = List.length(patterns)) 
			then all_answers match (ListPair.zip(valus, patterns))
			else NONE
		| (Constructor (str_v, valu), ConstructorP (str_p, pattern)) => 
			if (str_v = str_p) then match(valu, pattern) else NONE
		| _ => NONE

(*#12*)
fun first_match valu patterns =
	SOME(first_answer match (List.map (fn p => (valu, p)) patterns)) handle NoAnswer => NONE