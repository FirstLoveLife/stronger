
(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

fun only_capitals sl : string list =
    List.filter (fn s => Char.isUpper(String.sub(s, 0))) sl

fun longest_string1 sl =
    List.foldl (fn (s1, s2) => if (String.size(s1) > String.size(s2)) then s1 else s2) "" sl

fun longest_string2 sl =
    List.foldr (fn (s1, s2) => if (String.size(s1) > String.size(s2)) then s1 else s2)  "" sl
fun longest_string_helper f =
    if f (2, 1)
    then longest_string1
    else longest_string2

val longest_string3 = longest_string_helper (fn (a, b) => a > b)

val longest_string4 = longest_string_helper (fn (a, b) => a < b)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = implode o rev o explode

fun first_answer f list =
    case list of
        [] => raise NoAnswer
      | hd::tl =>
        case f hd of
            NONE => first_answer f tl
          | SOME ans => ans

(*I prefer nest cases(patterns) because it is pretty elegent in this case, though use @ and let is a little more efficient here*)
fun all_answers f list =
    case list of
        [] => SOME []
      | hd::tl =>
        case f hd of
            NONE => NONE
          | SOME list1  =>
            case all_answers f tl of
              NONE => NONE
                   | SOME list2 => SOME (list1@list2)

datatype pattern =
         Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu =
         Const of int
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

(* you can put all your code here *)

fun count_wildcards patt = g (fn x => 1) (fn y => 0) patt

fun count_wild_and_variable_lengths patt = g (fn x => 1) (fn x => String.size(x)) patt

fun count_some_var (str, patt) = g (fn x => 0) (fn y => if str = y then 1 else 0) patt

fun check_pat patt =
    let
    fun get_string_list patt =
        case patt of
            Wildcard => []
          | Variable x => [x]
          | TupleP ps => List.foldl (fn (p, i) => (get_string_list p) @ i) [] ps
          | ConstructorP(_,p) => get_string_list p
          | _ => []
    fun check string_list =
        case string_list of
            [] => true
         |  hd::tl =>
            if List.exists (fn x => x = hd) tl
            then false
            else true andalso check tl
    in
    (check o get_string_list) patt
    end

fun match (v, p) =
    case (v, p) of
        (_, Wildcard) => SOME []
      | (Unit, UnitP) => SOME []
      | (v, Variable s) => SOME [(s, v)]
      | (Const x, ConstP y) => if x = y then SOME [] else NONE
      | (Tuple vl, TupleP pl) => if length vl = length pl
                                 then all_answers match (ListPair.zip (vl, pl))
                                 else NONE
      | (Constructor (strv, valu), ConstructorP (strp, patt)) => if String.size(strv) = String.size(strp) then match (valu, patt) else NONE
      | (_, _) => NONE

fun first_match variable patternList =
    SOME (first_answer (fn pattern => match (variable, pattern)) patternList)
    handle NoAnswer => NONE

(* for the challenge problem only *)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(* Homework3 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test11 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1 = only_capitals ["a","b","c"] = []

val test20 = longest_string1 [] = ""
val test2 = longest_string1 ["A","bc","C"] = "bc"
val test22 = longest_string1 ["hahaha", "A","bc","C"] = "hahaha"
val test222= longest_string1 ["A","bc","C", "hahaha"] = "hahaha"
val test2222 = longest_string1 ["A","bc", "ef", "C"] = "bc"

val test33 = longest_string2 ["A","bc","C"] = "bc"
val test3 = longest_string2 ["A","bc", "ef", "C"] = "ef"
val test333 = longest_string2 ["ef", "A","bc","C"] = "bc"
val test3333 = longest_string2 ["A","bc","C", "ef"] = "ef"
val test33333 = longest_string2 ["A","B","C"] = "C"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"
val test55 = longest_capitalized ["A","bc","C", "Ab"] = "Ab"
val test455 = longest_capitalized ["A","Cd", "bc","C", "Ab"] = "Cd"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,3,3,4,5] = 4
val test7i = first_answer (fn x => if x > 3 then SOME x else NONE) [1,3, 9,3,4,5] = 9

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1

val test100 = check_pat (TupleP [Variable("x"), Variable("y")]) = true
val test1000 = check_pat (TupleP [Variable("y"), Variable("y")]) = false
val test10 = check_pat (TupleP [Variable("x"), Variable("y"), ConstructorP("Unrelate", Variable("z"))]) = true
val test10000 = check_pat (TupleP [Variable("x"), Variable("y"), ConstructorP("Unrelate", Variable("x"))]) = false

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []
