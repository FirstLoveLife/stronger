(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
fun all_except_option (except : string, strList : string list) =
    case strList of
        [] => NONE
      | str::tl =>
        case same_string (str, except) of
            true => SOME tl
          | false =>
            case all_except_option (except, tl) of
                NONE => NONE
              | SOME list=> SOME (str::list)

fun get_substitutions1 (lists : string list list, name : string) =
    case lists of
        [] => []
      | list::lists =>
        case all_except_option (name, list) of
            NONE => get_substitutions1 (lists, name)
          | SOME filteredlist => filteredlist @ get_substitutions1(lists, name)

fun get_substitutions2 (lists : string list list, name : string) =
    let
        fun get_substitutions2_helper (lists : string list list, name : string, ans : string list) =
            case lists of
                [] => ans
              | list::lists =>
                case all_except_option (name, list) of
                    NONE => get_substitutions2_helper (lists, name, ans)
                  | SOME filteredlist => get_substitutions2_helper (lists, name, ans @ filteredlist)
    in
        get_substitutions2_helper (lists, name, [])
    end

fun similar_names (firstnames: string list list, {first, last, middle}) =
    let
        fun similar_name_helper (firstnames : string list) =
            case firstnames of
                [] => []
             |  hd::tl => {first = hd, last = last, middle = middle} :: (similar_name_helper tl)

    in
        {first = first, last = last, middle = middle}::similar_name_helper (get_substitutions2(firstnames, first))
    end

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove


fun card_color (Spades, _) = Black
  | card_color (Clubs, _) = Black
  | card_color (Diamonds, _) = Red
  | card_color (Hearts, _) = Red

fun card_value (_, Ace) = 11
  | card_value (_, Num n) = n
  | card_value (_, _) = 10

fun remove_card (cards : card list, card, exn) =
    case cards of
        [] => raise exn
      | hd::tl =>
        if hd = card
        then tl
        else hd::remove_card (tl, card, exn)

fun all_same_color (cards : card list) =
    case cards of
        [] => true
     |  (_, _)::[] => true
     |  card1::(card2::rest) => (card_color card1) = (card_color card2) andalso all_same_color (card2::rest)


fun sum_cards (cards) =
    let
        fun sum_cards_helper (cards, ans) =
            case cards of
                [] => ans
              | card::hd => sum_cards_helper (hd, card_value (card) + ans)
    in
        sum_cards_helper (cards, 0)
    end

fun score (cards, goal) =
    let
        val sum = sum_cards cards
        val prelim = if sum > goal then 3 * (sum - goal) else goal - sum
        val score = if all_same_color cards then prelim div 2 else prelim
    in
        score
    end

fun officiate (cards, moves) =
    let
        fun officiate_helper (heldcards, moves, score, cards) =
            case moves of
                [] => score
              | Discard card => remove_card (heldcards, card)
              | Draw =>
                case

(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = all_except_option ("string", ["string"]) = SOME []
val test11 = all_except_option ("string", ["string", "apple"]) = SOME ["apple"]
val test1111 = all_except_option ("string", ["str"]) = NONE
val test11111 = all_except_option ("string", ["string", "banana", "apple"]) = SOME ["banana", "apple"]

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test22 = get_substitutions1 ([["foo", "bbb"],["there"]], "foo") = ["bbb"]
val test222 = get_substitutions1 ([["foo"],["there", "ttt"]], "foo") = []
val test2222 = get_substitutions1 ([["aaa", "bbb", "foo", "eee"],["there"]], "foo") = ["aaa", "bbb", "eee"
 val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test33 = get_substitutions2 ([["foo", "bbb"],["there"]], "foo") = ["bbb"]
val test333 = get_substitutions2 ([["foo"],["there", "ttt"]], "foo") = []
val test3333 = get_substitutions2 ([["aaa", "bbb", "foo", "eee"],["there"]], "foo") = ["aaa", "bbb", "eee"]

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]


val test5 = card_color (Clubs, Num 2) = Black
val test55 = card_color (Spades, Num 2) = Black

val test555 = card_color (Hearts, Num 2) = Red
val test5555 = card_color (Diamonds, Num 2) = Red
val test66 = card_value (Clubs, Num 2) = 2
val test666 = card_value (Clubs, Ace) = 11
val test6 = card_value (Clubs, Num 10) = 10
val test6666 = card_value (Clubs, Jack) = 10

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test77 = remove_card ([(Hearts, Ace), (Hearts, Ace), (Diamonds, Num 4)], (Hearts, Ace), IllegalMove) = [(Hearts, Ace), (Diamonds, Num 4)]
val test777 = remove_card ([(Clubs, Jack), (Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Clubs, Jack)]
val test7777 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test88 = all_same_color [(Hearts, Jack), (Hearts, Ace)] = true
val test88888 = all_same_color [(Spades, Jack), (Clubs, Ace)] = true
val test888888 = all_same_color [(Hearts, Jack), (Diamonds, Ace)] = true
val test888 = all_same_color [(Clubs, Num 10), (Hearts, Ace)] = false
val test8888 = all_same_color [(Hearts, Ace), (Spades, Num 2)] = false

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test99 = sum_cards [(Clubs, Num 2),(Clubs, Ace)] = 13
val test999 = sum_cards [(Clubs, Num 2),(Clubs, Jack)] = 12
val test9999 = sum_cards [(Clubs, Num 2),(Clubs, Num 9)] = 11

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test100 = score ([(Hearts, Num 2),(Clubs, Num 4)],6) = 0
val test1000 = score ([(Hearts, Num 2),(Hearts, Num 4)],10) = 2
val test10000 = score ([(Hearts, Num 2),(Clubs, Num 10)],10) = 6




val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false)
              handle IllegalMove => true)
