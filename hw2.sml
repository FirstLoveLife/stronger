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

fun officiate (cards, moves, goal) =
    let
        fun officiate_helper (heldcards, moves, currentscore : int , cards, goal) =
            let
                val sum = sum_cards heldcards
            in
                if sum > goal
                then score (heldcards, goal)
                else
                    case moves of
                        [] => score (heldcards, goal)
                      | (Discard card)::tl => officiate_helper (remove_card (heldcards, card, IllegalMove), tl, sum - card_value (card), cards, goal)
                      | (Draw)::restmoves =>
                        case cards of
                            [] => score (heldcards, goal)
                          | card::restcards => officiate_helper (card::heldcards, restmoves, currentscore, restcards, goal)
            end

    in
        officiate_helper ([], moves, 0, cards, goal)
    end

fun ace_number (cards) =
            case cards of
                [] => 0
             |  (_, Ace)::tl => 1 + ace_number tl
             | _::tl => ace_number tl

fun score_challenge (cards, goal) =
    let
        val sum1 = sum_cards cards
        val sum2 = sum_cards cards - (ace_number cards) * 10
        val prelim1 = if sum1 > goal then 3 * (sum1 - goal) else goal - sum1
        val prelim2 = if sum2 > goal then 3 * (sum2 - goal) else goal - sum2
        val score1 = if all_same_color cards then prelim1 div 2 else prelim1
        val score2 = if all_same_color cards then prelim2 div 2 else prelim2
        val score = if score1 < score2 then score1 else score2
    in
        score
    end

fun officiate_challenge (cards, moves, goal) =
    let
        fun officiate_helper (heldcards, moves, currentscore : int , cards, goal) =
            let
                val sum = sum_cards heldcards
            in
                if sum > goal
                then score_challenge (heldcards, goal)
                else
                    case moves of
                        [] => score_challenge (heldcards, goal)
                      | (Discard card)::tl => officiate_helper (remove_card (heldcards, card, IllegalMove), tl, sum - card_value (card), cards, goal)
                      | (Draw)::restmoves =>
                        case cards of
                            [] => score_challenge (heldcards, goal)
                          | card::restcards => officiate_helper (card::heldcards, restmoves, currentscore, restcards, goal)
            end

    in
        officiate_helper ([], moves, 0, cards, goal)
    end

(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

