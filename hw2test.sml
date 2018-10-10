val test1 = all_except_option ("string", ["string"]) = SOME []
val test11 = all_except_option ("string", ["string", "apple"]) = SOME ["apple"]
val test1111 = all_except_option ("string", ["str"]) = NONE
val test11111 = all_except_option ("string", ["string", "banana", "apple"]) = SOME ["banana", "apple"]

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test22 = get_substitutions1 ([["foo", "bbb"],["there"]], "foo") = ["bbb"]
val test222 = get_substitutions1 ([["foo"],["there", "ttt"]], "foo") = []
val test2222 = get_substitutions1 ([["aaa", "bbb", "foo", "eee"],["there"]], "foo") = ["aaa", "bbb", "eee"]

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

val test10t = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test100t = score ([(Hearts, Num 2),(Clubs, Num 4)],6) = 0
val test1000tt = score ([(Hearts, Num 2),(Hearts, Num 4)],10) = 2
val test10000tt = score ([(Hearts, Num 2),(Clubs, Num 10)],10) = 6
val test100000ttt = score ([(Hearts, Num 2),(Diamonds, Num 10)],10) = 3
val test1000000tttt = score ([(Hearts, Num 2),(Diamonds, Num 10)],20) = 4




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

val test10c = score_challenge ([(Hearts, Ace),(Clubs, Num 4)],10) = 5
val test100cc = score_challenge ([(Hearts, Ace),(Clubs, Num 4)],6) = 1
val test1000ccc = score_challenge ([(Hearts, Ace),(Hearts, Num 4)],10) = 5 div 2
val test10000cccc = score_challenge ([(Hearts, Num 2),(Diamonds, Num 10)],10) = 3

val test_officchallenge12_challenge11 = officiate_challenge ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test_officchallenge12_challenge12 = officiate_challenge ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        5)
             = 1

val test_officchallenge12_challenge13 = ((officiate_challenge([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false)
              handle IllegalMove => true)
