fun is_older (date1 : int * int * int, date2 : int * int * int) =
    let
        fun cal (date : int * int * int) =
            (#1 date) * 366 + (#2 date) * 31 + (#3 date)
    in
        cal(date1) < cal(date2)
    end

fun number_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else if month = (#2 (hd dates))
    then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month)


fun number_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else number_in_month (dates, hd months) + number_in_months (dates, tl months)

fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else if month = (#2 (hd dates))
    then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)


fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else dates_in_month (dates, hd months) @ dates_in_months (dates, tl months)

fun get_nth (list , n : int) =
    if n = 1
    then hd list
    else get_nth (tl list, n - 1)

fun date_to_string (year : int, month : int, day : int) =
    let
        val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth (months, month) ^ " " ^ Int.toString day ^ ", " ^ Int.toString year
    end


fun number_before_reaching_sum (sum : int, numbers : int list) =
    let
        fun number_before_reaching_sum_helper (sum : int, numbers : int list, now : int, ans : int) =
            if now + hd numbers >= sum
            then ans
            else number_before_reaching_sum_helper (sum, tl numbers, now + hd numbers, ans + 1)
    in number_before_reaching_sum_helper (sum, numbers, 0, 0)
    end

fun what_month (date : int) =
    let
        val months = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
        val daysOfMonths = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        get_nth(months, (number_before_reaching_sum (date, daysOfMonths)) + 1)
    end

fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month (day1) :: month_range (day1 + 1, day2)

fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else
        let
            fun oldest_helper (dates : (int * int * int) list) =
                if null (tl dates)
                then hd dates
                else
                    let val next = oldest_helper (tl dates)
                    in
                        if is_older (hd dates, next)
                        then hd dates
                        else next
                    end
        in
            SOME (oldest_helper dates)
        end

fun exist (list, elem) =
    if null list
    then false
    else
        let
            val res = (elem = (hd list))
        in
            res orelse exist (tl list, elem)
        end

fun remove_duplicates (list) =
    if null list
    then []
    else if exist(tl list, hd list)
    then remove_duplicates (tl list)
    else hd list :: remove_duplicates (tl list)

fun number_in_months_challenge (dates : (int * int * int) list, months : int list) =
    number_in_months (dates, remove_duplicates (months))

fun dates_in_months_challenge (dates : (int * int * int) list, months : int list) =
    dates_in_months (dates, remove_duplicates (months))

fun reasonable_date (year : int, month : int, day : int) =
    if (month > 12 orelse month < 1 orelse day < 1 orelse day > 31)
    then false
    else
        let
            val monthOfNonLeap = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
            val monthOfLeap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
            fun leap (year : int) =
                (year mod 400) = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
        in
            if leap year
            then
                if (day > (get_nth (monthOfLeap, month)))
                then          false
                else true
            else
                if (day > (get_nth (monthOfNonLeap, month)))
                then       false
                else true
        end




val test1 = is_older ((1,2,3),(2,3,4)) = true

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
