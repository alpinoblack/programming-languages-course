type date = int * int * int

(*#1*)
fun is_older (date1:date, date2:date) = 
    let val (year1: int, month1: int, day1: int) = date1
        val (year2: int, month2: int, day2: int) = date2
    in
        (year1 < year2) orelse 
        (year1 = year2 andalso month1 < month2) orelse
        (year1 = year2 andalso month1 = month2 andalso day1 < day2)
    end

(*#2*)
fun number_in_month(dates: date list, month: int) =
    let fun count_month_in_dates (dates: date list, month: int, agg: int) =
	    if null dates then agg
        else if #2 (hd(dates)) = month then count_month_in_dates(tl(dates), month, agg + 1)
        else count_month_in_dates(tl(dates), month, agg)
    in
	    count_month_in_dates(dates, month, 0)
    end

(*#3*)
fun number_in_months(dates: date list, months: int list) =
    let fun count_months_in_dates(dates: date list, months: int list, agg: int) =
        if null months then agg
        else count_months_in_dates(dates, tl(months), agg + number_in_month(dates, hd(months)))
    in
        count_months_in_dates(dates, months, 0)
    end
 
(*#4*)
fun dates_in_month(dates: date list, month: int) =
    let fun aggregate_dates_in_month(dates: date list, months: int, agg: date list) =
        if null dates then rev agg
        else if #2 (hd(dates)) = month then aggregate_dates_in_month(tl(dates), month, hd(dates) :: agg)
        else aggregate_dates_in_month(tl(dates), month, agg)
    in
        aggregate_dates_in_month(dates, month, [])
    end

(*#5*)
fun dates_in_months(dates: date list, months: int list) =
    let fun aggregates_dates_in_months(dates: date list, months: int list, agg: date list) =
        if null months then agg
        else aggregates_dates_in_months(dates, tl(months), agg @ dates_in_month(dates, hd(months)))
    in
        aggregates_dates_in_months(dates, months, [])
    end

(*#6*)
fun get_nth(strings: string list, elemNum: int) =
    if (elemNum = 1) then hd strings
    else get_nth(tl strings, elemNum - 1)

(*#7*)
fun date_to_string(date: date) =
    let val listOfMonths = ["January", "February", "March", "April",
        "May", "June", "July", "August", "September", "October", "November", "December"]
        val (year, month, day) = date
    in
        get_nth(listOfMonths,month)^" "^Int.toString(day)^", "^Int.toString(year)
    end

(*#8*)
fun number_before_reaching_sum(sum: int, numbers: int list) =
    let fun sum_partitions(sum: int, numbers: int list, currentSum: int ,pivot: int) =
            if null numbers 
            then raise Fail ("end of the list")
            else if currentSum < sum andalso currentSum + hd(numbers) >= sum then pivot
            else sum_partitions(sum, tl(numbers), currentSum + hd(numbers) , pivot + 1)
    in
        sum_partitions(sum, numbers, 0, 0)
    end

val commonYear = [31,28,31,30,31,30,31,31,30,31,30,31];
val leapYear = [31,29,31,30,31,30,31,31,30,31,30,31];


(*#9*)
fun what_month(day: int) =
    number_before_reaching_sum(day, commonYear) + 1

fun what_month_leap_year(day: int) =
    number_before_reaching_sum(day, leapYear) + 1

(*#10*) 
fun month_range(day1:int, day2:int) =
    let fun countRange (x1 : int, x2: int) =
        if (x2 - x1) <= 0
        then [x2]
        else x1 :: countRange(x1 + 1, x2)
        fun map_day_to_month(days: int list) =
        if null days
        then []
        else what_month(hd(days)) :: map_day_to_month(tl(days))
    in
        if (day1 > day2)
        then []
        else map_day_to_month(countRange(day1, day2))
    end

(*#11*)
fun oldest(dates: date list) =
    let fun oldest_date(dates: date list, oldestSoFar: date) =
        if null dates then SOME oldestSoFar
        else if is_older(oldestSoFar, hd(dates)) then oldest_date(tl(dates), oldestSoFar)
        else oldest_date(tl(dates), hd(dates))
    in   
        if null dates
        then NONE
        else oldest_date(tl(dates), hd(dates))
    end 

fun remove_duplicates_from_list(elements: int list) =
    let fun remove_element_from_list(elements: int list, element2Remove: int, newElementList: int list) =
        if null elements then newElementList
        else if hd(elements) = element2Remove then remove_element_from_list(tl(elements), element2Remove,newElementList)
        else remove_element_from_list(tl(elements), element2Remove, hd(elements) :: newElementList)
        fun remove_duplicates_from_list_aux(elements: int list, newElements: int list) =
            if null elements 
            then newElements
            else 
                let val element2Remove = hd(elements)
                    val withoutElement2Remove = remove_element_from_list(tl(elements), element2Remove, [])
                in
                    remove_duplicates_from_list_aux(withoutElement2Remove, element2Remove :: newElements)
                end
    in    
        remove_duplicates_from_list_aux(elements, [])
    end

(*#12*)
fun number_in_months_challenge(dates: date list, months: int list) =
    number_in_months(dates, remove_duplicates_from_list(months))

fun dates_in_months_challenge(dates: date list, months: int list) =
    dates_in_months(dates, remove_duplicates_from_list(months))

        fun is_leap_year(year: int) = 
            ((year div 400 = 0) orelse (year div 4 = 0) andalso ((year div 100) <> 0))  


(*#13*)
fun reasonable_date(date: date) =
    let 
        fun is_leap_year(year: int) = 
            ((year mod 400 = 0) orelse ((year mod 4 = 0) andalso (year mod 100) <> 0))    
        val (year: int, month: int, day:int) = date
    in
        (year > 0) andalso
        (month >= 1 andalso month <= 12) andalso
        if (month = 1) then (day >= 1 andalso day <= 31)
        else if (month = 3) then (day >= 1 andalso day <= 31)
        else if (month = 4) then (day >= 1 andalso day <= 30)
        else if (month = 5) then (day >= 1 andalso day <= 31)
        else if (month = 6) then (day >= 1 andalso day <= 30)
        else if (month = 7) then (day >= 1 andalso day <= 31)
        else if (month = 8) then (day >= 1 andalso day <= 31)
        else if (month = 9) then (day >= 1 andalso day <= 30)
        else if (month = 10) then (day >= 1 andalso day <= 31)
        else if (month = 11) then (day >= 1 andalso day <= 30)
        else if (month = 12) then (day >= 1 andalso day <= 31)
        else 
            if is_leap_year(year) then (day >= 1 andalso day <= 29)
            else (day >= 1 andalso day <= 28)

    end 