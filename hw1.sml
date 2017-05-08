 fun is_older(fir:int*int*int, sec:int*int*int)=
     let 
        val y1 = #1 date1
        val m1 = #2 date1
        val d1 = #3 date1
        val y2 = #1 date2
        val m2 = #2 date2
        val d2 = #3 date2
    in
        y1 < y2 orelse (y1=y2 andalso m1 < m2)
                orelse (y1=y2 andalso m1=m2 andalso d1 < d2)
    end

	      
 fun number_in_month(dates:(int*int*int)list,month:int)=
   let fun count(dates:int*int*int,month:int)=
	 if month=(#2 dates) then 1 else 0	   
   in   if null dates
	then 0
	else count(hd dates,month)+number_in_month(tl dates,month)
   end
       
 fun number_in_months(dates:(int*int*int)list,month:int list)=
   if null month
   then 0
   else number_in_month(dates,hd month)+number_in_months(dates,tl month)
       
 fun dates_in_month(dates:(int*int*int)list,month:int)=
   if null dates
   then []
   else
       if   month=(#2 (hd dates))
       then (hd dates)::dates_in_month(tl dates,month)
       else dates_in_month(tl dates,month)
	 		  
 fun dates_in_months(dates:(int*int*int)list,months:int list)=
   if null months
   then []
   else dates_in_month(dates,(hd months))::dates_in_months(dates,(tl months))

							  
 fun get_nth(strings:string list,n:int)=
   if n=1
   then (hd strings)
   else if null (tl  strings)
        then (hd strings)
   else get_nth((tl strings),n-1)
	       
 fun date_to_string(date:int*int*int)=
   get_nth(["January", "February", "March","April","May", "June", "July", "August", "September", "October","November", "December"],(#2 date))
   ^" "
   ^Int.toString(#3 date)
   ^", "
   ^Int.toString(#1 date)

 fun number_before_reaching_sum(sum : int, lst : int list) =
    if sum <= hd lst
    then 0
    else 1 + number_before_reaching_sum(sum - hd lst, tl lst)

 fun what_month(day:int)=
   number_before_reaching_sum(day,[31,28,31,30,31,30,31,31,30,31,30,31])+1

 fun month_range(day1:int,day2:int)=
   if day1>day2
   then []
   else what_month(day1)::month_range(day1+1,day2)

 fun oldest(date_list:(int*int*int)list)=
   if null date_list
   then NONE
   else let fun dates_nonempty(date_list:(int*int*int)list)=
	      if null(tl date_list)
	      then hd date_list
	      else let val tl_ans=dates_nonempty(tl date_list)
		   in
		       if is_older(hd date_list,tl_ans)
		       then hd date_list
		       else tl_ans
		   end
	in SOME (dates_nonempty date_list)
	end

fun mem(x : int, xs : int list) =
    not (null xs) andalso (x = hd xs orelse mem(x, tl xs))
fun remove_duplicates(xs : int list) =
    if null xs
    then []
    else
        let 
            val tl_ans = remove_duplicates (tl xs)
        in
            if mem(hd xs, tl_ans)
            then tl_ans
            else (hd xs)::tl_ans
        end
	    
 fun number_in_months_challenge(dates:(int*int*int)list,month:int list)=
   number_in_months(dates,remove_duplicates(month))

 fun dates_in_months_challenge(dates:(int*int*int)list,months:int list)=
   dates_in_months(dates,remove_duplicates(months))

fun reasonable_date (date : int * int * int) =
    let    
        fun get_nth (lst : int list, n : int) =
        if n=1
        then hd lst
        else get_nth(tl lst, n-1)
        val year  = #1 date
        val month = #2 date
        val day   = #3 date
        val leap  = year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
        val feb_len = if leap then 29 else 28
        val lengths = [31,feb_len,31,30,31,30,31,31,30,31,30,31]
    in
        year > 0 andalso month >= 1 andalso month <= 12
        andalso day >= 1 andalso day <= get_nth(lengths,month)
    end
       
	    
