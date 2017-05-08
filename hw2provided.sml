(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(st:string,st_list:string list)=
  case st_list of
      []=>NONE
    | x::lst =>case same_string(x,st) of
		       true=>SOME lst
		     | false =>case all_except_option(st,lst)of
				    NONE=>NONE
				   | SOME lst' => SOME (x::lst')
								     

fun get_substitutions1(st_lst_lst:string list list,s:string)=
  case st_lst_lst of
      []=>[]
    | x::xs =>case all_except_option(s,x) of
		  NONE=>get_substitutions1(xs,s)
		| SOME y => y@get_substitutions1(xs,s)

fun get_substitutions2(st_lst_lst:string list list,s:string)=
  case st_lst_lst of
      []=>[]
    | x::xs => case all_except_option(s,x) of
		   NONE=>get_substitutions2(xs,s)
		 | SOME y =>let fun append(y,acc)=
				  case y of
				      []=>acc
				    | h::tl =>h::append(tl,acc)
			    in append(y,get_substitutions2(xs,s))
			    end

fun similar_names(st_lst_lst:string list list,{first=first, middle=middle, last=last}) =
  let val first_names=first::get_substitutions1(st_lst_lst,first);
      fun helper(st_lst_lst)=
	  case st_lst_lst of
	  []=>[]
	   | x::xs =>{first=x, middle=middle, last=last}::helper(xs)

  in  helper(first_names)
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
fun card_color card =
    case card of
        (Clubs,_)    => Black
      | (Diamonds,_) => Red
      | (Hearts,_)   => Red
      | (Spades,_)   => Black
			    

fun card_value card =
  case card of
     (_, Num(i))=>i
    |(_, Ace)=>11
    |(_, _) =>10

fun remove_card(cs:card list,c:card,e)=
  let fun helper(cs:card list,c:card)=
  case cs of
      []=> []
     |x::xs  => case x=c of
		    true=>xs
		  | false => x::remove_card(xs,c,e)
      val result= helper(cs,c)
			
  in case result= cs of
	 true=>raise e
       | false =>result
  end
      
fun all_same_color(cs:card list)=
  case cs of
      []=>true
    | _::[] => true
    | head::(neck::rest) => (card_color(head)=card_color(neck)) andalso all_same_color(neck::rest)

fun  sum_cards(cs:card list)=
  let fun sum(cs:card list,acc:int)=
  case cs of
      []=>acc
     |x::xs  => sum(xs,acc+card_value(x))
  in sum(cs,0)
  end

fun score(held_cards:card list,goal:int)=
  let val sum=sum_cards(held_cards)
  in
      (if sum>goal then 3*(sum-goal) else (goal-sum))div( if all_same_color(held_cards) then 2 else 1)
  end
      
fun officiate(card_list:card list,move_list:move list,goal:int)=
  let fun play(held_cards:card list,move_list:move list,remain_cards:card list)=
	      case move_list of
		  []=>score(held_cards,goal)
		| x::xs' =>case x of
			       Discard (card) =>play(remove_card(held_cards,card,IllegalMove),xs',remain_cards)
			     | Draw => case remain_cards of
					   []=>score(held_cards,goal)
					 | y::y' =>let val  new_held_cards=y::held_cards
						   in
						       if sum_cards(new_held_cards)>goal
						       then
							   score(new_held_cards,goal)
						       else play(new_held_cards,xs',y')
						   end
  in
      play([],move_list,card_list)
  end
            
  

