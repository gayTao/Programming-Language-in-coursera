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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

fun only_capitals str_list=
  List.filter (fn x=> Char.isUpper(String.sub(x,0))) str_list
  
fun longest_string1 str_list=
  List.foldl (fn (x,acc)=>if String.size x>=String.size acc then x else acc) ""  str_list

fun longest_string2 str_list=
  List.foldl (fn (x,acc)=>if String.size x>String.size acc then x else acc) ""  str_list

fun longest_string_helper f  = List.foldl(fn(x,y) => if f( String.size(x), String.size (y) ) then y else x) ""
val longest_string3 =  longest_string_helper ( fn(x,y) => if ( x >= y ) then true else false) 
val longest_string4 = longest_string_helper ( fn(x,y) => if ( x > y) then true else false) 
  
val longest_capitalized = longest_string1 o only_capitals 

fun rev_string s=
  String.implode o List.rev o  String.explode

fun  first_answer f lst=
  case lst of
      []=>raise NoAnswer
    | x::xs =>case f x of
		  SOME v=>v
		| NONE => first_answer f xs

				       
fun all_answers f lst =
  let fun helper(f,acc,xs)=
  case xs of
      []=>SOME acc
    | x::xs => case f x of
		   SOME y=>helper(f,acc@y,xs)
		|  NONE => NONE
			       
  in
      helper(f,[],lst)
  end



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

      
fun count_wildcards p =
  g (fn x => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p =
  g (fn x => 1) (fn x => String.size(x)) p

fun count_some_var (s : string, p : pattern) =
  g (fn x => 0) (fn x => if x = s then 1 else 0) p

fun check_pat pat =
    let fun get_vars pat =
          case pat of
              Variable s => [s]
            | TupleP ps => List.foldl (fn (p,vs) => get_vars p @ vs) [] ps
            | ConstructorP(_,p) => get_vars p
            | _ => []
        fun unique xs =
          case xs of
              [] => true
            | x::xs' => (not (List.exists (fn y => y=x) xs'))
                        andalso unique xs'
    in
        unique (get_vars pat)
    end
	
fun match (valu,pat) =
    case (valu,pat) of
	      (_,Wildcard)    => SOME []
      | (_,Variable(s)) => SOME [(s,valu)]
      | (Unit,UnitP)    => SOME []
      | (Const i, ConstP j)    => if i=j then SOME [] else NONE
      | (Tuple(vs),TupleP(ps)) => if length vs = length ps
				  then all_answers match (ListPair.zip(vs,ps))
				   else NONE
      | (Constructor(s1,v), ConstructorP(s2,p)) => if s1=s2
				  then match(v,p)
                                  else NONE
      | _ => NONE



fun first_match v p_lst =
    SOME (first_answer (fn x => match(v, x)) p_lst) handle NoAnswer => NONE				     
