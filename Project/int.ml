(* Honor code comes here:

   First Name: Hengyuan 
   Last Name: Liu
   BU ID: U14069299

   I pledge that this program represents my own program code and that I have
   coded on my own. I received help from no one in designing and debugging my
   program. I have read the course syllabus of CS 320 and have read the sections
   on Collaboration and Academic Misconduct. I also understand that I may be
   asked to meet the instructor or the TF for a follow up interview on Zoom. I
   may be asked to explain my solution in person and may also ask you to solve a
   related problem. *)

(*NOTE: There are no restrictions on what you can use*)
open Str;;
let testinput1 = "Fun f1 x\nPush y\nReturn\nMut f2 x\nPush x\nPush 2\nMul\nLocal x\nPush x\nPush f1\nCall\nReturn\nMut f3 y\nPush y\nPush 1\nAdd\nLocal x\nPush x\nPush f2\nCall\nReturn\nEnd\nPush 3\nPush f2\nCall\nQuit\n"
let testinput2 = "Fun regular x\nPush 11\nPush x\nTuple 2\nReturn\nEnd\nPush 22\nPush regular\nCall\nQuit\n"
let testinput3 = "Fun odd x\nPush x\nPush 2\nMul\nLocal x\nPush x\nPush 46\nEqual\nIfThen\nPush x\nReturn\nElse\nPush x\nPush even\nCall\nReturn\nEnd\nMut even x\nPush 1\nPush x\nAdd\nLocal x\nPush x\nPush odd\nCall\nReturn\nEnd\nPush 5\nPush odd\nCall\nQuit"
let testinput4 = "Fun f1 x\nPush x\nReturn\nMut f2 x\nPush x\nPush 2\nMul\nLocal x\nPush x\nPush f1\nCall\nReturn\nMut f3 x\nPush x\nPush 1\nAdd\nLocal x\nPush x\nPush f2\nCall\nReturn\nEnd\nPush 3\nPush f3\nCall\nQuit\n"
let testinput5 = "Fun numOfStepsToOne x\nPush numOfSteps\nPush 1\nAdd\nGlobal numOfSteps\nPush x\nPush 1\nAdd\nLocal x\nPush x\nPush divideByTwo\nCall\nReturn\nMut divideByTwo x\nPush numOfSteps\nPush 1\nAdd\nGlobal numOfSteps\nPush 2\nPush x\nDiv\nLocal x\nPush x\nPush 1\nEqual\nIfThen\nPush numOfSteps\nReturn\nElse\nPush x\nPush numOfStepsToOne\nCall\nReturn\nEnd\nEnd\nPush 0\nGlobal numOfSteps\nPush 5\nPush numOfStepsToOne\nCall\nQuit"
let testinput6 = "Fun snack laddoo\nPush laddoo\nPush 100\nMul\nLocal cal\nPush cal\nPush calories\nCall\nReturn\nMut calories cal\nPush 9\nPush cal\nDiv\nLocal g\nPush g\nPush sugar\nCall\nReturn\nMut sugar g\nPush g\nReturn\nEnd\nPush 9\nPush snack\nCall\nQuit"
let stringregex = Str.regexp "[a-zA-Z0-9\"-]+[a-zA-Z0-9\"]*+\"+$"
let idregex = Str.regexp "[a-zA-Z0-9\"-]+[a-zA-Z0-9\"]*+$"
let numregex = Str.regexp "[0-9-]+[0-9]*+$";;
type value =
  | Int of int
  | Str of string
  | Pro of (string list)*string*string*int
  | LUni of value*value
  | RUni of value*value
  | Tuple of value list
  | Err;;
(*Writing a line to a file*)
let write (file_path: string) (text: string): unit =
  let fp = open_out file_path in
  let () = Printf.fprintf fp "%s" text in
    close_out fp;;

let checkbool(input : int) : bool=
(if ((input=0)||(input=1)) then true else false);;

let int_to_bool(input : int) : bool=
if (input=0) then false else true;;

let bool_to_int (input : bool) : int=
if input then 1 else 0;;

(* Serach a var in the env *)
let rec search (localenv : (string*value) list) (globalenv : (string*value) list) (key : string) : value=
match localenv with 
| (k,v)::tl -> (if (k=key) then v else (search tl globalenv key))
| [] -> (
  match globalenv with
| (k2,v2)::tl2 -> (if (k2=key) then v2 else (search [] tl2 key))
| [] ->Str("Not Found")
);;

(* Serach all functions belonging to a clo in the env *)
let rec searchclo (localenv : (string*value) list) (cloversion : int) : (string*value) list=
match localenv with 
| (funame,Pro(fu,name,parameter,clv))::tl -> (if (clv=cloversion) then ((funame,Pro(fu,name,parameter,clv))::(searchclo tl cloversion)) else (searchclo tl cloversion))
| _::tl -> searchclo tl cloversion
| [] -> [] ;;

(* Change value of a existing var in the env *)
let rec change (env : (string*value) list)  (key : string) (va : value): (string*value) list=
match env with
| (k,v)::tl -> (if (k=key) then (k,va)::(change tl key va) else (k,v)::(change tl key va))
| [] -> [];;


let setenv (env : (string*value) list) (key : string) (v: value) : (string*value) list=
match search env [] key with
| Str("Not Found") -> (key,v)::env
| _ -> change env key v;;
(*When it comes to parsing src, you should find it useful that fold_left can be
  defined and used with strings (like String.fold_left in new OCaml version).
  See String.get. These are suggestions though and you are welcome
  to use what you want :)  *)
(* Get One action from the beginning of this str *)
let rec getone (str : string) (num : int): string=
if num= String.length str then "" else
match str.[num] with
| '\n' -> ""
| c -> (Char.escaped c)^(getone str (num+1))

(* Remove the first action from this str *)
let rec removeone (str : string): string=
if (String.length str) ==0 then "" else
match str.[0] with
| '\n' -> String.sub str 1 (String.length str-1)
| c -> removeone (String.sub str 1 (String.length str-1))

(* Build a tuple output based on tuple values*)
let rec buildouttuple (tuple : value list):string=
match tuple with
| [] -> ""
| line::tl -> (match line with 
| Int(i) -> string_of_int(i)
| Str(s) -> s
| Pro(_,name,para,_) -> "Clo ("^name^" "^para^")"
| LUni(a,b) -> (String.sub (buildout [a]) 0 (String.length (buildout [a])-1))^" "^(buildout [b])
| RUni(a,b) -> (String.sub (buildout [a]) 0 (String.length (buildout [a])-1))^" "^(buildout [b])
| _ -> "")^","^buildouttuple tl

(* Build a output based on actions *)
and buildout (stack : value list):string=
match stack with
| [] -> ""
| line::tl -> (match line with 
| Int(i) -> string_of_int(i)
| Str(s) -> s
| Pro(_,name,para,_) -> "Clo ("^name^" "^para^")"
| LUni(a,b) -> (String.sub (buildout [a]) 0 (String.length (buildout [a])-1))^" "^(buildout [b])
| RUni(a,b) -> (String.sub (buildout [a]) 0 (String.length (buildout [a])-1))^" "^(buildout [b])
| Tuple(t) -> "("^(String.sub (buildouttuple t) 0 (String.length (buildouttuple t)-1))^")"
| _ -> "")^"\n"^buildout tl

(* Push from a stack into another *)

let rec pushstack (source : value list) (stack : value list)=
match source with
| [] -> stack
| hd::tl -> pushstack tl (hd::stack);;

(*Check vaild Else from actions *)
let rec findelse (act : string list) : bool=
match act with 
| "Else"::tl1 -> true
| s::tl2 -> findelse tl2
| [] -> false

(*Check vaild Right from actions *)
let rec findright (act : string list) : bool=
match act with 
| "Right"::tl1 -> true
| s::tl2 -> findright tl2
| [] -> false

(*Delete until the first End from actions *)
let rec skipuntilend (act : string list) : string list=
match act with 
| "End"::tl1 -> tl1
| s::tl2 -> skipuntilend tl2
| [] -> []

(*Skip those actions that belonging to a Right condition *)
let rec skipright (act : string list) : string list=
match act with
| "Right"::tl2 -> skipuntilend tl2
| s::tl -> s::(skipright tl)
| [] -> []

(*Skip those actions that belonging to a false ifthen condition *)
let rec skipleft (act : string list) : string list=
match act with
| "Right"::tl2 -> skipend tl2 0
| s::tl -> skipleft tl
| [] -> []

(*Check vaild End from actions *)
let rec findend (act : string list) : bool=
match act with 
| "End"::tl1 -> true
| s::tl2 -> findend tl2
| [] -> false

(* Build a tuple with length n from stack *)
let rec buildtuple (stack : value list) (n : int):(value list)*(value list)=
match n with
|0 -> [],stack
|s -> (match stack with
|hd::tl -> (match buildtuple tl (s-1) with
| res,rem -> (hd::res),rem) 
|[] -> [],stack)


(*Delete the first End from actions *)
let rec skipend (act : string list) (nested : int): string list=
match act with
| "Begin"::tl1 -> "Begin"::(skipend tl1 (nested+1)) 
| "End"::tl1 -> if nested=0 then tl1 else skipend tl1 (nested-1)
| s::tl2 -> s::skipend tl2 nested
| [] -> []
(*Skip those actions that belonging to a true ifthen condition *)
let rec skiptrue (act : string list) : string list=
match act with
| "Else"::tl -> skipend tl 0
| _::tl2 -> skiptrue tl2
| [] -> []

(*Skip those actions that belonging to a false ifthen condition *)
let rec skipfalse (act : string list) : string list=
match act with
| "Else"::tl2 -> skipuntilend tl2
| s::tl -> s::(skipfalse tl)
| [] -> []

(* Execute one action from a actions list *)
let executeone (act : string) (stack :value list) (localenv : (string*value) list) (globalenv : (string*value) list): string*(value list)*((string*value) list)*((string*value) list)= 
match (String.sub act 0 2) with
(* | "Pus" -> (If (String.string_match (Str.regexp "[0-9]+$") (String.sub act 5 (String.length act-5)) 0) then ("",(Int(int_of_string(String.sub act 5 (String.length act-5))))::stack) else ("",(Str(String.sub act 5 (String.length act-5)))::stack)) *)
| "Pu" -> (if Str.string_match idregex (String.sub act 5 (String.length act-5)) 0 then 
  (match (Str.string_match stringregex (String.sub act 5 (String.length act-5)) 0)||(Str.string_match numregex (String.sub act 5 (String.length act-5)) 0) with
| true -> ("",(
  match int_of_string_opt(String.sub act 5 (String.length act-5)) with
  | Some num -> Int(num)
  | None -> Str((String.sub act 5 (String.length act-5)))
  )::stack,localenv,globalenv)
| false -> (
  match (search localenv globalenv (String.sub act 5 (String.length act-5))) with
  | Str("Not Found") -> ("\"Error\"",[],localenv,globalenv)
  | v -> "",v::stack,localenv,globalenv
  ) 
  ) 
 else ("\"Error\"",stack,localenv,globalenv))
| "Po" -> (
  match stack with
  | [] -> ("\"Error\"",[],localenv,globalenv)
  | ele::tl -> ("",tl,localenv,globalenv)
)
| "Ad" -> (
  match stack with
  | Int(num1)::Int(num2)::tl -> ("",Int(num1+num2)::tl,localenv,globalenv)
  | _ -> ("\"Error\"",stack,localenv,globalenv)
)
| "Su" -> (
  match stack with
  | Int(num1)::Int(num2)::tl -> ("",Int(num1-num2)::tl,localenv,globalenv)
  | _ -> ("\"Error\"",stack,localenv,globalenv)
)
| "Mu" -> (
  match stack with
  | Int(num1)::Int(num2)::tl -> ("",Int(num1*num2)::tl,localenv,globalenv)
  | _ -> ("\"Error\"",stack,localenv,globalenv)
)
| "Di" -> (
  match stack with
  | Int(num1)::Int(num2)::tl -> if num2=0 then ("\"Error\"",stack,localenv,globalenv) else ("",Int(num1/num2)::tl,localenv,globalenv)
  | _ -> ("\"Error\"",stack,localenv,globalenv)
)
| "Sw" -> (
  match stack with
  | h1::h2::tl -> ("",h2::h1::tl,localenv,globalenv)
  | _ -> ("\"Error\"",stack,localenv,globalenv)
)
| "Ne" -> (
  match stack with
  | Int(num1)::tl -> ("",Int(-num1)::tl,localenv,globalenv)
  | _ -> ("\"Error\"",stack,localenv,globalenv)
)
| "Co" -> (
  match stack with
  | Str(str1)::Str(str2)::tl -> ("",Str((String.sub str1 0 (String.length str1-1))^(String.sub str2 1 (String.length str2-1)))::tl,localenv,globalenv)
  | _ -> ("\"Error\"",stack,localenv,globalenv)
)
| "An" -> (
  match stack with
  | Int(num1)::Int(num2)::tl -> (
    if (checkbool(num1)&&checkbool(num2)) then "",Int(bool_to_int(int_to_bool(num1)&&int_to_bool(num2)))::tl,localenv,globalenv else ("\"Error\"",stack,localenv,globalenv)
  )
  | _ -> ("\"Error\"",stack,localenv,globalenv)
)
| "Or" -> (
  match stack with
  | Int(num1)::Int(num2)::tl -> (
    if (checkbool(num1)&&checkbool(num2)) then "",Int(bool_to_int(int_to_bool(num1)||int_to_bool(num2)))::tl,localenv,globalenv else ("\"Error\"",stack,localenv,globalenv)
  )
  | _ -> ("\"Error\"",stack,localenv,globalenv)
)
| "No" -> (
  match stack with
  | Int(num1)::tl -> (
    if (checkbool(num1)) then "",Int(bool_to_int(not (int_to_bool(num1))))::tl,localenv,globalenv else ("\"Error\"",stack,localenv,globalenv)
  )
  | _ -> ("\"Error\"",stack,localenv,globalenv)
)
| "Eq" -> (
  match stack with
  | Int(num1)::Int(num2)::tl -> (
    "",Int(bool_to_int(num1==num2))::tl,localenv,globalenv
  )
  | _ -> ("\"Error\"",stack,localenv,globalenv)
)
| "Lt" -> (
  match stack with
  | Int(num1)::Int(num2)::tl -> (
    "",Int(bool_to_int(num1<=num2))::tl,localenv,globalenv
  )
  | _ -> ("\"Error\"",stack,localenv,globalenv)
)
| "Lo" -> (
  match stack with
  | num::tl -> ("",tl,(setenv localenv (String.sub act 6 (String.length act-6)) num),globalenv)
  | _ -> ("\"Error\"",stack,localenv,globalenv)
)
| "Gl" -> (
  match stack with
  | num::tl -> ("",tl,localenv,(setenv globalenv (String.sub act 7 (String.length act-7)) num))
  | _ -> ("\"Error\"",stack,localenv,globalenv)
)
| "Tu"->(
  match int_of_string_opt((String.sub act 6 (String.length act-6))) with
  | None -> ("\"Error\"",stack,localenv,globalenv)
  | Some 0 -> "",stack,localenv,globalenv
  | Some num->(if (List.length stack) < num then ("\"Error\"",stack,localenv,globalenv) else (match (buildtuple stack num) with
  | tu,st-> "",(Tuple(List.rev tu)::st),localenv,globalenv) 
  )
)
| "Ge"-> (let n=int_of_string_opt((String.sub act 4 (String.length act-4))) in (match n with
  |None -> ("\"Error\"",stack,localenv,globalenv)
  |Some x->
    ( match stack with
  | Tuple(tu)::tl -> if (List.length tu <= x||x<0) then ("\"Error\"",stack,localenv,globalenv) else "",((List.nth tu x)::stack),localenv,globalenv
  | _ -> ("\"Error\"",stack,localenv,globalenv)
  )
)
)
| "In" ->(
  match act with
  | "InjL" -> (
    match stack with
    | ele::tl -> ("",LUni(Str("Left"),ele)::tl,localenv,globalenv)
    | _ -> ("\"Error\"",stack,localenv,globalenv)
  )
  | "InjR" -> (
    match stack with
    | ele::tl -> ("",RUni(Str("Right"),ele)::tl,localenv,globalenv)
    | _ -> ("\"Error\"",stack,localenv,globalenv)
  )
  | _ -> ("\"Error\"",stack,localenv,globalenv)
)
| _ -> ("\"Error\"",stack,localenv,globalenv)

(* Read and store a function from a actions list *)

let rec read (act : string list) : (string list)*(string list)=
match act with 
| ac:: rem ->(match String.sub ac 0 3 with
| "Mut" -> [],act
| "End" -> [],rem
| hd -> (match read rem with
| ["\"Error\""],_ ->  ["\"Error\""],act
| fu,acp -> ac::fu,acp))
|[] ->  ["\"Error\""],[];;


(* Execute a local part from a actions list *)
let rec executelocal (act : string list) (stack : value list) (localenv : (string*value) list) (globalenv : (string*value) list) (cloversion : int): string*(string list)*(value list)*((string*value) list)*((string*value) list)*int=
match act with
  | [] -> "\"Error\"",act,stack,localenv,globalenv,cloversion
  | "End"::tl1 -> ("",tl1,stack,localenv,globalenv,cloversion)
  | "Begin" :: tl -> (
    match (executelocal tl [] localenv globalenv cloversion) with
      | ("\"Error\"",_,_,_,_,_) -> "\"Error\"",tl,[],localenv,globalenv,cloversion
      | (strrem,actrem,stackrem,localrem,globalrem,clo) ->  (
        match stackrem with 
        | [] -> "\"Error\"",tl,[],localenv,globalenv,cloversion
        | hd::_ ->(
          match (executelocal actrem [hd] localenv globalrem clo) with
          | ("\"Error\"",_,_,_,_,_) -> "\"Error\"",tl,[],localenv,globalenv,cloversion
          | (str,act,st,lo,gl,cl) -> strrem^str,act,st,lo,gl,cl
        )
      )
  )
  | "Call"::tl ->(
  match stack with
  | Pro(act,name,para,cl)::arg::pp ->(
    match (executeclosure act [] ((searchclo localenv cl)@(para,arg)::localenv) globalenv cl) with
  | ("\"Error\"",_,_,_,_,_) -> "\"Error\"",tl,[],localenv,globalenv,cloversion
  | (strrem,actrem,stackrem,localrem,globalrem,clv) -> (
    match stackrem with 
    | [] -> "\"Error\"",tl,[],localenv,globalenv,cloversion
    | hd::_ -> executelocal tl (hd::pp) localenv globalrem clv
  )
  )
  | _ -> "\"Error\"",tl,[],localenv,globalenv,cloversion
)
  | "IfThen"::tl -> (
  if (findelse tl)&&(findend tl) then(
    match stack with
  | Int(0)::tl1 -> executelocal (skiptrue tl) tl1 localenv globalenv cloversion
  | Int(1)::tl2 -> executelocal (skipfalse tl) tl2 localenv globalenv cloversion
  | _::_ -> "\"Error\"",tl,[],localenv,globalenv,cloversion
  | [] -> "\"Error\"",tl,[],localenv,globalenv,cloversion
  )
  else "\"Error\"",tl,[],localenv,globalenv,cloversion
)
| "CaseLeft"::tl -> (
  if (findright tl)&&(findend tl) then(
    match stack with
  | LUni(_,c)::tl1 -> executelocal (skipright tl) (c::tl1) localenv globalenv cloversion
  | RUni(_,c)::tl1 -> executelocal (skipleft tl) (c::tl1) localenv globalenv cloversion
  | _::_ -> "\"Error\"",tl,[],localenv,globalenv,cloversion
  | [] -> "\"Error\"",tl,[],localenv,globalenv,cloversion
  )
  else "\"Error\"",tl,[],localenv,globalenv,cloversion
)
  | a::tl2 ->(
    if ((String.sub a 0 3="Fun")||(String.sub a 0 3="Mut")) then
      (match read tl2 with
      | ["\"Error\""],_ -> "\"Error\"",tl2,stack,globalenv,localenv,cloversion
      | fu,ac-> executelocal ac stack (setenv localenv (List.nth (Str.split (Str.regexp " ") a) 1) (Pro(fu,List.nth (Str.split (Str.regexp " ") a) 1,(List.nth (Str.split (Str.regexp " ") a) 2),(if (String.sub a 0 3="Fun") then (cloversion+1) else cloversion)))) globalenv (if (String.sub a 0 3="Fun") then (cloversion+1) else cloversion)
      )
      else
    match executeone a stack localenv globalenv with
    | ("\"Error\"",_,_,_) -> "\"Error\"",tl2,stack,globalenv,localenv,cloversion
    | (str,st,lo,gl)-> match (executelocal tl2 st lo gl cloversion) with
      | ("\"Error\"",_,_,_,_,_) -> "\"Error\"",tl2,st,localenv,globalenv,cloversion
      | (strrem,actrem,stackrem,localrem,globalrem,clv) ->  (str^strrem,actrem,stackrem,localenv,globalrem,clv)
  )

  (* Execute a closure part from a actions list *)
and executeclosure (act : string list) (stack : value list) (localenv : (string*value) list) (globalenv : (string*value) list) (cloversion : int): string*(string list)*(value list)*((string*value) list)*((string*value) list)*int=
match act with
  | [] -> "\"Error\"",act,stack,localenv,globalenv,cloversion
  | "Return"::tl1 -> ("",tl1,stack,localenv,globalenv,cloversion)
  | "Begin" :: tl -> (
    match (executelocal tl [] localenv globalenv cloversion) with
      | ("\"Error\"",_,_,_,_,_) -> "\"Error\"",tl,[],localenv,globalenv,cloversion
      | (strrem,actrem,stackrem,localrem,globalrem,clo) ->  (
        match stackrem with 
        | [] -> "\"Error\"",tl,[],localenv,globalenv,cloversion
        | hd::_ ->(
          match (executelocal actrem [hd] localenv globalrem clo) with
          | ("\"Error\"",_,_,_,_,_) -> "\"Error\"",tl,[],localenv,globalenv,cloversion
          | (str,act,st,lo,gl,cl) -> strrem^str,act,st,lo,gl,cl
        )
      )
  )
  | "Call"::tl ->(
  match stack with
  | Pro(act,name,para,cl)::arg::pp ->(
    match (executeclosure act [] ((searchclo localenv cl)@(para,arg)::localenv) globalenv cl) with
  | ("\"Error\"",_,_,_,_,_) -> "\"Error\"",tl,[],localenv,globalenv,cloversion
  | (strrem,actrem,stackrem,localrem,globalrem,clv) -> (
    match stackrem with 
    | [] -> "\"Error\"",tl,[],localenv,globalenv,cloversion
    | hd::_ -> executeclosure tl (hd::pp) localenv globalrem clv
  )
  )
  | _ -> "\"Error\"",tl,[],localenv,globalenv,cloversion
)
  | "IfThen"::tl -> (
    (* if (findelse tl)&&(findend tl) then( *)
  match stack with
  | Int(0)::tl1 -> executeclosure (skiptrue tl) tl1 localenv globalenv cloversion
  | Int(1)::tl2 -> executeclosure (skipfalse tl) tl2 localenv globalenv cloversion
  | _::_ -> "\"Error\"",tl,[],localenv,globalenv,cloversion
  | [] -> "\"Error\"",tl,[],localenv,globalenv,cloversion
  (* ) *)
  (* else "\"Error\"",tl,[],localenv,globalenv,cloversion *)
)
| "CaseLeft"::tl -> (
  if (findright tl)&&(findend tl) then(
    match stack with
  | LUni(_,c)::tl1 -> executeclosure (skipright tl) (c::tl1) localenv globalenv cloversion
  | RUni(_,c)::tl1 -> executeclosure (skipleft tl) (c::tl1) localenv globalenv cloversion
  | _::_ -> "\"Error\"",tl,[],localenv,globalenv,cloversion
  | [] -> "\"Error\"",tl,[],localenv,globalenv,cloversion
  )
  else "\"Error\"",tl,[],localenv,globalenv,cloversion
)
| a::tl2 ->
  (
    if ((String.sub a 0 3="Fun")||(String.sub a 0 3="Mut")) then
      (match read tl2 with
      | ["\"Error\""],_ -> "\"Error\"",tl2,stack,globalenv,localenv,cloversion
      | fu,ac-> executeclosure ac stack (setenv localenv (List.nth (Str.split (Str.regexp " ") a) 1) (Pro(fu,List.nth (Str.split (Str.regexp " ") a) 1,(List.nth (Str.split (Str.regexp " ") a) 2),(if (String.sub a 0 3="Fun") then (cloversion+1) else cloversion)))) globalenv (if (String.sub a 0 3="Fun") then (cloversion+1) else cloversion)
      )
      else
       match executeone a stack localenv globalenv with
  | ("\"Error\"",_,_,_) -> "\"Error\"",tl2,stack,globalenv,localenv,cloversion
  | (str,st,lo,gl)-> match (executeclosure tl2 st lo gl cloversion) with
    | ("\"Error\"",_,_,_,_,_) -> "\"Error\"",tl2,st,localenv,globalenv,cloversion
    | (strrem,actrem,stackrem,localrem,globalrem,clv) ->  (str^strrem,actrem,stackrem,localenv,globalrem,clv)
)


(* Execute all from a actions list *)
let rec execute (act : string list) (stack : value list) (localenv : (string*value) list) (globalenv : (string*value) list) (cloversion : int): string= 
match act with
| [] -> ""
| "Quit"::_ -> buildout stack
| "Begin"::tl ->(
  match (executelocal tl [] localenv globalenv cloversion) with
  | ("\"Error\"",_,_,_,_,_) -> "\"Error\""
  | (strrem,actrem,stackrem,localrem,globalrem,cl) -> (
    match stackrem with 
    | [] -> "\"Error\""
    | hd::_ -> strrem^(execute actrem (hd::stack) localenv globalrem cl)
  )
)
| "Call"::tl ->(
  match stack with
  | Pro(act,name,para,cl)::arg::pp ->(
    match (executeclosure act [] ((searchclo localenv cl)@(para,arg)::localenv) globalenv cl) with
  | ("\"Error\"",_,_,_,_,_) -> "\"Error\""
  | (strrem,actrem,stackrem,localrem,globalrem,clv) -> (
    match stackrem with 
    | [] -> "\"Error\""
    | hd::_ -> strrem^(execute tl (hd::pp) localenv globalrem clv)
  )
  )
  | _ -> "\"Error\""
)
| "IfThen"::tl -> (
  if (findelse tl)&&(findend tl) then(
    match stack with
  | Int(0)::tl1 -> execute (skiptrue tl) tl1 localenv globalenv cloversion
  | Int(1)::tl2 -> execute (skipfalse tl) tl2 localenv globalenv cloversion
  | _::_ -> "\"Error\""
  | [] -> "\"Error\""
  )
  else "\"Error\""
)
| "CaseLeft"::tl -> (
  if (findright tl)&&(findend tl) then(
    match stack with
  | LUni(_,c)::tl1 -> execute (skipright tl) (c::tl1) localenv globalenv cloversion
  | RUni(_,c)::tl1 -> execute (skipleft tl) (c::tl1) localenv globalenv cloversion
  | _::_ -> "\"Error\""
  | [] -> "\"Error\""
  )
  else "\"Error\""
)

| a::tl ->(
  if ((String.sub a 0 3="Fun")||(String.sub a 0 3="Mut")) then
  (match read tl with
  | ["\"Error\""],_ -> "\"Error\""
  | fu,ac-> execute ac stack (setenv localenv (List.nth (Str.split (Str.regexp " ") a) 1) (Pro(fu,List.nth (Str.split (Str.regexp " ") a) 1,(List.nth (Str.split (Str.regexp " ") a) 2),(if (String.sub a 0 3="Fun") then (cloversion+1) else cloversion)))) globalenv (if (String.sub a 0 3="Fun") then (cloversion+1) else cloversion)
  )
  else
    match (executeone a stack localenv globalenv) with 
    | ("\"Error\"",_,_,_) -> "\"Error\""
    | (output,st,lev,gev) -> (
      match (execute tl st lev gev cloversion) with
    | "\"Error\"" -> "\"Error\""
    | out -> output^out
    ));;


(* The recursive parse function *)
let rec parse (str : string) (path: string) (actions : string list): unit=
if (String.length str) ==0 then (write path (execute actions [] [] [] 0) ) else let newline=(getone str 0)
in parse (removeone str) path (actions@[newline]);;

let interpreter (src : string) (output_file_path: string): unit =
 parse src output_file_path []