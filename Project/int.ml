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
let testinput1 = "Push 1\nLocal x\nPush 2\nLocal y\nBegin\nPush 20\nLocal x\nPush x\nPush y\nAdd\nEnd\nPush x\nAdd\nQuit\n"
let testinput2 = "Push 10\nLocal x\nPush 0\nIfThen\nPush 5\nAdd\nElse\nPush x\nPush 234\nLocal x\nPush x\nEnd\nPush x\nQuit\n"
let testinput3 = "Push \"red\"\nPush \"green\"\nPop\nPush 2934\nPop\nPush \"blue\"\nQuit"
let testinput4 = "Local x\nQuit\n"
let stringregex = Str.regexp "[a-zA-Z0-9\"-]+[a-zA-Z0-9\"]*+\"+$"
let idregex = Str.regexp "[a-zA-Z0-9\"-]+[a-zA-Z0-9\"]*+$"
let numregex = Str.regexp "[0-9-]+[0-9]*+$";;
type value =
  | Int of int
  | Str of string
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

(* Build a output based on actions *)
let rec buildout (stack : value list):string=
match stack with
| [] -> ""
| line::tl -> (match line with 
| Int(i) -> string_of_int(i)
| Str(s) -> s
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

(*Check vaild End from actions *)
let rec findend (act : string list) : bool=
match act with 
| "End"::tl1 -> true
| s::tl2 -> findend tl2
| [] -> false

(*Delete the first End from actions *)
let rec skipend (act : string list) : string list=
match act with 
| "End"::tl1 -> tl1
| s::tl2 -> s::skipend tl2
| [] -> []
(*Delete until the first End from actions *)
let rec skipuntilend (act : string list) : string list=
match act with 
| "End"::tl1 -> tl1
| s::tl2 -> skipuntilend tl2
| [] -> []
(*Skip those actions that belonging to a true ifthen condition *)
let rec skiptrue (act : string list) : string list=
match act with
| "Else"::tl -> skipend tl
| _::tl2 -> skipture tl2
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
| _ -> ("\"Error\"",stack,localenv,globalenv)

(* Execute a local part from a actions list *)
let rec executelocal (act : string list) (stack : value list) (localenv : (string*value) list) (globalenv : (string*value) list): string*(string list)*(value list)*((string*value) list)*((string*value) list)=
match act with
  | [] -> "\"Error\"",act,stack,localenv,globalenv
  | "End"::tl1 -> ("",tl1,stack,localenv,globalenv)
  | ac::tl2 ->(
    match executeone ac stack localenv globalenv with
    | ("\"Error\"",_,_,_) -> "\"Error\"",tl2,stack,globalenv,localenv
    | (str,st,lo,gl)-> match (executelocal tl2 st lo gl) with
      | ("\"Error\"",_,_,_,_) -> "\"Error\"",tl2,st,localenv,globalenv
      | (strrem,actrem,stackrem,localrem,globalrem) ->  (str^strrem,actrem,stackrem,localenv,globalrem)
  )

(* Execute all from a actions list *)
let rec execute (act : string list) (stack : value list) (localenv : (string*value) list) (globalenv : (string*value) list): string= 
match act with
| [] -> ""
| "Quit"::_ -> buildout stack
| "Begin"::tl ->(
  match (executelocal tl [] localenv globalenv) with
  | ("\"Error\"",_,_,_,_) -> "\"Error\""
  | (strrem,actrem,stackrem,localrem,globalrem) -> (
    match stackrem with 
    | [] -> "\"Error\""
    | hd::_ -> strrem^(execute actrem (hd::stack) localenv globalrem)
  )
)
| "IfThen"::tl -> (
  if (findelse tl)&&(findend tl) then(
    match stack with
  | Int(0)::tl1 -> execute (skiptrue tl) tl1 localenv globalenv
  | Int(1)::tl2 -> execute (skipfalse tl) tl2 localenv globalenv
  | _::_ -> "\"Error\""
  | [] -> "\"Error\""
  )
  else "\"Error\""
)
| a::tl ->
  (
    match (executeone a stack localenv globalenv) with 
    | ("\"Error\"",_,_,_) -> "\"Error\""
    | (output,st,lev,gev) -> (
      match (execute tl st lev gev) with
    | "\"Error\"" -> "\"Error\""
    | out -> output^out
    )
  );;


(* The recursive parse function *)
let rec parse (str : string) (path: string) (actions : string list): unit=
if (String.length str) ==0 then (write path (execute actions [] [] []) ) else let newline=(getone str 0)
in parse (removeone str) path (actions@[newline]);;

let interpreter (src : string) (output_file_path: string): unit =
 parse src output_file_path []