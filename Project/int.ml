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
let testinput1 = "Push 3\nPush 4\nPush 5\nPop\nAdd\nConcat\nQuit\nQuit\nQuit\nPush \"3\""
let testinput2 = "Push \"test\"\nPush 2\nPush 3\nPush 2\nPush 10\nDiv\nQuit"
let testinput3 = "Push \"chocolatechip\"\nPush \"cookie\"\nConcat\nQuit"
open Str
#load "str.cma";;
let idregex = Str.regexp "[a-zA-Z0-9\"]+[a-zA-Z0-9\"]*+$";;
type value =
  | Int of int
  | Str of string
  | Err
(*Writing a line to a file*)
let write (file_path: string) (text: string): unit =
  let fp = open_out file_path in
  let () = Printf.fprintf fp "%s" text in
    close_out fp

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

(* Execute one action from a actions list *)
let executeone (act : string) (stack :value list) : string*value list= 
match (String.sub act 0 3) with
(* | "Pus" -> (If (String.string_match (Str.regexp "[0-9]+$") (String.sub act 5 (String.length act-5)) 0) then ("",(Int(int_of_string(String.sub act 5 (String.length act-5))))::stack) else ("",(Str(String.sub act 5 (String.length act-5)))::stack)) *)
| "Pus" -> (if Str.string_match idregex (String.sub act 5 (String.length act-5)) 0 then ("",(
  match int_of_string_opt(String.sub act 5 (String.length act-5)) with
  | Some num -> Int(num)
  | None -> Str((String.sub act 5 (String.length act-5)))
  )::stack) else ("\"Error\"",stack))
| "Pop" -> (
  match stack with
  | [] -> ("\"Error\"",[])
  | ele::tl -> ("",tl)
)
| "Add" -> (
  match stack with
  | Int(num1)::Int(num2)::tl -> ("",Int(num1+num2)::tl)
  | _ -> ("\"Error\"",stack)
)
| "Sub" -> (
  match stack with
  | Int(num1)::Int(num2)::tl -> ("",Int(num1-num2)::tl)
  | _ -> ("\"Error\"",stack)
)
| "Mul" -> (
  match stack with
  | Int(num1)::Int(num2)::tl -> ("",Int(num1*num2)::tl)
  | _ -> ("\"Error\"",stack)
)
| "Div" -> (
  match stack with
  | Int(num1)::Int(num2)::tl -> ("",Int(num1/num2)::tl)
  | _ -> ("\"Error\"",stack)
)
| "Swa" -> (
  match stack with
  | h1::h2::tl -> ("",h2::h1::tl)
  | _ -> ("\"Error\"",stack)
)
| "Neg" -> (
  match stack with
  | Int(num1)::tl -> ("",Int(-num1)::tl)
  | _ -> ("\"Error\"",stack)
)
| "Con" -> (
  match stack with
  | Str(str1)::Str(str2)::tl -> ("",Str((String.sub str1 0 (String.length str1-1))^(String.sub str2 1 (String.length str2-1)))::tl)
  | _ -> ("\"Error\"",stack)
)
| _ -> ("",stack)

(* Execute all from a actions list *)
let rec execute (act : string list) (stack : value list): string= 
match act with
| [] -> ""
| "Quit"::_ -> buildout stack
| a::tl ->
  (
    match (executeone a stack) with 
    | ("\"Error\"",_) -> "\"Error\""
    | (output,st) -> (
      match (execute tl st) with
    | "\"Error\"" -> "\"Error\""
    | out -> output^out
    )
  )


(* The recursive parse function *)
let rec parse (str : string) (path: string) (actions : string list): unit=
if (String.length str) ==0 then (write path (execute actions []) ) else let newline=(getone str 0)
in parse (removeone str) path (actions@[newline]);;

let interpreter (src : string) (output_file_path: string): unit =
 parse src output_file_path []