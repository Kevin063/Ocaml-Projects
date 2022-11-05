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


(*Writing a line to a file*)
let write_file_example (file_path: string) (text: string): unit =
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

(* The recursive parse function *)
let rec parse (str : string) (path: string) : unit=
if (String.length str) ==0 then () else let ()=(print_string ((getone str 0)^"\n"))
in parse (removeone str) path;;

let interpreter (src : string) (output_file_path: string): unit =
  if (String.exists (fun c -> (c=='\n')) src) then write_file_example output_file_path
  else  print_string ""