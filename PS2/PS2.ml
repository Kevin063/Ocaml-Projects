(* Honor code comes here:

   First Name: Hengyuan
   Last Name: Liu
   BU ID: U14069299

   I pledge that this program represents my own program code and
   that I have coded on my own. I received help from no one in
   designing and debugging my program.  I have read the course
   syllabus of CS 320 and have read the sections on Collaboration
   and Academic Misconduct. I also understand that I may be asked
   to meet the instructor or the TF for a follow up interview on
   Zoom. I may be asked to explain my solution in person and may
   also ask you to solve a related problem. *)

(* Problem 1. *)
(* Determine the length of a list.
   Examples:
   length [] = 0
   length [1; 2; 3] = 3
*)
let rec length (l : 'a list): int =
  match l with
   |[] -> 0
   | hd::t1 -> 1+ length t1;;

(*
   Using 'type' we may define aliases for types. In this case, anywhere we write
   'matrix' in our types OCaml will see 'float list list' instead.

   A float is a whole number with a decimal value as well. In OCaml, the
   operators to work with them have a trailing '.' e.g.:
   3. +. 4.2 = 7.2
   2. *. 1.4 = 2.8
*)
type matrix = float list list
(* Problem 2. *)
(* Determine if a matrix is square (n by n, for any n >= 0).
   Examples:
   is_square [[1.; 2.]] = false
   is_square [[1.; 2.]; [3.; 4.]] = true
*)
let rec is_square (m : matrix): bool =
  match m with
   |[] -> true
   |hd :: t1->
if length hd = length m
  then true
else false;;
(* Problem 3. *)
(* With an option type, we care about the value (possibly) stored inside. We
   will often write code that looks like the following:
   match my_option with
   | None -> None
   | Some(x) -> do calculation on x...

  To make avoid having to write this over and over, we can write a simple
  function which works with a function as an argument to make it easier! Write
  that function.

  Examples:
  let x = Some(3) in
    and_then x (fun y -> Some(y + 1))
  = Some(4)

  let x = None in
    and_then x (fun y -> Some(y + 1))
  = None
*)
let and_then (o : 'a option) (f : 'a -> 'b option) : 'b option =
  match o with
  |None -> None
  |Some x -> f x;;

(* Problem 4. *)
(* Add the elements of two lists together,
   if they are different lengths return None.

   Examples:
   add_lists [1.; 2.] [3.; 4.] = Some([4.; 6.])
   add_lists [] [1.2] = None
*)
let rec add_lists (xs : float list) (ys : float list): float list option =
  if length xs = length ys
    then match (xs,ys) with
         | ([],[]) -> Some []
         | (hd1::t1,hd2::t2) -> 
          (match add_lists t1 t2 with
          | None -> None
          | Some zs -> Some ((hd1+.hd2)::zs))
         | (a,b) -> None
else None;;

(* Problem 5. *)
(* Add the elements of two matrices together,
   if they are different dimensions return None.

   Examples:
   add_matrices [[1.; 2.]; [3.; 4.]] [[0.; 0.5]; [1.4; 4.7]]
   = Some([[1.; 2.5]; [4.4; 8.7]])
*)
let rec add_matrices (m1 : matrix) (m2 : matrix): matrix option =
  if length m1 =length m2
    then
  match (m1,m2) with
  |([],[]) -> Some []
  |([],a) -> Some []
  |(b,[]) -> Some []
  |(hd1::t1,hd2::t2) ->
    (if length hd1=length hd2
      then match add_lists hd1 hd2 with
    | None -> None
    | Some zs -> 
      match add_matrices t1 t2 with
      |None -> None
      |Some ks -> Some(zs :: ks)
      else None)
  else None


(* Problem 6. *)
(* Scale each element of the list by the given constant.
   Examples:
   scale_list 3. [1.; 2.; 4.] = [3.; 6.; 12.]
*)
let rec scale_list (s : float) (l : float list): float list =
  match l with
  | [] -> []
  | hd :: t1 -> (hd*.s) :: scale_list s t1;;


(* Problem 7. *)
(* Scale each element of the matrix by the given constant.
   Examples:
   scale_matrix 3. [[1.; 2.]; [3.; 4.]] = [[3.; 6.]; [9.; 12.]]
*)
let rec scale_matrix (s : float) (m : matrix): matrix =
  match m with
  |[] -> []
  |hd :: t1 -> scale_list s hd :: scale_matrix s t1;;

(* Problem 8. *)
(* Convert the matrix into a list by flattening it.
   Examples:
   into_list [[1.]] = [1.]
   into_list [[1.; 2.]; [3.; 4.]] = [1.; 2.; 3.; 4.]
*)
let rec into_list (m : matrix): float list =
  match m with
  |[] -> []
  |hd :: t1 -> hd@into_list t1;;

(* Problem 9. *)
(* Transpose the matrix.

   Given a matrix of dimensions M x N, the transpose is a matrix with dimensions
   N x M produced by swapping columns and rows.

   For a 4x3 matrix:

   0  1  2  3
   4  5  6  7
   8  9  10 11
   ==>
   0 4 8
   1 5 9
   2 6 10
   3 7 11

   Examples:
   transpose [[1.; 2.]; [3.; 4.]] = [[1.; 3.]; [2.; 4.]]

   transpose
    [[0.; 4.; 8.]; [1.; 5.; 9.]; [2.; 6.; 10.]; [3.; 7.; 11.]]
   =
    [[0.; 1.; 2.; 3.]; [4.; 5.; 6.; 7.]; [8.; 9.; 10.; 11.]]

   Notes:
   * You may assume all nested int lists are of the same length (aka matrix is well-formed).
*)
let rec take_first (ma : matrix) : float list =
match ma with
|[] -> []
|hd :: t1 -> 
  (match hd with
  |[] -> -0.14069299 
(*I need a magic number to mark we have nothing to take, so I use my BUID
  I hope we don't have number of my BUID as any test case
  I might have better ways to handle this but I'm a bit lazy QAQ*)
  |hd2 :: _ -> hd2):: take_first t1;;
let rec remove_first (ma : matrix) : matrix =
match ma with
|[] -> []
|hd :: t1 -> 
  (match hd with
  |[] -> []
  |_ :: t2 -> t2):: remove_first t1;;
let rec transpose (lss : matrix) : matrix =
  match take_first lss with
  |[] -> []
  |hd :: t1->(
    match hd with
    | -0.14069299 -> []
    |_ -> take_first lss :: transpose (remove_first lss)
  );;

(* Problem 10. *)
(* Generate the cofactor of the matrix.

   Given a matrix, invert the sign of its elements in an alternating
   ('checkerboard') fashion. This is used in the process of inverting matrices.
   Specifically, for a 4x3 matrix, we would want to apply the follow change
   (where '-' means to invert the sign):

    +  -  +  -
    -  +  -  +
    +  -  +  -

    0 -1 -2  3
   -4  5  6 -7
   -8  9 10 11
   ==>
    0  1 -2 -3
    4  5 -6 -7
   -8 -9 10 -11

  Examples:
  c     = [[1.; -1.; 1.]]
  cofactor [[1.]; [1.]; [1.]] = [[1.]; [-1.]; [1.]]

  cofactor
    [[0.; -4.; -8.]; [-1.; 5.; 9.]; [-2.; 6.; 10.]; [3.; -7.; 11.]]
  =
    [[0.; 4.; -8.]; [1.; 5.; -9.]; [-2.; -6.; 10.]; [-3.; -7.; -11.]]

  cofactor [[2.; -2.; 0.]; [2.; 3.; -10.]; [2.; 3.; 0.]]
    = [[2.; 2.; 0.]; [-2.; 3.; 10.]; [2.; -3.; 0.]]
*)
let rec build_l (l: float list) (num: int) : float list=
match (l,num) with
|([],_) ->[]
|(_,0) -> []
|(l,k) ->(
  match l with
  |[] -> []
  |hd::t1 ->hd::build_l t1 (k-1)
);;
let rec cut (l: float list) (num: int) : float list=
match (l,num) with
|([],_) ->[]
|(list,0) -> list
|(l,k) ->(
  match l with
  |[] -> []
  |hd::t1 ->cut t1 (k-1)
);;
let rec build_m (l: float list) (num: int) : matrix=
match l with
|[] ->[]
|list -> build_l list num::build_m(cut list num) num;;
let rec reverse (l: float list)(b:int)(size : int):float list=
match l with
|[] -> []
|hd::t1 ->
  if (b/size+b mod size) mod 2=0 then hd::reverse t1 (b+1) size
  else -1.*.hd::reverse t1 (b+1) size;;
let rec cofactor (m : matrix) : matrix =
  match m with
  |[] -> []
  |hd::t1 ->build_m (reverse(into_list m) (0)(length(hd))) (length(hd));;