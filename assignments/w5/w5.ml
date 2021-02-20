(***********************************************************************************)
(*Exercise 2*)
(***********************************************************************************)

let test_upper_string_of_letters candidate =
  let b0 = (candidate "abc" = "ABC")
  and b1 = (candidate "def" = "DEF")
  and b2 = (candidate "" = "")
  in b0 && b1 && b2;;

let test_lower_string_of_letters candidate =
  let b0 = (candidate "ABC" = "abc")
  and b1 = (candidate "DEF" = "def")
  and b2 = (candidate "" = "")
  in b0 && b1 && b2;;

        val test_upper_string_of_letters : (string -> string) -> bool = <fun>
         val test_lower_string_of_letters : (string -> string) -> bool = <fun>
let test_upper_string_of_letters candidate =
  let b0 = (candidate "abc" = "ABC")
  and b1 = (candidate "def" = "DEF")
  and b2 = (candidate "" = "")
  in b0 && b1 && b2;;

let test_lower_string_of_letters candidate =
  let b0 = (candidate "ABC" = "abc")
  and b1 = (candidate "DEF" = "def")
  and b2 = (candidate "" = "")
  in b0 && b1 && b2;;
        val test_upper_string_of_letters : (string -> string) -> bool = <fun>
           val test_lower_string_of_letters : (string -> string) -> bool = <fun>
let upper_string_of_letters c =
    let a = int_of_string "a"
    and n = int_of_string c
    and z = int_of_string "z"
    and d = int_of_string "a" - int_of_string "A"
    in let () = assert (a <= n && n <= z)
       in string_of_int (n - d);;

            val upper_string_of_letters : string -> string = <fun>
let () = assert (test_upper_string_of_letters upper_string_of_letters);;

Exception: Failure "int_of_string".

(*The following function maps a string of LOWERCASE letters to the corresponding string of UPPERCASE letters*)

let upper_letter c =
  let a = int_of_char 'a'
  and n = int_of_char c
  and z = int_of_char 'z'
  and d = int_of_char 'a' - int_of_char 'A'
  in let () = assert (a <= n && n <= z)
     in char_of_int (n - d);;

            val upper_letter : char -> char = <fun>
let test_upper_string_of_letters candidate =
  let b0 = (candidate "abc" = "ABC")
  and b1 = (candidate "def" = "DEF")
  and b2 = (candidate "" = "")
  in b0 && b1 && b2;;

        val test_upper_string_of_letters : (string -> string) -> bool = <fun>
let upper_string_of_letters s = String.map (fun c -> upper_letter c) s;;

val upper_string_of_letters : string -> string = <fun>
let () = assert (test_upper_string_of_letters upper_string_of_letters);;
(* The following function maps a string of UPPERCASE letters to the corresponding string of LOWERCASE letters*)

let lower_letter c =
  let a = int_of_char 'A'
  and n = int_of_char c
  and z = int_of_char 'Z'
  and d = int_of_char 'a' - int_of_char 'A'
  in let () = assert (a <= n && n <= z)
     in char_of_int (n + d);;

            val lower_letter : char -> char = <fun>
let test_lower_letter candidate =
  let a = (candidate 'A' = 'a')
  and z = (candidate 'Z' = 'z')
  in a && z;;

      val test_lower_letter : (char -> char) -> bool = <fun>
let () = assert (test_lower_letter lower_letter);;

let lower_string_of_letters s = String.map (fun c -> lower_letter c) s;;

val lower_string_of_letters : string -> string = <fun>
  let test_lower_string_of_letters candidate =
  let b0 = (candidate "ABC" = "abc")
  and b1 = (candidate "DEF" = "def")
  and b2 = (candidate "" = "")
  in b0 && b1 && b2;;

        val test_lower_string_of_letters : (string -> string) -> bool = <fun>
let () = assert (test_lower_string_of_letters lower_string_of_letters);;

(***********************************************************************************)
(*Exercise 6*)
(***********************************************************************************)
(*implementation*)

let show_quad show_yourself1 show_yourself2 show_yourself3 show_yourself4 (v1, v2, v3, v4) =
 (* show_quad : ('a -> string) -> ('b -> string) -> ('c -> string) -> ('d -> string) -> 'a * 'b * 'c * 'd -> string *)
  "(" ^ (show_yourself1 v1) ^ ", " ^ (show_yourself2 v2) ^ ", " ^ (show_yourself3 v3) ^ ", " ^ (show_yourself4 v4) ^ ")";;

(*illustration*)
show_quad show_int show_bool show_int show_bool (1, true, 54, false);;

show_quad show_char show_int show_string show_bool ('z', 1234, "qUaDrUpLes", false);;

(***********************************************************************************)
(*Exercise 11*)
(***********************************************************************************)

let silent = false;;  (* <-- redefine this flag to enable the displaying of error messages *)

(* power : int -> nat -> int *)

let test_power candidate =
  let b0 = (let expected_result = 100
            and actual_result = candidate 10 2
            in if actual_result = expected_result
               then true
               else let () = if silent
                             then ()
                             else Printf.printf
                                    "test_power failed in b0 for %d as base and %d as exponent with result %d instead of %d\n"
                                    10 2 actual_result expected_result
                    in false)
  and b1 = (let expected_result = 1024
            and actual_result = candidate 2 10
            in if actual_result = expected_result
               then true
               else let () = if silent
                             then ()
                             else Printf.printf
                                    "test_power failed in b1 for %d as base and %d as exponent with result %d instead of %d\n"
                                    2 10 actual_result expected_result
                    in false)
  and b2 = (let x = Random.int 100
            in let expected_result = 1
               and actual_result = candidate x 0
               in if actual_result = expected_result
                  then true
                  else let () = if silent 
                                then ()
                                else Printf.printf
                                       "test_power failed in b2 with %d as random base and 0 as exponent with result %d instead of %d\n"
                                       x actual_result expected_result
                    in false)
  and b3 = (let n = Random.int 15
            in let expected_result = if n = 0 then 1 else 0
               and actual_result = candidate 0 n
               in if actual_result = expected_result
                  then true
                  else let () = if silent
                                then () 
                                else Printf.printf
                                       "test_power failed in b3 for with 0 as base and %d as random exponent with result %d instead of %d\n"
                                       n actual_result expected_result
                       in false)
  and b4 = (let n = Random.int 15
            in let expected_result = 1
               and actual_result = candidate 1 n
               in if actual_result = expected_result
                  then true
                  else let () = if silent 
                                then ()
                                else Printf.printf
                                       "test_power failed in b4 with 1 as base and %d as random exponent with result %d instead of %d\n"
                                       n actual_result expected_result
                       in false)
  and b5 = (let x = Random.int 100
            in let actual_result = candidate x 1
               and expected_result = x
               in if actual_result = expected_result
                  then true
                  else let () = if silent 
                                then ()
                                else Printf.printf
                                       "test_power failed in b5 with %d as random base and 1 as exponent with result %d instead of %d\n"
                                       x actual_result expected_result
                       in false)
  and b6 = (let x = Random.int 100
            and m = Random.int 10
            and n = Random.int 10
            in let p1 = candidate x (m + n)
               and p2 = candidate x m * candidate x n
               in if p1 = p2
                  then true
                  else let () = if silent 
                                then ()
                                else Printf.printf
                                       "test_power failed in b6 with %d as random base and (%d + %d) as exponent because %d != %d\n"
                                       x m n p1 p2
                       in false)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6;;

(* ***** *)

(*
   base case:
     power x 0 = 1
   induction step:
     if power x n = r (which is the induction hypothesis)
     then power x (n + 1) = x * r
*)

let power_v0 x n_given =
  assert (n_given >= 0);
  let rec visit n =
    if n = 0
    then 1
    else let n' = pred n
         in let ih = visit n'
            in x * ih
  in visit n_given;;

let () = assert (test_power power_v0);;

let () = assert (if silent then test_power (fun x n -> 42) = false else true);;

(* ***** *)

let power_v1 x n_given =
  assert (n_given >= 0);
  let rec visit n =
    if n = 0
    then 1
    else x * visit (pred n)
  in visit n_given;;

let () = assert (test_power power_v1);;

(* ********** *)

(* end of week-05_power.ml *)

let end_of_file = "week-05_power.ml";;

(***********************************************************************************)
(*Exercise 10*)
(***********************************************************************************)

let test_oddp candidate =
      (* a few handpicked numbers: *)
  let b0 = (candidate 0 = false)
  and b1 = (candidate 1 = true)
  and b2 = (candidate 2 = false)
  and b3 = (candidate 3 = true)
  and b4 = (candidate 4 = false)
  and b5 = (candidate 5 = true)
  and b6 = (candidate 1000 = false)
  and b7 = (candidate 1001 = true)
      (* testing the completeness of the even predicate: *)
  and b8 = (let n = Random.int 1000
            in candidate (2 * n) = false)
  and b9 = (let n = Random.int 1000
            in candidate (2 * n + 1) = true)
  and bc = (candidate 0 = false)
      (* an instance of the induction step: *)
  and is = (let n' = Random.int 1000
            in candidate (succ n') = not (candidate n'))
      (* etc. *)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && bc &&
      is;;

let oddp_v0 n_given =
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then false
    else let n' = pred n
         in let ih = visit n'
            in not ih
in visit n_given;;

let () = assert (test_oddp oddp_v0);;

(***********************************************************************************)
