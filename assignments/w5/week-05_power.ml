(* week-05_power.ml *)
(* Introduction to Computer Science (YSC1212), Sem2, 2020-2021 *)
(* Olivier Danvy <danvy@yale-nus.edu.sg> *)
(* Version of Sat 13 Feb 2021 *)

(* ********** *)

let silent = true;;  (* <-- redefine this flag to enable the displaying of error messages *)

(* ********** *)

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
            in let expected_result = 0
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
